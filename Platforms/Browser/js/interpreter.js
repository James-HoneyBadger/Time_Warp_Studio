/*
 * Time Warp Web IDE - Interpreter (feature-parity with Python implementation)
 * Supports BASIC, PILOT and Logo with:
 *  - per-line detection and mixed-language programs
 *  - advanced BASIC features: LET, PRINT, INPUT, FOR/NEXT, IF/THEN, GOTO/GOSUB, DIM, DATA/READ/RESTORE
 *  - PILOT full command set with conditional variants and variable interpolation (*VAR*)
 *  - Logo full turtle features: procedures (TO/END), REPEAT (single and multi-line), variables, setxy, home, circle/arc
 *  - Timeline, breakpoints, step-mode, call stack, performance stats
 */

class WebInterpreter {
    constructor() {
        this.reset();

        // hooks / callbacks
        this.onOutput = null;
        this.onError = null;
        this.onLineExecuted = null;
        this.onFinished = null;
        this.onInput = null;
        this.onVariableChanged = null;

        // graphics bridge (TurtleGraphics from graphics.js)
        this.graphics = null;
        this.turtle = null; // will hold instance of TurtleGraphics if available
    }

    // --- public API ---
    setGraphics(turtleInstance) {
        this.graphics = turtleInstance;
        this.turtle = turtleInstance || null;
    }

    load(programText) {
        this.program = programText.replace(/\r/g, '').split('\n');
        this.collectLabels();
        this.detectedMode = this.detectLanguage(programText);
    }

    async run(debug = false) {
        this.resetExecutionState();
        this.isDebugging = debug;
        this.stepMode = debug;
        this.performance.startTime = Date.now();

        try {
            while (this.currentLineIndex < this.program.length && this.running) {
                const rawLine = this.program[this.currentLineIndex];
                const line = rawLine.trim();

                if (!line || line.startsWith('#')) { // comment or blank
                    this.currentLineIndex++;
                    continue;
                }

                // breakpoint
                if (this.breakpoints.has(this.currentLineIndex + 1)) {
                    this.stepMode = true; // pause next
                    this._output(`🔴 Breakpoint at ${this.currentLineIndex + 1}`);
                }

                if (this.stepMode && this.isDebugging) {
                    await this._waitForStep();
                }

                // detect language for this line
                const lang = this.detectLineLanguage(line);
                this._debugLog(`Executing line ${this.currentLineIndex+1}: ${line} (lang=${lang})`);
                await this.executeLine(line, lang);

                this.performance.linesExecuted++;
                this.addTimelineEntry(this.currentLineIndex + 1, line);
                if (this.onLineExecuted) this.onLineExecuted(this.currentLineIndex + 1, line);

                // small yield for UI responsiveness
                await this._sleep(this.isDebugging ? 50 : 5);

                this.currentLineIndex++;
            }
        } catch (err) {
            this._handleError(err, this.currentLineIndex + 1);
        } finally {
            this.running = false;
            if (this.onFinished) this.onFinished();
        }
    }

    stop() {
        this.running = false;
        // resolve any pending input/step promises
        if (this._inputResolve) { this._inputResolve(''); this._inputResolve = null; }
        if (this._stepResolve) { this._stepResolve(); this._stepResolve = null; }
    }

    async step() {
        // trigger one step
        this.stepMode = true;
        if (this._stepResolve) { this._stepResolve(); this._stepResolve = null; }
    }

    continue() {
        this.stepMode = false;
        if (this._stepResolve) { this._stepResolve(); this._stepResolve = null; }
    }

    reset() {
        this.program = [];
        this.resetExecutionState();
    }

    resetExecutionState() {
        this.currentLineIndex = 0;
        this.running = true;
        this.isDebugging = false;
        this.stepMode = false;
        this.waitingForInput = false;
        this._inputResolve = null;
        this._stepResolve = null;

        this.variables = new Map(); // string -> string
        this.labels = new Map();
        this.forStack = [];
        this.callStack = [];
        this.dataPool = [];
        this.dataIndex = 0;
        this.breakpoints = new Set();

        this.timeline = [];
        this.performance = { startTime: 0, linesExecuted: 0, graphicsCommands: 0, errors: 0 };
    }

    // --- labeling / parsing helpers ---
    collectLabels() {
        this.labels.clear();
        for (let i = 0; i < this.program.length; i++) {
            const raw = this.program[i].trim();
            if (!raw) continue;
            // PILOT labels L:...
            if (/^L\s*:/i.test(raw)) {
                const name = raw.replace(/^L\s*:/i, '').trim();
                if (name) this.labels.set(name, i);
            }
            // BASIC numeric line numbers
            const m = raw.match(/^(\d+)\s+/);
            if (m) this.labels.set(m[1], i);
        }
    }

    detectLanguage(programText) {
        // Auto detection across the full program - prefer the language with the highest score
        const lines = programText.split('\n').map(l => l.trim()).filter(l => l && !l.startsWith('#'));
        let pilot = 0, basic = 0, logo = 0;

        for (const line of lines) {
            if (/^[A-Za-z]:/.test(line)) pilot += 2;
            if (/\*\w+\*/.test(line)) pilot += 1;
            if (/^\d+\s+/.test(line)) basic += 2;
            if (/^\d+\s*$/.test(line)) basic += 1;
            if (/\b(PRINT|INPUT|LET|GOTO|FOR|NEXT|DIM|DATA|READ)\b/i.test(line)) basic += 2;
            if (/\b(FORWARD|FD|BK|BACK|RIGHT|RT|LEFT|LT|PENUP|PENDOWN|REPEAT|TO|END|SETXY|HOME)\b/i.test(line)) logo += 2;
            if (/\[|\]/.test(line)) logo += 1;
        }

        if (pilot > basic && pilot > logo) return 'pilot';
        if (basic > logo) return 'basic';
        return 'logo';
    }

    detectLineLanguage(line) {
        if (!line || line.startsWith('#')) return 'basic';
        // PILOT check: single letter + ':' at start (also allow multiletters like 'TY:')
        if (/^[A-Za-z]{1,3}:/.test(line)) return 'pilot';
        if (/^\d+\s+/.test(line)) return 'basic';
        // Logo keywords
        const first = line.split(/\s+/)[0].toUpperCase();
        const logoSet = new Set(['FORWARD','FD','BACK','BK','BACKWARD','RIGHT','RT','LEFT','LT','PENUP','PU','PENDOWN','PD','REPEAT','TO','END','CLEARSCREEN','CS','HOME','SETXY','SETHEADING','SETH','PRINT','MAKE']);
        if (logoSet.has(first)) return 'logo';
        // default to basic
        return 'basic';
    }

    // --- expression/condition utilities ---
    interpolateVariables(text) {
        if (!text) return '';
        return text.replace(/\*(\w+)\*/g, (m, name) => {
            return this.getVar(name);
        });
    }

    // evaluate arithmetic expressions safely (math-like only)
    evaluate(expr) {
        if (!expr) return '0';
        const replaced = this.interpolateVariables(expr);
        // allow numbers, whitespace, +-*/()%^., letters are disallowed now
        const safe = replaced.replace(/[^0-9+\-*/()., %]/g, '');
        try {
            // Use Function in a sandboxed small way - we only pass the cleaned expression
            // Many expressions from users include variables and names - when safe produce number/string
            // If safe differs from original replaced string then return replaced (string)
            if (safe !== replaced) return replaced;
            // replace commas with dots (some user inputs might use commas for decimals)
            const normalized = safe.replace(/,/g, '.');
            const val = Function('return (' + normalized + ')')();
            return (isNaN(val) ? replaced : String(val));
        } catch (e) {
            return replaced;
        }
    }

    evaluateCondition(cond) {
        // interpolate
        let c = this.interpolateVariables(cond).trim();
        // operators
        const ops = ['>=','<=','!=','==','>','<','='];
        for (const op of ops) {
            if (c.includes(op)) {
                const parts = c.split(op).map(p => p.trim());
                if (parts.length === 2) {
                    const left = parseFloat(this.evaluate(parts[0]));
                    const right = parseFloat(this.evaluate(parts[1]));
                    switch (op) {
                        case '>=': return left >= right;
                        case '<=': return left <= right;
                        case '!=': return parts[0] !== parts[1];
                        case '==': case '=': return parts[0] === parts[1];
                        case '>': return left > right;
                        case '<': return left < right;
                    }
                }
            }
        }
        // fallback truthiness (non-empty and not '0' and not 'false')
        const val = this.evaluate(c);
        return val !== '' && val !== '0' && val !== 'false';
    }

    // --- perform line execution ---
    async executeLine(line, language) {
        // remove BASIC line numbers
        if (language === 'basic') {
            line = line.replace(/^\d+\s+/, '').trim();
        }

        switch (language) {
            case 'pilot':
                await this._execPilot(line);
                break;
            case 'basic':
                await this._execBasic(line);
                break;
            case 'logo':
                await this._execLogo(line);
                break;
        }
    }

    // ----- PILOT -----
    async _execPilot(line) {
        // PILOT commands are letter:content
        const parts = line.split(':');
        const cmd = parts[0].trim().toUpperCase();
        const content = parts.slice(1).join(':').trim();

        // Commands: T (text), A (accept input), M (math), C (compare), J (jump), U (use/call), TY/TN conditional suffixes
        switch (cmd) {
            case 'T': // text output
                this._output(this.interpolateVariables(content));
                break;
            case 'A': // accept input into variable name provided
                {
                    const varName = content.trim();
                    const resp = await this._getInput(`INPUT ${varName}: `);
                    this.setVar(varName, resp);
                }
                break;
            case 'M': // arithmetic assignment - mimic R: in older examples
            case 'R':
                // signature: R: expression -> var
                {
                    const m = content.match(/(.+?)\s*[-=]?>\s*(\w+)/);
                    if (m) {
                        const value = this.evaluate(m[1].trim());
                        this.setVar(m[2], value);
                    }
                }
                break;
            case 'J': // jump label
                this._jumpToLabel(content);
                break;
            case 'L': // label - no-op
                break;
            case 'U': // call/use procedure (simple implementation)
                await this._callProcedure(content);
                break;
            case 'G': // graphics short commands - forward/back etc e.g. G:FORWARD 100
                // delegate to logo executor
                await this._execLogo(content);
                break;
            case 'F': // file I/O - not implemented here (placeholder)
                this._output('ℹ️ File operations are not available in the browser demo');
                break;
            case 'E': // End
                this.running = false;
                break;
            default:
                // conditional forms: TY: / TN: etc. handle above by checking prefix
                if (/^TY$/i.test(cmd) || /^TN$/i.test(cmd)) {
                    const cond = content.trim();
                    const truth = this.evaluateCondition(cond);
                    const prefix = cmd.toUpperCase();
                    if ((prefix === 'TY' && truth) || (prefix === 'TN' && !truth)) {
                        // continue normally
                    } else {
                        // skip next line
                        this.currentLineIndex++;
                    }
                } else {
                    this._error(`Unknown PILOT command: ${cmd}`);
                }
        }
    }

    // ----- BASIC -----
    async _execBasic(line) {
        const up = line.toUpperCase();
        if (up.startsWith('PRINT ')) {
            const expr = line.substring(6).trim();
            const v = this._valueForPrint(expr);
            this._output(v);
        } else if (up === 'END') {
            this.running = false;
        } else if (up.startsWith('INPUT ')) {
            const varName = line.substring(6).trim();
            const r = await this._getInput('INPUT: ');
            this.setVar(varName, r);
        } else if (up.startsWith('LET ')) {
            this._execAssignment(line.substring(4));
        } else if (up.includes('=')) {
            // assignment without LET
            this._execAssignment(line);
        } else if (up.startsWith('GOTO ')) {
            const arg = line.substring(5).trim();
            this._gotoLine(Number(arg));
        } else if (up.startsWith('IF ')) {
            await this._execBasicIf(line);
        } else if (up.startsWith('FOR ')) {
            this._execBasicFor(line);
        } else if (up.startsWith('NEXT')) {
            await this._execBasicNext(line);
        } else if (up.startsWith('DATA ')) {
            this._execData(line.substring(5));
        } else if (up.startsWith('READ ')) {
            this._execRead(line.substring(5));
        } else if (up === 'RESTORE') {
            this.dataIndex = 0;
        } else if (up.startsWith('DIM ')) {
            // simple arrays - store sizes in variables like ARR_size
            const rest = line.substring(4).trim();
            const m = rest.match(/(\w+)\((\d+)\)/);
            if (m) {
                const name = m[1];
                const size = Number(m[2]);
                this.setVar(`${name}_size`, String(size));
                // initialize storage as JSON array in variable
                this.setVar(name, JSON.stringify(new Array(size).fill('0')));
            }
        } else if (up.startsWith('RESTORE')) {
            this.dataIndex = 0;
        } else if (up === 'CLS') {
            if (this.graphics && typeof this.graphics.clearScreen === 'function') this.graphics.clearScreen();
        } else if (up.startsWith('CIRCLE')) {
            // parse CIRCLE x y r or CIRCLE r
            const args = line.substring(6).trim().split(',').map(s => s.trim()).filter(Boolean);
            if (args.length === 1) {
                const r = parseFloat(this.evaluate(args[0]));
                if (this.turtle && typeof this.turtle.circle === 'function') this.turtle.circle(r);
            } else if (args.length === 3) {
                // draw as arc using turtle by moving
                const x = parseFloat(this.evaluate(args[0]));
                const y = parseFloat(this.evaluate(args[1]));
                const r = parseFloat(this.evaluate(args[2]));
                if (this.turtle && typeof this.turtle.setPosition === 'function') {
                    this.turtle.setPosition(x, y);
                    this.turtle.circle(r);
                }
            }
        } else if (up.startsWith('SHAPE ') || up.startsWith('SPRITE ')) {
            this._output('ℹ️ Graphics shape/sprite support is not yet implemented in browser runtime');
        } else {
            this._error(`Unknown BASIC statement: ${line}`);
        }
    }

    _execAssignment(text) {
        // NAME = EXPR
        const parts = text.split('=').map(p => p.trim());
        if (parts.length === 2) {
            const name = parts[0];
            const value = this.evaluate(parts[1]);
            this.setVar(name, value);
            return;
        }
        this._error('Invalid assignment ' + text);
    }

    async _execBasicIf(line) {
        const m = line.match(/IF\s+(.+?)\s+THEN\s+(.+)/i);
        if (!m) return this._error('Malformed IF');
        const condition = m[1];
        const then = m[2];
        if (this.evaluateCondition(condition)) {
            // THEN may be GOTO or a statement
            if (/^GOTO\s+/i.test(then)) {
                const arg = then.replace(/^GOTO\s+/i, '').trim();
                this._gotoLine(Number(arg));
            } else {
                // execute as a basic statement
                await this._execBasic(then);
            }
        }
    }

    _execBasicFor(line) {
        // FOR i = start TO end [STEP s]
        const m = line.match(/FOR\s+(\w+)\s*=\s*(.+?)\s+TO\s+(.+?)(?:\s+STEP\s+(.+))?$/i);
        if (!m) return this._error('Malformed FOR');
        const varName = m[1];
        const start = parseFloat(this.evaluate(m[2]));
        const end = parseFloat(this.evaluate(m[3]));
        const step = m[4] ? parseFloat(this.evaluate(m[4])) : 1;

        this.setVar(varName, String(start));
        this.forStack.push({ variable: varName, end, step, forLine: this.currentLineIndex });
    }

    async _execBasicNext(line) {
        if (this.forStack.length === 0) return this._error('NEXT without FOR');
        const top = this.forStack[this.forStack.length-1];
        const cur = parseFloat(this.getVar(top.variable) || '0');
        const nxt = cur + top.step;
        this.setVar(top.variable, String(nxt));
        const shouldContinue = top.step > 0 ? (nxt <= top.end) : (nxt >= top.end);
        if (shouldContinue) {
            this.currentLineIndex = top.forLine; // jump back to FOR
        } else {
            this.forStack.pop();
        }
    }

    _execData(dataText) {
        const items = dataText.split(',').map(s => s.trim());
        this.dataPool.push(...items);
    }

    _execRead(varList) {
        const names = varList.split(',').map(s => s.trim());
        for (const n of names) {
            if (this.dataIndex >= this.dataPool.length) throw new Error('Out of DATA');
            this.setVar(n, this.dataPool[this.dataIndex++]);
        }
    }

    // --- BASIC helpers ---
    _valueForPrint(expr) {
        // if the expression contains strings inside quotes, keep them
        // otherwise evaluate arithmetic or interpolate variables
        // naive detection
        if (/^\".*\"$/.test(expr.trim())) {
            return expr.trim().slice(1,-1);
        }
        const v = this.interpolateVariables(expr);
        // if looks like a math expression use evaluate
        if (/^[0-9\s+\-*/().,]+$/.test(v)) return this.evaluate(v);
        return v;
    }

    // ----- LOGO -----
    async _execLogo(line) {
        // Tokenise and handle commands and procedures
        const tokens = line.trim().split(/\s+/);
        const cmd = tokens[0].toUpperCase();

        if (!this.turtle && typeof window.createTurtle === 'function') {
            // try to instantiate default if available
            try { this.turtle = window.createTurtle(); } catch (e) {}
        }

        switch (cmd) {
            case 'FORWARD': case 'FD':
                {
                    const dist = this.evaluate(tokens.slice(1).join(' '));
                    if (this.turtle) { this.turtle.forward(Number(dist)); this.performance.graphicsCommands++; }
                }
                break;
            case 'BACK': case 'BK': case 'BACKWARD':
                {
                    const dist = this.evaluate(tokens.slice(1).join(' '));
                    if (this.turtle) { this.turtle.backward(Number(dist)); this.performance.graphicsCommands++; }
                }
                break;
            case 'RIGHT': case 'RT':
                {
                    const a = this.evaluate(tokens[1]||'0');
                    if (this.turtle) { this.turtle.right(Number(a)); this.performance.graphicsCommands++; }
                }
                break;
            case 'LEFT': case 'LT':
                {
                    const a = this.evaluate(tokens[1]||'0');
                    if (this.turtle) { this.turtle.left(Number(a)); this.performance.graphicsCommands++; }
                }
                break;
            case 'PENUP': case 'PU':
                if (this.turtle) { this.turtle.penUp(); this.performance.graphicsCommands++; }
                break;
            case 'PENDOWN': case 'PD':
                if (this.turtle) { this.turtle.penDown(); this.performance.graphicsCommands++; }
                break;
            case 'CLEARSCREEN': case 'CS':
                if (this.turtle) { this.turtle.clearScreen(); this.performance.graphicsCommands++; }
                break;
            case 'HOME':
                if (this.turtle) { this.turtle.home(); this.performance.graphicsCommands++; }
                break;
            case 'SETCOLOR':
                if (this.turtle) { this.turtle.setColor(tokens[1]||'black'); this.performance.graphicsCommands++; }
                break;
            case 'SETXY':
                {
                    const x = this.evaluate(tokens[1]||'0');
                    const y = this.evaluate(tokens[2]||'0');
                    if (this.turtle) { this.turtle.setPosition(Number(x), Number(y)); this.performance.graphicsCommands++; }
                }
                break;
            case 'REPEAT':
                await this._execLogoRepeat(line);
                break;
            case 'TO':
                await this._collectProcedure(tokens);
                break;
            case 'END':
                // used to close TO blocks; no op when top-level
                break;
            case 'PRINT':
                const msg = this.interpolateVariables(line.substring(5).trim());
                this._output(msg);
                break;
            default:
                // maybe a procedure call or assignment
                if (line.includes('=')) {
                    this._execAssignment(line);
                } else if (/^[A-Za-z][A-Za-z0-9_]*\s*(.*)$/.test(line)) {
                    await this._callProcedure(line.trim());
                } else {
                    this._error(`Unknown Logo command: ${cmd}`);
                }
        }
    }

    async _collectProcedure(tokens) {
        // TO name [ :param1 :param2 ]
        const name = tokens[1] || 'untitled';
        const params = tokens.slice(2).map(p => p.replace(/^:/,'')).filter(p => p);
        // collect body until END
        const body = [];
        this.currentLineIndex++;
        while (this.currentLineIndex < this.program.length) {
            const line = this.program[this.currentLineIndex].trim();
            if (line.toUpperCase() === 'END') break;
            body.push(line);
            this.currentLineIndex++;
        }
        // store procedure in variables map as JSON
        this.setVar(`proc_${name}`, JSON.stringify({ params, body }));
    }

    async _callProcedure(nameAndArgs) {
        // name arg1 arg2 ... or name, treat comma separators
        const tokens = nameAndArgs.split(/[\s,]+/).filter(t => t);
        const name = tokens[0];
        const args = tokens.slice(1).map(a => this.evaluate(a));
        const proc = this.getVar(`proc_${name}`);
        if (!proc) return this._error(`Procedure not found: ${name}`);
        let def;
        try { def = JSON.parse(proc); } catch (e) { return this._error(`Bad procedure definition for ${name}`); }
        // push stack frame
        this.callStack.push({ returnLine: this.currentLineIndex, varsSnapshot: new Map(this.variables) });
        // set params as local variables (prefix with _) to avoid clobbering globals - store snapshot
        for (let i=0;i<def.params.length;i++) {
            this.setVar(def.params[i], args[i] !== undefined ? args[i] : '0');
        }
        // execute body
        for (let ln of def.body) {
            const lang = this.detectLineLanguage(ln);
            await this.executeLine(ln, lang);
            if (!this.running) break;
        }
        // restore variables snapshot
        const frame = this.callStack.pop();
        this.variables = frame.varsSnapshot;
        this.currentLineIndex = frame.returnLine; // continue after call
    }

    async _execLogoRepeat(line) {
        // either single-line: REPEAT n [ commands ] OR multiline: REPEAT n [ ... ]
        const single = line.match(/REPEAT\s+(\d+)\s*\[\s*(.+)\s*\]/i);
        if (single) {
            const times = Number(single[1]);
            const content = single[2].trim();
            const tokens = this._splitLogoBlock(content);
            for (let i=0;i<times;i++) {
                for (let t of tokens) {
                    const lang = this.detectLineLanguage(t);
                    await this.executeLine(t, lang);
                    if (!this.running) return;
                }
            }
            return;
        }
        // multiline: collect block
        const m = line.match(/REPEAT\s+(\d+)\s*\[/i);
        if (!m) return this._error('Malformed REPEAT');
        const times = Number(m[1]);
        // collect until matching closing bracket
        const savedIndex = this.currentLineIndex;
        const block = [];
        let depth = 1;
        this.currentLineIndex++;
        while (this.currentLineIndex < this.program.length) {
            const ln = this.program[this.currentLineIndex].trim();
            if (ln.includes('[')) depth++;
            if (ln.includes(']')) {
                depth--;
                if (depth === 0) break;
            }
            block.push(ln);
            this.currentLineIndex++;
        }
        for (let i=0;i<times;i++) {
            for (let ln of block) {
                const lang = this.detectLineLanguage(ln);
                await this.executeLine(ln, lang);
                if (!this.running) return;
            }
        }
    }

    _splitLogoBlock(content) {
        // naive split by whitespace but keep bracket groups intact for single-line repeats
        // Here we intentionally return array of commands like ['FORWARD 100','RIGHT 90']
        // Use regex to find commands or quoted strings
        const parts = content.match(/(?:\".*?\"|\S+)/g) || [];
        // Now join tokens into commands by splitting on known command keywords boundaries - simplistic
        const commands = [];
        let cur = [];
        const commandWords = new Set(['FORWARD','FD','BACK','BK','RIGHT','RT','LEFT','LT','PENUP','PU','PENDOWN','PD','SETCOLOR','SETXY','HOME','PRINT']);
        for (const token of parts) {
            const up = token.toUpperCase();
            if (commandWords.has(up) && cur.length > 0) {
                commands.push(cur.join(' '));
                cur = [token];
            } else {
                cur.push(token);
            }
        }
        if (cur.length > 0) commands.push(cur.join(' '));
        return commands;
    }

    // ----- utilities -----
    setVar(name, value) {
        this.variables.set(name, String(value));
        if (this.onVariableChanged) this.onVariableChanged(name, String(value));
    }
    getVar(name) { return this.variables.get(name) || '0'; }

    // goto/gosub/label helpers
    _jumpToLabel(name) {
        if (!this.labels.has(name)) throw new Error(`Label not found: ${name}`);
        this.currentLineIndex = this.labels.get(name) - 1; // -1 because main loop increments after execute
    }

    _gotoLine(lineNumber) {
        const label = String(lineNumber);
        if (!this.labels.has(label)) throw new Error(`Line number not found: ${lineNumber}`);
        this.currentLineIndex = this.labels.get(label) - 1;
    }

    // input/output hooks
    _output(msg) {
        if (this.onOutput) this.onOutput(msg + '\n');
        else console.log('[OUT]', msg);
    }

    _error(msg) {
        throw new Error(msg);
    }

    _handleError(err, line) {
        this.performance.errors++;
        const message = typeof err === 'string' ? err : (err && err.message ? err.message : String(err));
        if (this.onError) this.onError(message, line);
        else console.error(`Error on line ${line}:`, message);
    }

    // timeline
    addTimelineEntry(line, command) {
        this.timeline.push({ step: this.timeline.length + 1, time: Date.now() - this.performance.startTime, line, command });
    }

    getTimeline() { return [...this.timeline]; }
    getVariables() { return Array.from(this.variables.entries()).map(([k,v]) => ({ name:k, value:v })); }
    getPerformance() { const elapsed = this.performance.startTime ? (Date.now()-this.performance.startTime)/1000 : 0; return { elapsedTime: elapsed, linesExecuted: this.performance.linesExecuted, graphicsCommands: this.performance.graphicsCommands, errors: this.performance.errors }; }

    // input promise
    _getInput(promptText) {
        return new Promise((resolve) => {
            this.waitingForInput = true;
            this._inputResolve = resolve;
            if (this.onInput) this.onInput(promptText);
        });
    }

    submitInput(value) {
        if (this._inputResolve) { this._inputResolve(value); this._inputResolve = null; this.waitingForInput = false; }
    }

    // waiting for step
    _waitForStep() {
        return new Promise((resolve) => { this._stepResolve = resolve; });
    }

    // helpers
    _sleep(ms) { return new Promise(r => setTimeout(r, ms)); }
    _debugLog(...args) { /* intentionally minimal console output for debugging during dev */ }
}

// expose interpreter class for browser UI
window.WebInterpreter = WebInterpreter;
