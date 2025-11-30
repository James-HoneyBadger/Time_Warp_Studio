/**
 * Time Warp Web IDE - Comprehensive Interpreter
 * Supports PILOT, BASIC, and Logo programming languages
 */

// WORKING TURTLE GRAPHICS - Simple, reliable turtle that ACTUALLY WORKS
class WorkingTurtle {
    constructor(canvas) {
        this.canvas = canvas;
        this.ctx = canvas.getContext('2d');
        this.x = canvas.width / 2;
        this.y = canvas.height / 2;
        this.angle = 90; // 90 = up (Logo standard), 0 = right, 180 = left, 270 = down
        this.penDown = true;
        this.color = 'blue';
        this.penWidth = 2;

        console.log(`🐢 Turtle initialized at (${this.x}, ${this.y}) facing ${this.angle}°`);
        this.clearCanvas();
    } clearCanvas() {
        this.ctx.fillStyle = 'white';
        this.ctx.fillRect(0, 0, this.canvas.width, this.canvas.height);

        // Draw grid
        this.ctx.strokeStyle = '#f0f0f0';
        this.ctx.lineWidth = 1;
        for (let x = 0; x <= this.canvas.width; x += 20) {
            this.ctx.beginPath();
            this.ctx.moveTo(x, 0);
            this.ctx.lineTo(x, this.canvas.height);
            this.ctx.stroke();
        }
        for (let y = 0; y <= this.canvas.height; y += 20) {
            this.ctx.beginPath();
            this.ctx.moveTo(0, y);
            this.ctx.lineTo(this.canvas.width, y);
            this.ctx.stroke();
        }

        // Draw center lines
        this.ctx.strokeStyle = '#e0e0e0';
        this.ctx.lineWidth = 2;
        this.ctx.beginPath();
        this.ctx.moveTo(this.canvas.width / 2, 0);
        this.ctx.lineTo(this.canvas.width / 2, this.canvas.height);
        this.ctx.moveTo(0, this.canvas.height / 2);
        this.ctx.lineTo(this.canvas.width, this.canvas.height / 2);
        this.ctx.stroke();

        this.drawTurtle();
    }

    forward(distance) {
        // Convert angle to radians - Logo standard: 0°=right, 90°=up, 180°=left, 270°=down
        const radians = this.angle * Math.PI / 180;
        const newX = this.x + Math.cos(radians) * distance;
        const newY = this.y - Math.sin(radians) * distance; // Negative Y because screen coords have Y increasing downward

        console.log(`🐢 Forward ${distance}: (${this.x.toFixed(1)}, ${this.y.toFixed(1)}) → (${newX.toFixed(1)}, ${newY.toFixed(1)}) at ${this.angle}°`);

        if (this.penDown) {
            this.ctx.beginPath();
            this.ctx.moveTo(this.x, this.y);
            this.ctx.lineTo(newX, newY);
            this.ctx.strokeStyle = this.color;
            this.ctx.lineWidth = this.penWidth;
            this.ctx.stroke();
            console.log(`🖊️ Drew line from (${this.x.toFixed(1)}, ${this.y.toFixed(1)}) to (${newX.toFixed(1)}, ${newY.toFixed(1)})`);
        }

        this.x = newX;
        this.y = newY;
        this.drawTurtle();
    }

    right(degrees) {
        this.angle = (this.angle - degrees) % 360;
        this.drawTurtle();
    }

    left(degrees) {
        this.angle = (this.angle + degrees) % 360;
        this.drawTurtle();
    }

    penup() {
        this.penDown = false;
    }

    pendown() {
        this.penDown = true;
    }

    setcolor(color) {
        this.color = color;
    }

    home() {
        this.x = this.canvas.width / 2;
        this.y = this.canvas.height / 2;
        this.angle = 90; // Face up (Logo standard)
        console.log(`🏠 Turtle home: (${this.x}, ${this.y}) facing ${this.angle}°`);
        this.drawTurtle();
    }

    drawTurtle() {
        // Clear old turtle area
        this.ctx.save();
        this.ctx.fillStyle = 'white';
        this.ctx.fillRect(this.x - 15, this.y - 15, 30, 30);

        // Redraw grid lines in turtle area
        this.ctx.strokeStyle = '#f0f0f0';
        this.ctx.lineWidth = 1;
        const startX = Math.floor((this.x - 15) / 20) * 20;
        const endX = Math.ceil((this.x + 15) / 20) * 20;
        const startY = Math.floor((this.y - 15) / 20) * 20;
        const endY = Math.ceil((this.y + 15) / 20) * 20;

        for (let x = startX; x <= endX; x += 20) {
            this.ctx.beginPath();
            this.ctx.moveTo(x, this.y - 15);
            this.ctx.lineTo(x, this.y + 15);
            this.ctx.stroke();
        }
        for (let y = startY; y <= endY; y += 20) {
            this.ctx.beginPath();
            this.ctx.moveTo(this.x - 15, y);
            this.ctx.lineTo(this.x + 15, y);
            this.ctx.stroke();
        }

        // Draw turtle - bigger and more visible
        this.ctx.translate(this.x, this.y);
        // Rotate to match turtle's heading (subtract 90° because default turtle points right, but we want 0° to be up)
        this.ctx.rotate((this.angle - 90) * Math.PI / 180);

        this.ctx.beginPath();
        this.ctx.moveTo(12, 0);  // Nose of turtle
        this.ctx.lineTo(-8, -8); // Left back
        this.ctx.lineTo(-4, 0);  // Back center
        this.ctx.lineTo(-8, 8);  // Right back
        this.ctx.closePath();

        this.ctx.fillStyle = this.penDown ? '#e74c3c' : '#95a5a6'; // Red when pen down, gray when up
        this.ctx.fill();
        this.ctx.strokeStyle = 'black';
        this.ctx.lineWidth = 2;
        this.ctx.stroke();

        this.ctx.restore();
    }
}

class TimeWarpInterpreter {
    constructor() {
        this.variables = new Map();
        this.labels = new Map();
        this.programLines = [];
        this.currentLine = 0;
        this.isRunning = false;
        this.isDebugging = false;
        this.languageMode = 'auto';
        this.callStack = [];
        this.forLoopStack = [];
        this.dataList = [];
        this.dataPointer = 0;

        // Performance tracking
        this.performance = {
            startTime: 0,
            linesExecuted: 0,
            graphicsCommands: 0,
            errors: 0
        };

        // Execution history
        this.timeline = [];

        // Event callbacks
        this.onOutput = null;
        this.onVariableChanged = null;
        this.onLineExecuted = null;
        this.onError = null;
        this.onFinished = null;
        this.onInput = null;

        // Input handling
        this.waitingForInput = false;
        this.inputCallback = null;

        // Breakpoints
        this.breakpoints = new Set();
        this.stepMode = false;

        // Graphics integration - WORKING TURTLE
        this.graphics = null;
        this.turtle = null;
    }

    setGraphics(graphicsEngine) {
        this.graphics = graphicsEngine;
        this.initWorkingTurtle();
    }

    initWorkingTurtle() {
        const canvas = document.getElementById('graphicsCanvas');
        if (canvas) {
            this.turtle = new WorkingTurtle(canvas);
            console.log('✅ WORKING Turtle Graphics initialized in interpreter!');
        } else {
            console.error('❌ graphicsCanvas not found for turtle graphics');
        }
    }

    // Helper to send output to the UI
    output(message) {
        if (this.onOutput) {
            this.onOutput(message + '\n');
        }
        console.log('Turtle:', message);
    }

    // Language Detection
    detectLanguage(program) {
        if (this.languageMode !== 'auto') {
            return this.languageMode;
        }

        const lines = program.split('\n').map(line => line.trim()).filter(line => line && !line.startsWith('#'));

        let pilotScore = 0;
        let basicScore = 0;
        let logoScore = 0;

        for (const line of lines) {
            // PILOT patterns
            if (/^[TARUYJLEMNCS]:/i.test(line)) pilotScore += 2;
            if (/\*\w+\*/.test(line)) pilotScore += 1; // Variable interpolation

            // BASIC patterns
            if (/^\d+\s+/i.test(line)) basicScore += 2; // Line numbers
            if (/^(PRINT|INPUT|LET|GOTO|IF|FOR|NEXT|DIM|DATA|READ|restore)\b/i.test(line)) basicScore += 2;

            // Logo patterns
            if (/^(FORWARD|FD|BACK|BK|BACKWARD|RIGHT|RT|LEFT|LT|PENUP|PU|PENDOWN|PD|REPEAT|TO|END|CLEARSCREEN|CS|HOME|SETXY|SETCOLOR|SETHEADING|SETH|HIDETURTLE|HT|SHOWTURTLE|ST)\b/i.test(line)) logoScore += 2;
            if (/\[\s*.*\s*\]/.test(line)) logoScore += 1; // Brackets
        }

        if (pilotScore > basicScore && pilotScore > logoScore) return 'pilot';
        if (basicScore > logoScore) return 'basic';
        return 'logo';
    }

    // Detect language for a single line (like Python version)
    detectLineLanguage(line) {
        if (this.languageMode !== 'auto') {
            return this.languageMode;
        }

        line = line.trim();
        if (!line || line.startsWith('#')) {
            return 'basic'; // Default for empty/comment lines
        }

        // PILOT: Single letter followed by colon (check this FIRST)
        if (/^[A-Za-z]:/.test(line)) {
            console.log(`Line "${line}" matches PILOT pattern -> pilot`);
            return 'pilot';
        }

        // Logo brackets (should be handled by REPEAT, not executed individually)
        if (line === '[' || line === ']') {
            return 'logo';
        }

        // BASIC: Line numbers
        if (/^\d+\s+/.test(line)) {
            return 'basic';
        }

        // Logo commands
        const firstWord = line.split(/\s+/)[0].toUpperCase();
        const logoCommands = new Set([
            'FORWARD', 'FD', 'BACKWARD', 'BACK', 'BK', 'RIGHT', 'RT', 'LEFT', 'LT',
            'PENUP', 'PU', 'PENDOWN', 'PD', 'CLEARSCREEN', 'CS', 'HOME', 'REPEAT',
            'SETXY', 'SETCOLOR', 'SETHEADING', 'SETH', 'HIDETURTLE', 'HT', 'SHOWTURTLE', 'ST',
            'TO', 'END'
        ]);

        console.log(`Detecting language for line: "${line}" -> firstWord: "${firstWord}" -> isLogo: ${logoCommands.has(firstWord)}`);

        if (logoCommands.has(firstWord)) {
            return 'logo';
        }

        // BASIC commands
        const basicCommands = new Set([
            'PRINT', 'INPUT', 'LET', 'GOTO', 'IF', 'FOR', 'NEXT', 'DIM', 'DATA', 'READ', 'RESTORE'
        ]);

        if (basicCommands.has(firstWord)) {
            return 'basic';
        }

        // Default to BASIC for unknown commands
        return 'basic';
    }

    // Main execution methods
    async executeProgram(program, debug = false) {
        this.reset();
        this.isRunning = true;
        this.isDebugging = debug;
        this.stepMode = debug;

        this.programLines = program.split('\n');
        this.performance.startTime = Date.now();

        // Detect language and collect labels/line numbers
        const detectedLanguage = this.detectLanguage(program);
        this.collectLabels();

        if (this.onOutput) {
            this.onOutput(`Starting execution with per-line language detection...\n`);
        }

        try {
            while (this.currentLine < this.programLines.length && this.isRunning) {
                const line = this.programLines[this.currentLine].trim();

                if (line && !line.startsWith('#')) {
                    // Check breakpoint
                    if (this.breakpoints.has(this.currentLine + 1)) {
                        this.stepMode = true;
                        if (this.onOutput) {
                            this.onOutput(`🔴 Breakpoint at line ${this.currentLine + 1}\n`);
                        }
                    }

                    // Step mode pause
                    if (this.stepMode && this.isDebugging) {
                        await this.waitForStep();
                    }

                    // Detect language for this specific line
                    const lineLanguage = this.detectLineLanguage(line);
                    console.log(`Executing line ${this.currentLine + 1}: "${line}" as ${lineLanguage}`);
                    await this.executeLine(line, lineLanguage);

                    // Track execution
                    this.performance.linesExecuted++;
                    this.addToTimeline(this.currentLine + 1, line);

                    if (this.onLineExecuted) {
                        this.onLineExecuted(this.currentLine + 1, line);
                    }

                    // Add small delay for visualization
                    if (this.isDebugging) {
                        await this.sleep(100);
                    } else {
                        await this.sleep(10);
                    }
                }

                this.currentLine++;
            }
        } catch (error) {
            this.handleError(error, this.currentLine + 1);
        } finally {
            this.isRunning = false;
            if (this.onFinished) {
                this.onFinished();
            }
        }
    }

    async executeLine(line, language) {
        const originalLine = line;
        console.log(`executeLine called with: "${line}" language: ${language}`);

        try {
            // Remove line numbers for BASIC
            if (language === 'basic' && /^\d+\s+/.test(line)) {
                line = line.replace(/^\d+\s+/, '');
            }

            switch (language) {
                case 'pilot':
                    console.log('Executing as PILOT command');
                    await this.executePilotCommand(line);
                    break;
                case 'basic':
                    console.log('Executing as BASIC command');
                    await this.executeBasicCommand(line);
                    break;
                case 'logo':
                    console.log('Executing as Logo command');
                    await this.executeLogoCommand(line);
                    break;
            }
        } catch (error) {
            throw new Error(`Line ${this.currentLine + 1}: ${error.message}`);
        }
    }

    // PILOT Command Execution
    async executePilotCommand(line) {
        const command = line.charAt(0).toUpperCase();
        const content = line.substring(2);

        switch (command) {
            case 'T': // Text output
                const text = this.interpolateVariables(content);
                if (this.onOutput) this.onOutput(text + '\n');
                break;

            case 'A': // Accept input
                const varName = content.trim();
                const value = await this.getInput(`Enter value for ${varName}:`);
                this.setVariable(varName, value);
                break;

            case 'R': // Compute/Set variable
                this.executeCompute(content);
                break;

            case 'J': // Jump
                const label = content.trim();
                this.jumpToLabel(label);
                break;

            case 'L': // Label (no action needed)
                break;

            case 'Y': // Yes (conditional)
                if (this.evaluateCondition(content)) {
                    // Continue to next line
                } else {
                    this.currentLine++; // Skip next line
                }
                break;

            case 'N': // No (conditional)
                if (!this.evaluateCondition(content)) {
                    // Continue to next line
                } else {
                    this.currentLine++; // Skip next line
                }
                break;

            case 'E': // End
                this.isRunning = false;
                break;

            case 'U': // Use procedure (call)
                await this.callProcedure(content);
                break;

            default:
                throw new Error(`Unknown PILOT command: ${command}`);
        }
    }

    // BASIC Command Execution
    async executeBasicCommand(line) {
        const upperLine = line.toUpperCase();

        if (upperLine.startsWith('PRINT ')) {
            const expr = line.substring(6);
            const value = this.interpolateVariables(expr);
            if (this.onOutput) this.onOutput(value + '\n');
        }
        else if (upperLine.startsWith('INPUT ')) {
            const varName = line.substring(6).trim();
            const value = await this.getInput(`INPUT: `);
            this.setVariable(varName, value);
        }
        else if (upperLine.startsWith('LET ')) {
            const assignment = line.substring(4);
            this.executeAssignment(assignment);
        }
        else if (upperLine.startsWith('GOTO ')) {
            const lineNum = line.substring(5).trim();
            this.gotoLine(parseInt(lineNum));
        }
        else if (upperLine.startsWith('IF ')) {
            await this.executeIf(line);
        }
        else if (upperLine.startsWith('FOR ')) {
            this.executeFor(line);
        }
        else if (upperLine.startsWith('NEXT')) {
            await this.executeNext(line);
        }
        else if (upperLine.startsWith('DATA ')) {
            this.executeData(line.substring(5));
        }
        else if (upperLine.startsWith('READ ')) {
            this.executeRead(line.substring(5));
        }
        else if (upperLine === 'RESTORE') {
            this.dataPointer = 0;
        }
        else if (upperLine === 'END') {
            this.isRunning = false;
        }
        else {
            // Try to execute as assignment if it contains =
            if (line.includes('=')) {
                this.executeAssignment(line);
            } else {
                throw new Error(`Unknown BASIC command: ${line}`);
            }
        }
    }

    // Logo Command Execution - USING WORKING TURTLE!
    async executeLogoCommand(line) {
        const parts = line.split(/\s+/);
        const command = parts[0].toUpperCase();

        // Ensure we have the working turtle
        if (!this.turtle) {
            this.initWorkingTurtle();
        }

        switch (command) {
            case 'FORWARD':
            case 'FD':
                const dist = this.evaluateExpression(parts[1] || '0');
                console.log('✅ FORWARD command:', dist, 'Using WorkingTurtle:', !!this.turtle);
                if (this.turtle) {
                    this.turtle.forward(parseFloat(dist));
                    this.performance.graphicsCommands++;
                    console.log('✅ FORWARD executed successfully with WorkingTurtle');
                    this.output(`Moved forward ${dist} units`);
                } else {
                    console.error('❌ No WorkingTurtle available for FORWARD command');
                }
                break;

            case 'BACK':
            case 'BK':
                const backDist = this.evaluateExpression(parts[1] || '0');
                if (this.turtle) {
                    this.turtle.forward(-parseFloat(backDist));
                    this.performance.graphicsCommands++;
                    this.output(`Moved backward ${backDist} units`);
                }
                break;

            case 'RIGHT':
            case 'RT':
                const rightAngle = this.evaluateExpression(parts[1] || '0');
                if (this.turtle) {
                    this.turtle.right(parseFloat(rightAngle));
                    this.performance.graphicsCommands++;
                    this.output(`Turned right ${rightAngle} degrees`);
                }
                break;

            case 'LEFT':
            case 'LT':
                const leftAngle = this.evaluateExpression(parts[1] || '0');
                if (this.turtle) {
                    this.turtle.left(parseFloat(leftAngle));
                    this.performance.graphicsCommands++;
                    this.output(`Turned left ${leftAngle} degrees`);
                }
                break;

            case 'PENUP':
            case 'PU':
                if (this.turtle) {
                    this.turtle.penup();
                    this.performance.graphicsCommands++;
                    this.output('Pen up');
                }
                break;

            case 'PENDOWN':
            case 'PD':
                if (this.turtle) {
                    this.turtle.pendown();
                    this.performance.graphicsCommands++;
                    this.output('Pen down');
                }
                break;

            case 'CLEARSCREEN':
            case 'CS':
                console.log('✅ CLEARSCREEN command, Using WorkingTurtle:', !!this.turtle);
                if (this.turtle) {
                    this.turtle.clearCanvas();
                    this.performance.graphicsCommands++;
                    console.log('✅ CLEARSCREEN executed successfully with WorkingTurtle');
                    this.output('Screen cleared');
                } else {
                    console.error('❌ No WorkingTurtle available for CLEARSCREEN command');
                }
                break;

            case 'HOME':
                if (this.turtle) {
                    this.turtle.home();
                    this.performance.graphicsCommands++;
                    this.output('Returned home');
                }
                break;

            case 'SETCOLOR':
                const color = parts[1] || 'black';
                if (this.turtle) {
                    this.turtle.setcolor(color);
                    this.performance.graphicsCommands++;
                    this.output(`Color set to ${color}`);
                }
                break;

            case 'REPEAT':
                await this.executeRepeat(line);
                break;

            case 'PRINT':
                const text = line.substring(5).trim();
                const value = this.interpolateVariables(text);
                if (this.onOutput) this.onOutput(value + '\n');
                break;

            case '[':
            case ']':
                // Brackets are handled by REPEAT command, ignore when encountered individually
                break;

            default:
                // Try as variable assignment or procedure call
                if (line.includes('=')) {
                    this.executeAssignment(line);
                } else {
                    throw new Error(`Unknown Logo command: ${command}`);
                }
        }
    }

    // Helper methods
    collectLabels() {
        this.labels.clear();
        for (let i = 0; i < this.programLines.length; i++) {
            const line = this.programLines[i].trim();

            // PILOT labels
            if (line.startsWith('L:')) {
                const label = line.substring(2).trim();
                this.labels.set(label, i);
            }

            // BASIC line numbers
            const match = line.match(/^(\d+)\s+/);
            if (match) {
                this.labels.set(match[1], i);
            }
        }
    }

    interpolateVariables(text) {
        if (!text) return '';

        return text.replace(/\*(\w+)\*/g, (match, varName) => {
            return this.getVariable(varName) || '0';
        });
    }

    evaluateExpression(expr) {
        if (!expr) return '0';

        // Interpolate variables first
        const interpolated = this.interpolateVariables(expr);

        // Simple math evaluation (secure)
        try {
            // Remove any non-safe characters
            const safe = interpolated.replace(/[^0-9+\-*/().\s]/g, '');
            if (safe !== interpolated) {
                return interpolated; // Return as string if contains non-math
            }

            // Use Function constructor for safe evaluation
            const result = Function('"use strict"; return (' + safe + ')')();
            return isNaN(result) ? interpolated : result.toString();
        } catch (e) {
            return interpolated;
        }
    }

    evaluateCondition(condition) {
        const expr = this.interpolateVariables(condition);

        // Handle comparisons
        const operators = ['>=', '<=', '!=', '==', '>', '<', '='];
        for (const op of operators) {
            if (expr.includes(op)) {
                const parts = expr.split(op).map(p => p.trim());
                if (parts.length === 2) {
                    const left = this.evaluateExpression(parts[0]);
                    const right = this.evaluateExpression(parts[1]);
                    const leftNum = parseFloat(left);
                    const rightNum = parseFloat(right);

                    switch (op) {
                        case '>=': return leftNum >= rightNum;
                        case '<=': return leftNum <= rightNum;
                        case '!=': return left !== right;
                        case '==':
                        case '=': return left === right;
                        case '>': return leftNum > rightNum;
                        case '<': return leftNum < rightNum;
                    }
                }
            }
        }

        // Simple truthiness
        const value = this.evaluateExpression(expr);
        return value !== '0' && value !== '' && value !== 'false';
    }

    executeCompute(content) {
        // Handle R: commands like "R: 100 -> x" or "R: x + 1 -> x"
        const parts = content.split('->').map(p => p.trim());
        if (parts.length === 2) {
            const value = this.evaluateExpression(parts[0]);
            const varName = parts[1];
            this.setVariable(varName, value);
        }
    }

    executeAssignment(assignment) {
        const parts = assignment.split('=').map(p => p.trim());
        if (parts.length === 2) {
            const varName = parts[0];
            const value = this.evaluateExpression(parts[1]);
            this.setVariable(varName, value);
        }
    }

    async executeIf(line) {
        const match = line.match(/IF\s+(.+?)\s+THEN\s+(.+)/i);
        if (match) {
            const condition = match[1];
            const thenPart = match[2];

            if (this.evaluateCondition(condition)) {
                // Execute the THEN part
                if (thenPart.toUpperCase().startsWith('GOTO ')) {
                    const lineNum = thenPart.substring(5).trim();
                    this.gotoLine(parseInt(lineNum));
                } else {
                    // Execute as a statement
                    await this.executeBasicCommand(thenPart);
                }
            }
        }
    }

    executeFor(line) {
        const match = line.match(/FOR\s+(\w+)\s*=\s*(.+?)\s+TO\s+(.+?)(?:\s+STEP\s+(.+))?/i);
        if (match) {
            const varName = match[1];
            const start = this.evaluateExpression(match[2]);
            const end = this.evaluateExpression(match[3]);
            const step = match[4] ? this.evaluateExpression(match[4]) : '1';

            this.setVariable(varName, start);
            this.forLoopStack.push({
                variable: varName,
                end: parseFloat(end),
                step: parseFloat(step),
                line: this.currentLine
            });
        }
    }

    async executeNext(line) {
        if (this.forLoopStack.length === 0) {
            throw new Error('NEXT without FOR');
        }

        const loop = this.forLoopStack[this.forLoopStack.length - 1];
        const current = parseFloat(this.getVariable(loop.variable) || '0');
        const next = current + loop.step;

        this.setVariable(loop.variable, next.toString());

        // Check if loop should continue
        const shouldContinue = loop.step > 0 ? next <= loop.end : next >= loop.end;

        if (shouldContinue) {
            this.currentLine = loop.line; // Jump back to FOR line
        } else {
            this.forLoopStack.pop(); // Exit loop
        }
    }

    async executeRepeat(line) {
        // Handle both single-line and multi-line REPEAT
        const singleLineMatch = line.match(/REPEAT\s+(\d+)\s*\[\s*(.+?)\s*\]/i);

        if (singleLineMatch) {
            // Single-line format: REPEAT 4 [FORWARD 100 RIGHT 90]
            const count = parseInt(singleLineMatch[1]);
            const commands = singleLineMatch[2].split(/\s+/);

            for (let i = 0; i < count; i++) {
                for (const cmd of commands) {
                    if (cmd.trim()) {
                        await this.executeLogoCommand(cmd.trim());
                    }
                }

                if (!this.isRunning) break;
                await this.sleep(10);
            }
        } else {
            // Multi-line format: REPEAT 4 [ ... ]
            const multiLineMatch = line.match(/REPEAT\s+(\d+)\s*\[/i);
            if (multiLineMatch) {
                const count = parseInt(multiLineMatch[1]);
                const commands = [];

                // Collect commands until we find the closing bracket
                this.currentLine++; // Move past REPEAT line
                let bracketCount = 1;

                while (this.currentLine < this.programLines.length && bracketCount > 0) {
                    const blockLine = this.programLines[this.currentLine].trim();

                    if (blockLine === ']') {
                        bracketCount--;
                    } else if (blockLine.includes('[')) {
                        bracketCount++;
                        commands.push(blockLine);
                    } else if (blockLine && !blockLine.startsWith('#')) {
                        commands.push(blockLine);
                    }

                    if (bracketCount > 0) {
                        this.currentLine++;
                    }
                }

                // Execute the collected commands the specified number of times
                for (let i = 0; i < count; i++) {
                    for (const cmd of commands) {
                        if (cmd.trim()) {
                            const cmdLanguage = this.detectLineLanguage(cmd);
                            await this.executeLine(cmd, cmdLanguage);
                        }
                    }

                    if (!this.isRunning) break;
                    await this.sleep(10);
                }
            }
        }
    }

    executeData(data) {
        const values = data.split(',').map(v => v.trim());
        this.dataList.push(...values);
    }

    executeRead(variables) {
        const vars = variables.split(',').map(v => v.trim());
        for (const varName of vars) {
            if (this.dataPointer < this.dataList.length) {
                this.setVariable(varName, this.dataList[this.dataPointer]);
                this.dataPointer++;
            } else {
                throw new Error('Out of DATA');
            }
        }
    }

    jumpToLabel(label) {
        if (this.labels.has(label)) {
            this.currentLine = this.labels.get(label) - 1; // -1 because it will be incremented
        } else {
            throw new Error(`Label not found: ${label}`);
        }
    }

    gotoLine(lineNumber) {
        const label = lineNumber.toString();
        if (this.labels.has(label)) {
            this.currentLine = this.labels.get(label) - 1;
        } else {
            throw new Error(`Line number not found: ${lineNumber}`);
        }
    }

    async callProcedure(name) {
        // Placeholder for procedure calls
        throw new Error(`Procedure '${name}' not implemented`);
    }

    // Variable management
    setVariable(name, value) {
        this.variables.set(name, value.toString());
        if (this.onVariableChanged) {
            this.onVariableChanged(name, value.toString());
        }
    }

    getVariable(name) {
        return this.variables.get(name) || '0';
    }

    // Input handling
    async getInput(prompt) {
        return new Promise((resolve) => {
            this.waitingForInput = true;
            this.inputCallback = resolve;
            if (this.onInput) {
                this.onInput(prompt);
            }
        });
    }

    submitInput(value) {
        if (this.waitingForInput && this.inputCallback) {
            this.waitingForInput = false;
            this.inputCallback(value);
            this.inputCallback = null;
        }
    }

    // Debug controls
    async waitForStep() {
        return new Promise((resolve) => {
            this.stepCallback = resolve;
        });
    }

    step() {
        this.stepMode = true;
        if (this.stepCallback) {
            this.stepCallback();
            this.stepCallback = null;
        }
    }

    continue() {
        this.stepMode = false;
        if (this.stepCallback) {
            this.stepCallback();
            this.stepCallback = null;
        }
    }

    toggleBreakpoint(lineNumber) {
        if (this.breakpoints.has(lineNumber)) {
            this.breakpoints.delete(lineNumber);
        } else {
            this.breakpoints.add(lineNumber);
        }
    }

    // Utility methods
    stop() {
        this.isRunning = false;
        this.waitingForInput = false;
        if (this.inputCallback) {
            this.inputCallback('');
            this.inputCallback = null;
        }
        if (this.stepCallback) {
            this.stepCallback();
            this.stepCallback = null;
        }
    }

    reset() {
        this.variables.clear();
        this.labels.clear();
        this.programLines = [];
        this.currentLine = 0;
        this.isRunning = false;
        this.isDebugging = false;
        this.callStack = [];
        this.forLoopStack = [];
        this.dataList = [];
        this.dataPointer = 0;
        this.breakpoints.clear();
        this.stepMode = false;
        this.waitingForInput = false;
        this.inputCallback = null;
        this.stepCallback = null;

        this.performance = {
            startTime: 0,
            linesExecuted: 0,
            graphicsCommands: 0,
            errors: 0
        };

        this.timeline = [];
    }

    addToTimeline(lineNumber, command) {
        this.timeline.push({
            step: this.timeline.length + 1,
            time: Date.now() - this.performance.startTime,
            line: lineNumber,
            command: command
        });
    }

    handleError(error, lineNumber) {
        this.performance.errors++;
        if (this.onError) {
            this.onError(error.message, lineNumber);
        }
    }

    sleep(ms) {
        return new Promise(resolve => setTimeout(resolve, ms));
    }

    // Public API
    getVariables() {
        return Array.from(this.variables.entries()).map(([name, value]) => ({
            name,
            value,
            type: isNaN(parseFloat(value)) ? 'string' : 'number'
        }));
    }

    getPerformance() {
        const elapsed = this.performance.startTime ? (Date.now() - this.performance.startTime) / 1000 : 0;
        return {
            elapsedTime: elapsed,
            linesExecuted: this.performance.linesExecuted,
            linesPerSecond: elapsed > 0 ? (this.performance.linesExecuted / elapsed) : 0,
            variablesCount: this.variables.size,
            graphicsCommands: this.performance.graphicsCommands,
            errors: this.performance.errors
        };
    }

    getTimeline() {
        return [...this.timeline];
    }
}