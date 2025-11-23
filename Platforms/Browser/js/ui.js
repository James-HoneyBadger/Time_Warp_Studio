/**
 * Time Warp Web IDE - UI Controller
 * Handles user interface interactions and state management
 */

class TimeWarpUI {
    constructor() {
        try {
            this.interpreter = new TimeWarpInterpreter();
            this.graphics = null;
            this.editor = null;
            this.currentTab = 'output';
            this.isRunning = false;
            this.isDebugging = false;

            // Performance chart
            this.performanceChart = null;
            this.performanceData = [];

            // Code snippets
            this.snippets = this.getCodeSnippets();

            // Help content
            this.helpContent = this.getHelpContent();

            this.initializeUI();
            this.setupEventListeners();
            this.setupInterpreterCallbacks();

            console.log('TimeWarpUI initialized successfully');
        } catch (error) {
            console.error('TimeWarpUI constructor error:', error);
            throw new Error(`TimeWarpUI initialization failed: ${error.message}`);
        }
    }

    initializeUI() {
        // Initialize graphics with detailed logging
        const canvas = document.getElementById('graphicsCanvas');
        console.log('Canvas element:', canvas);

        if (canvas) {
            console.log('Canvas dimensions:', canvas.width, 'x', canvas.height);
            console.log('Canvas context available:', !!canvas.getContext('2d'));
            console.log('Canvas bounding rect:', canvas.getBoundingClientRect());

            try {
                // Don't create graphics immediately - wait for graphics tab to be shown
                this.graphicsCanvas = canvas;
                console.log('Graphics canvas stored for later initialization');

                // Create a placeholder for now
                this.graphics = null;
                console.log('Graphics initialization deferred until tab is shown');
            } catch (error) {
                console.error('Failed to initialize turtle graphics:', error);
            }
        } else {
            console.error('Graphics canvas not found! Available elements with graphics in ID:');
            const allElements = document.querySelectorAll('[id*="graphics"]');
            allElements.forEach(el => console.log('- Found element:', el.id, el));
        }

        // Initialize editor
        this.editor = document.getElementById('codeEditor');
        console.log('Code editor element:', this.editor);

        // Load demo program
        this.loadGraphicsTest();

        // Initialize tabs
        this.showTab('output');

        // Initialize snippets
        this.populateSnippets();

        // Initialize help
        this.populateHelp();

        // Update line numbers
        this.updateLineNumbers();

        // Update performance display (with error handling)
        try {
            this.updatePerformanceDisplay();
        } catch (error) {
            console.warn('Initial performance display update failed:', error);
        }

        // Update memory usage (with error handling)
        try {
            this.updateMemoryUsage();
        } catch (error) {
            console.warn('Initial memory usage update failed:', error);
        }

        // Set initial status
        this.updateStatus('Ready');
    }

    setupEventListeners() {
        // Toolbar buttons
        document.getElementById('runBtn').addEventListener('click', () => this.runProgram());
        document.getElementById('stopBtn').addEventListener('click', () => this.stopProgram());
        document.getElementById('debugBtn').addEventListener('click', () => this.debugProgram());
        document.getElementById('stepBtn').addEventListener('click', () => this.stepProgram());
        document.getElementById('continueBtn').addEventListener('click', () => this.continueProgram());

        document.getElementById('newBtn').addEventListener('click', () => this.newFile());
        document.getElementById('loadDemoBtn').addEventListener('click', () => this.loadDemoProgram());
        document.getElementById('saveBtn').addEventListener('click', () => this.saveProgram());
        document.getElementById('clearBtn').addEventListener('click', () => this.clearOutput());
        document.getElementById('testGraphicsBtn').addEventListener('click', () => this.testWorkingGraphics());
        document.getElementById('simpleCanvasBtn').addEventListener('click', () => this.simpleCanvasTest());
        document.getElementById('helpBtn').addEventListener('click', () => this.showHelp());
        document.getElementById('fullscreenBtn').addEventListener('click', () => this.toggleFullscreen());

        // Editor events
        this.editor.addEventListener('input', () => {
            this.updateLineNumbers();
            this.updateCursorInfo();
        });
        this.editor.addEventListener('scroll', () => this.syncLineNumbers());
        this.editor.addEventListener('keydown', (e) => this.handleKeydown(e));
        this.editor.addEventListener('click', () => this.updateCursorInfo());

        // Tab switching
        document.querySelectorAll('.tab').forEach(tab => {
            tab.addEventListener('click', () => {
                const tabName = tab.dataset.tab;
                this.showTab(tabName);
            });
        });

        // Language selector
        document.getElementById('languageMode').addEventListener('change', (e) => {
            this.interpreter.languageMode = e.target.value;
        });

        // Input handling
        document.getElementById('inputSubmit').addEventListener('click', () => this.submitInput());
        document.getElementById('inputField').addEventListener('keypress', (e) => {
            if (e.key === 'Enter') this.submitInput();
        });

        // Variables and watches
        document.getElementById('refreshVarsBtn').addEventListener('click', () => this.updateVariablesDisplay());
        document.getElementById('addWatchBtn').addEventListener('click', () => this.addWatch());
        document.getElementById('watchExpression').addEventListener('keypress', (e) => {
            if (e.key === 'Enter') this.addWatch();
        });

        // Graphics controls
        document.getElementById('clearGraphicsBtn').addEventListener('click', () => {
            console.log('Clear graphics clicked');
            if (this.ensureGraphicsInitialized()) {
                this.graphics.clearScreen();
                console.log('Graphics cleared');
            } else {
                console.error('No graphics object available');
            }
        });
        document.getElementById('centerTurtleBtn').addEventListener('click', () => {
            console.log('Center turtle clicked');
            if (this.ensureGraphicsInitialized()) {
                this.graphics.home();
                console.log('Turtle centered');
            } else {
                console.error('No graphics object available');
            }
        });
        document.getElementById('showGridBtn').addEventListener('change', (e) => {
            console.log('Grid toggle clicked');
            if (this.ensureGraphicsInitialized()) {
                this.graphics.setShowGrid(e.target.checked);
                console.log('Grid toggled to:', e.target.checked);
            } else {
                console.error('No graphics object available');
            }
        });

        document.getElementById('testGraphicsBtn').addEventListener('click', () => {
            console.log('Test graphics clicked');
            if (this.ensureGraphicsInitialized()) {
                console.log('Drawing test shapes...');
                this.graphics.clearScreen();
                this.graphics.forward(50);
                this.graphics.right(90);
                this.graphics.forward(50);
                this.graphics.right(90);
                this.graphics.forward(50);
                this.graphics.right(90);
                this.graphics.forward(50);
                console.log('Test shapes drawn');
            } else {
                console.error('No graphics object available for test');
            }
        });

        // Performance controls
        document.getElementById('exportPerfBtn').addEventListener('click', () => this.exportPerformance());

        // Timeline controls
        document.getElementById('clearTimelineBtn').addEventListener('click', () => this.clearTimeline());

        // Snippet controls
        document.getElementById('snippetCategory').addEventListener('change', () => this.filterSnippets());

        // Help controls
        document.getElementById('helpLanguage').addEventListener('change', () => this.updateHelpContent());

        // Modal controls
        document.querySelector('.modal-close').addEventListener('click', () => this.hideError());
        document.getElementById('errorOkBtn').addEventListener('click', () => this.hideError());

        // Other controls
        document.getElementById('clearOutputBtn').addEventListener('click', () => this.clearOutput());

        // Keyboard shortcuts
        document.addEventListener('keydown', (e) => {
            if (e.ctrlKey || e.metaKey) {
                switch (e.key) {
                    case 'r':
                        e.preventDefault();
                        this.runProgram();
                        break;
                    case 'n':
                        e.preventDefault();
                        this.newFile();
                        break;
                    case 's':
                        e.preventDefault();
                        this.saveProgram();
                        break;
                    case 'g':
                        e.preventDefault();
                        this.loadGraphicsTest();
                        break;
                }
            }
        });

        // Window resize
        window.addEventListener('resize', () => {
            if (this.graphics) {
                this.graphics.resize();
            }
        });
    }

    setupInterpreterCallbacks() {
        this.interpreter.onOutput = (text) => this.addOutput(text);
        this.interpreter.onVariableChanged = () => this.updateVariablesDisplay();
        this.interpreter.onLineExecuted = (line, command) => this.addToTimeline(line, command);
        this.interpreter.onError = (error, line) => this.showError(error, line);
        this.interpreter.onFinished = () => this.onProgramFinished();
        this.interpreter.onInput = (prompt) => this.showInputPrompt(prompt);
    }

    // Program execution
    async runProgram() {
        if (this.isRunning) return;

        const code = this.editor.value.trim();
        if (!code) {
            this.showError('No program to run!');
            return;
        }

        console.log('Running program, graphics object:', this.graphics);
        console.log('Interpreter graphics:', this.interpreter.graphics);
        console.log('Program code:', code.substring(0, 200));

        // Ensure graphics are initialized before running
        if (!this.graphics) {
            console.log('Initializing graphics for program execution...');
            this.ensureGraphicsInitialized();
        }

        this.isRunning = true;
        this.isDebugging = false;
        this.updateRunButtons(true, false);
        this.clearOutput();
        this.updateStatus('Running...');

        if (this.graphics) {
            this.graphics.clearScreen();
            console.log('Graphics cleared before execution');
        } else {
            console.error('No graphics object available for execution');
        }

        try {
            // Check if program contains graphics commands
            const hasGraphicsCommands = /\b(FORWARD|FD|RIGHT|RT|LEFT|LT|CLEARSCREEN|CS|HOME|PENUP|PU|PENDOWN|PD)\b/i.test(code);
            if (hasGraphicsCommands) {
                console.log('Graphics commands detected, switching to graphics tab');
                this.showTab('graphics');
            }

            await this.interpreter.executeProgram(code, false);
        } catch (error) {
            console.error('Program execution error:', error);
            this.showError(error.message);
        }
    }

    async debugProgram() {
        if (this.isRunning) return;

        const code = this.editor.value.trim();
        if (!code) {
            this.showError('No program to debug!');
            return;
        }

        this.isRunning = true;
        this.isDebugging = true;
        this.updateRunButtons(true, true);
        this.clearOutput();
        this.updateStatus('Debugging...');

        if (this.graphics) {
            this.graphics.clearScreen();
        }

        try {
            await this.interpreter.executeProgram(code, true);
        } catch (error) {
            this.showError(error.message);
        }
    }

    stopProgram() {
        this.interpreter.stop();
        this.onProgramFinished();
    }

    stepProgram() {
        this.interpreter.step();
    }

    continueProgram() {
        this.interpreter.continue();
    }

    onProgramFinished() {
        this.isRunning = false;
        this.isDebugging = false;
        this.updateRunButtons(false, false);
        this.updateStatus('Ready');
        this.updatePerformanceDisplay();
        this.hideInputPrompt();

        this.addOutput('\n--- Program finished ---\n');
    }

    // UI State Management
    updateRunButtons(running, debugging) {
        document.getElementById('runBtn').disabled = running;
        document.getElementById('debugBtn').disabled = running;
        document.getElementById('stopBtn').disabled = !running;
        document.getElementById('stepBtn').disabled = !debugging;
        document.getElementById('continueBtn').disabled = !debugging;
    }

    updateStatus(status) {
        document.getElementById('status').textContent = status;
    }

    // Tab Management
    showTab(tabName) {
        console.log(`Switching to tab: ${tabName}`);

        // Hide all tab panes
        document.querySelectorAll('.tab-pane').forEach(pane => {
            pane.classList.remove('active');
        });

        // Remove active class from all tabs
        document.querySelectorAll('.tab').forEach(tab => {
            tab.classList.remove('active');
        });

        // Show selected tab pane
        const pane = document.getElementById(`${tabName}-tab`);
        console.log(`Tab pane element:`, pane);
        if (pane) {
            pane.classList.add('active');
            console.log(`Tab pane ${tabName} activated`);
        } else {
            console.error(`Tab pane ${tabName}-tab not found`);
        }

        // Add active class to selected tab
        const tab = document.querySelector(`[data-tab="${tabName}"]`);
        console.log(`Tab button element:`, tab);
        if (tab) {
            tab.classList.add('active');
            console.log(`Tab button ${tabName} activated`);
        } else {
            console.error(`Tab button with data-tab="${tabName}" not found`);
        }

        this.currentTab = tabName;

        // Special handling for graphics tab
        if (tabName === 'graphics') {
            console.log('Graphics tab selected, checking canvas...');
            const canvas = document.getElementById('graphicsCanvas');
            console.log('Graphics canvas:', canvas);

            if (canvas && this.graphics) {
                console.log('Refreshing canvas dimensions...');

                // Multiple attempts to setup canvas properly when tab becomes visible
                const setupAttempts = [0, 50, 100, 200, 300];
                let attemptIndex = 0;

                const attemptSetup = () => {
                    console.log(`Setup attempt ${attemptIndex + 1}...`);

                    const rect = canvas.getBoundingClientRect();
                    console.log('Canvas rect:', rect);

                    if (rect.width > 0 && rect.height > 0) {
                        // Success! Canvas is visible with proper dimensions
                        console.log('Canvas visible, checking if graphics already exist...');

                        // DON'T recreate graphics if they already exist - this was clearing the canvas!
                        if (!this.graphics && !this.interpreter.turtle) {
                            console.log('Creating graphics for the first time...');
                            // Force canvas style dimensions
                            canvas.style.width = rect.width + 'px';
                            canvas.style.height = rect.height + 'px';

                            // Only create graphics if they don't exist
                            this.interpreter.initWorkingTurtle();
                            console.log('Graphics setup completed successfully');
                        } else {
                            console.log('Graphics already exist, NOT recreating (this was the bug!)');
                        }

                        return true;
                    } else if (attemptIndex < setupAttempts.length - 1) {
                        // Try again after delay
                        attemptIndex++;
                        setTimeout(attemptSetup, setupAttempts[attemptIndex]);
                        console.log(`Retrying setup in ${setupAttempts[attemptIndex]}ms...`);
                    } else {
                        // Final fallback - but DON'T recreate if graphics already exist
                        if (!this.graphics && !this.interpreter.turtle) {
                            console.warn('Using fallback canvas setup - first time only');
                            canvas.style.width = canvas.width + 'px';
                            canvas.style.height = canvas.height + 'px';
                            this.interpreter.initWorkingTurtle();
                            console.log('Fallback graphics setup completed');
                        } else {
                            console.log('Fallback: Graphics already exist, NOT recreating');
                        }
                    }
                    return false;
                };

                // Start setup attempts
                attemptSetup();
            } else if (canvas && !this.graphics && !this.interpreter.turtle) {
                console.log('Creating graphics for first time - no complex setup needed...');
                this.interpreter.initWorkingTurtle();
            }
        }

        // Update content if needed
        if (tabName === 'performance') {
            this.updatePerformanceDisplay();
        } else if (tabName === 'timeline') {
            this.updateTimelineDisplay();
        }
    }

    // Ensure graphics are initialized - NOW USING WORKING TURTLE!
    ensureGraphicsInitialized() {
        console.log('🔄 Ensuring WORKING turtle graphics are initialized...');

        // The interpreter will handle its own turtle initialization
        // Just make sure the canvas exists
        const canvas = document.getElementById('graphicsCanvas');
        if (canvas) {
            console.log('✅ Canvas found, letting interpreter initialize WorkingTurtle');
            this.interpreter.initWorkingTurtle();
            return true;
        } else {
            console.error('❌ No graphicsCanvas found for graphics initialization');
            return false;
        }
    }

    // Test the working graphics system - AGGRESSIVE FORCED INITIALIZATION
    testWorkingGraphics() {
        console.log('🧪 AGGRESSIVE Graphics Test Starting...');

        // Clear output and switch to graphics tab FIRST
        this.clearOutput();
        this.showTab('graphics');

        this.addOutput('🧪 AGGRESSIVE Graphics Test Starting...', 'info');
        this.addOutput('🔄 Force initializing turtle graphics...', 'info');

        // FORCE turtle initialization with delays
        setTimeout(() => {
            try {
                // Get canvas directly
                const canvas = document.getElementById('graphicsCanvas');
                this.addOutput('📦 Canvas found: ' + !!canvas, 'info');
                this.addOutput('📏 Canvas dimensions: ' + (canvas ? canvas.width + 'x' + canvas.height : 'N/A'), 'info');

                if (canvas) {
                    // FORCE create turtle directly
                    this.interpreter.turtle = new WorkingTurtle(canvas);
                    this.addOutput('✅ WorkingTurtle FORCE created!', 'info');

                    // Test it immediately
                    this.addOutput('🧪 Testing direct turtle movement...', 'info');
                    this.interpreter.turtle.forward(50);
                    this.interpreter.turtle.right(90);
                    this.interpreter.turtle.forward(50);
                    this.addOutput('✅ Direct turtle test SUCCESSFUL!', 'success');

                    // Now test via interpreter
                    this.addOutput('🔄 Testing via interpreter commands...', 'info');
                    const testProgram = `FORWARD 100
RIGHT 90
FORWARD 100
RIGHT 90
FORWARD 100
RIGHT 90
FORWARD 100`;

                    this.editor.value = testProgram;
                    this.addOutput('📝 Test program loaded', 'info');

                    // Run after another small delay
                    setTimeout(() => {
                        this.runProgram();
                    }, 100);

                } else {
                    this.addOutput('❌ No canvas found!', 'error');
                }

            } catch (error) {
                this.addOutput('❌ Force init failed: ' + error.message, 'error');
                console.error('Force init error:', error);
            }
        }, 200); // Wait for tab switch to complete
    }

    // SIMPLE canvas test - just draw basic shapes without any complex turtle system
    simpleCanvasTest() {
        console.log('🎨 Simple Canvas Test Starting...');

        this.clearOutput();
        this.showTab('graphics');

        this.addOutput('🎨 Simple Canvas Test Starting...', 'info');
        this.addOutput('🔄 Getting canvas directly...', 'info');

        setTimeout(() => {
            try {
                const canvas = document.getElementById('graphicsCanvas');
                this.addOutput('📦 Canvas element: ' + !!canvas, 'info');

                if (canvas) {
                    const ctx = canvas.getContext('2d');
                    this.addOutput('🖼️ Canvas context: ' + !!ctx, 'info');
                    this.addOutput('📏 Canvas size: ' + canvas.width + 'x' + canvas.height, 'info');

                    // Clear canvas with white background
                    ctx.fillStyle = 'white';
                    ctx.fillRect(0, 0, canvas.width, canvas.height);
                    this.addOutput('⚪ Canvas cleared to white', 'info');

                    // Draw a red square
                    ctx.fillStyle = 'red';
                    ctx.fillRect(50, 50, 100, 100);
                    this.addOutput('🔴 Red square drawn at (50,50)', 'info');

                    // Draw a blue circle  
                    ctx.fillStyle = 'blue';
                    ctx.beginPath();
                    ctx.arc(250, 100, 40, 0, 2 * Math.PI);
                    ctx.fill();
                    this.addOutput('🔵 Blue circle drawn at (250,100)', 'info');

                    // Draw a green line
                    ctx.strokeStyle = 'green';
                    ctx.lineWidth = 5;
                    ctx.beginPath();
                    ctx.moveTo(100, 200);
                    ctx.lineTo(300, 300);
                    ctx.stroke();
                    this.addOutput('🟢 Green line drawn', 'info');

                    // Draw some text
                    ctx.fillStyle = 'black';
                    ctx.font = '20px Arial';
                    ctx.fillText('Canvas Works!', 150, 250);
                    this.addOutput('📝 Text drawn', 'info');

                    this.addOutput('✅ SIMPLE CANVAS TEST SUCCESSFUL!', 'success');
                    this.addOutput('👀 Check the Graphics tab to see the shapes!', 'success');

                } else {
                    this.addOutput('❌ No canvas element found!', 'error');
                }

            } catch (error) {
                this.addOutput('❌ Simple canvas test failed: ' + error.message, 'error');
                console.error('Simple canvas test error:', error);
            }
        }, 100);
    }

    showHelp() {
        this.showTab('help');
    }

    // Output Management
    addOutput(text, className = '') {
        const outputArea = document.getElementById('output');
        if (!outputArea) return;

        const line = document.createElement('div');
        line.className = `output-line ${className}`.trim();
        line.textContent = text.replace(/\n$/, ''); // Remove trailing newline
        outputArea.appendChild(line);

        // Auto-scroll to bottom
        outputArea.scrollTop = outputArea.scrollHeight;

        // Switch to output tab if not visible
        if (this.currentTab !== 'output') {
            this.showTab('output');
        }
    }

    clearOutput() {
        const outputArea = document.getElementById('output');
        if (outputArea) {
            outputArea.innerHTML = '';
        }
    }

    // Input Handling
    showInputPrompt(prompt) {
        const inputPrompt = document.getElementById('inputPrompt');
        const inputLabel = document.getElementById('inputLabel');
        const inputField = document.getElementById('inputField');

        if (inputPrompt && inputLabel && inputField) {
            inputLabel.textContent = prompt;
            inputField.value = '';
            inputPrompt.classList.remove('hidden');
            inputField.focus();
        }
    }

    hideInputPrompt() {
        const inputPrompt = document.getElementById('inputPrompt');
        if (inputPrompt) {
            inputPrompt.classList.add('hidden');
        }
    }

    submitInput() {
        const inputField = document.getElementById('inputField');
        if (inputField) {
            const value = inputField.value;
            this.interpreter.submitInput(value);
            this.addOutput(`> ${value}`);
            this.hideInputPrompt();
        }
    }

    // Editor Management
    newFile() {
        this.editor.value = '';
        this.updateLineNumbers();
        this.updateCursorInfo();
        this.clearOutput();
        if (this.graphics) {
            this.graphics.clearScreen();
        }
    }

    loadDemoProgram() {
        const demoProgram = `T:Welcome to Time Warp 4.0.0 Web Edition!
T:This is a comprehensive educational IDE for programming.
A:name
T:Hello *name*! Let's explore the features...

# Draw a colorful square
R:100 -> size
T:Drawing a square with size *size*

CLEARSCREEN
PENDOWN
SETCOLOR blue
FORWARD *size*
RIGHT 90
SETCOLOR red
FORWARD *size*
RIGHT 90
SETCOLOR green
FORWARD *size*
RIGHT 90
SETCOLOR orange
FORWARD *size*
RIGHT 90

T:Square completed!
T:Try the different tabs to explore:
T:📺 Output - Program messages
T:📊 Variables - Live variable values
T:🐢 Graphics - Turtle drawing canvas
T:⚡ Performance - Execution metrics
T:⏱️ Timeline - Step-by-step history
T:📝 Snippets - Code templates
T:❓ Help - Language reference

T:Change the language mode to try BASIC or Logo!
E:`;

        this.editor.value = demoProgram;
        this.updateLineNumbers();
        this.updateCursorInfo();
    }

    loadGraphicsTest() {
        const graphicsProgram = `FORWARD 100
RIGHT 90
FORWARD 100
E:`;

        this.editor.value = graphicsProgram;
        this.updateLineNumbers();
        this.updateCursorInfo();
        console.log('Minimal graphics test program loaded - no PILOT commands');
    }

    saveProgram() {
        const code = this.editor.value;
        const blob = new Blob([code], { type: 'text/plain' });
        const url = URL.createObjectURL(blob);

        const a = document.createElement('a');
        a.href = url;
        a.download = 'program.spt';
        document.body.appendChild(a);
        a.click();
        document.body.removeChild(a);
        URL.revokeObjectURL(url);
    }

    // Editor Support
    updateLineNumbers() {
        const lineNumbers = document.getElementById('lineNumbers');
        if (!lineNumbers || !this.editor) return;

        const lines = this.editor.value.split('\n');
        const numbers = lines.map((_, i) => i + 1).join('\n');
        lineNumbers.textContent = numbers;
    }

    syncLineNumbers() {
        const lineNumbers = document.getElementById('lineNumbers');
        if (!lineNumbers || !this.editor) return;

        lineNumbers.scrollTop = this.editor.scrollTop;
    }

    updateCursorInfo() {
        const cursorInfo = document.getElementById('cursorInfo');
        if (!cursorInfo || !this.editor) return;

        const pos = this.editor.selectionStart;
        const text = this.editor.value.substring(0, pos);
        const lines = text.split('\n');
        const line = lines.length;
        const column = lines[lines.length - 1].length + 1;

        cursorInfo.textContent = `Line: ${line}, Column: ${column}`;
    }

    handleKeydown(e) {
        // Tab key handling
        if (e.key === 'Tab') {
            e.preventDefault();
            const start = this.editor.selectionStart;
            const end = this.editor.selectionEnd;

            if (e.shiftKey) {
                // Unindent
                const beforeCursor = this.editor.value.substring(0, start);
                const afterCursor = this.editor.value.substring(end);
                const lines = beforeCursor.split('\n');
                const currentLine = lines[lines.length - 1];

                if (currentLine.startsWith('  ')) {
                    lines[lines.length - 1] = currentLine.substring(2);
                    this.editor.value = lines.join('\n') + afterCursor;
                    this.editor.setSelectionRange(start - 2, end - 2);
                }
            } else {
                // Indent
                this.editor.value = this.editor.value.substring(0, start) + '  ' + this.editor.value.substring(end);
                this.editor.setSelectionRange(start + 2, end + 2);
            }

            this.updateLineNumbers();
        }
    }

    // Variables Display
    updateVariablesDisplay() {
        const variablesList = document.getElementById('variablesList');
        if (!variablesList) return;

        const variables = this.interpreter.getVariables();

        variablesList.innerHTML = '';

        if (variables.length === 0) {
            const emptyMessage = document.createElement('div');
            emptyMessage.textContent = 'No variables defined';
            emptyMessage.style.color = '#6c757d';
            emptyMessage.style.fontStyle = 'italic';
            emptyMessage.style.padding = '20px';
            emptyMessage.style.textAlign = 'center';
            variablesList.appendChild(emptyMessage);
            return;
        }

        variables.forEach(variable => {
            const item = document.createElement('div');
            item.className = 'variable-item';

            const name = document.createElement('span');
            name.className = 'variable-name';
            name.textContent = variable.name;

            const value = document.createElement('span');
            value.className = 'variable-value';
            value.textContent = variable.value;

            item.appendChild(name);
            item.appendChild(value);
            variablesList.appendChild(item);
        });
    }

    addWatch() {
        const watchInput = document.getElementById('watchExpression');
        const expression = watchInput.value.trim();

        if (!expression) return;

        const watchesList = document.getElementById('watchesList');
        if (!watchesList) return;

        const item = document.createElement('div');
        item.className = 'watch-item';

        const expr = document.createElement('span');
        expr.className = 'watch-expression';
        expr.textContent = expression;

        const value = document.createElement('span');
        value.className = 'watch-value';

        try {
            const result = this.interpreter.evaluateExpression(expression);
            value.textContent = result;
        } catch (e) {
            value.textContent = 'Error';
            value.style.color = '#e74c3c';
        }

        item.appendChild(expr);
        item.appendChild(value);
        watchesList.appendChild(item);

        watchInput.value = '';
    }

    // Performance Display
    updatePerformanceDisplay() {
        try {
            const perf = this.interpreter.getPerformance();

            // Safely update performance metrics with fallbacks
            const execTimeEl = document.getElementById('execTime');
            if (execTimeEl && perf.elapsedTime !== undefined) {
                execTimeEl.textContent = `${(perf.elapsedTime || 0).toFixed(2)} s`;
            }

            const linesExecutedEl = document.getElementById('linesExecuted');
            if (linesExecutedEl) {
                linesExecutedEl.textContent = perf.linesExecuted || 0;
            }

            const linesPerSecEl = document.getElementById('linesPerSec');
            if (linesPerSecEl && perf.linesPerSecond !== undefined) {
                linesPerSecEl.textContent = (perf.linesPerSecond || 0).toFixed(1);
            }

            const variablesCountEl = document.getElementById('variablesCount');
            if (variablesCountEl) {
                variablesCountEl.textContent = perf.variablesCount || 0;
            }

            const graphicsCommandsEl = document.getElementById('graphicsCommands');
            if (graphicsCommandsEl) {
                graphicsCommandsEl.textContent = perf.graphicsCommands || 0;
            }

            // Update performance chart
            this.updatePerformanceChart(perf);
        } catch (error) {
            console.warn('Error updating performance display:', error);
        }
    }

    updatePerformanceChart(perf) {
        try {
            const canvas = document.getElementById('performanceChart');
            if (!canvas) return;

            const ctx = canvas.getContext('2d');
            if (!ctx) return;

            const width = canvas.width || 400;
            const height = canvas.height || 200;

            // Clear canvas
            ctx.clearRect(0, 0, width, height);

            // Add current data point with safe fallbacks
            this.performanceData.push({
                time: perf.elapsedTime || 0,
                lines: perf.linesExecuted || 0
            });

            // Keep only last 50 points
            if (this.performanceData.length > 50) {
                this.performanceData.shift();
            }

            if (this.performanceData.length < 2) return;

            // Draw chart with error handling
            ctx.strokeStyle = '#3498db';
            ctx.lineWidth = 2;
            ctx.beginPath();

            const validData = this.performanceData.filter(d =>
                d && typeof d.lines === 'number' && typeof d.time === 'number'
            );

            if (validData.length === 0) return;

            const maxLines = Math.max(...validData.map(d => d.lines));
            const maxTime = Math.max(...validData.map(d => d.time));

            validData.forEach((point, i) => {
                const x = (i / (validData.length - 1)) * (width - 20) + 10;
                const y = height - 10 - ((point.lines / (maxLines || 1)) * (height - 20));

                if (i === 0) {
                    ctx.moveTo(x, y);
                } else {
                    ctx.lineTo(x, y);
                }
            });

            ctx.stroke();

            // Draw axes
            ctx.strokeStyle = '#ddd';
            ctx.lineWidth = 1;
            ctx.beginPath();
            ctx.moveTo(10, height - 10);
            ctx.lineTo(width - 10, height - 10);
            ctx.moveTo(10, 10);
            ctx.lineTo(10, height - 10);
            ctx.stroke();
        } catch (error) {
            console.warn('Error updating performance chart:', error);
        }
    }

    exportPerformance() {
        const perf = this.interpreter.getPerformance();
        const data = {
            timestamp: new Date().toISOString(),
            performance: perf,
            timeline: this.interpreter.getTimeline(),
            variables: this.interpreter.getVariables()
        };

        const blob = new Blob([JSON.stringify(data, null, 2)], { type: 'application/json' });
        const url = URL.createObjectURL(blob);

        const a = document.createElement('a');
        a.href = url;
        a.download = 'performance_data.json';
        document.body.appendChild(a);
        a.click();
        document.body.removeChild(a);
        URL.revokeObjectURL(url);
    }

    // Timeline Display
    addToTimeline(lineNumber, command) {
        const timelineList = document.getElementById('timelineList');
        if (!timelineList) return;

        const timeline = this.interpreter.getTimeline();
        const entry = timeline[timeline.length - 1];
        if (!entry) return;

        const item = document.createElement('div');
        item.className = 'timeline-item fade-in';

        item.innerHTML = `
            <div class="timeline-step">Step ${entry.step}</div>
            <div class="timeline-time">${entry.time}ms</div>
            <div class="timeline-command">Line ${entry.line}: ${entry.command}</div>
        `;

        timelineList.appendChild(item);
        timelineList.scrollTop = timelineList.scrollHeight;
    }

    updateTimelineDisplay() {
        const timelineList = document.getElementById('timelineList');
        if (!timelineList) return;

        const timeline = this.interpreter.getTimeline();
        timelineList.innerHTML = '';

        timeline.forEach(entry => {
            const item = document.createElement('div');
            item.className = 'timeline-item';

            item.innerHTML = `
                <div class="timeline-step">Step ${entry.step}</div>
                <div class="timeline-time">${entry.time}ms</div>
                <div class="timeline-command">Line ${entry.line}: ${entry.command}</div>
            `;

            timelineList.appendChild(item);
        });
    }

    clearTimeline() {
        const timelineList = document.getElementById('timelineList');
        if (timelineList) {
            timelineList.innerHTML = '';
        }
    }

    // Snippets
    getCodeSnippets() {
        return [
            {
                category: 'pilot',
                title: 'PILOT Input & Output',
                description: 'Get user input and display greeting',
                code: 'A:name\nT:Hello *name*!'
            },
            {
                category: 'pilot',
                title: 'PILOT Loop',
                description: 'Simple counting loop',
                code: 'R:1 -> i\nL:LOOP\nT:Count: *i*\nR:*i* + 1 -> i\nY:*i* <= 10\nJ:LOOP'
            },
            {
                category: 'pilot',
                title: 'PILOT Calculator',
                description: 'Simple addition calculator',
                code: 'A:a\nA:b\nR:*a* + *b* -> sum\nT:*a* + *b* = *sum*'
            },
            {
                category: 'basic',
                title: 'BASIC For Loop',
                description: 'FOR loop counting 1 to 10',
                code: 'FOR I = 1 TO 10\n  PRINT "Count:", I\nNEXT I'
            },
            {
                category: 'basic',
                title: 'BASIC Input',
                description: 'Get user input and display',
                code: 'INPUT "Enter your name: "; NAME$\nPRINT "Hello "; NAME$'
            },
            {
                category: 'basic',
                title: 'BASIC Array',
                description: 'Array processing example',
                code: 'DIM A(10)\nFOR I = 1 TO 10\n  A(I) = I * 2\n  PRINT A(I)\nNEXT I'
            },
            {
                category: 'logo',
                title: 'Logo Square',
                description: 'Draw a square with turtle',
                code: 'REPEAT 4 [\n  FORWARD 100\n  RIGHT 90\n]'
            },
            {
                category: 'logo',
                title: 'Logo Circle',
                description: 'Draw a circle approximation',
                code: 'REPEAT 36 [\n  FORWARD 10\n  RIGHT 10\n]'
            },
            {
                category: 'logo',
                title: 'Logo Spiral',
                description: 'Draw a colorful spiral',
                code: 'REPEAT 100 [\n  FORWARD 2\n  RIGHT 91\n  SETCOLOR :REPCOUNT\n]'
            },
            {
                category: 'logo',
                title: 'Logo Flower',
                description: 'Draw a flower pattern',
                code: 'REPEAT 8 [\n  REPEAT 36 [\n    FORWARD 2\n    RIGHT 10\n  ]\n  RIGHT 45\n]'
            }
        ];
    }

    populateSnippets() {
        this.filterSnippets();
    }

    filterSnippets() {
        const category = document.getElementById('snippetCategory').value;
        const snippetsList = document.getElementById('snippetsList');
        if (!snippetsList) return;

        const filtered = category === 'all'
            ? this.snippets
            : this.snippets.filter(s => s.category === category);

        snippetsList.innerHTML = '';

        filtered.forEach(snippet => {
            const item = document.createElement('div');
            item.className = 'snippet-item';

            item.innerHTML = `
                <div class="snippet-title">${snippet.title}</div>
                <div class="snippet-description">${snippet.description}</div>
                <div class="snippet-code">${snippet.code}</div>
            `;

            item.addEventListener('click', () => {
                this.insertSnippet(snippet.code);
            });

            snippetsList.appendChild(item);
        });
    }

    insertSnippet(code) {
        const start = this.editor.selectionStart;
        const end = this.editor.selectionEnd;
        const before = this.editor.value.substring(0, start);
        const after = this.editor.value.substring(end);

        this.editor.value = before + code + after;
        this.editor.focus();
        this.editor.setSelectionRange(start + code.length, start + code.length);

        this.updateLineNumbers();
        this.updateCursorInfo();
    }

    // Help System
    getHelpContent() {
        return {
            overview: {
                title: 'Time Warp Overview',
                content: `
<h2>Welcome to Time Warp 4.0.0</h2>
<p>Time Warp is a comprehensive educational IDE supporting three programming languages:</p>
<ul>
<li><strong>PILOT</strong> - Simple, intuitive commands for beginners</li>
<li><strong>BASIC</strong> - Classic programming with line numbers and structured commands</li>
<li><strong>Logo</strong> - Graphics-focused language with turtle graphics</li>
</ul>

<h3>Getting Started</h3>
<p>1. Choose your language mode from the dropdown (Auto-Detect works well)</p>
<p>2. Type your program in the code editor</p>
<p>3. Click Run to execute, or Debug for step-by-step execution</p>
<p>4. Explore the different tabs to see output, variables, graphics, and more</p>

<h3>Features</h3>
<ul>
<li>Multi-language support with auto-detection</li>
<li>Interactive turtle graphics</li>
<li>Variable inspection and watches</li>
<li>Performance monitoring</li>
<li>Execution timeline</li>
<li>Code snippets library</li>
<li>Comprehensive debugging tools</li>
</ul>
                `
            },
            pilot: {
                title: 'PILOT Commands',
                content: `
<h2>PILOT Programming Language</h2>
<p>PILOT uses single-letter commands followed by a colon.</p>

<h3>Basic Commands</h3>
<ul>
<li><code>T:text</code> - Display text output</li>
<li><code>A:variable</code> - Accept input into variable</li>
<li><code>R:value -> variable</code> - Set variable to value</li>
<li><code>J:label</code> - Jump to label</li>
<li><code>L:label</code> - Define label</li>
<li><code>E:</code> - End program</li>
</ul>

<h3>Conditional Commands</h3>
<ul>
<li><code>Y:condition</code> - Execute next line if condition is true</li>
<li><code>N:condition</code> - Execute next line if condition is false</li>
</ul>

<h3>Variable Interpolation</h3>
<p>Use <code>*variable*</code> in text to insert variable values:</p>
<pre>A:name
T:Hello *name*!</pre>

<h3>Example Program</h3>
<pre>T:Enter two numbers:
A:a
A:b
R:*a* + *b* -> sum
T:*a* + *b* = *sum*
E:</pre>
                `
            },
            basic: {
                title: 'BASIC Commands',
                content: `
<h2>BASIC Programming Language</h2>
<p>BASIC uses English-like commands, optionally with line numbers.</p>

<h3>Basic Commands</h3>
<ul>
<li><code>PRINT text</code> - Display output</li>
<li><code>INPUT variable</code> - Get user input</li>
<li><code>LET variable = expression</code> - Assignment</li>
<li><code>GOTO line</code> - Jump to line number</li>
<li><code>END</code> - End program</li>
</ul>

<h3>Control Structures</h3>
<ul>
<li><code>IF condition THEN statement</code> - Conditional execution</li>
<li><code>FOR variable = start TO end</code> - Loop start</li>
<li><code>NEXT variable</code> - Loop end</li>
</ul>

<h3>Data Handling</h3>
<ul>
<li><code>DATA value1, value2, ...</code> - Define data</li>
<li><code>READ variable1, variable2, ...</code> - Read data</li>
<li><code>RESTORE</code> - Reset data pointer</li>
</ul>

<h3>Example Program</h3>
<pre>10 PRINT "Multiplication Table"
20 FOR I = 1 TO 10
30   PRINT I; " x 5 = "; I * 5
40 NEXT I
50 END</pre>
                `
            },
            logo: {
                title: 'Logo Commands',
                content: `
<h2>Logo Programming Language</h2>
<p>Logo is designed for graphics programming with turtle graphics.</p>

<h3>Movement Commands</h3>
<ul>
<li><code>FORWARD distance</code> (or <code>FD</code>) - Move forward</li>
<li><code>BACK distance</code> (or <code>BK</code>) - Move backward</li>
<li><code>RIGHT angle</code> (or <code>RT</code>) - Turn right</li>
<li><code>LEFT angle</code> (or <code>LT</code>) - Turn left</li>
</ul>

<h3>Pen Commands</h3>
<ul>
<li><code>PENUP</code> (or <code>PU</code>) - Lift pen (don't draw)</li>
<li><code>PENDOWN</code> (or <code>PD</code>) - Lower pen (draw)</li>
<li><code>SETCOLOR color</code> - Set pen color</li>
</ul>

<h3>Screen Commands</h3>
<ul>
<li><code>CLEARSCREEN</code> (or <code>CS</code>) - Clear graphics</li>
<li><code>HOME</code> - Return to center, heading up</li>
</ul>

<h3>Control Structures</h3>
<ul>
<li><code>REPEAT count [commands]</code> - Repeat commands</li>
</ul>

<h3>Example Program</h3>
<pre>CLEARSCREEN
REPEAT 4 [
  FORWARD 100
  RIGHT 90
]
PENUP
FORWARD 50
PENDOWN
REPEAT 36 [
  FORWARD 5
  RIGHT 10
]</pre>
                `
            },
            examples: {
                title: 'Example Programs',
                content: `
<h2>Example Programs</h2>

<h3>PILOT: Simple Calculator</h3>
<pre>T:Simple Calculator
A:first
A:second
R:*first* + *second* -> sum
R:*first* - *second* -> diff
T:*first* + *second* = *sum*
T:*first* - *second* = *diff*
E:</pre>

<h3>BASIC: Guessing Game</h3>
<pre>10 PRINT "Guess my number (1-10)!"
20 LET SECRET = 7
30 INPUT "Your guess"; GUESS
40 IF GUESS = SECRET THEN GOTO 70
50 PRINT "Wrong! Try again."
60 GOTO 30
70 PRINT "Correct! You win!"
80 END</pre>

<h3>Logo: Colorful Spiral</h3>
<pre>CLEARSCREEN
REPEAT 100 [
  SETCOLOR REPCOUNT
  FORWARD REPCOUNT / 2
  RIGHT 91
]</pre>

<h3>Cross-Language Features</h3>
<p>All languages support:</p>
<ul>
<li>Variable interpolation with *variable*</li>
<li>Mathematical expressions</li>
<li>Conditional logic</li>
<li>Comments with # at start of line</li>
</ul>
                `
            }
        };
    }

    populateHelp() {
        this.updateHelpContent();
    }

    updateHelpContent() {
        const helpLanguage = document.getElementById('helpLanguage').value;
        const helpContent = document.getElementById('helpContent');

        if (helpContent && this.helpContent[helpLanguage]) {
            helpContent.innerHTML = this.helpContent[helpLanguage].content;
        }
    }

    // Error Handling
    showError(message, line = null) {
        const errorDialog = document.getElementById('errorDialog');
        const errorMessage = document.getElementById('errorMessage');

        if (errorDialog && errorMessage) {
            const fullMessage = line ? `Line ${line}: ${message}` : message;
            errorMessage.textContent = fullMessage;
            errorDialog.classList.remove('hidden');
        }

        // Also add to output
        const errorText = line ? `❌ Line ${line}: ${message}` : `❌ ${message}`;
        this.addOutput(errorText, 'error');
    }

    hideError() {
        const errorDialog = document.getElementById('errorDialog');
        if (errorDialog) {
            errorDialog.classList.add('hidden');
        }
    }

    // Utility Functions
    toggleFullscreen() {
        if (!document.fullscreenElement) {
            document.documentElement.requestFullscreen();
        } else {
            document.exitFullscreen();
        }
    }

    updateMemoryUsage() {
        try {
            // Simulate memory usage tracking with safe fallback
            let usage;
            if (performance.memory && performance.memory.usedJSHeapSize) {
                usage = (performance.memory.usedJSHeapSize / 1024 / 1024).toFixed(1);
            } else {
                usage = (Math.random() * 10 + 5).toFixed(1);
            }

            const memoryEl = document.getElementById('memoryUsage');
            if (memoryEl) {
                memoryEl.textContent = `Memory: ${usage} MB`;
            }

            // Update periodically
            setTimeout(() => this.updateMemoryUsage(), 5000);
        } catch (error) {
            console.warn('Error updating memory usage:', error);
        }
    }
}