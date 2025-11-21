// Direct canvas test for Time Warp IDE
// This bypasses all interpreter logic and directly draws on the canvas

function directCanvasTest() {
    console.log('=== DIRECT CANVAS TEST ===');

    // Find the graphics canvas
    const canvas = document.getElementById('graphicsCanvas');
    console.log('Canvas found:', !!canvas);

    if (!canvas) {
        console.error('No canvas found!');
        return false;
    }

    // Get context
    const ctx = canvas.getContext('2d');
    console.log('Context:', !!ctx);
    console.log('Canvas dimensions:', canvas.width, 'x', canvas.height);
    console.log('Canvas style:', canvas.style.width, 'x', canvas.style.height);

    // Clear and draw
    console.log('Clearing canvas...');
    ctx.fillStyle = 'lightgreen';
    ctx.fillRect(0, 0, canvas.width, canvas.height);

    // Draw a border
    console.log('Drawing border...');
    ctx.strokeStyle = 'red';
    ctx.lineWidth = 5;
    ctx.strokeRect(5, 5, canvas.width - 10, canvas.height - 10);

    // Draw some shapes
    console.log('Drawing shapes...');
    ctx.fillStyle = 'blue';
    ctx.fillRect(50, 50, 100, 100);

    ctx.fillStyle = 'yellow';
    ctx.beginPath();
    ctx.arc(300, 300, 50, 0, 2 * Math.PI);
    ctx.fill();

    // Draw text
    ctx.fillStyle = 'black';
    ctx.font = '20px Arial';
    ctx.fillText('DIRECT TEST', 150, 250);

    console.log('Direct canvas test completed');
    return true;
}

function testGraphicsTab() {
    console.log('=== GRAPHICS TAB TEST ===');

    // Switch to graphics tab first
    const graphicsTab = document.querySelector('[data-tab="graphics"]');
    if (graphicsTab) {
        console.log('Clicking graphics tab...');
        graphicsTab.click();

        // Wait a moment for tab to switch
        setTimeout(() => {
            directCanvasTest();
        }, 500);
    } else {
        console.error('Graphics tab button not found');
        directCanvasTest();
    }
}

function testSimpleGraphics() {
    console.log('=== SIMPLE GRAPHICS TEST ===');

    // Switch to graphics tab first
    const graphicsTab = document.querySelector('[data-tab="graphics"]');
    if (graphicsTab) {
        console.log('Clicking graphics tab...');
        graphicsTab.click();

        setTimeout(() => {
            const canvas = document.getElementById('graphicsCanvas');
            if (canvas) {
                console.log('Creating SimpleTurtleGraphics...');
                const turtle = new SimpleTurtleGraphics(canvas);

                console.log('Drawing test pattern...');
                turtle.clearScreen();
                turtle.forward(100);
                turtle.right(90);
                turtle.forward(100);
                turtle.right(90);
                turtle.forward(100);
                turtle.right(90);
                turtle.forward(100);

                console.log('Simple graphics test completed');
            } else {
                console.error('Canvas not found for simple graphics test');
            }
        }, 500);
    } else {
        console.error('Graphics tab button not found');
    }
}

function testLanguageDetection() {
    console.log('=== LANGUAGE DETECTION TEST ===');

    // Get the UI instance
    const ui = window.app;
    if (!ui || !ui.interpreter) {
        console.error('UI or interpreter not found');
        return;
    }

    const interpreter = ui.interpreter;

    const testLines = [
        'FORWARD 100',
        'RIGHT 90',
        'CLEARSCREEN',
        'T:Hello world',
        'PRINT "test"',
        'LET X = 5'
    ];

    testLines.forEach(line => {
        const detected = interpreter.detectLineLanguage(line);
        console.log(`"${line}" -> ${detected}`);
    });
}

function testMinimalProgram() {
    console.log('=== MINIMAL PROGRAM TEST ===');

    const ui = window.app;
    if (!ui) {
        console.error('UI not found');
        return;
    }

    // Set a minimal program
    ui.editor.value = `FORWARD 50
RIGHT 90
FORWARD 50`;

    console.log('Minimal program set, now run it manually');
}

// Add this to global scope for easy testing
window.directCanvasTest = directCanvasTest;
window.testGraphicsTab = testGraphicsTab;
window.testSimpleGraphics = testSimpleGraphics;
window.testLanguageDetection = testLanguageDetection;
window.testMinimalProgram = testMinimalProgram;

console.log('Direct graphics test functions loaded');
console.log('Available tests: testGraphicsTab(), directCanvasTest(), testSimpleGraphics(), testLanguageDetection(), testMinimalProgram()');