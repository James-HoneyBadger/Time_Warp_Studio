// Node-based tests for WebInterpreter (no browser required)
// This script runs the interpreter in Node using a minimal global.window shim

(async () => {
    global.window = {};

    require('../js/interpreter.js');
    const I = new window.WebInterpreter();

    // Capture outputs
    let outs = [];
    I.onOutput = (m) => { outs.push(String(m).trim()); };

    // BASIC test
    outs = [];
    I.load('10 PRINT "hello"\n20 END');
    await I.run();
    if (!outs.some(s => s.includes('hello'))) {
        console.error('BASIC test failed — expected output "hello"', outs);
        process.exit(2);
    }
    console.log('BASIC test passed');

    // PILOT test
    outs = [];
    I.reset();
    I.load('R: 42 -> X\nT: Value is *X*\nE:');
    await I.run();
    if (!outs.some(s => s.includes('Value is 42'))) {
        console.error('PILOT test failed', outs);
        process.exit(2);
    }
    console.log('PILOT test passed');

    // Logo test (graphics not available in node - we test print + control)
    outs = [];
    I.reset();
    I.load('PRINT Done\nEND');
    await I.run();
    if (!outs.some(s => s.includes('Done'))) {
        console.error('Logo/basic print test failed', outs);
        process.exit(2);
    }
    console.log('Logo/print test passed');

    console.log('All node-based interpreter tests passed');
})();
