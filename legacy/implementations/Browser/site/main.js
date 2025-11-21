import init, { run_code } from './pkg/time.warp_web.js';

const codeEl = document.getElementById('code');
const canvas = document.getElementById('canvas');
const ctx = canvas.getContext('2d');
const consoleEl = document.getElementById('console');
const statusEl = document.getElementById('status');

const exampleSelect = document.getElementById('exampleSelect');
const loadBtn = document.getElementById('loadBtn');
const runBtn = document.getElementById('runBtn');
const resetBtn = document.getElementById('resetBtn');
const clearBtn = document.getElementById('clearBtn');
const saveBtn = document.getElementById('saveBtn');
const selfTestBtn = document.getElementById('selfTestBtn');

const EXAMPLES = [
    { name: 'Hello + Line', path: 'examples/hello_line.tc' },
    { name: 'Square', path: 'examples/square.tc' },
];

EXAMPLES.forEach((e, i) => {
    const opt = document.createElement('option');
    opt.value = String(i);
    opt.textContent = e.name;
    exampleSelect.appendChild(opt);
});

async function loadExample() {
    const idx = Number(exampleSelect.value || 0);
    const path = EXAMPLES[idx]?.path;
    if (!path) return;
    try {
        const text = await fetch(path).then(r => r.text());
        codeEl.value = text;
        statusEl.textContent = `Loaded: ${EXAMPLES[idx].name}`;
    } catch (e) {
        statusEl.textContent = `Error loading example: ${e}`;
    }
}

let lastResult = null;

function resizeCanvas() {
    const dpr = window.devicePixelRatio || 1;
    const cssWidth = canvas.clientWidth;
    const cssHeight = canvas.clientHeight;
    // Set the internal pixel size to match CSS size * DPR for crisp rendering
    const targetW = Math.max(1, Math.floor(cssWidth * dpr));
    const targetH = Math.max(1, Math.floor(cssHeight * dpr));
    if (canvas.width !== targetW || canvas.height !== targetH) {
        canvas.width = targetW;
        canvas.height = targetH;
        // Redraw last result after resize
        if (lastResult) {
            draw(lastResult);
        } else {
            resetCanvas();
        }
    }
}

function draw(result) {
    const { width, height, lines, console: out } = result;
    // Clear
    ctx.fillStyle = 'black';
    ctx.fillRect(0, 0, canvas.width, canvas.height);

    const sx = canvas.width / Math.max(1, width);
    const sy = canvas.height / Math.max(1, height);

    const dpr = window.devicePixelRatio || 1;
    ctx.lineWidth = 1.5 * dpr;
    for (const seg of lines) {
        const [r, g, b] = seg.color;
        ctx.strokeStyle = `rgb(${r},${g},${b})`;
        ctx.beginPath();
        ctx.moveTo(seg.x1 * sx, seg.y1 * sy);
        ctx.lineTo(seg.x2 * sx, seg.y2 * sy);
        ctx.stroke();
    }
    consoleEl.textContent = out || '';
}

async function run() {
    statusEl.textContent = 'Running...';
    try {
        const result = run_code(codeEl.value);
        lastResult = result;
        draw(result);
        statusEl.textContent = 'Done';
    } catch (e) {
        statusEl.textContent = `Error: ${e}`;
    }
}

function resetCanvas() {
    ctx.fillStyle = 'black';
    ctx.fillRect(0, 0, canvas.width, canvas.height);
}

function clearConsole() {
    consoleEl.textContent = '';
}

function savePNG() {
    const url = canvas.toDataURL('image/png');
    const a = document.createElement('a');
    const ts = Math.floor(Date.now() / 1000);
    a.href = url;
    a.download = `time.warp_${ts}.png`;
    document.body.appendChild(a);
    a.click();
    a.remove();
}

async function selfTest() {
    // Minimal drawing to validate lines render
    const testCode = [
        'REM Self Test',
        'PEN RGB(255,0,0)',
        'FD 80',
        'RT 120',
        'PEN RGB(0,255,0)',
        'FD 80',
        'RT 120',
        'PEN RGB(0,128,255)',
        'FD 80',
    ].join('\n');
    statusEl.textContent = 'Self test running...';
    try {
        const result = run_code(testCode);
        lastResult = result;
        draw(result);
        const n = (result.lines || []).length;
        if (n > 0) {
            statusEl.textContent = `Graphics OK (lines: ${n})`;
        } else {
            statusEl.textContent = 'Graphics FAILED (no lines)';
        }
    } catch (e) {
        statusEl.textContent = `Self test error: ${e}`;
    }
}

window.addEventListener('keydown', (e) => {
    if ((e.ctrlKey || e.metaKey) && e.key === 'Enter') {
        e.preventDefault();
        run();
    }
});

window.addEventListener('resize', resizeCanvas);

runBtn.addEventListener('click', run);
resetBtn.addEventListener('click', resetCanvas);
clearBtn.addEventListener('click', clearConsole);
loadBtn.addEventListener('click', loadExample);
saveBtn.addEventListener('click', savePNG);
selfTestBtn.addEventListener('click', selfTest);

codeEl.value = 'REM Time Warp (Web)\nPRINT "Hello from Web"\nFOR I = 1 TO 4\n  PRINT I\nNEXT\nCOLOR 0,255,0\nFD 100\nRT 90\nFD 100\n';
resetCanvas();

// Initialize wasm and load default example
await init();
await loadExample();
resizeCanvas();
await run();
