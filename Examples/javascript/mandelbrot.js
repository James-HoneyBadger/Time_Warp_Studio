// ============================================================
// MANDELBROT SET — JavaScript Showcase
// ASCII art, zoomed views, Julia sets, escape time coloring
// Time Warp Studio — JavaScript Language Demo
// ============================================================

// ============================================================
// CORE: Mandelbrot escape-time algorithm
// ============================================================

function mandelbrot(cx, cy, maxIter) {
    let zx = 0, zy = 0;
    let i = 0;
    while (i < maxIter && zx * zx + zy * zy < 4.0) {
        let tmp = zx * zx - zy * zy + cx;
        zy = 2 * zx * zy + cy;
        zx = tmp;
        i++;
    }
    return i;
}

function julia(zx, zy, cx, cy, maxIter) {
    let i = 0;
    while (i < maxIter && zx * zx + zy * zy < 4.0) {
        let tmp = zx * zx - zy * zy + cx;
        zy = 2 * zx * zy + cy;
        zx = tmp;
        i++;
    }
    return i;
}

function burning_ship(cx, cy, maxIter) {
    let zx = 0, zy = 0;
    let i = 0;
    while (i < maxIter && zx * zx + zy * zy < 4.0) {
        let tmp = zx * zx - zy * zy + cx;
        zy = 2 * Math.abs(zx) * Math.abs(zy) + cy;
        zx = tmp;
        i++;
    }
    return i;
}

function tricorn(cx, cy, maxIter) {
    let zx = 0, zy = 0;
    let i = 0;
    while (i < maxIter && zx * zx + zy * zy < 4.0) {
        let tmp = zx * zx - zy * zy + cx;
        zy = -2 * zx * zy + cy;
        zx = tmp;
        i++;
    }
    return i;
}

// ============================================================
// RENDERING
// ============================================================

// ASCII gradient from low to high iteration count
const DENSITY = ' .`-_\':,;^=+/"|)\\<>)iv%xclrs{*}I?!][1taeo7zjLunT#JCwfy395Vbghd6PKqmED0W&8%B@$';

function iterToChar(iter, maxIter) {
    if (iter >= maxIter) return ' ';  // inside the set
    const idx = Math.floor((iter / maxIter) * (DENSITY.length - 1));
    return DENSITY[idx];
}

// Simpler two-level coloring for readability
function iterToCharSimple(iter, maxIter) {
    if (iter >= maxIter) return '█';
    if (iter > maxIter * 0.8) return '▓';
    if (iter > maxIter * 0.6) return '▒';
    if (iter > maxIter * 0.4) return '░';
    if (iter > maxIter * 0.2) return '.';
    return ' ';
}

function render(fn, xMin, xMax, yMin, yMax, cols, rows, maxIter, charFn) {
    const lines = [];
    for (let row = 0; row < rows; row++) {
        const cy = yMin + (yMax - yMin) * row / rows;
        let line = '';
        for (let col = 0; col < cols; col++) {
            const cx = xMin + (xMax - xMin) * col / cols;
            const iter = fn(cx, cy, maxIter);
            line += charFn(iter, maxIter);
        }
        lines.push(line);
    }
    return lines;
}

function printFrame(lines, title) {
    const w = lines[0].length;
    console.log('  ╔' + '═'.repeat(w) + '╗');
    for (const line of lines) {
        console.log('  ║' + line + '║');
    }
    console.log('  ╚' + '═'.repeat(w) + '╝');
    if (title) console.log('  ' + title);
}

// ============================================================
// STATS ON A RENDERED FRACTAL
// ============================================================

function fractalStats(fn, xMin, xMax, yMin, yMax, cols, rows, maxIter) {
    let inside = 0, total = rows * cols;
    let iterSum = 0;
    for (let row = 0; row < rows; row++) {
        const cy = yMin + (yMax - yMin) * row / rows;
        for (let col = 0; col < cols; col++) {
            const cx = xMin + (xMax - xMin) * col / cols;
            const iter = fn(cx, cy, maxIter);
            if (iter >= maxIter) inside++;
            iterSum += iter;
        }
    }
    return {
        inside,
        total,
        pct: (inside / total * 100).toFixed(1),
        avgIter: (iterSum / total).toFixed(2)
    };
}

// ============================================================
// INTERESTING ZOOM COORDINATES
// ============================================================

const ZOOM_POINTS = [
    { name: "Seahorse Valley",  x: -0.743, y: 0.127,  zoom: 0.04 },
    { name: "Elephant Valley",  x:  0.300, y: 0.020,  zoom: 0.08 },
    { name: "Triple Spiral",    x: -0.088, y: 0.654,  zoom: 0.06 },
    { name: "Tip of Main Bud",  x: -1.250, y: 0.000,  zoom: 0.10 },
];

// ============================================================
// MAIN PROGRAM
// ============================================================

console.log('============================================================');
console.log('  MANDELBROT SET — JavaScript Showcase');
console.log('  Fractals, complex dynamics, escape-time coloring');
console.log('============================================================\n');

// ---- View 1: Full Mandelbrot ----
console.log('SECTION 1: FULL MANDELBROT SET (-2.5 to 1.0, -1.2 to 1.2)');
console.log('------------------------------------------------------------\n');
{
    const rows = 24, cols = 72, maxIter = 64;
    const lines = render(
        (cx, cy, mi) => mandelbrot(cx, cy, mi),
        -2.5, 1.0, -1.2, 1.2,
        cols, rows, maxIter, iterToCharSimple
    );
    printFrame(lines, 'Mandelbrot Set — full view');
    const stats = fractalStats(
        (cx, cy, mi) => mandelbrot(cx, cy, mi),
        -2.5, 1.0, -1.2, 1.2,
        cols, rows, maxIter
    );
    console.log(`  Points inside set: ${stats.inside}/${stats.total} (${stats.pct}%)`);
    console.log(`  Average escape iterations: ${stats.avgIter}\n`);
}

// ---- View 2: Zoom views ----
console.log('SECTION 2: ZOOM INTO INTERESTING REGIONS');
console.log('------------------------------------------------------------\n');

for (const zp of ZOOM_POINTS) {
    console.log(`  Zooming into: ${zp.name} at (${zp.x}, ${zp.y})`);
    const rows = 18, cols = 60, maxIter = 150;
    const xMin = zp.x - zp.zoom, xMax = zp.x + zp.zoom;
    const yMin = zp.y - zp.zoom * rows / cols;
    const yMax = zp.y + zp.zoom * rows / cols;
    const lines = render(
        (cx, cy, mi) => mandelbrot(cx, cy, mi),
        xMin, xMax, yMin, yMax,
        cols, rows, maxIter, iterToCharSimple
    );
    printFrame(lines, `  ${zp.name}`);
    console.log();
}

// ---- View 3: Julia sets ----
console.log('SECTION 3: JULIA SETS (related to Mandelbrot)');
console.log('------------------------------------------------------------');
console.log('Each point c in the Mandelbrot set gives a connected Julia set.\n');

const JULIA_PARAMS = [
    { cx: -0.7269,  cy:  0.1889, name: "Dragon spiral" },
    { cx: -0.4,     cy:  0.6,    name: "Rabbit Julia" },
    { cx:  0.285,   cy:  0.01,   name: "Dendrite" },
    { cx: -0.835,   cy: -0.2321, name: "Siegel disk" },
];

for (const jp of JULIA_PARAMS) {
    console.log(`  Julia set c=(${jp.cx}, ${jp.cy}) — ${jp.name}`);
    const rows = 16, cols = 56, maxIter = 100;
    const lines = render(
        (zx, zy, mi) => julia(zx, zy, jp.cx, jp.cy, mi),
        -1.5, 1.5, -1.0, 1.0,
        cols, rows, maxIter, iterToCharSimple
    );
    printFrame(lines, `  ${jp.name}`);
    console.log();
}

// ---- View 4: Burning Ship fractal ----
console.log('SECTION 4: BURNING SHIP FRACTAL');
console.log('------------------------------------------------------------');
console.log('Variant: |Re(z)| and |Im(z)| before squaring\n');
{
    const rows = 20, cols = 64, maxIter = 80;
    const lines = render(
        (cx, cy, mi) => burning_ship(cx, cy, mi),
        -2.5, 1.5, -2.0, 0.5,
        cols, rows, maxIter, iterToCharSimple
    );
    printFrame(lines, 'Burning Ship fractal');
    console.log();
}

// ---- View 5: Tricorn / Mandelbar ----
console.log('SECTION 5: TRICORN (MANDELBAR) FRACTAL');
console.log('------------------------------------------------------------');
console.log('Uses conjugate of z instead of z during iteration\n');
{
    const rows = 20, cols = 64, maxIter = 80;
    const lines = render(
        (cx, cy, mi) => tricorn(cx, cy, mi),
        -2.5, 1.0, -1.2, 1.2,
        cols, rows, maxIter, iterToCharSimple
    );
    printFrame(lines, 'Tricorn fractal');
    console.log();
}

// ---- View 6: Detail with density gradient ----
console.log('SECTION 6: HIGH-DETAIL DENSITY GRADIENT (Seahorse Valley)');
console.log('------------------------------------------------------------\n');
{
    const rows = 22, cols = 70, maxIter = 200;
    const lines = render(
        (cx, cy, mi) => mandelbrot(cx, cy, mi),
        -0.783, -0.703, 0.087, 0.167,
        cols, rows, maxIter, iterToChar
    );
    printFrame(lines, 'Seahorse Valley — density gradient coloring');
    console.log();
}

// ---- Mathematical explanation ----
console.log('============================================================');
console.log('  MATHEMATICAL BACKGROUND');
console.log('============================================================');
console.log('  Mandelbrot: z_(n+1) = z_n² + c, start z_0 = 0');
console.log('  Point c is IN the set if |z_n| stays bounded forever');
console.log('  Boundary is a fractal with infinite complexity');
console.log('  Hausdorff dimension ≈ 2 (fills 2D area near boundary)');
console.log('  Julia set for c: same iteration, z_0 = point being tested');
console.log('  Connected Julia set ⟺ c is in the Mandelbrot set');
console.log('  Mandelbrot set is simply-connected and locally connected');
console.log('  Area ≈ 1.5065918849... (computed by numerical integration)');
console.log('============================================================');
