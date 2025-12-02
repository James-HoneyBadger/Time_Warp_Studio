// Headless test using Puppeteer to exercise demo-interpreter.html
const { spawn } = require('child_process');
const path = require('path');
const puppeteer = require('puppeteer');

const ROOT = path.resolve(__dirname, '..');
const PORT = process.env.PORT || 8123;
const URL = `http://127.0.0.1:${PORT}/demo-interpreter.html`;

function startServer() {
    return new Promise((resolve, reject) => {
        const proc = spawn('python3', ['-m', 'http.server', PORT], { cwd: ROOT, stdio: 'inherit' });
        proc.on('error', (err) => reject(err));
        // Small delay to let the server start
        setTimeout(() => resolve(proc), 800);
    });
}

(async () => {
    let server = null;
    try {
        server = await startServer();
        const browser = await puppeteer.launch({ args: ['--no-sandbox', '--disable-setuid-sandbox'] });
        const page = await browser.newPage();
        page.setDefaultTimeout(10000);

        console.log('Loading', URL);
        await page.goto(URL, { waitUntil: 'networkidle2' });

        // set a simple program that prints Done
        await page.evaluate(() => {
            const prog = document.getElementById('prog');
            prog.value = 'FORWARD 10\nPRINT Done\nEND';
        });

        // intercept console messages from the page to show here
        page.on('console', msg => console.log('PAGE:', msg.text()));

        await page.click('#run');

        // wait for the output to show 'Done' in the console div
        await page.waitForFunction(() => {
            const c = document.getElementById('console');
            if (!c) return false;
            return c.textContent.includes('Done');
        }, { timeout: 8000 });

        console.log('Test passed: program completed and produced "Done"');

        await browser.close();
    } catch (err) {
        console.error('Test failed', err);
        process.exitCode = 2;
    } finally {
        if (server) {
            server.kill('SIGTERM');
            // small delay
            await new Promise(r=>setTimeout(r,200));
        }
    }
})();
