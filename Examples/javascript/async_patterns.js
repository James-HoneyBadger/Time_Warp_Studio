/**
 * async_patterns.js -- Async Patterns in JavaScript
 * Demonstrates: error-first callbacks, event emitter, task queue,
 * retry pattern, and continuation passing style (CPS).
 * All patterns are synchronous simulations for compatibility.
 */

'use strict';

// == ERROR-FIRST CALLBACKS ==

console.log('=== Error-First Callbacks ===');

function fetchUser(id, callback) {
    if (id <= 0) { callback('Invalid ID', null); return; }
    callback(null, { id: id, name: 'User' + String(id), role: 'member' });
}

function fetchBadges(user, callback) {
    var badgeMap = { 'u1': ['starter', 'learner'], 'u2': ['expert', 'mentor', 'star'] };
    var key = 'u' + String(user.id);
    var badges = badgeMap[key];
    if (badges === undefined) { badges = ['newcomer']; }
    callback(null, badges);
}

function onBadges(err2, badges) {
    if (err2) { console.log('Error loading badges:', err2); return; }
    console.log('Badges:', badges.join(', '));
}

function onUser(err, user) {
    if (err) { console.log('Error:', err); return; }
    console.log('User:', user.name, '(' + user.role + ')');
    fetchBadges(user, onBadges);
}

fetchUser(1, onUser);
fetchUser(2, onUser);
fetchUser(-1, onUser);

// == SIMPLE EVENT EMITTER ==

console.log('\n=== Simple Event Emitter ===');

function makeEventEmitter() {
    return { listeners: {} };
}

function emitterOn(emitter, event, fn) {
    if (!(event in emitter.listeners)) { emitter.listeners[event] = []; }
    emitter.listeners[event].push(fn);
}

function emitterEmit(emitter, event, data) {
    if (!(event in emitter.listeners)) { return; }
    var fns = emitter.listeners[event];
    for (var i = 0; i < fns.length; i++) { fns[i](data); }
}

var bus = makeEventEmitter();
function onLoginMsg(u) { console.log('User logged in:', u.name); }
function onLoginAudit(u) { console.log('Audit: login from', u.name); }
function onLogout(u) { console.log('User logged out:', u.name); }
emitterOn(bus, 'login', onLoginMsg);
emitterOn(bus, 'login', onLoginAudit);
emitterOn(bus, 'logout', onLogout);

emitterEmit(bus, 'login',  { name: 'Alice' });
emitterEmit(bus, 'login',  { name: 'Bob' });
emitterEmit(bus, 'logout', { name: 'Alice' });

// == TASK QUEUE ==

console.log('\n=== Task Queue ===');

function runTasks(tasks, onDone) {
    var results = [];
    var failed = false;
    for (var i = 0; i < tasks.length; i++) {
        var taskResult = tasks[i]();
        if (taskResult.err) { onDone(taskResult.err, null); failed = true; break; }
        results.push(taskResult.val);
    }
    if (!failed) { onDone(null, results); }
}

function makeTask(label, value) {
    return function() { console.log('Running task:', label); return { err: null, val: value }; };
}

function onTasksDone(err, results) {
    if (err) { console.log('Task error:', err); return; }
    console.log('All results:', results.join(', '));
}

var tasks = [makeTask('fetch-data', 42), makeTask('process', 84), makeTask('store', 126)];
runTasks(tasks, onTasksDone);

// == RETRY PATTERN ==

console.log('\n=== Retry Pattern ===');

function withRetry(fn, maxAttempts) {
    var attempt = 0;
    var err = 'not started';
    var result = null;
    while (attempt < maxAttempts) {
        attempt = attempt + 1;
        var res = fn(attempt);
        if (!res.err) { return { err: null, val: res.val }; }
        err = res.err;
        if (attempt < maxAttempts) { console.log('Retrying (attempt ' + String(attempt) + ')...'); }
    }
    return { err: err, val: null };
}

function flakyOp(attempt) {
    if (attempt < 3) { return { err: 'Temporary error', val: null }; }
    return { err: null, val: 'Success on attempt ' + String(attempt) };
}

var retryResult = withRetry(flakyOp, 5);
if (retryResult.err) { console.log('Failed after retries:', retryResult.err); } else { console.log('Result:', retryResult.val); }

// == CONTINUATION PASSING STYLE ==

console.log('\n=== Continuation Passing Style (CPS) ===');

function cpsDouble(x, k)   { return k(x * 2); }
function cpsAddOne(x, k)   { return k(x + 1); }
function cpsSquare(x, k)   { return k(x * x); }

function cpsFactorial(n, k) {
    if (n <= 1) { return k(1); }
    return cpsFactorial(n - 1, r => k(n * r));
}

function cpsFib(n, k) {
    if (n <= 1) { return k(n); }
    return cpsFib(n - 1, a => cpsFib(n - 2, b => k(a + b)));
}

cpsDouble(5, r => console.log('double(5) =', r));
cpsAddOne(5, r => console.log('addOne(5) =', r));
cpsSquare(5, r => console.log('square(5) =', r));

cpsFactorial(8, r => console.log('factorial(8) =', r));
cpsFib(10,      r => console.log('fib(10) =', r));

// CPS pipeline: double -> addOne -> square
cpsDouble(3, a => cpsAddOne(a, b => cpsSquare(b, c => console.log('pipeline(3):', c))));

console.log('\nDone.');
