/**
 * functional.js -- Functional Programming in JavaScript
 * Demonstrates: closures, higher-order functions, memoization,
 * partial application, pipelines, and immutable patterns.
 */

'use strict';

// == HIGHER-ORDER FUNCTIONS ==

console.log('=== Higher-Order Functions ===');

function makeAdder(n) { return x => x + n; }
function makeMultiplier(n) { return x => x * n; }
function makeGreaterThan(n) { return x => x > n; }

var add10  = makeAdder(10);
var double = makeMultiplier(2);
var triple = makeMultiplier(3);
var gt5    = makeGreaterThan(5);

console.log('add10(5)   =', add10(5));
console.log('double(7)  =', double(7));
console.log('triple(4)  =', triple(4));
console.log('[1..10].filter(>5) =', [1,2,3,4,5,6,7,8,9,10].filter(gt5));

// == FUNCTION COMPOSITION ==

console.log('\n=== Function Composition ===');

function compose2(f, g) { return x => f(g(x)); }
function pipe2(f, g)    { return x => g(f(x)); }

var trim       = s => s.trim();
var toLower    = s => s.toLowerCase();
var splitWords = s => s.split(' ');
var countWords = a => a.length;

var trimAndLower  = pipe2(trim, toLower);
var splitAndCount = compose2(countWords, splitWords);
var wordCount     = pipe2(trimAndLower, splitAndCount);

console.log('trimAndLower("  FORTH  ") =', trimAndLower('  FORTH  '));
console.log('wordCount("  Hello World Erlang  ") =', wordCount('  Hello World Erlang  '));

var inc    = makeAdder(1);
var square = x => x * x;
var incThenSquare = compose2(square, inc);
var squareThenInc = pipe2(square, inc);

console.log('incThenSquare(4) =', incThenSquare(4));
console.log('squareThenInc(4) =', squareThenInc(4));

// == MEMOIZATION ==

console.log('\n=== Memoization ===');

function memoize(fn) {
    var cache = {};
    function memoized(n) {
        if (n in cache) { return cache[n]; }
        var r = fn(n);
        cache[n] = r;
        return r;
    }
    return memoized;
}

var stats = { calls: 0 };
function rawFib(n) {
    stats.calls = stats.calls + 1;
    if (n <= 1) { return n; }
    return memoFib(n - 1) + memoFib(n - 2);
}
var memoFib = memoize(rawFib);

var fibNums = [10, 15, 20, 10, 15].map(memoFib);
console.log('fib(10,15,20,10,15) =', fibNums);
console.log('Total calls:', stats.calls, '(memoization avoids redundant computation)');

// == CLOSURES AND ACCUMULATORS ==

console.log('\n=== Closures & Accumulators ===');

function makeCounter(start) {
    var state = { count: start || 0 };
    function inc()   { state.count = state.count + 1; return state.count; }
    function dec()   { state.count = state.count - 1; return state.count; }
    function reset() { state.count = start || 0; return state.count; }
    function val()   { return state.count; }
    return { inc: inc, dec: dec, reset: reset, val: val };
}

var c = makeCounter(0);
console.log('inc:', c.inc(), c.inc(), c.inc());
console.log('dec:', c.dec());
console.log('get:', c.val());
c.reset();
console.log('after reset:', c.val());

function makeAccumulator(fn, init) {
    var state = { acc: init };
    function step(val) { state.acc = fn(state.acc, val); return state.acc; }
    return step;
}

function addPair(a, b) { return a + b; }
var runningSum = makeAccumulator(addPair, 0);
var running = [1, 2, 3, 4, 5].map(runningSum);
console.log('running sum [1..5]:', running);

// == PARTIAL APPLICATION ==

console.log('\n=== Partial Application ===');

function formatNumber(prefix, value, suffix) {
    var rounded = String(Math.round(value * 100) / 100);
    return prefix + rounded + suffix;
}

function formatCurrency(v) { return formatNumber('$', v, ''); }
function formatPercent(v)  { return formatNumber('', v, '%'); }

console.log('formatCurrency(1234.5) =', formatCurrency(1234.5));
console.log('formatPercent(98.6)    =', formatPercent(98.6));

// == IMMUTABLE ARRAY OPS ==

console.log('\n=== Immutable Array Ops ===');

function updateAt(xs, i, v) { var before = xs.slice(0, i); var after = xs.slice(i + 1); return before.concat([v]).concat(after); }
function removeAt(xs, i)    { var before = xs.slice(0, i); var after = xs.slice(i + 1); return before.concat(after); }
function insertAt(xs, i, v) { var before = xs.slice(0, i); var after = xs.slice(i);     return before.concat([v]).concat(after); }

var original = [1, 2, 3, 4, 5];
console.log('original       =', original);
console.log('updateAt(2,99) =', updateAt(original, 2, 99));
console.log('removeAt(1)    =', removeAt(original, 1));
console.log('insertAt(3,42) =', insertAt(original, 3, 42));
console.log('original still =', original);

// == TRANSFORMATION PIPELINE ==

console.log('\n=== Transformation Pipeline ===');

var numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
var isEven   = x => x % 2 === 0;
var sqrIt    = x => x * x;
var sumUp    = (acc, x) => acc + x;
var result   = numbers.filter(isEven).map(sqrIt).reduce(sumUp, 0);

console.log('numbers:', numbers);
console.log('sum of squares of evens:', result);

var nested = [[1,2],[3,4],[5,6]];
var flatCat = (acc, arr) => acc.concat(arr);
var flat    = nested.reduce(flatCat, []);
console.log('flatten([[1,2],[3,4],[5,6]]):', flat);

// == GROUP BY ==

console.log('\n=== Group By ===');

var words = ['apple', 'banana', 'cherry', 'avocado', 'blueberry', 'cranberry'];
function groupWordsByLetter(words) {
    var result = {};
    for (var j = 0; j < words.length; j++) {
        var w = words[j];
        var k = w[0];
        if (k in result) { result[k].push(w); } else { result[k] = [w]; }
    }
    return result;
}
var byLetter = groupWordsByLetter(words);
var letters = Object.keys(byLetter).sort();
for (var i = 0; i < letters.length; i++) {
    var k = letters[i];
    var list = byLetter[k];
    console.log(' ', k + ':', list.join(', '));
}

console.log('\nDone.');
