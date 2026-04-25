// ============================================================
// ALGORITHM MASTERCLASS — JavaScript Showcase
// Data Structures * Sorting * Search * Functional Programming
// Time Warp Studio — JavaScript Language Demo
// ============================================================

// ===== DATA STRUCTURES =====

class Stack {
    constructor() { this._data = []; }
    push(x)     { this._data.push(x); }
    pop()       { return this._data.pop(); }
    peek()      { return this._data[this._data.length - 1]; }
    isEmpty()   { return this._data.length === 0; }
    size()      { return this._data.length; }
    toString()  { return `Stack[${this._data.join(', ')}]`; }
}

class Queue {
    constructor() { this._data = []; }
    enqueue(x)  { this._data.push(x); }
    dequeue()   { return this._data.shift(); }
    front()     { return this._data[0]; }
    isEmpty()   { return this._data.length === 0; }
    size()      { return this._data.length; }
    toString()  { return `Queue[${this._data.join(' → ')}]`; }
}

class BSTNode {
    constructor(val) { this.val = val; this.left = null; this.right = null; }
}

class BST {
    constructor() { this.root = null; }

    insert(val) {
        const node = new BSTNode(val);
        if (!this.root) { this.root = node; return; }
        let cur = this.root;
        while (true) {
            if (val < cur.val) {
                if (!cur.left) { cur.left = node; return; }
                cur = cur.left;
            } else {
                if (!cur.right) { cur.right = node; return; }
                cur = cur.right;
            }
        }
    }

    contains(val) {
        let cur = this.root;
        while (cur) {
            if (val === cur.val) return true;
            cur = val < cur.val ? cur.left : cur.right;
        }
        return false;
    }

    inorder(node = this.root, result = []) {
        if (!node) return result;
        this.inorder(node.left, result);
        result.push(node.val);
        this.inorder(node.right, result);
        return result;
    }

    height(node = this.root) {
        if (!node) return 0;
        return 1 + Math.max(this.height(node.left), this.height(node.right));
    }
}

// Adjacency list graph with BFS/DFS
class Graph {
    constructor() { this.adj = new Map(); }

    addEdge(u, v) {
        if (!this.adj.has(u)) this.adj.set(u, []);
        if (!this.adj.has(v)) this.adj.set(v, []);
        this.adj.get(u).push(v);
        this.adj.get(v).push(u);
    }

    bfs(start) {
        const visited = new Set([start]);
        const queue   = new Queue();
        const order   = [];
        queue.enqueue(start);
        while (!queue.isEmpty()) {
            const node = queue.dequeue();
            order.push(node);
            for (const neighbour of (this.adj.get(node) || [])) {
                if (!visited.has(neighbour)) {
                    visited.add(neighbour);
                    queue.enqueue(neighbour);
                }
            }
        }
        return order;
    }

    dfs(start, visited = new Set(), order = []) {
        visited.add(start);
        order.push(start);
        for (const neighbour of (this.adj.get(start) || [])) {
            if (!visited.has(neighbour)) this.dfs(neighbour, visited, order);
        }
        return order;
    }

    shortestPath(src, dst) {
        const dist = new Map([[src, 0]]);
        const prev = new Map();
        const queue = new Queue();
        queue.enqueue(src);
        while (!queue.isEmpty()) {
            const u = queue.dequeue();
            if (u === dst) break;
            for (const v of (this.adj.get(u) || [])) {
                if (!dist.has(v)) {
                    dist.set(v, dist.get(u) + 1);
                    prev.set(v, u);
                    queue.enqueue(v);
                }
            }
        }
        if (!dist.has(dst)) return null;
        const path = [];
        let cur = dst;
        while (cur !== undefined) { path.unshift(cur); cur = prev.get(cur); }
        return { path, length: dist.get(dst) };
    }
}

// ===== SORTING ALGORITHMS =====

function quickSort(arr) {
    if (arr.length <= 1) return arr;
    const pivot = arr[Math.floor(arr.length / 2)];
    const left  = arr.filter(x => x < pivot);
    const mid   = arr.filter(x => x === pivot);
    const right = arr.filter(x => x > pivot);
    return [...quickSort(left), ...mid, ...quickSort(right)];
}

function mergeSort(arr) {
    if (arr.length <= 1) return arr;
    const mid = Math.floor(arr.length / 2);
    const left  = mergeSort(arr.slice(0, mid));
    const right = mergeSort(arr.slice(mid));
    const result = [];
    let i = 0, j = 0;
    while (i < left.length && j < right.length)
        result.push(left[i] <= right[j] ? left[i++] : right[j++]);
    return [...result, ...left.slice(i), ...right.slice(j)];
}

function heapSort(arr) {
    const a = [...arr];
    const n = a.length;
    const heapify = (i, size) => {
        let largest = i, l = 2*i+1, r = 2*i+2;
        if (l < size && a[l] > a[largest]) largest = l;
        if (r < size && a[r] > a[largest]) largest = r;
        if (largest !== i) {
            [a[i], a[largest]] = [a[largest], a[i]];
            heapify(largest, size);
        }
    };
    for (let i = Math.floor(n/2)-1; i >= 0; i--) heapify(i, n);
    for (let i = n-1; i > 0; i--) { [a[0], a[i]] = [a[i], a[0]]; heapify(0, i); }
    return a;
}

// ===== FUNCTIONAL PROGRAMMING =====

// Compose functions right-to-left
const compose = (...fns) => x => fns.reduceRight((v, f) => f(v), x);

// Curry any function
const curry = fn => {
    const arity = fn.length;
    return function curried(...args) {
        if (args.length >= arity) return fn(...args);
        return (...more) => curried(...args, ...more);
    };
};

// Memoize (cache results)
const memoize = fn => {
    const cache = new Map();
    return (...args) => {
        const key = JSON.stringify(args);
        if (cache.has(key)) return cache.get(key);
        const result = fn(...args);
        cache.set(key, result);
        return result;
    };
};

// Lazy infinite sequences via generators
function* naturals(start = 1) {
    while (true) yield start++;
}

function* take(n, gen) {
    for (const val of gen) {
        if (n-- <= 0) return;
        yield val;
    }
}

function* filter(pred, gen) {
    for (const val of gen) if (pred(val)) yield val;
}

function* map(fn, gen) {
    for (const val of gen) yield fn(val);
}

// Infinite primes via sieve
function* sieve(nums) {
    const p = nums.next().value;
    yield p;
    yield* sieve(filter(n => n % p !== 0, nums));
}

// ===== NUMBER THEORY =====

const gcd = memoize((a, b) => b === 0 ? a : gcd(b, a % b));
const lcm = (a, b) => a / gcd(a, b) * b;

function totient(n) {
    let result = n;
    for (let p = 2; p * p <= n; p++) {
        if (n % p === 0) {
            while (n % p === 0) n = Math.floor(n / p);
            result -= Math.floor(result / p);
        }
    }
    if (n > 1) result -= Math.floor(result / n);
    return result;
}

function isPrime(n) {
    if (n < 2) return false;
    if (n === 2) return true;
    if (n % 2 === 0) return false;
    for (let i = 3; i * i <= n; i += 2) if (n % i === 0) return false;
    return true;
}

// ===== STATISTICS =====

function statistics(data) {
    const n = data.length;
    const sorted = [...data].sort((a, b) => a - b);
    const mean = data.reduce((s, x) => s + x, 0) / n;
    const variance = data.reduce((s, x) => s + (x - mean) ** 2, 0) / n;
    const stddev = Math.sqrt(variance);
    const median = n % 2 === 0
        ? (sorted[n/2 - 1] + sorted[n/2]) / 2
        : sorted[Math.floor(n/2)];
    const mode = (() => {
        const freq = new Map();
        data.forEach(x => freq.set(x, (freq.get(x) || 0) + 1));
        return [...freq.entries()].sort((a, b) => b[1] - a[1])[0][0];
    })();
    return { n, mean, median, mode, stddev, min: sorted[0], max: sorted[n-1],
             range: sorted[n-1] - sorted[0] };
}

// ===== MAIN PROGRAM =====

console.log("=".repeat(60));
console.log("  ALGORITHM MASTERCLASS — JavaScript");
console.log("  Data Structures | Sorting | Functional | Number Theory");
console.log("=".repeat(60));

// ---- Section 1: Data Structures ----
console.log("\n[ 1 ] DATA STRUCTURES");

const stack = new Stack();
[10, 20, 30, 40, 50].forEach(x => stack.push(x));
console.log("  Stack after pushing 10-50:", stack.toString());
console.log("  Pop:", stack.pop(), "→", stack.toString());

const queue = new Queue();
[1, 2, 3, 4, 5].forEach(x => queue.enqueue(x));
console.log("  Queue:", queue.toString());
console.log("  Dequeue:", queue.dequeue(), "→", queue.toString());

const bst = new BST();
[50, 30, 70, 20, 40, 60, 80, 10, 25].forEach(x => bst.insert(x));
console.log("  BST in-order:", bst.inorder().join(', '));
console.log("  BST height:", bst.height());
console.log("  Contains 40:", bst.contains(40), " Contains 45:", bst.contains(45));

// ---- Section 2: Graph traversal ----
console.log("\n[ 2 ] GRAPH TRAVERSAL (BFS & DFS)");
const g = new Graph();
const edges = [['A','B'],['A','C'],['B','D'],['B','E'],['C','F'],['D','G'],['E','G'],['F','H']];
edges.forEach(([u,v]) => g.addEdge(u, v));
console.log("  BFS from A:", g.bfs('A').join(' → '));
console.log("  DFS from A:", g.dfs('A').join(' → '));
const path = g.shortestPath('A', 'H');
console.log(`  Shortest path A→H: ${path.path.join(' → ')} (length ${path.length})`);

// ---- Section 3: Sorting ----
console.log("\n[ 3 ] SORTING ALGORITHMS");
const data = [64, 25, 12, 22, 11, 90, 55, 44, 77, 33, 88, 66, 99, 15, 42];
console.log("  Input:   ", data.join(', '));
console.log("  QuickSort:", quickSort(data).join(', '));
console.log("  MergeSort:", mergeSort(data).join(', '));
console.log("  HeapSort: ", heapSort(data).join(', '));

// ---- Section 4: Functional Programming ----
console.log("\n[ 4 ] FUNCTIONAL PROGRAMMING");

const double   = x => x * 2;
const addTen   = x => x + 10;
const square   = x => x * x;
const pipeline = compose(square, addTen, double);
console.log("  compose(square, addTen, double)(5) =", pipeline(5));

const add = curry((a, b, c) => a + b + c);
const add5 = add(5);
const add5and3 = add5(3);
console.log("  curry: add(5)(3)(2) =", add(5)(3)(2));
console.log("  partial: add5(3)(7) =", add5and3(7));

const fib = memoize(n => n <= 1 ? n : fib(n-1) + fib(n-2));
const fibs = Array.from({ length: 15 }, (_, i) => fib(i));
console.log("  Memoized Fibonacci:", fibs.join(', '));

// Infinite prime generator
const primes = [...take(15, sieve(naturals(2)))];
console.log("  First 15 primes (generator):", primes.join(', '));

// ---- Section 5: Number Theory ----
console.log("\n[ 5 ] NUMBER THEORY");
console.log("  GCD(1071, 462) =", gcd(1071, 462));
console.log("  LCM(12, 18)    =", lcm(12, 18));
console.log("  Euler's totient φ(30) =", totient(30));

const perfects = Array.from({ length: 10000 }, (_, i) => i+1)
    .filter(n => {
        let s = 1;
        for (let i = 2; i * i <= n; i++)
            if (n % i === 0) { s += i; if (i !== n/i) s += n/i; }
        return s === n && n > 1;
    });
console.log("  Perfect numbers ≤ 10000:", perfects.join(', '));

const mersennes = primes.filter(p => isPrime(2**p - 1)).map(p => `2^${p}-1`);
console.log("  Mersenne primes (small):", mersennes.slice(0,5).join(', '));

// ---- Section 6: Statistics ----
console.log("\n[ 6 ] STATISTICS ENGINE");
const dataset = [4, 8, 6, 5, 3, 2, 8, 9, 2, 5, 7, 4, 8, 3, 6, 7, 5, 8, 3, 4];
const stats = statistics(dataset);
console.log("  Dataset:", dataset.join(', '));
console.log(`  n=${stats.n}  mean=${stats.mean.toFixed(3)}  median=${stats.median}`);
console.log(`  mode=${stats.mode}  stddev=${stats.stddev.toFixed(3)}`);
console.log(`  min=${stats.min}  max=${stats.max}  range=${stats.range}`);

// Histogram
const hist = new Map();
dataset.forEach(x => hist.set(x, (hist.get(x) || 0) + 1));
console.log("  Distribution:");
[...hist.entries()].sort((a,b) => a[0]-b[0]).forEach(([v, c]) =>
    console.log(`    ${v}: ${'█'.repeat(c)} (${c})`));

// ---- Section 7: String Processing ----
console.log("\n[ 7 ] STRING ALGORITHMS");

const text = "the quick brown fox jumps over the lazy dog";
const words = text.split(' ');
const freq  = words.reduce((m, w) => { m.set(w, (m.get(w)||0)+1); return m; }, new Map());
const top5  = [...freq.entries()].sort((a,b) => b[1]-a[1]).slice(0,5);
console.log("  Text:", text);
console.log("  Word count:", words.length, " Unique:", freq.size);
console.log("  Top words:", top5.map(([w,c])=>`${w}(${c})`).join(', '));

// Anagram detection
const isAnagram = (a, b) =>
    a.split('').sort().join('') === b.split('').sort().join('');
const pairs = [['listen','silent'],['hello','world'],['astronomer','moon starer']];
pairs.forEach(([a,b]) =>
    console.log(`  "${a}" / "${b}" → anagram: ${isAnagram(a.replace(/ /g,''), b.replace(/ /g,''))}`));

console.log("\n" + "=".repeat(60));
console.log("  JavaScript Algorithm Masterclass Complete!");
console.log("  ES6+ | Classes | Generators | Closures | Functional");
console.log("=".repeat(60));
