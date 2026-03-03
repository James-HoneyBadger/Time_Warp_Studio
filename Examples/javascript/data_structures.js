// ═══════════════════════════════════════════════════════════════
//  DATA STRUCTURES LIBRARY — JavaScript Implementation
//  A complete toolkit: Stack, Queue, LinkedList, BST, HashTable,
//  MinHeap, Graph. Each structure is a reusable, tested class.
// ═══════════════════════════════════════════════════════════════

// ─── STACK ─────────────────────────────────────────────────────
class Stack {
    #data = [];
    push(item)  { this.#data.push(item); }
    pop()       { return this.#data.pop(); }
    peek()      { return this.#data[this.#data.length - 1]; }
    isEmpty()   { return this.#data.length === 0; }
    size()      { return this.#data.length; }
    toArray()   { return [...this.#data].reverse(); }
    toString()  { return `Stack[${this.toArray().join(' → ')}]`; }
}

// ─── QUEUE ─────────────────────────────────────────────────────
class Queue {
    #head = 0;
    #data = [];
    enqueue(item) { this.#data.push(item); }
    dequeue() {
        if (this.isEmpty()) return undefined;
        const val = this.#data[this.#head++];
        if (this.#head > this.#data.length / 2) {
            this.#data = this.#data.slice(this.#head);
            this.#head = 0;
        }
        return val;
    }
    front()   { return this.#data[this.#head]; }
    isEmpty() { return this.#head >= this.#data.length; }
    size()    { return this.#data.length - this.#head; }
    toArray() { return this.#data.slice(this.#head); }
}

// ─── DOUBLY LINKED LIST ────────────────────────────────────────
class ListNode {
    constructor(val) { this.val = val; this.prev = null; this.next = null; }
}

class DoublyLinkedList {
    #head = null; #tail = null; #size = 0;

    append(val) {
        const node = new ListNode(val);
        if (!this.#tail) { this.#head = this.#tail = node; }
        else { node.prev = this.#tail; this.#tail.next = node; this.#tail = node; }
        this.#size++;
    }
    prepend(val) {
        const node = new ListNode(val);
        if (!this.#head) { this.#head = this.#tail = node; }
        else { node.next = this.#head; this.#head.prev = node; this.#head = node; }
        this.#size++;
    }
    delete(val) {
        let cur = this.#head;
        while (cur) {
            if (cur.val === val) {
                if (cur.prev) cur.prev.next = cur.next; else this.#head = cur.next;
                if (cur.next) cur.next.prev = cur.prev; else this.#tail = cur.prev;
                this.#size--;
                return true;
            }
            cur = cur.next;
        }
        return false;
    }
    reverse() {
        let cur = this.#head;
        while (cur) {
            [cur.next, cur.prev] = [cur.prev, cur.next];
            this.#tail = cur;
            cur = cur.prev;
        }
        [this.#head, this.#tail] = [this.#tail, this.#head];
    }
    toArray() {
        const arr = []; let cur = this.#head;
        while (cur) { arr.push(cur.val); cur = cur.next; }
        return arr;
    }
    size() { return this.#size; }
}

// ─── BINARY SEARCH TREE ────────────────────────────────────────
class BSTNode { constructor(val) { this.val=val; this.left=null; this.right=null; } }

class BST {
    #root = null;
    insert(val) { this.#root = this.#insert(this.#root, val); }
    #insert(node, val) {
        if (!node) return new BSTNode(val);
        if (val < node.val) node.left = this.#insert(node.left, val);
        else if (val > node.val) node.right = this.#insert(node.right, val);
        return node;
    }
    contains(val) { return this.#contains(this.#root, val); }
    #contains(node, val) {
        if (!node) return false;
        if (val === node.val) return true;
        return val < node.val ? this.#contains(node.left, val) : this.#contains(node.right, val);
    }
    inOrder()  { const r=[]; this.#inOrder(this.#root, r); return r; }
    #inOrder(node, r) { if(node){ this.#inOrder(node.left,r); r.push(node.val); this.#inOrder(node.right,r); } }
    preOrder() { const r=[]; this.#preOrder(this.#root, r); return r; }
    #preOrder(node,r){ if(node){ r.push(node.val); this.#preOrder(node.left,r); this.#preOrder(node.right,r); } }
    height() { return this.#height(this.#root); }
    #height(node) { if(!node) return 0; return 1 + Math.max(this.#height(node.left), this.#height(node.right)); }
}

// ─── HASH TABLE ────────────────────────────────────────────────
class HashTable {
    #buckets;
    #size = 0;
    constructor(capacity = 53) {
        this.#buckets = new Array(capacity).fill(null).map(() => []);
    }
    #hash(key) {
        return [...String(key)].reduce((h, c) => (h * 31 + c.charCodeAt(0)) % this.#buckets.length, 0);
    }
    set(key, val) {
        const h = this.#hash(key);
        const bucket = this.#buckets[h];
        const entry = bucket.find(e => e[0] === key);
        if (entry) { entry[1] = val; } else { bucket.push([key, val]); this.#size++; }
    }
    get(key) {
        const entry = this.#buckets[this.#hash(key)].find(e => e[0] === key);
        return entry ? entry[1] : undefined;
    }
    delete(key) {
        const h = this.#hash(key);
        const idx = this.#buckets[h].findIndex(e => e[0] === key);
        if (idx !== -1) { this.#buckets[h].splice(idx, 1); this.#size--; return true; }
        return false;
    }
    keys()   { return this.#buckets.flat().map(e => e[0]); }
    values() { return this.#buckets.flat().map(e => e[1]); }
    get size() { return this.#size; }
}

// ─── MIN-HEAP ─────────────────────────────────────────────────
class MinHeap {
    #heap = [];
    #swap(i, j) { [this.#heap[i], this.#heap[j]] = [this.#heap[j], this.#heap[i]]; }
    insert(val) {
        this.#heap.push(val);
        let i = this.#heap.length - 1;
        while (i > 0) {
            const p = Math.floor((i - 1) / 2);
            if (this.#heap[p] > this.#heap[i]) { this.#swap(p, i); i = p; } else break;
        }
    }
    extractMin() {
        if (this.#heap.length === 0) return undefined;
        const min = this.#heap[0];
        const last = this.#heap.pop();
        if (this.#heap.length > 0) { this.#heap[0] = last; this.#siftDown(0); }
        return min;
    }
    #siftDown(i) {
        const n = this.#heap.length;
        while (true) {
            let smallest = i, l = 2*i+1, r = 2*i+2;
            if (l < n && this.#heap[l] < this.#heap[smallest]) smallest = l;
            if (r < n && this.#heap[r] < this.#heap[smallest]) smallest = r;
            if (smallest !== i) { this.#swap(i, smallest); i = smallest; } else break;
        }
    }
    peek()  { return this.#heap[0]; }
    size()  { return this.#heap.length; }
    toSortedArray() {
        const clone = new MinHeap();
        this.#heap.forEach(v => clone.insert(v));
        const res = [];
        while (clone.size() > 0) res.push(clone.extractMin());
        return res;
    }
}

// ─── GRAPH (Weighted Directed) + Dijkstra ─────────────────────
class Graph {
    #adj = new Map();
    addVertex(v) { if (!this.#adj.has(v)) this.#adj.set(v, []); }
    addEdge(from, to, w=1) {
        this.addVertex(from); this.addVertex(to);
        this.#adj.get(from).push({to, w});
    }
    bfs(start) {
        const visited = new Set(), order = [], q = new Queue();
        visited.add(start); q.enqueue(start);
        while (!q.isEmpty()) {
            const v = q.dequeue(); order.push(v);
            for (const {to} of this.#adj.get(v) || []) {
                if (!visited.has(to)) { visited.add(to); q.enqueue(to); }
            }
        }
        return order;
    }
    dijkstra(start) {
        const dist = {}, prev = {}, visited = new Set();
        for (const v of this.#adj.keys()) dist[v] = Infinity;
        dist[start] = 0;
        const pq = new MinHeap(); // min by dist
        // Simple array-based for clarity
        const nodes = [...this.#adj.keys()];
        while (visited.size < nodes.length) {
            const u = nodes.filter(n => !visited.has(n)).reduce((a,b) => dist[a]<=dist[b]?a:b, nodes.find(n => !visited.has(n)));
            if (!u || dist[u] === Infinity) break;
            visited.add(u);
            for (const {to, w} of this.#adj.get(u) || []) {
                const alt = dist[u] + w;
                if (alt < dist[to]) { dist[to] = alt; prev[to] = u; }
            }
        }
        return {dist, prev};
    }
    vertices() { return [...this.#adj.keys()]; }
}

// ─── DEMO & TESTS ──────────────────────────────────────────────
function section(title) {
    console.log(`\n  ─── ${title} ───`);
}

console.log("╔═══════════════════════════════════════════╗");
console.log("║    JAVASCRIPT DATA STRUCTURES LIBRARY     ║");
console.log("╚═══════════════════════════════════════════╝");

section("STACK");
const s = new Stack();
[1,2,3,4,5].forEach(x => s.push(x));
console.log(" ", s.toString());
console.log("  peek:", s.peek(), "| pop:", s.pop(), "| size:", s.size());

section("QUEUE");
const q = new Queue();
["apple","banana","cherry","date"].forEach(x => q.enqueue(x));
console.log("  front:", q.front(), "| size:", q.size());
console.log("  dequeue:", q.dequeue(), "→ size:", q.size());
console.log("  toArray:", q.toArray());

section("DOUBLY LINKED LIST");
const dll = new DoublyLinkedList();
[10,20,30,40,50].forEach(x => dll.append(x));
dll.prepend(5);
console.log("  list:", dll.toArray());
dll.delete(30);
console.log("  after delete 30:", dll.toArray());
dll.reverse();
console.log("  reversed:", dll.toArray());

section("BINARY SEARCH TREE");
const bst = new BST();
[5,3,7,1,4,6,8,2].forEach(x => bst.insert(x));
console.log("  in-order:", bst.inOrder());
console.log("  pre-order:", bst.preOrder());
console.log("  height:", bst.height());
console.log("  contains 4:", bst.contains(4), "| 9:", bst.contains(9));

section("HASH TABLE");
const ht = new HashTable();
ht.set("name", "Alice"); ht.set("age", 30); ht.set("city", "London");
console.log("  get name:", ht.get("name"));
console.log("  get city:", ht.get("city"));
ht.set("age", 31);
console.log("  updated age:", ht.get("age"));
ht.delete("city");
console.log("  keys:", ht.keys(), "| size:", ht.size);

section("MIN-HEAP");
const heap = new MinHeap();
[9,4,7,1,8,3,5,6,2].forEach(x => heap.insert(x));
console.log("  min:", heap.peek());
console.log("  sorted:", heap.toSortedArray());

section("GRAPH + BFS + DIJKSTRA");
const g = new Graph();
g.addEdge("A","B",4); g.addEdge("A","C",2);
g.addEdge("B","D",3); g.addEdge("C","B",1);
g.addEdge("C","D",5); g.addEdge("D","E",1);
console.log("  BFS from A:", g.bfs("A"));
const {dist} = g.dijkstra("A");
console.log("  Dijkstra from A:", dist);

console.log("\n  ✓ All data structures demonstrated!");
