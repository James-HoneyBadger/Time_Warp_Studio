// ═══════════════════════════════════════════════════════════════
//  DATA STRUCTURES LIBRARY — JavaScript Implementation
//  Stack, Queue, LinkedList, BST, HashTable, MinHeap, Graph
//  Compatible with educational JS interpreter.
// ═══════════════════════════════════════════════════════════════

// ─── STACK ─────────────────────────────────────────────────────
class Stack {
    constructor() {
        this._data = [];
    }
    push(item) { this._data.push(item); }
    pop() { return this._data.pop(); }
    peek() { return this._data[this._data.length - 1]; }
    isEmpty() { return this._data.length === 0; }
    size() { return this._data.length; }
    toString() { return 'Stack[' + this._data.slice().reverse().join(' -> ') + ']'; }
}

// ─── QUEUE ─────────────────────────────────────────────────────
class Queue {
    constructor() {
        this._data = [];
        this._head = 0;
    }
    enqueue(item) { this._data.push(item); }
    dequeue() {
        if (this.isEmpty()) { return undefined; }
        const val = this._data[this._head];
        this._head += 1;
        return val;
    }
    front() { return this._data[this._head]; }
    isEmpty() { return this._head >= this._data.length; }
    size() { return this._data.length - this._head; }
}

// ─── DOUBLY LINKED LIST ────────────────────────────────────────
class ListNode {
    constructor(val) {
        this.val = val;
        this.prev = null;
        this.next = null;
    }
}

class DoublyLinkedList {
    constructor() {
        this._head = null;
        this._tail = null;
        this._size = 0;
    }
    append(val) {
        const node = new ListNode(val);
        if (!this._tail) {
            this._head = node;
            this._tail = node;
        } else {
            node.prev = this._tail;
            this._tail.next = node;
            this._tail = node;
        }
        this._size += 1;
    }
    toArray() {
        const arr = [];
        let cur = this._head;
        while (cur) {
            arr.push(cur.val);
            cur = cur.next;
        }
        return arr;
    }
    size() { return this._size; }
}

// ─── BINARY SEARCH TREE ────────────────────────────────────────
class BSTNode {
    constructor(val) {
        this.val = val;
        this.left = null;
        this.right = null;
    }
}

class BST {
    constructor() {
        this._root = null;
    }
    insert(val) {
        this._root = this._insertNode(this._root, val);
    }
    _insertNode(node, val) {
        if (!node) { return new BSTNode(val); }
        if (val < node.val) {
            node.left = this._insertNode(node.left, val);
        } else if (val > node.val) {
            node.right = this._insertNode(node.right, val);
        }
        return node;
    }
    contains(val) {
        return this._containsNode(this._root, val);
    }
    _containsNode(node, val) {
        if (!node) { return false; }
        if (val === node.val) { return true; }
        if (val < node.val) {
            return this._containsNode(node.left, val);
        }
        return this._containsNode(node.right, val);
    }
    inOrder() {
        const result = [];
        this._inOrderNode(this._root, result);
        return result;
    }
    _inOrderNode(node, result) {
        if (node) {
            this._inOrderNode(node.left, result);
            result.push(node.val);
            this._inOrderNode(node.right, result);
        }
    }
}

// ─── HASH TABLE ────────────────────────────────────────────────
class HashTable {
    constructor(capacity) {
        if (!capacity) { capacity = 53; }
        this._capacity = capacity;
        this._buckets = [];
        for (let i = 0; i < capacity; i++) {
            this._buckets.push([]);
        }
        this._size = 0;
    }
    _hash(key) {
        let h = 0;
        const s = String(key);
        for (let i = 0; i < s.length; i++) {
            h = (h * 31 + s.charCodeAt(i)) % this._capacity;
        }
        return h;
    }
    set(key, value) {
        const idx = this._hash(key);
        const bucket = this._buckets[idx];
        for (let i = 0; i < bucket.length; i++) {
            if (bucket[i][0] === key) {
                bucket[i][1] = value;
                return;
            }
        }
        bucket.push([key, value]);
        this._size += 1;
    }
    get(key) {
        const idx = this._hash(key);
        const bucket = this._buckets[idx];
        for (let i = 0; i < bucket.length; i++) {
            if (bucket[i][0] === key) { return bucket[i][1]; }
        }
        return undefined;
    }
    size() { return this._size; }
}

// ─── DEMO ──────────────────────────────────────────────────────
function runDemo() {
    console.log('=== STACK ===');
    const stack = new Stack();
    stack.push(10);
    stack.push(20);
    stack.push(30);
    console.log('Push 10,20,30 -> ' + stack.toString());
    console.log('Pop: ' + stack.pop());
    console.log('Peek: ' + stack.peek());

    console.log('\n=== QUEUE ===');
    const queue = new Queue();
    queue.enqueue('a');
    queue.enqueue('b');
    queue.enqueue('c');
    console.log('Dequeue: ' + queue.dequeue());
    console.log('Front: ' + queue.front());
    console.log('Size: ' + queue.size());

    console.log('\n=== LINKED LIST ===');
    const list = new DoublyLinkedList();
    list.append(1);
    list.append(2);
    list.append(3);
    console.log('List: ' + list.toArray().join(' <-> '));
    console.log('Size: ' + list.size());

    console.log('\n=== BST ===');
    const bst = new BST();
    const vals = [5, 3, 7, 1, 4, 6, 9];
    for (let i = 0; i < vals.length; i++) {
        bst.insert(vals[i]);
    }
    console.log('In-order: ' + bst.inOrder().join(', '));
    console.log('Contains 4: ' + bst.contains(4));
    console.log('Contains 8: ' + bst.contains(8));

    console.log('\n=== HASH TABLE ===');
    const ht = new HashTable();
    ht.set('name', 'Alice');
    ht.set('age', 30);
    ht.set('city', 'NYC');
    console.log('name -> ' + ht.get('name'));
    console.log('age  -> ' + ht.get('age'));
    console.log('size -> ' + ht.size());
}

runDemo();
