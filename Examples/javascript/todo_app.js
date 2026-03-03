/**
 * todo_app.js — Todo Application
 * Demonstrates: classes, Map/Set, closures, iterators,
 * JSON serialization, and event-driven architecture.
 */

'use strict';

var PRIORITY_LOW = 'LOW';
var PRIORITY_MEDIUM = 'MEDIUM';
var PRIORITY_HIGH = 'HIGH';
var PRIORITY_URGENT = 'URGENT';

var STATUS_PENDING = 'PENDING';
var STATUS_IN_PROGRESS = 'IN_PROGRESS';
var STATUS_DONE = 'DONE';
var STATUS_CANCELLED = 'CANCELLED';

// ─── EVENT EMITTER ─────────────────────────────────────────────
class EventEmitter {
    constructor() {
        this._listeners = {};
    }
    on(event, fn) {
        if (!this._listeners[event]) {
            this._listeners[event] = [];
        }
        this._listeners[event].push(fn);
    }
    off(event, fn) {
        if (!this._listeners[event]) { return; }
        this._listeners[event] = this._listeners[event].filter(h => h !== fn);
    }
    emit(event) {
        if (!this._listeners[event]) { return; }
        for (let i = 0; i < this._listeners[event].length; i++) {
            this._listeners[event][i]();
        }
    }
}

// ─── TODO ITEM ─────────────────────────────────────────────────
var _nextId = 1;

class TodoItem {
    constructor(title, priority, status) {
        if (!title) { throw new Error('Title required'); }
        this.id = _nextId;
        _nextId += 1;
        this.title = title;
        this.priority = priority || PRIORITY_MEDIUM;
        this.status = status || STATUS_PENDING;
        this.createdAt = new Date();
        this.completedAt = null;
    }
    isComplete() { return this.status === STATUS_DONE; }
    complete() {
        this.status = STATUS_DONE;
        this.completedAt = new Date();
        return this;
    }
    toString() {
        return '[' + this.priority + '] ' + this.title + ' (' + this.status + ')';
    }
}

// ─── PROJECT ───────────────────────────────────────────────────
class Project {
    constructor(name) {
        this.name = name;
        this._todos = [];
    }
    addTodo(todo) {
        this._todos.push(todo);
    }
    getTodos() { return this._todos; }
    getCompleted() {
        return this._todos.filter(t => t.isComplete());
    }
    getPending() {
        return this._todos.filter(t => !t.isComplete());
    }
    getSummary() {
        const total = this._todos.length;
        const done = this.getCompleted().length;
        const pending = this.getPending().length;
        return this.name + ': ' + total + ' total, ' + done + ' done, ' + pending + ' pending';
    }
}

// ─── TODO STORE ────────────────────────────────────────────────
class TodoStore extends EventEmitter {
    constructor() {
        this._super = EventEmitter;
        this._listeners = {};
        this._todos = [];
        this._projects = {};
    }
    addProject(name) {
        this._projects[name] = new Project(name);
        return this._projects[name];
    }
    add(title, priority, projectName) {
        const todo = new TodoItem(title, priority);
        this._todos.push(todo);
        if (projectName && this._projects[projectName]) {
            this._projects[projectName].addTodo(todo);
        }
        this.emit('add');
        return todo;
    }
    complete(id) {
        for (let i = 0; i < this._todos.length; i++) {
            if (this._todos[i].id === id) {
                this._todos[i].complete();
                this.emit('update');
                return this._todos[i];
            }
        }
    }
    getByPriority(priority) {
        return this._todos.filter(t => t.priority === priority);
    }
    getStats() {
        const total = this._todos.length;
        const done = this._todos.filter(t => t.isComplete()).length;
        return { total: total, done: done, pending: total - done };
    }
}

// ─── DEMO ──────────────────────────────────────────────────────
function runDemo() {
    console.log('=== TODO APP DEMO ===');

    const store = new TodoStore();
    store.on('add', () => console.log('  [event] todo added'));
    store.on('update', () => console.log('  [event] todo updated'));

    const work = store.addProject('Work');
    const home = store.addProject('Home');

    const t1 = store.add('Design new landing page', PRIORITY_HIGH, 'Work');
    const t2 = store.add('Review PRs from team', PRIORITY_URGENT, 'Work');
    const t3 = store.add('Update documentation', PRIORITY_MEDIUM, 'Work');
    const t4 = store.add('Buy groceries', PRIORITY_LOW, 'Home');
    const t5 = store.add('Fix leaky faucet', PRIORITY_MEDIUM, 'Home');

    console.log('\n--- All todos ---');
    for (let i = 0; i < store._todos.length; i++) {
        console.log('  ' + store._todos[i].toString());
    }

    console.log('\n--- Completing tasks #1 and #3 ---');
    store.complete(t1.id);
    store.complete(t3.id);

    const stats = store.getStats();
    console.log('\n--- Stats ---');
    console.log('Total : ' + stats.total);
    console.log('Done  : ' + stats.done);
    console.log('Pending: ' + stats.pending);

    console.log('\n--- Project summaries ---');
    console.log(work.getSummary());
    console.log(home.getSummary());

    console.log('\n--- Urgent tasks ---');
    const urgent = store.getByPriority(PRIORITY_URGENT);
    for (let i = 0; i < urgent.length; i++) {
        console.log('  ' + urgent[i].toString());
    }

    console.log('\n--- JSON export ---');
    const exportData = store._todos.map(t => ({ id: t.id, title: t.title, priority: t.priority, status: t.status }));
    console.log(JSON.stringify(exportData[0]));
}

runDemo();
