/**
 * todo_app.js — Full-Featured Todo Application
 * Demonstrates: classes, closures, Map/Set, generators,
 * Symbol, Proxy, iterators, async/await simulation,
 * JSON serialization, and event-driven architecture.
 */

'use strict';

// ──────────────────────────────────────────────────────────────
//  CONSTANTS & ENUMS
// ──────────────────────────────────────────────────────────────

const Priority = Object.freeze({
  LOW:    'LOW',
  MEDIUM: 'MEDIUM',
  HIGH:   'HIGH',
  URGENT: 'URGENT',
});

const Status = Object.freeze({
  PENDING:     'PENDING',
  IN_PROGRESS: 'IN_PROGRESS',
  DONE:        'DONE',
  CANCELLED:   'CANCELLED',
});

const PRIORITY_ORDER = { LOW: 0, MEDIUM: 1, HIGH: 2, URGENT: 3 };

// ──────────────────────────────────────────────────────────────
//  EVENT EMITTER
// ──────────────────────────────────────────────────────────────

class EventEmitter {
  #listeners = new Map();

  on(event, fn) {
    if (!this.#listeners.has(event)) this.#listeners.set(event, []);
    this.#listeners.get(event).push(fn);
    return () => this.off(event, fn);  // returns unsubscribe function
  }

  off(event, fn) {
    const handlers = this.#listeners.get(event) ?? [];
    this.#listeners.set(event, handlers.filter(h => h !== fn));
  }

  emit(event, ...args) {
    (this.#listeners.get(event) ?? []).forEach(fn => fn(...args));
  }

  once(event, fn) {
    const wrapper = (...args) => { fn(...args); this.off(event, wrapper); };
    this.on(event, wrapper);
  }
}

// ──────────────────────────────────────────────────────────────
//  TODO ITEM
// ──────────────────────────────────────────────────────────────

let _nextId = 1;

class TodoItem {
  #id;
  #createdAt;
  #completedAt = null;
  #history = [];

  constructor({
    title,
    description = '',
    priority    = Priority.MEDIUM,
    status      = Status.PENDING,
    dueDate     = null,
    tags        = [],
    projectId   = null,
  }) {
    if (!title || !title.trim()) throw new Error('Title is required');

    this.#id       = _nextId++;
    this.#createdAt = new Date();
    this.title      = title.trim();
    this.description = description;
    this.priority   = priority;
    this.status     = status;
    this.dueDate    = dueDate ? new Date(dueDate) : null;
    this.tags       = new Set(tags);
    this.projectId  = projectId;

    this.#log('created');
  }

  get id()          { return this.#id; }
  get createdAt()   { return this.#createdAt; }
  get completedAt() { return this.#completedAt; }
  get history()     { return [...this.#history]; }

  get isOverdue() {
    return this.dueDate && this.dueDate < new Date() &&
           this.status !== Status.DONE &&
           this.status !== Status.CANCELLED;
  }

  get age() {
    const ms = Date.now() - this.#createdAt.getTime();
    return Math.floor(ms / 86_400_000);  // days
  }

  // Fluent setters that track changes
  setStatus(newStatus) {
    const old = this.status;
    this.status = newStatus;
    if (newStatus === Status.DONE)       this.#completedAt = new Date();
    if (newStatus !== Status.DONE)       this.#completedAt = null;
    this.#log('status', { from: old, to: newStatus });
    return this;
  }

  setPriority(newPriority) {
    const old = this.priority;
    this.priority = newPriority;
    this.#log('priority', { from: old, to: newPriority });
    return this;
  }

  addTag(tag)    { this.tags.add(tag); return this; }
  removeTag(tag) { this.tags.delete(tag); return this; }
  hasTag(tag)    { return this.tags.has(tag); }

  complete()  { return this.setStatus(Status.DONE); }
  start()     { return this.setStatus(Status.IN_PROGRESS); }
  cancel()    { return this.setStatus(Status.CANCELLED); }

  #log(action, meta = {}) {
    this.#history.push({ action, timestamp: new Date(), ...meta });
  }

  toJSON() {
    return {
      id:          this.#id,
      title:       this.title,
      description: this.description,
      priority:    this.priority,
      status:      this.status,
      dueDate:     this.dueDate?.toISOString() ?? null,
      tags:        [...this.tags],
      projectId:   this.projectId,
      createdAt:   this.#createdAt.toISOString(),
      completedAt: this.#completedAt?.toISOString() ?? null,
    };
  }

  toString() {
    const dueStr = this.dueDate
      ? ` [due ${this.dueDate.toLocaleDateString()}]` : '';
    const overdue = this.isOverdue ? ' ⚠OVERDUE' : '';
    const tags = this.tags.size ? ` #${[...this.tags].join(' #')}` : '';
    return `[${this.#id}] ${this.status.padEnd(11)} ${this.priority.padEnd(6)}`
         + `  ${this.title}${dueStr}${overdue}${tags}`;
  }
}

// ──────────────────────────────────────────────────────────────
//  PROJECT
// ──────────────────────────────────────────────────────────────

class Project {
  #id;
  #createdAt;

  constructor(name, color = '#888') {
    this.#id        = 'proj_' + Date.now();
    this.#createdAt = new Date();
    this.name       = name;
    this.color      = color;
  }

  get id()        { return this.#id; }
  get createdAt() { return this.#createdAt; }

  toString() { return `[${this.#id}] ${this.name}`; }
}

// ──────────────────────────────────────────────────────────────
//  TODO STORE  (main data store with query API)
// ──────────────────────────────────────────────────────────────

class TodoStore extends EventEmitter {
  #todos    = new Map();   // id → TodoItem
  #projects = new Map();   // id → Project

  // ── CRUD ────────────────────────────────────────────────────

  add(todoData) {
    const item = new TodoItem(todoData);
    this.#todos.set(item.id, item);
    this.emit('add', item);
    return item;
  }

  get(id) {
    const item = this.#todos.get(id);
    if (!item) throw new Error(`Todo #${id} not found`);
    return item;
  }

  update(id, changes) {
    const item = this.get(id);
    const allowed = ['title','description','dueDate','projectId'];
    for (const key of allowed) {
      if (key in changes) item[key] = changes[key];
    }
    if ('priority' in changes) item.setPriority(changes.priority);
    if ('status'   in changes) item.setStatus(changes.status);
    if ('tags'     in changes) {
      item.tags = new Set(changes.tags);
    }
    this.emit('update', item);
    return item;
  }

  delete(id) {
    const item = this.get(id);
    this.#todos.delete(id);
    this.emit('delete', item);
    return item;
  }

  complete(id)  { const t = this.get(id); t.complete(); this.emit('complete', t); return t; }
  start(id)     { const t = this.get(id); t.start();    this.emit('start', t);    return t; }
  cancel(id)    { const t = this.get(id); t.cancel();   this.emit('cancel', t);   return t; }

  // ── PROJECTS ─────────────────────────────────────────────────

  addProject(name, color) {
    const p = new Project(name, color);
    this.#projects.set(p.id, p);
    this.emit('project:add', p);
    return p;
  }

  getProject(id) { return this.#projects.get(id); }

  // ── QUERY API ────────────────────────────────────────────────

  all() { return [...this.#todos.values()]; }

  /**
   * query({ status, priority, tag, projectId, overdue, search })
   * Chainable filter returning an array.
   */
  query({
    status    = null,
    priority  = null,
    tag       = null,
    projectId = null,
    overdue   = null,
    search    = null,
  } = {}) {
    return this.all().filter(item => {
      if (status    && item.status   !== status)   return false;
      if (priority  && item.priority !== priority) return false;
      if (tag       && !item.hasTag(tag))          return false;
      if (projectId && item.projectId !== projectId) return false;
      if (overdue   !== null && item.isOverdue !== overdue) return false;
      if (search) {
        const q = search.toLowerCase();
        const hit = item.title.toLowerCase().includes(q)
                 || item.description.toLowerCase().includes(q)
                 || [...item.tags].some(t => t.includes(q));
        if (!hit) return false;
      }
      return true;
    });
  }

  /** Sort todos by criteria: 'priority'|'dueDate'|'created'|'status' */
  sortBy(todos, criterion) {
    return [...todos].sort((a, b) => {
      switch (criterion) {
        case 'priority': return PRIORITY_ORDER[b.priority] - PRIORITY_ORDER[a.priority];
        case 'dueDate':
          if (!a.dueDate) return 1;
          if (!b.dueDate) return -1;
          return a.dueDate - b.dueDate;
        case 'created':  return a.id - b.id;
        case 'status':   return a.status.localeCompare(b.status);
        default:         return 0;
      }
    });
  }

  // ── STATISTICS ───────────────────────────────────────────────

  stats() {
    const todos = this.all();
    const byStatus   = {};
    const byPriority = {};
    let overdue = 0, completedToday = 0;
    const today = new Date().toDateString();

    for (const t of todos) {
      byStatus[t.status]     = (byStatus[t.status]   ?? 0) + 1;
      byPriority[t.priority] = (byPriority[t.priority] ?? 0) + 1;
      if (t.isOverdue) overdue++;
      if (t.completedAt?.toDateString() === today) completedToday++;
    }

    return { total: todos.length, byStatus, byPriority, overdue, completedToday };
  }

  // ── PERSISTENCE (JSON) ───────────────────────────────────────

  toJSON() {
    return JSON.stringify({
      todos: this.all().map(t => t.toJSON()),
      projects: [...this.#projects.values()].map(p => ({
        id: p.id, name: p.name, color: p.color, createdAt: p.createdAt,
      })),
    }, null, 2);
  }

  // ── GENERATOR: iterate by priority ──────────────────────────

  * [Symbol.iterator]() {
    const sorted = this.sortBy(this.all(), 'priority');
    yield* sorted;
  }

  // ── DISPLAY ──────────────────────────────────────────────────

  print(todos = null, title = 'TODO LIST') {
    const items = todos ?? this.sortBy(this.all(), 'priority');
    console.log('\n' + '═'.repeat(70));
    console.log(`  ${title}  (${items.length} items)`);
    console.log('─'.repeat(70));
    if (items.length === 0) {
      console.log('  (empty)');
    } else {
      items.forEach(t => console.log(' ', t.toString()));
    }
    console.log('═'.repeat(70));
  }

  printStats() {
    const s = this.stats();
    console.log('\n── STATISTICS ─────────────────────────────────────────');
    console.log(`  Total todos:     ${s.total}`);
    console.log(`  Overdue:         ${s.overdue}`);
    console.log(`  Completed today: ${s.completedToday}`);
    console.log('  By status:');
    for (const [k, v] of Object.entries(s.byStatus)) {
      console.log(`    ${k.padEnd(14)} ${v}`);
    }
    console.log('  By priority:');
    for (const [k, v] of Object.entries(s.byPriority)) {
      console.log(`    ${k.padEnd(14)} ${v}`);
    }
  }
}

// ──────────────────────────────────────────────────────────────
//  LOCAL-STORAGE SIMULATION (for Node.js demo)
// ──────────────────────────────────────────────────────────────

const storage = {
  _data: {},
  getItem: function(k) { return this._data[k] ?? null; },
  setItem: function(k, v) { this._data[k] = v; },
  removeItem: function(k) { delete this._data[k]; },
};

// ──────────────────────────────────────────────────────────────
//  DEMO
// ──────────────────────────────────────────────────────────────

const store = new TodoStore();

// Subscribe to events
store.on('add',      t => console.log(`  [+] Added: "${t.title}"`));
store.on('complete', t => console.log(`  [✓] Completed: "${t.title}"`));
store.on('delete',   t => console.log(`  [✗] Deleted: "${t.title}"`));

console.log('╔══════════════════════════════════════════════════════════════╗');
console.log('║               TODO APP — JavaScript OOP Demo                 ║');
console.log('╚══════════════════════════════════════════════════════════════╝');

// Create projects
const workProj = store.addProject('Work', '#4A90D9');
const homeProj = store.addProject('Home', '#7ED321');
const learnProj = store.addProject('Learning', '#F5A623');

// Add todos
store.add({ title: 'Finish quarterly report',         priority: Priority.URGENT,  projectId: workProj.id,  tags: ['report','deadline'], dueDate: new Date(Date.now() - 86400000) }); // yesterday = overdue
store.add({ title: 'Code review for PR #42',           priority: Priority.HIGH,    projectId: workProj.id,  tags: ['code-review'] });
store.add({ title: 'Team standup meeting',             priority: Priority.HIGH,    projectId: workProj.id,  tags: ['meeting'] });
store.add({ title: 'Grocery shopping',                 priority: Priority.MEDIUM,  projectId: homeProj.id,  tags: ['errands'] });
store.add({ title: 'Fix the leaky faucet',             priority: Priority.LOW,     projectId: homeProj.id,  tags: ['home-repair'] });
store.add({ title: 'Read "SICP" chapter 3',            priority: Priority.MEDIUM,  projectId: learnProj.id, tags: ['books','scheme'] });
store.add({ title: 'Complete JavaScript exercises',    priority: Priority.HIGH,    projectId: learnProj.id, tags: ['javascript','exercises'] });
store.add({ title: 'Meditate 10 minutes daily',        priority: Priority.LOW,     tags: ['health','routine'] });
store.add({ title: 'Reply to client emails',           priority: Priority.URGENT,  projectId: workProj.id,  tags: ['email','client'] });
store.add({ title: 'Plan weekend hiking trip',         priority: Priority.LOW,     tags: ['outdoors','weekend'] });

// Manipulate some todos
store.start(2);     // start code review
store.complete(3);  // standup done
store.complete(4);  // groceries done

// Update one
store.update(5, { title: 'Fix the leaky faucet + check pipes', priority: Priority.HIGH });

// Delete one
store.delete(8);

// Print all
store.print(null, 'ALL TODOS (sorted by priority)');

// Filtered views
console.log('\n── HIGH/URGENT todos:');
const urgent = store.query({ priority: Priority.URGENT });
const high   = store.query({ priority: Priority.HIGH });
store.print([...urgent, ...high], 'URGENT + HIGH PRIORITY');

console.log('\n── OVERDUE todos:');
store.print(store.query({ overdue: true }), 'OVERDUE');

console.log('\n── Work project todos:');
store.print(store.query({ projectId: workProj.id }), 'WORK PROJECT');

console.log('\n── Search for "report":');
store.print(store.query({ search: 'report' }), 'SEARCH: report');

console.log('\n── Todos tagged #javascript:');
store.print(store.query({ tag: 'javascript' }), 'TAG: javascript');

// Stats
store.printStats();

// Iterator demo
console.log('\n── Iterator (for...of, priority order):');
for (const todo of store) {
  if (todo.status !== Status.DONE && todo.status !== Status.CANCELLED) {
    console.log(`  ${todo.priority.padEnd(6)} → ${todo.title}`);
  }
}

// JSON export
const json = store.toJSON();
storage.setItem('todos', json);
const jsonPreview = json.split('\n').slice(0, 8).join('\n');
console.log('\n── JSON Export (first 8 lines):');
console.log(jsonPreview + '\n  ...');

console.log('\nTodo app demo complete!');
