// Event System — Publish/Subscribe Pattern in JavaScript
// Demonstrates: closures, Promises, Map, Symbol, destructuring

// ══════════════════════════════════════
//   📡 Event System (Pub/Sub)
// ══════════════════════════════════════

class EventEmitter {
    constructor() {
        this.listeners = new Map();
        this.onceMap = new Map();
    }

    on(event, callback) {
        if (!this.listeners.has(event)) {
            this.listeners.set(event, []);
        }
        this.listeners.get(event).push(callback);
        return this;  // chainable
    }

    once(event, callback) {
        this.onceMap.set(callback, true);
        return this.on(event, callback);
    }

    emit(event, ...args) {
        const handlers = this.listeners.get(event) || [];
        let results = [];
        for (let handler of handlers) {
            results.push(handler(...args));
        }
        // Remove once listeners
        let remaining = handlers.filter(h => !this.onceMap.has(h));
        for (let h of handlers) {
            if (this.onceMap.has(h)) this.onceMap.delete(h);
        }
        this.listeners.set(event, remaining);
        return results;
    }

    off(event, callback) {
        if (callback) {
            let handlers = this.listeners.get(event) || [];
            this.listeners.set(event, handlers.filter(h => h !== callback));
        } else {
            this.listeners.delete(event);
        }
    }

    listenerCount(event) {
        return (this.listeners.get(event) || []).length;
    }
}

// ── Logger middleware ──
class Logger {
    constructor(name) {
        this.name = name;
        this.entries = [];
    }

    log(level, message) {
        let entry = { time: Date.now(), level, message, source: this.name };
        this.entries.push(entry);
        console.log("[" + level.toUpperCase() + "] " + this.name + ": " + message);
    }

    getEntries() {
        return this.entries;
    }
}

// ── Application simulation ──
console.log("╔══════════════════════════════════════╗");
console.log("║   📡 Event System Demo              ║");
console.log("╚══════════════════════════════════════╝");
console.log("");

let bus = new EventEmitter();
let logger = new Logger("App");

// Register event handlers
bus.on("user:login", function(user) {
    logger.log("info", "User logged in: " + user.name);
    return { status: "ok", user: user.name };
});

bus.on("user:login", function(user) {
    logger.log("info", "Setting up session for: " + user.name);
});

bus.once("user:login", function(user) {
    logger.log("info", "Welcome message sent to: " + user.name + " (first time only)");
});

bus.on("data:update", function(data) {
    logger.log("info", "Data updated: " + data.key + " = " + data.value);
});

bus.on("error", function(err) {
    logger.log("error", "Error occurred: " + err.message);
});

// Emit events
console.log("── Simulating user login ──");
bus.emit("user:login", { name: "Alice", role: "admin" });

console.log("\n── Second login (once handler removed) ──");
bus.emit("user:login", { name: "Bob", role: "user" });

console.log("\n── Data updates ──");
bus.emit("data:update", { key: "temperature", value: "72°F" });
bus.emit("data:update", { key: "humidity", value: "45%" });

console.log("\n── Error handling ──");
bus.emit("error", { message: "Connection timeout", code: 408 });

// Listener counts
console.log("\n── Listener Statistics ──");
console.log("  user:login listeners: " + bus.listenerCount("user:login"));
console.log("  data:update listeners: " + bus.listenerCount("data:update"));
console.log("  error listeners: " + bus.listenerCount("error"));

// Log summary
console.log("\n── Log Entries ──");
let entries = logger.getEntries();
for (let i = 0; i < entries.length; i++) {
    let e = entries[i];
    console.log("  " + (i + 1) + ". [" + e.level + "] " + e.message);
}

// Symbol usage
let id = Symbol("eventId");
let meta = {};
meta[id] = "evt-001";
console.log("\n── Symbol metadata ──");
console.log("  Event ID (Symbol): " + meta[id]);

console.log("\n✅ Event system demo complete!");
