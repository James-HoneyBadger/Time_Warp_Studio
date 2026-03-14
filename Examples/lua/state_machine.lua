-- State Machine — Traffic Light Controller in Lua
-- Demonstrates tables, coroutines, metatables, and OOP patterns

-- ══════════════════════════════════════════
--   🚦 Traffic Light State Machine
-- ══════════════════════════════════════════

-- State Machine class using metatables
StateMachine = {}
StateMachine.__index = StateMachine

function StateMachine.new(name)
    local self = setmetatable({}, StateMachine)
    self.name = name
    self.states = {}
    self.current = nil
    self.history = {}
    return self
end

function StateMachine:addState(name, config)
    self.states[name] = {
        name = name,
        onEnter = config.onEnter or function() end,
        onExit = config.onExit or function() end,
        transitions = config.transitions or {},
        duration = config.duration or 0
    }
end

function StateMachine:setState(name)
    if self.current then
        self.states[self.current].onExit(self)
    end
    table.insert(self.history, {from_state = self.current, to_state = name})
    self.current = name
    self.states[self.current].onEnter(self)
end

function StateMachine:trigger(event)
    local state = self.states[self.current]
    if state and state.transitions[event] then
        local next_state = state.transitions[event]
        self:setState(next_state)
        return true
    end
    return false
end

function StateMachine:printHistory()
    print("\n📋 Transition History:")
    for i, entry in ipairs(self.history) do
        local from = entry.from_state or "(start)"
        print(string.format("  %d. %s → %s", i, from, entry.to_state))
    end
end

-- ── Build the traffic light ──

print("╔══════════════════════════════════════╗")
print("║   🚦 Traffic Light Controller       ║")
print("╚══════════════════════════════════════╝\n")

local light = StateMachine.new("Traffic Light")

light:addState("RED", {
    duration = 30,
    onEnter = function(sm)
        print("🔴 RED — STOP! All vehicles must stop.")
    end,
    onExit = function(sm)
        print("   (Red phase ending...)")
    end,
    transitions = { timer = "RED_YELLOW" }
})

light:addState("RED_YELLOW", {
    duration = 5,
    onEnter = function(sm)
        print("🔴🟡 RED+YELLOW — Prepare to go!")
    end,
    transitions = { timer = "GREEN" }
})

light:addState("GREEN", {
    duration = 25,
    onEnter = function(sm)
        print("🟢 GREEN — GO! Vehicles may proceed.")
    end,
    onExit = function(sm)
        print("   (Green phase ending...)")
    end,
    transitions = {
        timer = "YELLOW",
        emergency = "RED"
    }
})

light:addState("YELLOW", {
    duration = 5,
    onEnter = function(sm)
        print("🟡 YELLOW — CAUTION! Prepare to stop.")
    end,
    transitions = { timer = "RED" }
})

-- Run the simulation
print("── Starting simulation ──\n")
light:setState("RED")

-- Normal cycle
local events = {"timer", "timer", "timer", "timer", "timer", "timer"}
for i, event in ipairs(events) do
    print(string.format("\n⏰ Event: %s (cycle %d)", event, i))
    if not light:trigger(event) then
        print("  ⚠️  No transition for event: " .. event)
    end
end

-- Emergency!
print("\n🚨 EMERGENCY VEHICLE DETECTED!")
light:trigger("emergency")

-- Resume normal
print("\n── Resuming normal cycle ──")
light:trigger("timer")
light:trigger("timer")
light:trigger("timer")
light:trigger("timer")

-- Print history
light:printHistory()

-- Summary
print("\n── Statistics ──")
local counts = {}
for _, entry in ipairs(light.history) do
    counts[entry.to_state] = (counts[entry.to_state] or 0) + 1
end
for state, count in pairs(counts) do
    print(string.format("  %s: entered %d time(s)", state, count))
end

print("\n✅ Traffic light simulation complete!")
