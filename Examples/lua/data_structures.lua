-- Lua Data Structures
-- Tables as arrays, dictionaries, sets, stacks, and queues

-- ── Arrays ──────────────────────────────────────────────────────────
print("=== Arrays ===")

local fruits = {"apple", "banana", "cherry", "date", "elderberry"}
print("Fruits: " .. table.concat(fruits, ", "))
print("Count: " .. #fruits)

-- Iterate
for i, v in ipairs(fruits) do
    io.write(i .. ":" .. v .. "  ")
end
print()

-- Slice simulation
local function slice(t, from, to)
    local s = {}
    for i = from, to do s[#s+1] = t[i] end
    return s
end
local middle = slice(fruits, 2, 4)
print("Slice [2..4]: " .. table.concat(middle, ", "))

-- ── Dictionaries (hash maps) ─────────────────────────────────────────
print("\n=== Dictionaries ===")

local person = {
    name = "Alice",
    age  = 30,
    city = "Wonderland",
}
for k, v in pairs(person) do
    print(string.format("  %s: %s", k, tostring(v)))
end

-- ── Sets ──────────────────────────────────────────────────────────────
print("\n=== Sets ===")

local function Set(list)
    local s = {}
    for _, v in ipairs(list) do s[v] = true end
    return s
end

local a = Set{1, 2, 3, 4, 5}
local b = Set{3, 4, 5, 6, 7}

-- Union
local union = {}
for k in pairs(a) do union[k] = true end
for k in pairs(b) do union[k] = true end

-- Intersection
local inter = {}
for k in pairs(a) do
    if b[k] then inter[k] = true end
end

io.write("Union: ")
for k in pairs(union) do io.write(k .. " ") end
print()

io.write("Intersection: ")
for k in pairs(inter) do io.write(k .. " ") end
print()

-- ── Stack ─────────────────────────────────────────────────────────────
print("\n=== Stack ===")

local Stack = {}
Stack.__index = Stack
function Stack.new()  return setmetatable({items={}}, Stack) end
function Stack:push(v) self.items[#self.items+1] = v end
function Stack:pop()
    if #self.items == 0 then return nil end
    local v = self.items[#self.items]
    self.items[#self.items] = nil
    return v
end
function Stack:peek() return self.items[#self.items] end
function Stack:size() return #self.items end

local st = Stack.new()
for _, v in ipairs{10, 20, 30, 40} do st:push(v) end
print("Stack size: " .. st:size())
print("Peek: " .. tostring(st:peek()))
while st:size() > 0 do io.write(st:pop() .. " ") end
print()

-- ── Queue ─────────────────────────────────────────────────────────────
print("\n=== Queue ===")

local Queue = {}
Queue.__index = Queue
function Queue.new()  return setmetatable({items={}, head=1, tail=0}, Queue) end
function Queue:enqueue(v)
    self.tail = self.tail + 1
    self.items[self.tail] = v
end
function Queue:dequeue()
    if self.head > self.tail then return nil end
    local v = self.items[self.head]
    self.items[self.head] = nil
    self.head = self.head + 1
    return v
end
function Queue:size() return self.tail - self.head + 1 end

local q = Queue.new()
for _, v in ipairs{"A", "B", "C", "D"} do q:enqueue(v) end
print("Queue size: " .. q:size())
while q:size() > 0 do io.write(q:dequeue() .. " ") end
print()
