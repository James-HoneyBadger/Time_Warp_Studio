-- Lua OOP with Metatables
-- Classes, inheritance, mixins, and operator overloading

-- ── Base class: Animal ────────────────────────────────────────────────
print("=== Class-based OOP with Metatables ===\n")

local Animal = {}
Animal.__index = Animal

function Animal.new(name, sound, speed)
    return setmetatable({
        name  = name,
        sound = sound,
        speed = speed,
    }, Animal)
end

function Animal:speak()
    print(self.name .. " says: " .. self.sound)
end

function Animal:describe()
    print(string.format("Animal: %-10s | speed: %d km/h", self.name, self.speed))
end

function Animal:__tostring()
    return "Animal(" .. self.name .. ")"
end

-- ── Subclass: Dog ─────────────────────────────────────────────────────
local Dog = setmetatable({}, {__index = Animal})
Dog.__index = Dog

function Dog.new(name, breed, speed)
    local self = Animal.new(name, "Woof!", speed)
    self.breed = breed
    return setmetatable(self, Dog)
end

function Dog:fetch(item)
    print(self.name .. " fetches the " .. item .. "!")
end

function Dog:describe()
    Animal.describe(self)
    print("  Breed: " .. self.breed)
end

-- ── Subclass: Bird ────────────────────────────────────────────────────
local Bird = setmetatable({}, {__index = Animal})
Bird.__index = Bird

function Bird.new(name, canFly, speed)
    local self = Animal.new(name, "Tweet!", speed)
    self.canFly = canFly
    return setmetatable(self, Bird)
end

function Bird:fly()
    if self.canFly then
        print(self.name .. " soars through the sky!")
    else
        print(self.name .. " flaps wings but stays grounded.")
    end
end

-- ── Mixin: Trainable ─────────────────────────────────────────────────
local Trainable = {}

function Trainable:train(trick)
    self.tricks = self.tricks or {}
    self.tricks[#self.tricks+1] = trick
    print(self.name .. " learned: " .. trick)
end

function Trainable:showTricks()
    if not self.tricks or #self.tricks == 0 then
        print(self.name .. " knows no tricks yet.")
        return
    end
    io.write(self.name .. " knows: ")
    print(table.concat(self.tricks, ", "))
end

-- Mix in methods
for k, v in pairs(Trainable) do
    Dog[k] = v
    Bird[k] = v
end

-- ── Vector2 with operator overloading ────────────────────────────────
print("=== Operator Overloading ===")

local Vec2 = {}
Vec2.__index = Vec2

function Vec2.new(x, y) return setmetatable({x=x, y=y}, Vec2) end
Vec2.__add = function(a, b) return Vec2.new(a.x+b.x, a.y+b.y) end
Vec2.__sub = function(a, b) return Vec2.new(a.x-b.x, a.y-b.y) end
Vec2.__mul = function(a, s)
    if type(a) == "number" then return Vec2.new(a*s.x, a*s.y) end
    return Vec2.new(a.x*s, a.y*s)
end
Vec2.__eq  = function(a, b) return a.x==b.x and a.y==b.y end
Vec2.__tostring = function(v)
    return string.format("(%g, %g)", v.x, v.y)
end
function Vec2:length() return math.sqrt(self.x^2 + self.y^2) end
function Vec2:dot(b) return self.x*b.x + self.y*b.y end

local v1 = Vec2.new(3, 4)
local v2 = Vec2.new(1, 2)
print("v1 = " .. tostring(v1))
print("v2 = " .. tostring(v2))
print("v1+v2 = " .. tostring(v1+v2))
print("v1-v2 = " .. tostring(v1-v2))
print("v1*3  = " .. tostring(v1*3))
print("|v1| = " .. v1:length())
print("v1·v2 = " .. v1:dot(v2))

-- ── Demo ──────────────────────────────────────────────────────────────
print("\n=== Animals Demo ===")

local animals = {
    Dog.new("Rex", "German Shepherd", 50),
    Dog.new("Buddy", "Labrador", 45),
    Bird.new("Tweety", true, 80),
    Bird.new("Penguin Pete", false, 10),
}

for _, a in ipairs(animals) do
    a:describe()
    a:speak()
end

print("\n=== Training ===")
animals[1]:train("sit")
animals[1]:train("shake")
animals[1]:showTricks()

animals[3]:train("whistle")
animals[3]:fly()
