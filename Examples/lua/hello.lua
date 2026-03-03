-- =============================================
--  Lua Comprehensive Demo - Time Warp Studio
-- =============================================

-- --- Hello World ---
print("===== HELLO WORLD =====")
print("Welcome to Lua!")
print("")

-- --- Variables ---
print("===== VARIABLES =====")
local x = 10
local y = 3
local pi = 3.14159
local name = "Lua"
local flag = true
print("int: " .. x)
print("float: " .. pi)
print("string: " .. name)
print("bool: " .. tostring(flag))
print("")

-- --- Arithmetic ---
print("===== ARITHMETIC =====")
print("x + y = " .. (x + y))
print("x - y = " .. (x - y))
print("x * y = " .. (x * y))
print("x / y = " .. (x / y))
print("x % y = " .. (x % y))
print("2 ^ 10 = " .. (2 ^ 10))
print("")

-- --- Strings ---
print("===== STRINGS =====")
print("upper: " .. string.upper("hello"))
print("lower: " .. string.lower("HELLO"))
print("rep: " .. string.rep("ab", 3))
print("sub: " .. string.sub("Hello World", 1, 5))
print("len: " .. string.len("Hello"))
print("reverse: " .. string.reverse("Hello"))
print("concat: " .. "Hello" .. " " .. "World")
print("")

-- --- Tables (Arrays) ---
print("===== TABLES =====")
local fruits = {"apple", "banana", "cherry"}
print("first: " .. fruits[1])
print("second: " .. fruits[2])
print("third: " .. fruits[3])
print("count: " .. #fruits)
print("joined: " .. table.concat(fruits, ", "))
print("")

-- --- Tables (Dictionaries) ---
print("===== DICT TABLES =====")
local person = {name = "Alice", age = 30, city = "Wonderland"}
print("name: " .. person.name)
print("age: " .. person.age)
print("city: " .. person.city)
print("")

-- --- Control Flow ---
print("===== CONDITIONALS =====")
if x > 5 then
  print("x is greater than 5")
elseif x == 5 then
  print("x equals 5")
else
  print("x is less than 5")
end
print("")

-- --- For Loop (numeric) ---
print("===== FOR LOOP =====")
for i = 1, 5 do
  print(i)
end
print("")

-- --- For Loop (generic) ---
print("===== GENERIC FOR =====")
for i, v in ipairs(fruits) do
  print(i .. ": " .. v)
end
print("")

-- --- While Loop ---
print("===== WHILE LOOP =====")
local n = 1
while n <= 3 do
  print("n = " .. n)
  n = n + 1
end
print("")

-- --- Repeat-Until ---
print("===== REPEAT-UNTIL =====")
local m = 1
repeat
  print("m = " .. m)
  m = m + 1
until m > 3
print("")

-- --- Functions ---
print("===== FUNCTIONS =====")
function factorial(n)
  if n <= 1 then return 1 end
  return n * factorial(n - 1)
end
print("5! = " .. factorial(5))
print("10! = " .. factorial(10))

function greet(who)
  return "Hello " .. who
end
print(greet("World"))
print("")

-- --- Math ---
print("===== MATH =====")
print("sqrt(16) = " .. math.sqrt(16))
print("pi = " .. math.pi)
print("floor(3.7) = " .. math.floor(3.7))
print("ceil(3.2) = " .. math.ceil(3.2))
print("abs(-5) = " .. math.abs(-5))
print("max(3,7) = " .. math.max(3, 7))
print("min(3,7) = " .. math.min(3, 7))
print("")

-- --- Type Checking ---
print("===== TYPES =====")
print("type(42) = " .. type(42))
print("type('hi') = " .. type("hi"))
print("type(true) = " .. type(true))
print("type({}) = " .. type({}))
print("")

print("===== DONE =====")
