# Ruby Tutorial

## Introduction

Ruby (1995) is a dynamic, expressive object-oriented scripting language created by Yukihiro Matsumoto ("Matz"). It is designed for programmer happiness and productivity, and inspired Python, Kotlin, and many other modern languages.

**Key characteristics:**
- Everything is an object (even `nil`, `true`, `false`, integers)
- Blocks and iterators make collection code elegant
- Mixins via `include` (not multiple inheritance)
- Open classes — you can add methods to any class, even built-ins

## Hello World

```ruby
puts "Hello, World!"
```

## Variables

```ruby
# Local variables (lowercase)
x = 42
name = "Alice"
pi = 3.14159

# Constants (uppercase)
MAX_SIZE = 100

# String interpolation
puts "Hello, #{name}! x = #{x}"
puts "Pi is approximately #{pi.round(2)}"
```

## Numbers and Arithmetic

```ruby
puts 3 + 4        # 7
puts 10 - 3       # 7
puts 6 * 7        # 42
puts 10.0 / 3     # 3.3333...
puts 2 ** 10      # 1024
puts 17 % 5       # 2
puts -7.abs       # 7
puts 16.sqrt      # error — use Math::sqrt
puts Math.sqrt(16) # 4.0
```

## Strings

```ruby
s = "Hello, World!"
puts s.length           # 13
puts s.upcase           # HELLO, WORLD!
puts s.downcase         # hello, world!
puts s.reverse          # !dlroW ,olleH
puts s.include?("World") # true
puts s[7, 5]            # World  (start, length)
puts s.gsub("World", "Ruby")  # Hello, Ruby!
puts s.split(", ")      # ["Hello", "World!"]
puts "  hi  ".strip     # "hi"
puts "ha" * 3           # hahaha
```

## Arrays

```ruby
arr = [1, 2, 3, 4, 5]

puts arr.length         # 5
puts arr.first          # 1
puts arr.last           # 5
puts arr[2]             # 3
puts arr[-1]            # 5 (from end)

# Common methods
puts arr.sum            # 15
puts arr.min            # 1
puts arr.max            # 5
puts arr.sort.inspect   # [1, 2, 3, 4, 5]
puts arr.reverse.inspect # [5, 4, 3, 2, 1]

# Transform
squares = arr.map { |n| n * n }
puts squares.inspect    # [1, 4, 9, 16, 25]

evens = arr.select { |n| n.even? }
puts evens.inspect      # [2, 4]

total = arr.reduce(0) { |sum, n| sum + n }
puts total              # 15

# Modify
arr.push(6)
arr << 7
arr.pop                 # removes 7
puts arr.inspect        # [1, 2, 3, 4, 5, 6]
```

## Hashes

```ruby
person = { name: "Alice", age: 30, city: "London" }

puts person[:name]       # Alice
puts person[:age]        # 30

person[:email] = "alice@example.com"
person.delete(:city)

person.each do |key, value|
    puts "#{key}: #{value}"
end

puts person.keys.inspect   # [:name, :age, :email]
puts person.has_key?(:age) # true
```

## Control Flow

```ruby
x = 42

# if / elsif / else
if x > 100
    puts "Very large"
elsif x > 10
    puts "Large"
else
    puts "Small"
end

# unless (inverse if)
unless x.zero?
    puts "Non-zero"
end

# Inline modifiers
puts "Positive" if x > 0
puts "Non-zero" unless x == 0

# case / when
case x
when 0
    puts "zero"
when 1..10
    puts "small"
when 42
    puts "the answer"
else
    puts "other"
end
```

## Loops and Iterators

```ruby
# times
5.times { |i| puts i }

# upto / downto
1.upto(5) { |i| print "#{i} " }
5.downto(1) { |i| print "#{i} " }

# each
[10, 20, 30].each { |x| puts x }

# each_with_index
["a", "b", "c"].each_with_index do |val, idx|
    puts "#{idx}: #{val}"
end

# while
n = 1
while n <= 5
    puts n
    n += 1
end

# loop with break
loop do
    x -= 1
    break if x <= 0
end
```

## Methods

```ruby
def greet(name = "World")
    "Hello, #{name}!"
end

puts greet           # Hello, World!
puts greet("Alice")  # Hello, Alice!

def factorial(n)
    n <= 1 ? 1 : n * factorial(n - 1)
end

puts factorial(6)    # 720

# Multiple return values (via array)
def min_max(arr)
    [arr.min, arr.max]
end

min, max = min_max([3, 1, 4, 1, 5, 9])
puts "#{min} .. #{max}"   # 1 .. 9
```

## Classes

```ruby
class Animal
    attr_reader :name
    attr_accessor :sound

    def initialize(name, sound = "...")
        @name = name
        @sound = sound
    end

    def speak
        "#{@name} says #{@sound}"
    end

    def to_s
        "Animal(#{@name})"
    end
end

class Dog < Animal
    def initialize(name)
        super(name, "Woof!")
    end

    def fetch(item)
        "#{@name} fetches the #{item}!"
    end
end

dog = Dog.new("Rex")
puts dog.speak        # Rex says Woof!
puts dog.fetch("ball") # Rex fetches the ball!
puts dog              # Animal(Rex)
```

## Blocks and Procs

```ruby
# Block passed to a method
def repeat(n, &block)
    n.times { block.call }
end

repeat(3) { puts "Hello!" }

# Proc (reusable block)
double = Proc.new { |x| x * 2 }
puts double.call(5)   # 10

# Lambda (stricter than Proc)
square = lambda { |x| x * x }
puts square.call(4)   # 16
puts square.(7)        # 49  (shorthand)
```

## Quick Reference

| Feature | Syntax |
|---------|--------|
| Variable | `x = value` |
| String interpolation | `"Hello, #{name}"` |
| Print | `puts`, `print`, `p` |
| Array literal | `[1, 2, 3]` |
| Hash literal | `{key: val}` |
| Block | `{ \|arg\| ... }` or `do \|arg\| ... end` |
| Method | `def name(args) ... end` |
| Class | `class Name < Parent ... end` |
| Condition | `if / elsif / else / end` |
| Loop | `n.times`, `.each`, `while`, `loop` |
| Symbol | `:my_symbol` |
| Range | `1..5` (inclusive), `1...5` (exclusive) |
