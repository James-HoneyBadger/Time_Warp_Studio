# Ruby Tutorial

## Introduction

Ruby is a dynamic, object-oriented, general-purpose programming language designed for programmer happiness. Created by Yukihiro Matsumoto in 1995, Ruby emphasizes simplicity and productivity.

**File extension:** `.rb`

---

## Hello World

```ruby
puts "Hello, World!"
```

---

## Variables and Types

Ruby is dynamically typed — variables take the type of whatever they hold.

```ruby
name = "Alice"        # String
age = 30              # Integer
pi = 3.14159          # Float
active = true         # Boolean
empty = nil           # Nil (null)

puts "#{name} is #{age} years old"
```

### String Interpolation

Use `#{}` inside double-quoted strings:

```ruby
city = "London"
puts "Hello from #{city.upcase}!"
```

---

## Control Flow

```ruby
x = 15

if x > 10
  puts "big"
elsif x > 5
  puts "medium"
else
  puts "small"
end

# unless = if not
unless x.zero?
  puts "x is not zero"
end

# one-line form
puts "positive" if x > 0
```

### Case/When

```ruby
day = "Monday"
case day
when "Saturday", "Sunday"
  puts "Weekend!"
when "Monday".."Friday"
  puts "Weekday"
else
  puts "Unknown day"
end
```

---

## Loops

```ruby
# while
i = 0
while i < 5
  puts i
  i += 1
end

# until (opposite of while)
n = 10
until n <= 0
  print "#{n} "
  n -= 1
end
puts

# times
5.times { |i| puts "Iteration #{i}" }

# upto / downto
1.upto(5) { |i| print "#{i} " }

# each
[1, 2, 3].each do |x|
  puts x * 2
end

# for..in (less common in Ruby)
for x in 1..5
  puts x
end
```

---

## Arrays

```ruby
fruits = ["apple", "banana", "cherry"]

puts fruits[0]           # apple
puts fruits.length       # 3
puts fruits.first        # apple
puts fruits.last         # cherry

fruits.push("date")
fruits << "elderberry"

puts fruits.include?("banana")  # true
puts fruits.join(", ")

# map / select / reject
squares = (1..5).map { |x| x ** 2 }
evens   = (1..10).select { |x| x.even? }
odds    = (1..10).reject { |x| x.even? }

puts squares.inspect   # [1, 4, 9, 16, 25]
puts evens.inspect     # [2, 4, 6, 8, 10]
```

---

## Hashes

```ruby
person = {
  name: "Alice",
  age: 30,
  city: "London"
}

puts person[:name]    # Alice
person[:job] = "Engineer"

person.each do |key, value|
  puts "#{key}: #{value}"
end
```

---

## Methods

```ruby
def greet(name, greeting = "Hello")
  "#{greeting}, #{name}!"
end

puts greet("Alice")           # Hello, Alice!
puts greet("Bob", "Hi")       # Hi, Bob!

# Return value is the last expression
def square(n)
  n * n
end

# Multiple return values (as array)
def min_max(arr)
  [arr.min, arr.max]
end

lo, hi = min_max([3, 1, 4, 1, 5, 9, 2])
puts "min=#{lo}, max=#{hi}"
```

---

## Blocks and Iterators

Blocks are anonymous code chunks passed to methods:

```ruby
# Block with do..end
[1, 2, 3].each do |x|
  puts x * 10
end

# Block with braces
[1, 2, 3].map { |x| x ** 2 }

# yield: calling the block inside a method
def repeat(n)
  n.times { yield }
end

repeat(3) { puts "Hello!" }

# block_given? — check if a block was passed
def optional_block
  if block_given?
    yield
  else
    puts "No block given"
  end
end
```

---

## Classes and Objects

```ruby
class Animal
  attr_accessor :name, :age

  def initialize(name, age)
    @name = name   # instance variable
    @age = age
  end

  def speak
    "..."
  end

  def to_s
    "#{self.class.name}(#{@name})"
  end
end

class Dog < Animal
  def speak
    "Woof! My name is #{@name}"
  end

  def fetch(item)
    "#{@name} fetches the #{item}"
  end
end

dog = Dog.new("Rex", 3)
puts dog.speak
puts dog.fetch("ball")
puts dog          # Dog(Rex)
puts dog.is_a?(Animal)   # true
```

### Modules (Mixins)

```ruby
module Swimmable
  def swim
    "#{@name} is swimming!"
  end
end

class Duck < Animal
  include Swimmable

  def speak
    "Quack!"
  end
end

duck = Duck.new("Donald", 2)
puts duck.speak
puts duck.swim
```

---

## Ranges

```ruby
r = (1..10)         # inclusive
r2 = (1...10)       # exclusive (1 to 9)

puts r.include?(5)  # true
puts r.sum          # 55
puts r.to_a.inspect # [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

("a".."e").each { |c| print "#{c} " }
```

---

## Exception Handling

```ruby
begin
  x = 10 / 0
rescue ZeroDivisionError => e
  puts "Error: #{e.message}"
ensure
  puts "This always runs"
end
```

---

## Turtle Graphics

```ruby
# Move the turtle using turtle:: commands
forward(100)
right(90)
forward(100)
right(90)
forward(100)
right(90)
forward(100)

# Draw a star
5.times do
  forward(150)
  right(144)
end
```

---

## Tips

- Everything in Ruby is an object (even integers)
- Methods ending in `?` return a boolean: `empty?`, `nil?`, `include?`
- Methods ending in `!` modify the object in-place: `sort!`, `upcase!`
- `puts` adds a newline; `print` does not; `p` outputs the debug form
- Use `irb` (interactive Ruby) to experiment
