# Classes and objects in Ruby
# Demonstrates OOP: classes, inheritance, modules, attr_accessor

module Describable
  def describe
    "I am a #{self.class.name}: #{to_s}"
  end
end

class Animal
  include Describable

  attr_accessor :name, :age

  def initialize(name, age)
    @name = name
    @age  = age
  end

  def speak
    "..."
  end

  def to_s
    "#{@name} (age #{@age})"
  end
end

class Dog < Animal
  def initialize(name, age, breed)
    super(name, age)
    @breed = breed
  end

  def speak
    "Woof!"
  end

  def to_s
    "#{@name} the #{@breed} (age #{@age})"
  end
end

class Cat < Animal
  def speak
    "Meow!"
  end
end

# Create instances
dog = Dog.new("Rex", 3, "Labrador")
cat = Cat.new("Whiskers", 5)

puts "=== Animal Showcase ==="
[dog, cat].each do |animal|
  puts animal.describe
  puts "  #{animal.name} says: #{animal.speak}"
end

# Class with class methods and constants
class Circle
  PI = 3.14159265358979

  attr_reader :radius

  def initialize(radius)
    @radius = radius
  end

  def area
    PI * @radius ** 2
  end

  def circumference
    2 * PI * @radius
  end

  def self.unit
    new(1)
  end

  def to_s
    "Circle(r=#{@radius})"
  end
end

puts "\n=== Geometry ==="
[1, 2, 3, 5].each do |r|
  c = Circle.new(r)
  puts "#{c}: area=#{c.area.round(2)}, circumference=#{c.circumference.round(2)}"
end

# Counter class
class Counter
  @@total = 0

  def initialize(start = 0)
    @count = start
    @@total += 1
  end

  def increment(n = 1)
    @count += n
    self
  end

  def decrement(n = 1)
    @count -= n
    self
  end

  def value
    @count
  end

  def self.total_created
    @@total
  end
end

puts "\n=== Counter ==="
c1 = Counter.new
c2 = Counter.new(10)
c1.increment.increment.increment(5)
c2.decrement(3)
puts "c1 = #{c1.value}"
puts "c2 = #{c2.value}"
puts "Total counters created: #{Counter.total_created}"
