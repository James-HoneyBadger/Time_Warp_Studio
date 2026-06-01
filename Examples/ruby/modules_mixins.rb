# Modules and Mixins in Ruby
# Demonstrates modules as namespaces and mixins for shared behavior

# ── Module as namespace ───────────────────────────────────────────────
module MathUtils
  PI = 3.14159265358979

  def self.circle_area(r)    = PI * r * r
  def self.circle_perimeter(r) = 2 * PI * r
  def self.degrees_to_rad(d)  = d * PI / 180
  def self.factorial(n)       = n <= 1 ? 1 : n * factorial(n - 1)
  def self.fibonacci(n)
    return n if n <= 1
    a, b = 0, 1
    (n - 1).times { a, b = b, a + b }
    b
  end
end

# ── Mixins ────────────────────────────────────────────────────────────
module Printable
  def print_info
    puts "#{self.class.name}: #{to_s}"
  end
end

module Comparable2
  def between?(a, b)
    self >= a && self <= b
  end
end

module Serializable
  def to_csv
    instance_variables.map { |v| instance_variable_get(v) }.join(',')
  end

  def to_hash
    instance_variables.each_with_object({}) do |v, h|
      h[v.to_s.delete('@')] = instance_variable_get(v)
    end
  end
end

# ── Classes using mixins ──────────────────────────────────────────────
class Animal
  include Printable
  include Serializable

  attr_reader :name, :species, :age

  def initialize(name, species, age)
    @name    = name
    @species = species
    @age     = age
  end

  def to_s
    "#{@name} (#{@species}, age #{@age})"
  end
end

class Product
  include Printable
  include Serializable

  attr_reader :name, :price, :stock

  def initialize(name, price, stock)
    @name  = name
    @price = price
    @stock = stock
  end

  def to_s
    "#{@name} — $#{@price} (#{@stock} in stock)"
  end
end

# ── Demo ──────────────────────────────────────────────────────────────
puts "=== Module as Namespace ==="
puts "  Circle area (r=5):     #{MathUtils.circle_area(5).round(2)}"
puts "  Circle perimeter (r=5): #{MathUtils.circle_perimeter(5).round(2)}"
puts "  45° in radians:         #{MathUtils.degrees_to_rad(45).round(4)}"
puts "  10! = #{MathUtils.factorial(10)}"
puts "  fib(10) = #{MathUtils.fibonacci(10)}"

puts "\n=== Mixin: Printable ==="
animals = [
  Animal.new("Leo",   "Lion",  5),
  Animal.new("Nemo",  "Fish",  2),
  Animal.new("Buddy", "Dog",   3),
]
animals.each(&:print_info)

puts "\n=== Mixin: Serializable ==="
products = [
  Product.new("Widget", 9.99,  50),
  Product.new("Gadget", 24.99, 10),
]
products.each do |p|
  puts "  CSV: #{p.to_csv}"
  puts "  Hash: #{p.to_hash}"
end

puts "\n=== Module::PI constant ==="
puts "  MathUtils::PI = #{MathUtils::PI}"
