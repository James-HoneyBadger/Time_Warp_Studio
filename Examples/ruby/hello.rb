# Hello World in Ruby
# Demonstrates basic output, string interpolation, and variables

name = "World"
puts "Hello, #{name}!"

# Greet several people
["Alice", "Bob", "Charlie"].each do |person|
  puts "Hello, #{person}!"
end

# String methods
greeting = "hello, ruby"
puts greeting.upcase
puts greeting.capitalize
puts greeting.length

# Numbers and arithmetic
x = 42
y = 7
puts "#{x} divided by #{y} is #{x / y}"
puts "#{x} modulo #{y} is #{x % y}"
puts "#{x} to the power of 2 is #{x ** 2}"

# Ranges
(1..5).each { |i| puts i }

puts "Done!"
