# Ruby Hello World
# Time Warp Studio - Ruby example

puts "Hello, World!"
puts "Ruby is a dynamic, object-oriented language."

# String interpolation
name = "Time Warp"
version = 10
puts "Welcome to #{name} Studio v#{version}!"

# Simple arithmetic
result = (1..10).sum
puts "Sum of 1 to 10: #{result}"

# Array and iteration
languages = ["Ruby", "Erlang", "Rust", "Python", "Lua"]
puts "\nLanguages:"
languages.each { |lang| puts "  - #{lang}" }

# Hash (dictionary)
greetings = {
  "English" => "Hello",
  "Spanish" => "Hola",
  "French"  => "Bonjour"
}

puts "\nGreetings:"
greetings.each do |lang, word|
  puts "  #{lang}: #{word}"
end
