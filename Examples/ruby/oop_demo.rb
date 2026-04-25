# Ruby OOP Demo
# Time Warp Studio - Ruby classes and objects

class Animal
  attr_accessor :name, :sound

  def initialize(name, sound)
    @name = name
    @sound = sound
  end

  def speak
    "#{@name} says #{@sound}!"
  end

  def to_s
    "Animal(#{@name})"
  end
end

class Dog < Animal
  attr_accessor :breed

  def initialize(name, breed)
    super(name, "Woof")
    @breed = breed
  end

  def fetch(item)
    "#{@name} fetches the #{item}!"
  end

  def to_s
    "Dog(#{@name}, #{@breed})"
  end
end

module Trainable
  def train(command)
    "#{@name} learned: #{command}"
  end
end

class SmartDog < Dog
  include Trainable
end

# Main program
puts "=== Ruby OOP Demo ==="

cat = Animal.new("Whiskers", "Meow")
puts cat.speak

dog = Dog.new("Rex", "Labrador")
puts dog.speak
puts dog.fetch("ball")

buddy = SmartDog.new("Buddy", "Poodle")
puts buddy.speak
puts buddy.train("sit")
puts buddy.train("stay")

# Array of animals
animals = [cat, dog, buddy]
puts "\nAll animals:"
animals.each { |a| puts "  #{a}" }

# Block with map
names = animals.map { |a| a.name }
puts "\nNames: #{names.join(", ")}"
