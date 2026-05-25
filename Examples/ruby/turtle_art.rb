# Turtle graphics in Ruby
# Draws a colorful spiral pattern

# Draw a square
def draw_square(size)
  4.times do
    forward size
    right 90
  end
end

# Draw a spiral of squares
puts "Drawing spiral..."

pendown
color 255, 100, 0

20.times do |i|
  size = 20 + i * 8
  r = (i * 12) % 255
  g = (200 - i * 8) % 255
  b = (i * 20) % 255
  color r, g, b
  draw_square(size)
  right 18
end

puts "Spiral complete!"
