# Blocks, Procs, and Lambdas in Ruby
# Demonstrates Ruby's callable objects

# ── Blocks ────────────────────────────────────────────────────────────
puts "=== Blocks ==="

# yield to a block
def repeat(n)
  n.times { |i| yield i }
end

repeat(4) { |i| puts "  Iteration #{i}" }

# Block with explicit &block parameter
def capture(&block)
  puts "  Block given: #{block_given?}"
  block.call(42)
end

capture { |n| puts "  Got: #{n}" }

# ── Procs ─────────────────────────────────────────────────────────────
puts "\n=== Procs ==="

square  = Proc.new { |x| x * x }
double  = proc { |x| x * 2 }
greet   = proc { |name| "Hello, #{name}!" }

[1, 2, 3, 4, 5].each { |n| puts "  square(#{n}) = #{square.call(n)}" }
puts "  double(7) = #{double.call(7)}"
puts "  #{greet.call('World')}"

# Proc in array
transforms = [
  proc { |x| x + 1 },
  proc { |x| x * 2 },
  proc { |x| x ** 2 }
]
puts "\n  Pipeline on 3:"
result = transforms.reduce(3) { |val, fn| fn.call(val) }
puts "  Result: #{result}"

# ── Lambdas ───────────────────────────────────────────────────────────
puts "\n=== Lambdas ==="

add     = lambda { |a, b| a + b }
multiply = ->(a, b) { a * b }
safe_div = ->(a, b) { b == 0 ? "Error: div/0" : a.to_f / b }

puts "  add(3, 5) = #{add.call(3, 5)}"
puts "  multiply(4, 6) = #{multiply.(4, 6)}"
puts "  safe_div(10, 3) = #{safe_div.(10, 3).round(3)}"
puts "  safe_div(10, 0) = #{safe_div.(10, 0)}"

# ── Higher-order functions ────────────────────────────────────────────
puts "\n=== Higher-Order Functions ==="

numbers = (1..10).to_a
puts "  Numbers:  #{numbers}"
puts "  Evens:    #{numbers.select(&:even?)}"
puts "  Odds:     #{numbers.reject(&:even?)}"
puts "  Squares:  #{numbers.map { |n| n**2 }}"
puts "  Sum:      #{numbers.reduce(:+)}"
puts "  Product:  #{numbers.reduce(:*)}"
puts "  Max:      #{numbers.max}"
puts "  Sort rev: #{numbers.sort_by { |n| -n }.first(5)}"

# Compose two lambdas
def compose(f, g)
  ->(x) { f.call(g.call(x)) }
end

inc     = ->(x) { x + 1 }
dbl     = ->(x) { x * 2 }
inc_dbl = compose(inc, dbl)
puts "\n  compose(inc, double)(5) = #{inc_dbl.(5)}"
