# Fibonacci sequence in Ruby
# Demonstrates recursion, iteration, and method definitions

# Recursive approach
def fib_recursive(n)
  return n if n <= 1
  fib_recursive(n - 1) + fib_recursive(n - 2)
end

# Iterative approach using array
def fib_iterative(n)
  return [0] if n == 0
  seq = [0, 1]
  (2..n).each do |i|
    seq << seq[-1] + seq[-2]
  end
  seq
end

# Memoized approach using hash
def fib_memo(n, memo = {})
  return n if n <= 1
  memo[n] ||= fib_memo(n - 1, memo) + fib_memo(n - 2, memo)
end

puts "=== Fibonacci Sequence ==="

# First 10 Fibonacci numbers (recursive)
puts "Recursive (first 10):"
10.times do |i|
  print "#{fib_recursive(i)} "
end
puts ""

# First 15 Fibonacci numbers (iterative)
puts "Iterative (first 15):"
puts fib_iterative(14).join(", ")

# Larger values (memoized)
puts "Memoized fib(30) = #{fib_memo(30)}"
puts "Memoized fib(35) = #{fib_memo(35)}"

# Fibonacci with enumeration
puts "With index:"
fib_iterative(9).each_with_index do |val, idx|
  puts "fib(#{idx}) = #{val}"
end
