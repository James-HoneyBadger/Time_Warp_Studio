# Data processing with Ruby
# Demonstrates arrays, hashes, enumerable methods, blocks, and lambdas

students = [
  { name: "Alice",   score: 92, grade: "A" },
  { name: "Bob",     score: 78, grade: "B" },
  { name: "Charlie", score: 85, grade: "B" },
  { name: "Diana",   score: 96, grade: "A" },
  { name: "Eve",     score: 61, grade: "D" },
  { name: "Frank",   score: 74, grade: "C" },
]

puts "=== Student Report ==="

# Sort by score descending
ranked = students.sort_by { |s| -s[:score] }
ranked.each_with_index do |s, i|
  puts "#{i + 1}. #{s[:name].ljust(10)} #{s[:score]}  (#{s[:grade]})"
end

# Statistics
scores = students.map { |s| s[:score] }
avg    = scores.sum.to_f / scores.length
puts "\nAverage: #{avg.round(2)}"
puts "Highest: #{scores.max}"
puts "Lowest:  #{scores.min}"

# Group by grade
by_grade = students.group_by { |s| s[:grade] }
puts "\n=== By Grade ==="
by_grade.sort_by { |k, _| k }.each do |grade, group|
  names = group.map { |s| s[:name] }.join(", ")
  puts "#{grade}: #{names}"
end

# Passing students (score >= 70)
passing = students.select { |s| s[:score] >= 70 }
puts "\nPassing students (#{passing.length}/#{students.length}):"
passing.each { |s| puts "  #{s[:name]}" }

puts "\n=== Array Operations ==="
nums = (1..10).to_a
puts "Numbers:      #{nums.join(', ')}"
puts "Evens:        #{nums.select(&:even?).join(', ')}"
puts "Squares:      #{nums.map { |n| n ** 2 }.join(', ')}"
puts "Sum of odds:  #{nums.select(&:odd?).sum}"
puts "Running sum:  #{nums.reduce([0]) { |acc, n| acc + [acc.last + n] }[1..].join(', ')}"

# Word frequency counter
words = "the quick brown fox jumps over the lazy dog the fox".split
freq  = words.tally
puts "\n=== Word Frequency ==="
freq.sort_by { |_, c| -c }.each do |word, count|
  bar = "#" * count
  puts "#{word.ljust(8)} #{bar} (#{count})"
end
