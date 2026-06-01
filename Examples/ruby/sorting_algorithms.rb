# Sorting Algorithms in Ruby
# Implements and compares multiple sorting strategies

# ── Implementations ───────────────────────────────────────────────────
def bubble_sort(arr)
  a = arr.dup
  n = a.length
  (n - 1).times do
    (n - 1).times { |i| a[i], a[i+1] = a[i+1], a[i] if a[i] > a[i+1] }
  end
  a
end

def insertion_sort(arr)
  a = arr.dup
  (1...a.length).each do |i|
    key = a[i]
    j   = i - 1
    while j >= 0 && a[j] > key
      a[j + 1] = a[j]
      j -= 1
    end
    a[j + 1] = key
  end
  a
end

def merge_sort(arr)
  return arr if arr.length <= 1
  mid   = arr.length / 2
  left  = merge_sort(arr[0...mid])
  right = merge_sort(arr[mid..])
  merge(left, right)
end

def merge(left, right)
  result = []
  until left.empty? || right.empty?
    result << (left.first <= right.first ? left.shift : right.shift)
  end
  result + left + right
end

def quicksort(arr)
  return arr if arr.length <= 1
  pivot   = arr[arr.length / 2]
  left    = arr.select { |x| x < pivot }
  middle  = arr.select { |x| x == pivot }
  right   = arr.select { |x| x > pivot }
  quicksort(left) + middle + quicksort(right)
end

# ── Demo ──────────────────────────────────────────────────────────────
unsorted = [64, 25, 12, 22, 11, 90, 3, 77, 44, 55, 8, 37]

puts "=== Sorting Algorithms in Ruby ==="
puts "Unsorted:       #{unsorted}"
puts "Bubble sort:    #{bubble_sort(unsorted)}"
puts "Insertion sort: #{insertion_sort(unsorted)}"
puts "Merge sort:     #{merge_sort(unsorted)}"
puts "Quicksort:      #{quicksort(unsorted)}"
puts "Built-in sort:  #{unsorted.sort}"

# Sort strings
words = %w[banana apple cherry date elderberry fig grape]
puts "\n=== String Sorting ==="
puts "Original:   #{words}"
puts "Sorted:     #{words.sort}"
puts "By length:  #{words.sort_by(&:length)}"
puts "Reversed:   #{words.sort.reverse}"

# Custom sort: sort people by age then name
people = [
  { name: "Charlie", age: 30 },
  { name: "Alice",   age: 25 },
  { name: "Bob",     age: 25 },
  { name: "Diana",   age: 28 },
]
puts "\n=== Custom Sort (age, then name) ==="
sorted_people = people.sort_by { |p| [p[:age], p[:name]] }
sorted_people.each { |p| puts "  #{p[:name]} (#{p[:age]})" }
