# ============================================================
# WORD GAMES — Ruby Language Showcase
# Anagrams, Scrabble scorer, palindromes, word frequency
# Time Warp Studio — Ruby Language Demo
# ============================================================

# ============================================================
# WORD LIST (built-in, no file I/O needed)
# ============================================================

WORD_LIST = %w[
  listen silent enlist inlets tinsel
  race care acre arce
  heart earth hater rathe
  notes stone tones senot
  angered enraged
  garden danger
  parsed drapes padres rasped
  petal leapt plate pleat
  stare tears tares rates aster
  satin antis saint antis
  cream macer
  lemon melon
  least steal tales tales stale
  mane name amen nema
  rate tear tare
  able bale
  trap part rapt tarp
  evil vile live veil
  loop polo pool polo
  span naps snap pans
  emit time mite item
  arts rats star tsar tar
  lair rail liar lira
  pore repo rope ropes
  notes steno tones
  dog god
  cat act
  was saw
  bee
  ale lea
  step pets
  tops stop pots spot opts
  reed deer
  laps alps
  slog logs gols
].uniq

SCRABBLE_VALUES = {
  'a' => 1, 'e' => 1, 'i' => 1, 'o' => 1, 'u' => 1,
  'l' => 1, 'n' => 1, 's' => 1, 't' => 1, 'r' => 1,
  'd' => 2, 'g' => 2,
  'b' => 3, 'c' => 3, 'm' => 3, 'p' => 3,
  'f' => 4, 'h' => 4, 'v' => 4, 'w' => 4, 'y' => 4,
  'k' => 5,
  'j' => 8, 'x' => 8,
  'q' => 10, 'z' => 10
}

SAMPLE_TEXT = <<~TEXT
  The quick brown fox jumps over the lazy dog.
  A fox is a clever animal that can adapt to many environments.
  The dog chased the fox through the garden near the river.
  Foxes are known for their intelligence and their quick reflexes.
  The brown dog barked at the fox near the garden gate.
  Quick thinking helps animals survive in the wild.
  The lazy cat watched the dog and the fox from the window.
  Over time, animals develop remarkable skills to survive.
  The fox jumped over the dog and ran into the forest.
  Animals that are clever and quick tend to thrive.
TEXT

# ============================================================
# MODULE 1: ANAGRAM FINDER
# ============================================================

def anagram_key(word)
  word.downcase.chars.sort.join
end

def find_anagram_groups(words)
  groups = Hash.new { |h, k| h[k] = [] }
  words.each { |w| groups[anagram_key(w)] << w.downcase }
  groups.values.select { |g| g.length > 1 }.sort_by { |g| -g.length }
end

def are_anagrams?(word1, word2)
  anagram_key(word1) == anagram_key(word2)
end

# ============================================================
# MODULE 2: SCRABBLE SCORER
# ============================================================

def scrabble_score(word)
  word.downcase.chars.sum { |c| SCRABBLE_VALUES[c] || 0 }
end

def best_scrabble_words(words, top_n = 10)
  words.map { |w| [w, scrabble_score(w)] }
       .sort_by { |_, s| -s }
       .first(top_n)
end

# ============================================================
# MODULE 3: PALINDROME CHECKER
# ============================================================

def palindrome_word?(word)
  clean = word.downcase.gsub(/[^a-z0-9]/, '')
  clean == clean.reverse
end

def palindrome_phrase?(phrase)
  clean = phrase.downcase.gsub(/[^a-z0-9]/, '')
  clean == clean.reverse
end

PALINDROME_PHRASES = [
  "racecar",
  "A man a plan a canal Panama",
  "Was it a car or a cat I saw",
  "Never odd or even",
  "Do geese see God",
  "No lemon no melon",
  "Step on no pets",
  "Able was I ere I saw Elba",
  "Madam Im Adam",
  "Not a palindrome",
  "Hello world",
  "Nurses run",
  "Eva can I see bees in a cave",
  "Red rum sir is murder"
]

# ============================================================
# MODULE 4: WORD FREQUENCY ANALYSIS
# ============================================================

def word_frequency(text)
  words = text.downcase.scan(/[a-z]+/)
  freq = Hash.new(0)
  words.each { |w| freq[w] += 1 }
  freq.sort_by { |_, v| -v }
end

def print_freq_chart(freq_pairs, top_n = 20, bar_width = 30)
  top = freq_pairs.first(top_n)
  max_count = top.first[1]
  puts "  #{"Word":-15s}  Count  Chart"
  puts "  " + "-" * 50
  top.each_with_index do |(word, count), i|
    bar_len = (count.to_f / max_count * bar_width).round
    bar = "█" * bar_len
    puts "  #{(i+1).to_s.rjust(2)}. #{word.ljust(12)} #{count.to_s.rjust(4)}  #{bar}"
  end
end

# ============================================================
# MODULE 5: PIG LATIN
# ============================================================

def to_pig_latin(word)
  vowels = 'aeiouAEIOU'
  w = word.downcase
  if vowels.include?(w[0])
    w + "way"
  else
    # Find first vowel
    idx = w.chars.index { |c| vowels.include?(c) } || w.length
    w[idx..] + w[0...idx] + "ay"
  end
end

def sentence_to_pig_latin(sentence)
  sentence.split.map { |w| to_pig_latin(w) }.join(' ')
end

# ============================================================
# MODULE 6: WORD STATS
# ============================================================

def word_stats(text)
  words = text.downcase.scan(/[a-z]+/)
  sentences = text.split(/[.!?]+/).reject(&:empty?)
  chars = text.gsub(/\s+/, '').length
  {
    total_words: words.length,
    unique_words: words.uniq.length,
    sentences: sentences.length,
    avg_sentence_len: words.length.to_f / [sentences.length, 1].max,
    avg_word_len: words.sum(&:length).to_f / [words.length, 1].max,
    chars: chars,
    longest_word: words.max_by(&:length) || "",
    shortest_word: words.min_by(&:length) || ""
  }
end

def flesch_kincaid(text)
  words = text.downcase.scan(/[a-z]+/)
  sentences = text.scan(/[.!?]+/).length
  syllables = words.sum { |w| count_syllables(w) }
  return 0 if words.empty? || sentences.zero?
  score = 206.835 - 1.015 * (words.length.to_f / sentences) -
          84.6 * (syllables.to_f / words.length)
  score.round(1)
end

def count_syllables(word)
  word = word.downcase.gsub(/[^a-z]/, '')
  return 1 if word.empty?
  count = word.scan(/[aeiouy]+/).length
  count -= 1 if word.end_with?('e') && word.length > 2
  [count, 1].max
end

# ============================================================
# MAIN PROGRAM
# ============================================================

puts "=" * 60
puts "  WORD GAMES — Ruby Showcase"
puts "  Anagrams, Scrabble, Palindromes, Frequency, Pig Latin"
puts "=" * 60
puts

# --- Section 1: Anagram Groups ---
puts "SECTION 1: ANAGRAM GROUPS"
puts "-" * 60
groups = find_anagram_groups(WORD_LIST)
puts "  Found #{groups.length} anagram groups in word list:"
puts
groups.first(12).each_with_index do |group, i|
  words = group.sort.join(', ')
  key   = anagram_key(group[0])
  puts "  #{(i+1).to_s.rjust(2)}. [#{key}] => #{words}"
end
puts
puts "  Spot checks:"
[["listen", "silent"], ["earth", "heart"], ["race", "care"], ["notes", "stone"]].each do |a, b|
  res = are_anagrams?(a, b) ? "YES ✓" : "NO ✗"
  puts "    '#{a}' and '#{b}' are anagrams: #{res}"
end
puts

# --- Section 2: Scrabble Scores ---
puts "SECTION 2: SCRABBLE SCORING"
puts "-" * 60
puts "  Tile values: A/E/I/O/U/L/N/S/T/R=1, D/G=2, B/C/M/P=3, F/H/V/W/Y=4, K=5, J/X=8, Q/Z=10"
puts
test_words = %w[quiz jazz foxy zebra xenon quick jumpy zap buzzard ox]
puts "  Individual word scores:"
test_words.each do |w|
  puts "    #{w.ljust(10)} => #{scrabble_score(w)} points"
end
puts
puts "  Top 10 highest-scoring words from word list:"
best_scrabble_words(WORD_LIST + test_words).each_with_index do |(word, score), i|
  bar = "★" * [score / 2, 1].max
  puts "    #{(i+1).to_s.rjust(2)}. #{word.ljust(12)} #{score.to_s.rjust(3)} pts  #{bar}"
end
puts

# --- Section 3: Palindromes ---
puts "SECTION 3: PALINDROME CHECKER"
puts "-" * 60
puts "  Testing #{PALINDROME_PHRASES.length} phrases:"
puts
PALINDROME_PHRASES.each do |phrase|
  result = palindrome_phrase?(phrase) ? "YES ✓" : "NO  ✗"
  puts "  [#{result}] \"#{phrase}\""
end
puts

# --- Section 4: Word Frequency ---
puts "SECTION 4: WORD FREQUENCY ANALYSIS"
puts "-" * 60
puts "  Analyzing sample text (#{SAMPLE_TEXT.split.length} words)..."
puts
freq = word_frequency(SAMPLE_TEXT)
print_freq_chart(freq, 15)
puts
stats = word_stats(SAMPLE_TEXT)
puts "  Document statistics:"
puts "    Total words:       #{stats[:total_words]}"
puts "    Unique words:      #{stats[:unique_words]}"
puts "    Sentences:         #{stats[:sentences]}"
puts "    Avg sentence len:  #{stats[:avg_sentence_len].round(1)} words"
puts "    Avg word length:   #{stats[:avg_word_len].round(2)} chars"
puts "    Total characters:  #{stats[:chars]}"
puts "    Longest word:      '#{stats[:longest_word]}'"
puts "    Shortest word:     '#{stats[:shortest_word]}'"
fk = flesch_kincaid(SAMPLE_TEXT)
puts "    Flesch-Kincaid:    #{fk} (#{fk >= 70 ? 'Easy' : fk >= 50 ? 'Standard' : 'Difficult'})"
puts

# --- Section 5: Pig Latin ---
puts "SECTION 5: PIG LATIN TRANSLATOR"
puts "-" * 60
pig_sentences = [
  "The quick brown fox jumps over the lazy dog",
  "Ruby is a wonderful programming language",
  "Hello world this is a test",
  "Apple orange umbrella ice elephant",
]
pig_sentences.each do |sentence|
  puts "  Original:  #{sentence}"
  puts "  Pig Latin: #{sentence_to_pig_latin(sentence)}"
  puts
end

# --- Section 6: Word ladder hint ---
puts "SECTION 6: WORD SIMILARITY (edit distance)"
puts "-" * 60
def edit_distance(a, b)
  m, n = a.length, b.length
  dp = Array.new(m + 1) { Array.new(n + 1, 0) }
  (0..m).each { |i| dp[i][0] = i }
  (0..n).each { |j| dp[0][j] = j }
  (1..m).each do |i|
    (1..n).each do |j|
      if a[i-1] == b[j-1]
        dp[i][j] = dp[i-1][j-1]
      else
        dp[i][j] = 1 + [dp[i-1][j], dp[i][j-1], dp[i-1][j-1]].min
      end
    end
  end
  dp[m][n]
end

word_pairs = [["kitten", "sitting"], ["saturday", "sunday"], ["race", "face"], ["algorithm", "altruistic"]]
word_pairs.each do |a, b|
  d = edit_distance(a, b)
  puts "  edit_distance('#{a}', '#{b}') = #{d}"
end
puts

puts "=" * 60
puts "  Word Games complete!"
puts "  Anagrams * Scrabble * Palindromes * Frequency * Pig Latin"
puts "=" * 60
