⍝ APL String Processing
⍝ Character arrays, string operations, and text analysis

⍝ ── Basic string operations ──────────────────────────────────────────
⎕← '=== APL String Processing ==='
s ← 'Hello, World!'
⎕← 'String:    ' , s
⎕← 'Length:    ' , ⍕⍴s
⎕← 'Uppercase: ' , 1⎕C s
⎕← 'Lowercase: ' , 0⎕C s
⎕← 'Reversed:  ' , ⌽s

⍝ ── Character testing ────────────────────────────────────────────────
⎕← ''
⎕← '=== Character Classification ==='
text ← 'Hello World 123!'
⎕← 'Text: "' , text , '"'
⎕← 'Letters:  ' , ⍕+/text∊⎕A,⎕a
⎕← 'Digits:   ' , ⍕+/text∊'0123456789'
⎕← 'Spaces:   ' , ⍕+/' '=text

⍝ ── Word counting ────────────────────────────────────────────────────
⎕← ''
⎕← '=== Word Count ==='
sentence ← 'the quick brown fox jumps over the lazy dog'
⎕← 'Sentence: ' , sentence
words ← ' '(≠⊆⊢)sentence     ⍝ split on spaces
⎕← 'Word count: ' , ⍕⍴words
⎕← 'Words: ' , ⍕words
unique ← ∪words
⎕← 'Unique words: ' , ⍕⍴unique

⍝ ── Caesar cipher ────────────────────────────────────────────────────
⎕← ''
⎕← '=== Caesar Cipher (shift 3) ==='
encode ← {
    alpha ← ⎕A
    mask  ← ⍵∊alpha
    shifted ← alpha[1+26|¯1+alpha⍳⍵+3]
    mask/⍵ ← shifted
    ⍵
}
plaintext ← 'HELLO WORLD'
⎕← 'Plain:   ' , plaintext
⎕← 'Encoded: ' , encode plaintext

⍝ ── Frequency analysis ───────────────────────────────────────────────
⎕← ''
⎕← '=== Letter Frequency ==='
pangram ← 'THE QUICK BROWN FOX JUMPS OVER THE LAZY DOG'
letters ← pangram/⍨pangram∊⎕A
uniq    ← ∪letters
counts  ← {+/⍵=letters}¨uniq
order   ← ⍒counts
⎕← 'Top 5 letters:'
top5 ← order[⍳5]
⎕← (uniq[top5]) ,⍪ counts[top5]
