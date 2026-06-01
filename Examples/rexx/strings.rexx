/* REXX String Operations Demo */
greeting = 'Hello, REXX!'
SAY 'Original:' greeting
SAY 'Length:' LENGTH(greeting)
SAY 'Uppercase:' UPPER(greeting)
SAY 'Lowercase:' LOWER(greeting)
SAY 'Reversed:' REVERSE(greeting)
SAY 'Substring(1,5):' SUBSTR(greeting, 1, 5)
SAY 'Copies x3:' COPIES('Ha', 3)

name = '  Alice  '
SAY 'Before strip: [' || name || ']'
SAY 'After strip: [' || STRIP(name) || ']'

sentence = 'The quick brown fox'
SAY 'Words:' WORDS(sentence)
SAY 'Word 2:' WORD(sentence, 2)
SAY 'Center(20):' CENTER('REXX', 20, '*')
