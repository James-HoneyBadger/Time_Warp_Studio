/* REXX - Pattern Parsing */
/* PARSE instruction — REXX's powerful string decomposition */

SAY '=== REXX Pattern Parsing ==='
SAY ''

/* Basic PARSE VAR */
line = 'Alice  30  Engineer  New York'
PARSE VAR line name age role city
SAY 'Name: 'name
SAY 'Age:  'age
SAY 'Role: 'role
SAY 'City: 'city
SAY ''

/* Positional parsing */
SAY '=== Positional Parsing ==='
data = 'John     25   London'
PARSE VAR data 1 fname 10 age 15 city
SAY 'Name: 'STRIP(fname)
SAY 'Age:  'STRIP(age)
SAY 'City: 'STRIP(city)
SAY ''

/* Template with literals */
SAY '=== Template Parsing ==='
csv = 'Smith,Jane,1985-04-15,F'
PARSE VAR csv last ',' first ',' dob ',' gender
SAY 'Last:   'last
SAY 'First:  'first
SAY 'DOB:    'dob
SAY 'Gender: 'gender
SAY ''

/* Parse a date */
PARSE VAR dob year '-' month '-' day
SAY '=== Date Components ==='
SAY 'Year:  'year
SAY 'Month: 'month
SAY 'Day:   'day
SAY ''

/* PARSE with UPPER */
SAY '=== PARSE UPPER ==='
mixed = 'Hello World from REXX'
PARSE UPPER VAR mixed word1 word2 word3 word4
SAY word1 word2 word3 word4
SAY ''

/* Word parsing loop */
SAY '=== Word-by-word parsing ==='
sentence = 'The quick brown fox jumps over the lazy dog'
count = 0
rest = sentence
DO WHILE rest \= ''
    PARSE VAR rest word rest
    count = count + 1
    SAY '  Word 'count': 'word
END
SAY 'Total words: 'count
