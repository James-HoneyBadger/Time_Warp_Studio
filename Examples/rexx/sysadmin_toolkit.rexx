/* ================================================================
   REXX SYSTEM ADMINISTRATION TOOLKIT
   A genuinely useful collection of system administration utilities
   demonstrating REXX's strengths: string handling, OS interface,
   parsing, and automation.
   ================================================================ */

say "╔══════════════════════════════════════════════════════╗"
say "║         REXX SYSTEM ADMINISTRATION TOOLKIT           ║"
say "╚══════════════════════════════════════════════════════╝"

/* ── Main Menu ─────────────────────────────────────────────────── */
MAIN:
  say ""
  say "  Select a utility:"
  say "  [1] Text File Analyzer"
  say "  [2] CSV Parser & Reporter"
  say "  [3] Directory Scanner"
  say "  [4] String Processing Toolkit"
  say "  [5] Simple Calculator (expression evaluator)"
  say "  [6] Template Processor"
  say "  [7] Run All Demos"
  say "  [0] Exit"
  say ""
  call charout, "  Choice: "
  pull choice
  choice = strip(choice)

  select
    when choice = '1' then call TEXT_ANALYZER
    when choice = '2' then call CSV_DEMO
    when choice = '3' then call DIR_SCANNER
    when choice = '4' then call STRING_TOOLKIT
    when choice = '5' then call CALCULATOR
    when choice = '6' then call TEMPLATE_DEMO
    when choice = '7' then do
        call TEXT_ANALYZER
        call CSV_DEMO
        call STRING_TOOLKIT
        call CALCULATOR
        call TEMPLATE_DEMO
    end
    when choice = '0' then do
        say "  Goodbye!"
        exit 0
    end
    otherwise say "  Invalid choice."
  end
  signal MAIN

/* ================================================================
   1. TEXT FILE ANALYZER
   Simulate reading a text and computing statistics.
   ================================================================ */
TEXT_ANALYZER:
  say ""
  say "  ─── TEXT FILE ANALYZER ───"

  /* Sample text (in real usage, read from a file) */
  text = "The quick brown fox jumps over the lazy dog. The fox was",
         "very quick indeed. Dogs are often lazy but loyal animals.",
         "Brown foxes are rare in nature."

  totalWords  = 0
  totalChars  = length(text)
  wordCounts. = 0   /* stem for word frequency */

  /* Parse word by word */
  remaining = text
  do while remaining \= ""
    parse var remaining word remaining
    word = translate(word, "abcdefghijklmnopqrstuvwxyz",,
                    "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
    /* Strip punctuation */
    word = translate(word,, ".,:;!?""'()")
    word = strip(word)
    if word \= "" then do
      totalWords = totalWords + 1
      wordCounts.word = wordCounts.word + 1
    end
  end

  /* Compute unique words — not easily done with stems alone */
  sentences = 0
  do i = 1 to length(text)
    ch = substr(text, i, 1)
    if ch = '.' | ch = '!' | ch = '?' then sentences = sentences + 1
  end

  avgWordsPerSent = format(totalWords / max(sentences,1), , 1)

  say "  Total characters: " totalChars
  say "  Total words:      " totalWords
  say "  Estimated sentences: " sentences
  say "  Avg words/sentence:  " avgWordsPerSent
  say ""
  say "  Word frequency (selected):"
  say "    'the':  " wordCounts.the
  say "    'fox':  " wordCounts.fox
  say "    'lazy': " wordCounts.lazy
  say "    'brown':" wordCounts.brown

  return

/* ================================================================
   2. CSV PARSER & REPORTER
   Parse a CSV string and produce a report.
   ================================================================ */
CSV_DEMO:
  say ""
  say "  ─── CSV PARSER & REPORTER ───"

  /* Embedded simulated CSV data */
  csv.0 = 6
  csv.1 = "Name,Department,Salary,Years"
  csv.2 = "Alice Smith,Engineering,85000,5"
  csv.3 = "Bob Jones,Marketing,62000,3"
  csv.4 = "Carol White,Engineering,92000,8"
  csv.5 = "Dave Brown,Finance,71000,4"
  csv.6 = "Eve Davis,Engineering,78000,2"

  /* Parse header */
  parse var csv.1 hdr1 "," hdr2 "," hdr3 "," hdr4
  say "  Columns: "hdr1", "hdr2", "hdr3", "hdr4

  totalSalary = 0
  count       = 0
  engSalary   = 0
  engCount    = 0

  say ""
  say "  " left("Name",15) left("Dept",12) right("Salary",9) right("Yrs",4)
  say "  " copies("-",44)

  do i = 2 to csv.0
    parse var csv.i name "," dept "," salary "," years
    say "  " left(name,15) left(dept,12) right(salary,9) right(years,4)
    totalSalary = totalSalary + salary
    count = count + 1
    if dept = "Engineering" then do
      engSalary = engSalary + salary
      engCount  = engCount  + 1
    end
  end

  avgSalary    = format(totalSalary / count, , 2)
  avgEngSalary = format(engSalary   / max(engCount,1), , 2)

  say "  " copies("-",44)
  say "  Total employees:    " count
  say "  Average salary:    $" avgSalary
  say "  Eng dept average:  $" avgEngSalary

  return

/* ================================================================
   3. DIRECTORY SCANNER (simulated — portable)
   ================================================================ */
DIR_SCANNER:
  say ""
  say "  ─── DIRECTORY SCANNER ───"
  say "  (Simulated file listing — real version uses ADDRESS TSO or"
  say "   ADDRESS CMD 'DIR /B' on Windows / 'ls -la' on Linux)"
  say ""

  /* Simulated files */
  file.0 = 8
  file.1 = "budget2025.xlsx    45231  2025-11-15"
  file.2 = "payroll_q4.pdf     128450 2025-12-01"
  file.3 = "inventory.csv      8734   2025-12-03"
  file.4 = "report_dec.docx    67891  2025-12-15"
  file.5 = "archive_old.zip    452100 2024-06-30"
  file.6 = "config.cfg         1024   2025-01-10"
  file.7 = "database.bak       987654 2025-12-20"
  file.8 = "notes.txt          512    2025-12-24"

  totalSize = 0
  xlsxCount = 0; pdfCount = 0; csvCount = 0; otherCount = 0

  say "  " left("Filename",25) right("Size",10) "  Date"
  say "  " copies("-",48)

  do i = 1 to file.0
    parse var file.i fname size date
    say "  " left(fname,25) right(size,10) "  " date
    totalSize  = totalSize + size

    ext = translate(word(translate(fname,"         ","._"),2))
    select
      when ext = "XLSX" then xlsxCount = xlsxCount + 1
      when ext = "PDF"  then pdfCount  = pdfCount  + 1
      when ext = "CSV"  then csvCount  = csvCount  + 1
      otherwise              otherCount = otherCount + 1
    end
  end

  say "  " copies("-",48)
  say "  Files: " file.0 "    Total size:" totalSize "bytes"
  say "  By type: XLSX="xlsxCount " PDF="pdfCount " CSV="csvCount,
      " Other="otherCount

  return

/* ================================================================
   4. STRING PROCESSING TOOLKIT
   ================================================================ */
STRING_TOOLKIT:
  say ""
  say "  ─── STRING PROCESSING TOOLKIT ───"

  original = "  Hello, World! This is REXX String Processing.  "
  say "  Original: [" || original || "]"
  say "  strip:    [" || strip(original) || "]"
  say "  upper:    [" || translate(original) || "]"
  say "  lower:    [" || translate(original,,
                        "ABCDEFGHIJKLMNOPQRSTUVWXYZ",
                        "abcdefghijklmnopqrstuvwxyz") || "]"
  say "  length:   " length(original)
  say "  words:    " words(original)
  say "  word 3:  [" || word(original,3) || "]"
  say "  substr(3,5): [" || substr(strip(original),3,5) || "]"

  /* Pattern matching with PARSE */
  record = "2025-12-26|E0001001|Smith, Robert|95000.00|PAYCHECK"
  say ""
  say "  Parse record: " record
  parse var record year "-" month "-" day "|" empid "|" name "|",
                         amount "|" rectype
  say "    Year:"    year "  Month:" month "  Day:" day
  say "    EmpID:"   empid
  say "    Name:"    name
  say "    Amount: $" amount
  say "    Type:"    rectype

  /* Reverse a string */
  s = "REXX"
  rev = ""
  do i = length(s) to 1 by -1
    rev = rev || substr(s,i,1)
  end
  say ""
  say "  Reverse of '" || s || "' = '" || rev || "'"

  /* Check palindrome */
  pal = "RACECAR"
  revp = ""
  do i = length(pal) to 1 by -1
    revp = revp || substr(pal,i,1)
  end
  if pal = revp then say "  '" || pal || "' is a palindrome!"
  else               say "  '" || pal || "' is NOT a palindrome."

  return

/* ================================================================
   5. EXPRESSION CALCULATOR (using INTERPRET)
   ================================================================ */
CALCULATOR:
  say ""
  say "  ─── REXX EXPRESSION CALCULATOR ───"
  say "  REXX's INTERPRET executes strings as live code."
  say ""

  exprs.0 = 6
  exprs.1 = "2 + 3 * 4"
  exprs.2 = "(100 - 32) * 5 / 9"
  exprs.3 = "2 ** 10"
  exprs.4 = "sqrt(2)"
  exprs.5 = "10 // 3"
  exprs.6 = "abs(-42)"

  do i = 1 to exprs.0
    e = exprs.i
    interpret "result = " e
    say "  " right(e,20) " = " result
  end

  return

/* ================================================================
   6. TEMPLATE PROCESSOR
   Substitute {{variable}} placeholders in a template.
   ================================================================ */
TEMPLATE_DEMO:
  say ""
  say "  ─── TEMPLATE PROCESSOR ───"

  template = "Dear {{NAME}},",
             "Your paycheck of ${{AMOUNT}} has been deposited.",
             "Pay Period: {{PERIOD}}  Department: {{DEPT}}",
             "Thank you, {{COMPANY}} HR Department."

  /* Variable map using stems */
  vars.NAME    = "Robert Smith"
  vars.AMOUNT  = "3,461.54"
  vars.PERIOD  = "Dec 8-21, 2025"
  vars.DEPT    = "Operations"
  vars.COMPANY = "Acme Corporation"

  say "  Result:"
  do j = 1 to words(template)
    line = word(template,j)
    /* This is simplified — real template proc handles {{}} inline */
  end

  /* Process full template as a block */
  do i = 1 to 4
    tmpl_lines.1 = "Dear {{NAME}},"
    tmpl_lines.2 = "Your paycheck of ${{AMOUNT}} has been deposited."
    tmpl_lines.3 = "Pay Period: {{PERIOD}}  Department: {{DEPT}}"
    tmpl_lines.4 = "Thank you, {{COMPANY}} HR Department."
  end

  do i = 1 to 4
    line = tmpl_lines.i
    do forever
      start = pos("{{", line)
      if start = 0 then leave
      end_p = pos("}}", line, start)
      if end_p = 0 then leave
      varname = substr(line, start+2, end_p-start-2)
      interpret "val = vars." || varname
      line = left(line, start-1) || val || substr(line, end_p+2)
    end
    say "    " line
  end

  return
