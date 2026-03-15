⍝ ══════════════════════════════════════
⍝   📊 APL Statistics & Data Analysis
⍝ ══════════════════════════════════════

⍝ === Sample Datasets ===
⍝ Student test scores
SCORES ← 85 92 78 95 88 72 90 84 91 76 83 97 80 86 93

⍝ Monthly sales data (thousands)
SALES ← 120 135 142 128 155 148 162 170 158 145 175 180

⍝ === Basic Statistics ===
⍝ "📊 Descriptive Statistics for Test Scores"
⍝ "─────────────────────────────────────────"

N ← ⍴SCORES
⍝ "Count:" N

MEAN ← (+/SCORES)÷N
⍝ "Mean:" MEAN

⍝ Sort for median
SORTED ← SCORES[⍋SCORES]
MEDIAN ← SORTED[⌈N÷2]
⍝ "Median:" MEDIAN

⍝ "Min:" ⌊/SCORES
⍝ "Max:" ⌈/SCORES
⍝ "Range:" (⌈/SCORES)-(⌊/SCORES)

⍝ Variance and Standard Deviation
VAR ← (+/(SCORES-MEAN)*2)÷N
⍝ "Variance:" VAR

⍝ === Data Transformations ===
⍝ "📈 Sales Analysis"
⍝ "────────────────"

⍝ Month-over-month growth
GROWTH ← 100×((1↓SALES)-(¯1↓SALES))÷¯1↓SALES
⍝ "Growth rates (%):" GROWTH

⍝ Cumulative sales
CUMULATIVE ← +\SALES
⍝ "Cumulative sales:" CUMULATIVE

⍝ Running average
RUNNING_AVG ← CUMULATIVE÷⍳⍴SALES
⍝ "Running average:" RUNNING_AVG

⍝ === Frequency Distribution ===
⍝ "📊 Score Distribution"
⍝ "────────────────────"

⍝ Grade buckets: A(90+), B(80-89), C(70-79)
A_COUNT ← +/SCORES≥90
B_COUNT ← +/(SCORES≥80)∧(SCORES<90)
C_COUNT ← +/SCORES<80
⍝ "A grades (90+):" A_COUNT
⍝ "B grades (80-89):" B_COUNT
⍝ "C grades (70-79):" C_COUNT

⍝ === Matrix Operations ===
⍝ "🔢 Matrix Operations"
⍝ "───────────────────"

M ← 3 3⍴1 2 3 4 5 6 7 8 9
⍝ "3×3 Matrix:" M

⍝ Row sums
⍝ "Row sums:" +/M

⍝ Column sums
⍝ "Column sums:" +⌿M

⍝ Diagonal
⍝ "Main diagonal:" (1 1)(2 2)(3 3)⌷¨⊂M

⍝ Transpose
⍝ "Transposed:" ⍉M

⍝ === Correlation (simplified) ===
⍝ "📐 Correlation Analysis"
⍝ "──────────────────────"

X ← 1 2 3 4 5 6 7 8 9 10
Y ← 2.1 3.9 6.2 7.8 10.1 12.3 13.8 16.1 18.0 20.2

MX ← (+/X)÷⍴X
MY ← (+/Y)÷⍴Y

⍝ Covariance numerator
COV_NUM ← +/(X-MX)×(Y-MY)

⍝ Standard deviations (sum of squared deviations)
SX ← (+/(X-MX)*2)*0.5
SY ← (+/(Y-MY)*2)*0.5

⍝ Pearson correlation coefficient
R ← COV_NUM÷SX×SY
⍝ "Pearson r:" R
⍝ "Strong positive correlation ✅"

⍝ === Output All Results ===
'╔══════════════════════════════════════╗'
'║   📊 APL Statistics Suite           ║'
'╚══════════════════════════════════════╝'
''
'── Test Score Statistics ──'
'Scores:' SCORES
'Count:' N '  Mean:' MEAN '  Median:' MEDIAN
'Min:' (⌊/SCORES) '  Max:' (⌈/SCORES) '  Range:' ((⌈/SCORES)-(⌊/SCORES))
''
'── Sales Growth Analysis ──'
'Monthly sales:' SALES
'Cumulative:' CUMULATIVE
''
'── Grade Distribution ──'
'A (90+):' A_COUNT '  B (80-89):' B_COUNT '  C (<80):' C_COUNT
''
'── Correlation ──'
'r =' R
'✅ Analysis complete!'
