"""
STATISTICS SUITE — Python Language Showcase
Statistical analysis with turtle graphics visualizations
Time Warp Studio — Python Language Demo
"""

import math
import turtle

# ============================================================
# STATISTICAL FUNCTIONS
# ============================================================

def mean(data):
    return sum(data) / len(data)

def median(data):
    s = sorted(data)
    n = len(s)
    if n % 2 == 1:
        return s[n // 2]
    return (s[n // 2 - 1] + s[n // 2]) / 2

def mode(data):
    counts = {}
    for x in data:
        counts[x] = counts.get(x, 0) + 1
    max_count = max(counts.values())
    return [k for k, v in counts.items() if v == max_count]

def variance(data, population=True):
    m = mean(data)
    denom = len(data) if population else len(data) - 1
    return sum((x - m) ** 2 for x in data) / denom

def std_dev(data, population=True):
    return math.sqrt(variance(data, population))

def quartiles(data):
    s = sorted(data)
    n = len(s)
    q2 = median(s)
    lower = s[:n // 2]
    upper = s[(n + 1) // 2:]
    q1 = median(lower) if lower else q2
    q3 = median(upper) if upper else q2
    return q1, q2, q3

def iqr(data):
    q1, _, q3 = quartiles(data)
    return q3 - q1

def skewness(data):
    m = mean(data)
    s = std_dev(data)
    if s == 0:
        return 0
    n = len(data)
    return sum(((x - m) / s) ** 3 for x in data) / n

def kurtosis(data):
    m = mean(data)
    s = std_dev(data)
    if s == 0:
        return 0
    n = len(data)
    return sum(((x - m) / s) ** 4 for x in data) / n - 3

def pearson_corr(x, y):
    mx, my = mean(x), mean(y)
    num = sum((xi - mx) * (yi - my) for xi, yi in zip(x, y))
    denom = math.sqrt(sum((xi - mx)**2 for xi in x) * sum((yi - my)**2 for yi in y))
    return num / denom if denom != 0 else 0

def percentile(data, p):
    s = sorted(data)
    idx = (p / 100) * (len(s) - 1)
    lo = int(idx)
    hi = min(lo + 1, len(s) - 1)
    return s[lo] + (idx - lo) * (s[hi] - s[lo])

# ============================================================
# DATASETS
# ============================================================

heights_cm = [
    158, 162, 165, 167, 168, 169, 170, 171, 172, 173,
    174, 174, 175, 175, 176, 177, 178, 178, 179, 180,
    181, 182, 183, 184, 185, 186, 187, 188, 190, 195
]

test_scores = [
    45, 52, 58, 61, 64, 66, 68, 70, 71, 72,
    73, 74, 75, 76, 77, 78, 79, 80, 81, 82,
    83, 84, 85, 86, 88, 89, 91, 93, 96, 99
]

monthly_returns_pct = [
    2.1, -1.3, 3.4, 0.8, -2.5, 4.1, 1.2, -0.9, 2.7, 3.8,
    -1.1, 5.2, -3.4, 2.9, 1.5, -0.7, 4.3, -2.1, 3.1, 1.8,
    -4.5, 6.2, 2.0, -1.8, 3.5
]

# Heights over years (trend analysis)
years = list(range(2000, 2025))
avg_height_trend = [
    172.1, 172.3, 172.4, 172.6, 172.8, 173.0, 173.1, 173.2, 173.3, 173.5,
    173.6, 173.7, 173.8, 174.0, 174.1, 174.2, 174.3, 174.4, 174.5, 174.6,
    174.7, 174.8, 174.9, 175.0, 175.1
]

# ============================================================
# ANALYSIS FUNCTIONS
# ============================================================

def analyze(name, data):
    print(f"  Dataset: {name} (n={len(data)})")
    print(f"  Min: {min(data):.2f}   Max: {max(data):.2f}")
    print(f"  Mean: {mean(data):.2f}   Median: {median(data):.2f}")
    m = mode(data)
    print(f"  Mode: {m}")
    print(f"  Std Dev: {std_dev(data):.2f}   Variance: {variance(data):.2f}")
    q1, q2, q3 = quartiles(data)
    print(f"  Q1: {q1:.2f}   Q2 (median): {q2:.2f}   Q3: {q3:.2f}")
    print(f"  IQR: {iqr(data):.2f}")
    p10 = percentile(data, 10)
    p90 = percentile(data, 90)
    print(f"  P10: {p10:.2f}   P90: {p90:.2f}")
    sk = skewness(data)
    kt = kurtosis(data)
    print(f"  Skewness: {sk:.3f} ({'left skewed' if sk < 0 else 'right skewed' if sk > 0 else 'symmetric'})")
    print(f"  Kurtosis (excess): {kt:.3f} ({'leptokurtic' if kt > 0 else 'platykurtic' if kt < 0 else 'mesokurtic'})")

def ascii_histogram(data, bins=10, width=40, label="Frequency"):
    min_v = min(data)
    max_v = max(data)
    bin_size = (max_v - min_v) / bins
    counts = [0] * bins
    for x in data:
        idx = min(int((x - min_v) / bin_size), bins - 1)
        counts[idx] += 1
    max_count = max(counts)
    print(f"  {label}:")
    for i in range(bins):
        lo = min_v + i * bin_size
        hi = lo + bin_size
        bar = "█" * int(counts[i] * width / max_count)
        print(f"  [{lo:6.1f}-{hi:6.1f}] {bar} {counts[i]}")

def ascii_boxplot(data, width=50):
    s = sorted(data)
    lo = min(s)
    hi = max(s)
    q1, q2, q3 = quartiles(s)
    rng = hi - lo
    def pos(v):
        return int((v - lo) / rng * width) if rng > 0 else 0
    line = [' '] * (width + 1)
    # whiskers
    for i in range(pos(lo), pos(q1)):   line[i] = '-'
    for i in range(pos(q3), pos(hi)):   line[i] = '-'
    # box
    for i in range(pos(q1), pos(q3)+1): line[i] = '='
    line[pos(q2)] = '|'
    line[pos(lo)] = '<'
    line[pos(hi)] = '>'
    print(f"  {''.join(line)}")
    print(f"  Min={lo:.1f}  Q1={q1:.1f}  Median={q2:.1f}  Q3={q3:.1f}  Max={hi:.1f}")

# ============================================================
# TURTLE GRAPHICS VISUALIZATIONS
# ============================================================

def draw_bar_chart(data, labels, title, colors):
    """Draw a bar chart using turtle graphics."""
    t = turtle.Turtle()
    t.speed(0)
    t.hideturtle()
    t.penup()
    n = len(data)
    max_val = max(data)
    chart_h = 200
    chart_w = 350
    bar_w = chart_w // n - 4
    origin_x = -170
    origin_y = -120
    # Draw axes
    t.goto(origin_x, origin_y)
    t.pendown()
    t.pencolor("white")
    t.goto(origin_x, origin_y + chart_h + 20)
    t.penup()
    t.goto(origin_x, origin_y)
    t.pendown()
    t.goto(origin_x + chart_w + 20, origin_y)
    t.penup()
    # Draw bars
    for i, (val, label, color) in enumerate(zip(data, labels, colors)):
        h = int(val / max_val * chart_h)
        x = origin_x + 10 + i * (bar_w + 4)
        y = origin_y
        t.goto(x, y)
        t.pendown()
        t.fillcolor(color)
        t.begin_fill()
        t.goto(x, y + h)
        t.goto(x + bar_w, y + h)
        t.goto(x + bar_w, y)
        t.goto(x, y)
        t.end_fill()
        t.penup()
    t.done()

def draw_scatter_plot(x_data, y_data, title):
    """ASCII scatter plot (turtle not needed for scatter)."""
    width = 60
    height = 20
    min_x, max_x = min(x_data), max(x_data)
    min_y, max_y = min(y_data), max(y_data)
    grid = [[' '] * width for _ in range(height)]
    for xi, yi in zip(x_data, y_data):
        col = int((xi - min_x) / (max_x - min_x + 1e-9) * (width - 1))
        row = int((yi - min_y) / (max_y - min_y + 1e-9) * (height - 1))
        row = height - 1 - row  # flip y
        grid[row][col] = '●'
    print(f"  Scatter: {title}")
    print("  " + "─" * width)
    for row in grid:
        print("  │" + ''.join(row) + "│")
    print("  " + "─" * width)
    corr = pearson_corr(x_data, y_data)
    print(f"  Pearson correlation r = {corr:.4f}")

# ============================================================
# MAIN
# ============================================================

print("=" * 60)
print("  STATISTICS SUITE — Python Language Showcase")
print("  Mean, Median, Mode, Std Dev, Quartiles, Correlation")
print("=" * 60)
print()

# --- Dataset 1: Heights ---
print("DATASET 1: HEIGHT DISTRIBUTION (cm)")
print("-" * 60)
analyze("Heights (cm)", heights_cm)
print()
ascii_histogram(heights_cm, bins=8, label="Height distribution")
print()
print("  Box Plot:")
ascii_boxplot(heights_cm)
print()

# --- Dataset 2: Test Scores ---
print("DATASET 2: TEST SCORES")
print("-" * 60)
analyze("Test Scores", test_scores)
print()
ascii_histogram(test_scores, bins=10, label="Score distribution")
print()
print("  Box Plot:")
ascii_boxplot(test_scores)
print()

# --- Dataset 3: Monthly Returns ---
print("DATASET 3: MONTHLY INVESTMENT RETURNS (%)")
print("-" * 60)
analyze("Monthly Returns (%)", monthly_returns_pct)
print()
ascii_histogram(monthly_returns_pct, bins=8, label="Returns distribution")
print()
print("  Box Plot:")
ascii_boxplot(monthly_returns_pct)
print()

# --- Correlation: Year vs height trend ---
print("DATASET 4: HEIGHT TREND CORRELATION (Year vs Average Height)")
print("-" * 60)
draw_scatter_plot(years, avg_height_trend, "Year vs Average Height (cm)")
print()

# --- Z-Score analysis ---
print("STATISTICAL DEEP DIVE: Z-SCORES FOR TEST SCORES")
print("-" * 60)
m = mean(test_scores)
s = std_dev(test_scores)
outliers = []
print("  Score    Z-Score   Interpretation")
print("  -----    -------   ----------------")
for score in test_scores:
    z = (score - m) / s
    interp = ""
    if abs(z) > 2.0:
        interp = " ← OUTLIER"
        outliers.append(score)
    elif abs(z) > 1.5:
        interp = " ← Notable"
    print(f"    {score:3d}     {z:+.2f}   {'★' if abs(z) > 2 else ' '}{interp}")

print()
print(f"  Outliers (|z| > 2): {outliers if outliers else 'none'}")
print()

# --- Normal distribution check ---
print("NORMALITY CHECK: 68-95-99.7 RULE")
print("-" * 60)
m = mean(test_scores)
s = std_dev(test_scores)
within_1 = sum(1 for x in test_scores if abs(x - m) <= s)
within_2 = sum(1 for x in test_scores if abs(x - m) <= 2*s)
within_3 = sum(1 for x in test_scores if abs(x - m) <= 3*s)
n = len(test_scores)
print(f"  Mean: {m:.1f}   Std Dev: {s:.1f}")
print(f"  Within 1σ: {within_1}/{n} = {100*within_1/n:.1f}% (expected 68.2%)")
print(f"  Within 2σ: {within_2}/{n} = {100*within_2/n:.1f}% (expected 95.4%)")
print(f"  Within 3σ: {within_3}/{n} = {100*within_3/n:.1f}% (expected 99.7%)")
print()

# --- Turtle bar chart ---
print("TURTLE GRAPHICS: Drawing bar chart of grade distribution...")
grade_ranges = ["A (90+)", "B (80-89)", "C (70-79)", "D (60-69)", "F (<60)"]
grade_counts = [
    sum(1 for s in test_scores if s >= 90),
    sum(1 for s in test_scores if 80 <= s < 90),
    sum(1 for s in test_scores if 70 <= s < 80),
    sum(1 for s in test_scores if 60 <= s < 70),
    sum(1 for s in test_scores if s < 60),
]
grade_colors = ["#00aa44", "#2288ff", "#ffaa00", "#ff6600", "#cc2222"]

t = turtle.Turtle()
t.speed(0)
t.hideturtle()
t.penup()
screen = turtle.Screen()
screen.title("Statistics Suite — Grade Distribution")
screen.bgcolor("#1e1e2e")

# Draw title
t.goto(0, 230)
t.pencolor("white")
t.write("Grade Distribution", align="center", font=("Arial", 14, "bold"))

# Draw bars
chart_h = 180
bar_w = 50
gap = 15
total_w = len(grade_counts) * (bar_w + gap)
start_x = -total_w // 2
max_count = max(grade_counts)

for i, (count, label, color) in enumerate(zip(grade_counts, grade_ranges, grade_colors)):
    x = start_x + i * (bar_w + gap)
    y = -150
    h = int(count / max_count * chart_h)
    # Draw bar
    t.goto(x, y)
    t.pendown()
    t.fillcolor(color)
    t.pencolor(color)
    t.begin_fill()
    for dx, dy in [(0, h), (bar_w, 0), (0, -h), (-bar_w, 0)]:
        t.forward(math.sqrt(dx**2 + dy**2) if dx*dy == 0 else 0)
        t.goto(t.xcor() + dx, t.ycor() + dy)
    t.end_fill()
    t.penup()
    # Label
    t.goto(x + bar_w//2, y - 20)
    t.pencolor("white")
    t.write(label, align="center", font=("Arial", 7, "normal"))
    # Count
    t.goto(x + bar_w//2, y + h + 5)
    t.write(str(count), align="center", font=("Arial", 10, "bold"))

# Draw axes
t.goto(start_x - 10, -150)
t.pendown()
t.pencolor("#888888")
t.goto(start_x + total_w + 10, -150)
t.penup()

print(f"  Grade counts: A={grade_counts[0]}, B={grade_counts[1]}, C={grade_counts[2]}, D={grade_counts[3]}, F={grade_counts[4]}")
print()
print("=" * 60)
print("  Statistics Suite complete!")
print("  Datasets analyzed: Heights, Scores, Returns, Trend")
print("=" * 60)

turtle.done()
