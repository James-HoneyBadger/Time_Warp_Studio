#!/usr/bin/env python3
"""
text_analyzer.py — Comprehensive Text Analysis Toolkit
Analyzes text for readability, vocabulary, sentence structure,
word frequency, and produces an ASCII word-cloud style display.
"""

import re
import math
import string
from collections import Counter
from typing import List, Dict, Tuple


# ──────────────────────────────────────────────────────────────
#  TEXT UTILS
# ──────────────────────────────────────────────────────────────

STOP_WORDS = {
    'the','a','an','and','or','but','in','on','at','to','for',
    'of','with','by','from','up','about','into','through','during',
    'is','are','was','were','be','been','being','have','has','had',
    'do','does','did','will','would','shall','should','may','might',
    'must','can','could','that','this','these','those','it','its',
    'i','you','he','she','we','they','me','him','her','us','them',
    'my','your','his','our','their','what','which','who','how','when',
    'where','if','then','than','so','as','not','no','nor'
}

def tokenize(text: str) -> List[str]:
    """Split text into lowercase word tokens, stripping punctuation."""
    return re.findall(r"[a-z']+", text.lower())

def sentences(text: str) -> List[str]:
    """Split text into sentences."""
    return [s.strip() for s in re.split(r'[.!?]+', text) if s.strip()]

def syllable_count(word: str) -> int:
    """Estimate syllables in a word using vowel-group heuristic."""
    word = word.lower().rstrip('e')
    count = len(re.findall(r'[aeiou]+', word))
    return max(1, count)

def count_syllables_in_text(text: str) -> int:
    return sum(syllable_count(w) for w in tokenize(text))


# ──────────────────────────────────────────────────────────────
#  READABILITY SCORES
# ──────────────────────────────────────────────────────────────

def flesch_reading_ease(text: str) -> float:
    """
    Flesch Reading Ease score (0-100).
    Higher = easier to read. ~60-70 is plain English.
    FRE = 206.835 - 1.015*(words/sentences) - 84.6*(syllables/words)
    """
    words     = tokenize(text)
    sents     = sentences(text)
    sylls     = count_syllables_in_text(text)
    nw, ns    = len(words), max(1, len(sents))
    asl       = nw / ns                   # avg sentence length
    asw       = sylls / max(1, nw)        # avg syllables per word
    return round(206.835 - 1.015 * asl - 84.6 * asw, 1)

def flesch_kincaid_grade(text: str) -> float:
    """
    Flesch-Kincaid Grade Level.
    FK = 0.39*(words/sentences) + 11.8*(syllables/words) - 15.59
    """
    words  = tokenize(text)
    sents  = sentences(text)
    sylls  = count_syllables_in_text(text)
    nw, ns = len(words), max(1, len(sents))
    return round(0.39 * (nw / ns) + 11.8 * (sylls / max(1, nw)) - 15.59, 1)

def gunning_fog(text: str) -> float:
    """
    Gunning Fog Index.
    GF = 0.4 * ((words/sentences) + 100*(complex_words/words))
    Complex: 3+ syllables
    """
    words    = tokenize(text)
    sents    = sentences(text)
    nw       = len(words)
    ns       = max(1, len(sents))
    complex_ = sum(1 for w in words if syllable_count(w) >= 3)
    return round(0.4 * (nw / ns + 100 * complex_ / max(1, nw)), 1)

def coleman_liau(text: str) -> float:
    """
    Coleman-Liau Index.
    Counts characters per word / sentence.
    CLI = 0.0588*L - 0.296*S - 15.8
    L = avg letters per 100 words; S = avg sentences per 100 words
    """
    words = tokenize(text)
    sents = sentences(text)
    nw    = max(1, len(words))
    chars = sum(len(w) for w in words)
    L     = (chars / nw) * 100
    S     = (len(sents) / nw) * 100
    return round(0.0588 * L - 0.296 * S - 15.8, 1)


def grade_to_label(grade: float) -> str:
    if grade < 6:   return "Elementary"
    if grade < 9:   return "Middle School"
    if grade < 13:  return "High School"
    if grade < 16:  return "College"
    return "Graduate"


# ──────────────────────────────────────────────────────────────
#  VOCABULARY & FREQUENCY ANALYSIS
# ──────────────────────────────────────────────────────────────

def word_frequency(text: str,
                   top_n: int = 30,
                   exclude_stopwords: bool = True) -> List[Tuple[str, int]]:
    """Return top-N words by frequency."""
    words = tokenize(text)
    if exclude_stopwords:
        words = [w for w in words if w not in STOP_WORDS and len(w) > 1]
    return Counter(words).most_common(top_n)

def vocabulary_richness(text: str) -> Dict[str, float]:
    """
    Type-Token Ratio (TTR) and related vocabulary richness metrics.
    TTR = unique_words / total_words  (1.0 = every word is unique)
    """
    tokens  = tokenize(text)
    types   = set(tokens)
    n       = len(tokens)
    nt      = len(types)
    hapax   = sum(1 for w, c in Counter(tokens).items() if c == 1)
    return {
        'total_words':   n,
        'unique_words':  nt,
        'ttr':           round(nt / max(1, n), 4),
        'hapax_legomena': hapax,   # words appearing exactly once
        'hapax_ratio':   round(hapax / max(1, n), 4),
    }

def avg_word_length(text: str) -> float:
    words = tokenize(text)
    return round(sum(len(w) for w in words) / max(1, len(words)), 2)


# ──────────────────────────────────────────────────────────────
#  SENTENCE ANALYSIS
# ──────────────────────────────────────────────────────────────

def sentence_stats(text: str) -> Dict:
    sents  = sentences(text)
    words  = [tokenize(s) for s in sents]
    lens   = [len(w) for w in words if w]
    if not lens:
        return {}
    return {
        'sentence_count': len(lens),
        'avg_length':     round(sum(lens) / len(lens), 1),
        'min_length':     min(lens),
        'max_length':     max(lens),
        'longest_sentence': sents[lens.index(max(lens))][:80] + '...',
        'shortest_sentence': sents[lens.index(min(lens))],
    }


# ──────────────────────────────────────────────────────────────
#  ASCII WORD CLOUD
# ──────────────────────────────────────────────────────────────

def ascii_word_cloud(text: str, width: int = 72, rows: int = 8) -> str:
    """
    Create a simple ASCII word-cloud display.
    Larger words = higher frequency. Uses ALL CAPS for top tier.
    """
    freq = word_frequency(text, top_n=40)
    if not freq:
        return "(no significant words found)"

    max_count = freq[0][1]
    lines     = []

    # Sort by word length for visual variety
    display = [(w, c) for w, c in freq[:35]]
    display.sort(key=lambda x: (-x[1], len(x[0])))

    # Build rows by filling left-to-right
    row_buf  = ""
    line_no  = 0
    for word, count in display:
        tier = count / max_count
        if tier > 0.7:
            token = word.upper()
        elif tier > 0.4:
            token = word.capitalize()
        else:
            token = word

        # Scale font with spaces proportional to rank
        spacing = ' ' * (2 if tier > 0.5 else 1)

        candidate = row_buf + spacing + token
        if len(candidate) > width:
            if row_buf.strip():
                lines.append(row_buf.center(width))
            row_buf = token
            line_no += 1
            if line_no >= rows:
                break
        else:
            row_buf = candidate

    if row_buf.strip():
        lines.append(row_buf.center(width))

    border = '┌' + '─' * width + '┐'
    bottom = '└' + '─' * width + '┘'
    body   = '\n'.join('│' + line.ljust(width) + '│' for line in lines)
    return border + '\n' + body + '\n' + bottom


# ──────────────────────────────────────────────────────────────
#  BIGRAM / COLLOCATION ANALYSIS
# ──────────────────────────────────────────────────────────────

def top_bigrams(text: str, top_n: int = 15) -> List[Tuple[str, int]]:
    """Find most common two-word phrases (bigrams)."""
    words  = [w for w in tokenize(text) if w not in STOP_WORDS and len(w) > 2]
    bigrams = [(words[i] + ' ' + words[i+1]) for i in range(len(words)-1)]
    return Counter(bigrams).most_common(top_n)


# ──────────────────────────────────────────────────────────────
#  FULL REPORT
# ──────────────────────────────────────────────────────────────

def analyze(text: str, title: str = "Text Analysis") -> None:
    """Run all analyses and print a formatted report."""
    sep  = '═' * 72
    sep2 = '─' * 72

    print(f"\n{sep}")
    print(f"  {title}")
    print(sep)

    # Basic stats
    words  = tokenize(text)
    sents  = sentences(text)
    chars  = len(text.replace(' ',''))
    paras  = [p.strip() for p in text.split('\n\n') if p.strip()]

    print(f"\n{'DOCUMENT STATISTICS':}")
    print(sep2)
    print(f"  Characters (no spaces): {chars:>10,}")
    print(f"  Total words:            {len(words):>10,}")
    print(f"  Unique words:           {len(set(words)):>10,}")
    print(f"  Sentences:              {len(sents):>10,}")
    print(f"  Paragraphs:             {len(paras):>10,}")
    print(f"  Avg word length:        {avg_word_length(text):>10.2f} chars")

    # Sentence stats
    ss = sentence_stats(text)
    if ss:
        print(f"\n{'SENTENCE ANALYSIS':}")
        print(sep2)
        print(f"  Avg sentence length:    {ss['avg_length']:>10.1f} words")
        print(f"  Shortest sentence:      {ss['min_length']:>10} words")
        print(f"  Longest sentence:       {ss['max_length']:>10} words")
        print(f"  Longest (excerpt):  {ss['longest_sentence']}")

    # Vocabulary richness
    vr = vocabulary_richness(text)
    print(f"\n{'VOCABULARY RICHNESS':}")
    print(sep2)
    print(f"  Type-Token Ratio:       {vr['ttr']:>10.4f}  (1.0 = all unique)")
    print(f"  Hapax Legomena:         {vr['hapax_legomena']:>10,}  (once-only words)")
    print(f"  Hapax Ratio:            {vr['hapax_ratio']:>10.4f}")

    # Readability
    fre  = flesch_reading_ease(text)
    fkg  = flesch_kincaid_grade(text)
    gf   = gunning_fog(text)
    cli  = coleman_liau(text)
    print(f"\n{'READABILITY SCORES':}")
    print(sep2)
    print(f"  Flesch Reading Ease:    {fre:>10.1f}  /100  (higher=easier)")
    print(f"  Flesch-Kincaid Grade:   {fkg:>10.1f}  → {grade_to_label(fkg)}")
    print(f"  Gunning Fog Index:      {gf:>10.1f}  → {grade_to_label(gf)}")
    print(f"  Coleman-Liau Index:     {cli:>10.1f}  → {grade_to_label(cli)}")

    avg_grade = (fkg + gf + cli) / 3
    print(f"\n  Consensus Grade Level:  {avg_grade:>10.1f}  → {grade_to_label(avg_grade)}")

    # Top words
    top = word_frequency(text, top_n=20)
    print(f"\n{'TOP 20 CONTENT WORDS':}")
    print(sep2)
    max_c = top[0][1] if top else 1
    for i, (word, count) in enumerate(top, 1):
        bar  = '█' * int(count / max_c * 30)
        print(f"  {i:>2}. {word:<20} {count:>4}  {bar}")

    # Top bigrams
    bigrams = top_bigrams(text, 10)
    if bigrams:
        print(f"\n{'TOP WORD PAIRS (BIGRAMS)':}")
        print(sep2)
        for phrase, count in bigrams:
            print(f"  {phrase:<30} {count:>4}×")

    # ASCII word cloud
    print(f"\n{'WORD CLOUD':}")
    print(sep2)
    print(ascii_word_cloud(text))
    print()


# ──────────────────────────────────────────────────────────────
#  DEMO
# ──────────────────────────────────────────────────────────────

SAMPLE_TEXT = """
The art of programming is the art of organizing complexity, of mastering
multitude and avoiding its bastard chaos as effectively as possible.
Programming languages are the tools we think with — they shape the way
we approach problems and craft solutions.

A good programming language should be concise, expressive, and powerful.
It should allow the programmer to express ideas clearly, without forcing
unnecessary verbosity or obscuring intent behind boilerplate syntax.

The most elegant programs are those that are easy to read, easy to modify,
and easy to understand. Simplicity is the ultimate sophistication. When you
can express a complex algorithm in a few clear lines, you have achieved
something truly beautiful. The best code reads almost like well-written prose.

Object-oriented programming offers one way of managing complexity by
encapsulating behavior and data into reusable components called objects.
Functional programming takes a different approach, treating computation as
the evaluation of mathematical functions while avoiding changing state and
mutable data. Both paradigms have their strengths and their appropriate uses.

Learning multiple programming languages deepens your understanding of
computation itself. Each language embodies different trade-offs and
philosophies, revealing new ways to think about problems. A polyglot
programmer approaches each challenge with a richer toolkit of conceptual
frameworks and implementation strategies.
"""

if __name__ == '__main__':
    analyze(SAMPLE_TEXT, title="Programming & Languages — Text Analysis")

    # Interactive mode
    print("\nEnter your own text to analyze (blank line + Enter to finish):")
    lines = []
    while True:
        line = input()
        if not line and lines:
            break
        lines.append(line)
    if lines:
        user_text = '\n'.join(lines)
        analyze(user_text, title="Your Text Analysis")
