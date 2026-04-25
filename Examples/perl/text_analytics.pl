#!/usr/bin/perl
# ============================================================
# TEXT ANALYTICS TOOLKIT — Perl Language Showcase
# Readability, frequency, n-grams, concordance, statistics
# Time Warp Studio — Perl Language Demo
# ============================================================

use strict;
use warnings;

# ============================================================
# SAMPLE TEXTS
# ============================================================

my $text_alice = <<'END_TEXT';
Alice was beginning to get very tired of sitting by her sister on the bank,
and of having nothing to do: once or twice she had peeped into the book her
sister was reading, but it had no pictures or conversations in it, and what
is the use of a book, thought Alice, without pictures or conversations? So she
was considering in her own mind, as well as she could, for the hot day made her
feel very sleepy and stupid, whether the pleasure of making a daisy-chain would
be worth the trouble of getting up and picking the daisies, when suddenly a White
Rabbit with pink eyes ran close by her. There was nothing so very remarkable in
that; nor did Alice think it so very much out of the way to hear the Rabbit say
to itself, such a thought, such an odd thing. It is so very curious, she thought,
that she was not in the least surprised. But when the Rabbit actually took a watch
out of its waistcoat-pocket, and looked at it, and then hurried on, Alice started
to her feet, for it flashed across her mind that she had never before seen a rabbit
with either a waistcoat-pocket, or a watch to take out of it. And burning with
curiosity, she ran across the field after it, and was just in time to see it pop
down a large rabbit-hole under the hedge. In another moment down went Alice after
it, never once considering how in the world she was to get out again.
END_TEXT

my $text_science = <<'END_TEXT';
The scientific method is a systematic approach to understanding the natural world.
It begins with observation of a phenomenon, followed by formulation of a hypothesis
that could explain the observation. The hypothesis must be testable and falsifiable.
Scientists then design experiments to test the hypothesis, collect data systematically,
and analyze the results. If the data supports the hypothesis, it may be accepted as
a theory. A scientific theory is not a guess — it is a well-substantiated explanation
supported by extensive evidence. Theories can be refined or replaced as new evidence
emerges. This self-correcting nature of science is one of its greatest strengths.
Famous examples include Newton's theory of gravity, Darwin's theory of evolution,
Einstein's theory of relativity, and quantum mechanics. Each built on previous work
and added new predictive power. The history of science is a story of human curiosity,
careful observation, creative thinking, and rigorous testing. Science has transformed
our understanding of the universe, from the smallest particles to the largest structures.
Modern technology — computers, medicine, space exploration — all rest on scientific
foundations built through centuries of careful inquiry and experiment.
END_TEXT

# ============================================================
# UTILITY FUNCTIONS
# ============================================================

sub tokenize {
    my ($text) = @_;
    my $lower = lc($text);
    my @words = ($lower =~ /\b[a-z]+\b/g);
    return @words;
}

sub get_sentences {
    my ($text) = @_;
    my @sentences = split /[.!?]+/, $text;
    @sentences = grep { /\S/ } @sentences;
    return @sentences;
}

sub count_syllables {
    my ($word) = @_;
    $word = lc($word);
    $word =~ s/[^a-z]//g;
    return 1 if length($word) <= 3;

    my $count = 0;
    $word =~ s/e$//;           # drop trailing e
    my @vowels = ($word =~ /[aeiou]+/g);
    $count = scalar @vowels;
    $count = 1 if $count < 1;
    return $count;
}

sub word_frequency {
    my (@words) = @_;
    my %freq;
    for my $w (@words) {
        $freq{$w}++;
    }
    return %freq;
}

sub mean {
    my (@nums) = @_;
    return 0 unless @nums;
    my $sum = 0;
    $sum += $_ for @nums;
    return $sum / scalar(@nums);
}

sub median {
    my @sorted = sort { $a <=> $b } @_;
    my $n = scalar @sorted;
    if ($n % 2 == 0) {
        return ($sorted[$n/2 - 1] + $sorted[$n/2]) / 2;
    } else {
        return $sorted[$n/2];
    }
}

sub stddev {
    my (@nums) = @_;
    my $m = mean(@nums);
    my $var = mean(map { ($_ - $m) ** 2 } @nums);
    return sqrt($var);
}

# ============================================================
# FLESCH-KINCAID READABILITY
# ============================================================

sub flesch_kincaid {
    my ($text) = @_;
    my @words = tokenize($text);
    my @sentences = get_sentences($text);

    my $num_words = scalar @words;
    my $num_sentences = scalar(@sentences) || 1;
    my $num_syllables = 0;

    $num_syllables += count_syllables($_) for @words;

    my $avg_sentence_len = $num_words / $num_sentences;
    my $avg_syllables    = $num_syllables / ($num_words || 1);

    # Flesch Reading Ease
    my $ease = 206.835
             - 1.015  * $avg_sentence_len
             - 84.6   * $avg_syllables;

    # Flesch-Kincaid Grade Level
    my $grade = 0.39  * $avg_sentence_len
              + 11.8  * $avg_syllables
              - 15.59;

    return {
        words         => $num_words,
        sentences     => $num_sentences,
        syllables     => $num_syllables,
        avg_sent_len  => $avg_sentence_len,
        avg_syllables => $avg_syllables,
        ease          => $ease,
        grade         => $grade,
    };
}

sub grade_description {
    my ($grade) = @_;
    return "5th grade"   if $grade <= 5;
    return "6th grade"   if $grade <= 6;
    return "7th grade"   if $grade <= 7;
    return "8th grade"   if $grade <= 8;
    return "9th grade"   if $grade <= 9;
    return "10th grade"  if $grade <= 10;
    return "11th grade"  if $grade <= 11;
    return "12th grade"  if $grade <= 12;
    return "College"     if $grade <= 16;
    return "Graduate";
}

# ============================================================
# N-GRAM ANALYSIS
# ============================================================

sub get_ngrams {
    my ($n, @words) = @_;
    my %ngrams;
    for my $i (0 .. $#words - $n + 1) {
        my $gram = join(' ', @words[$i .. $i + $n - 1]);
        $ngrams{$gram}++;
    }
    return %ngrams;
}

sub top_ngrams {
    my ($n_gram, $limit, @words) = @_;
    my %ngrams = get_ngrams($n_gram, @words);
    my @sorted = sort { $ngrams{$b} <=> $ngrams{$a} } keys %ngrams;
    return map { [$_, $ngrams{$_}] } @sorted[0 .. ($limit-1 < $#sorted ? $limit-1 : $#sorted)];
}

# ============================================================
# CONCORDANCE (KWIC)
# ============================================================

sub concordance {
    my ($text, $word, $context_len) = @_;
    my @results;
    my $pattern = qr/(?i)\b\Q$word\E\b/;

    my @sentences = split /[.!?\n]+/, $text;
    for my $sent (@sentences) {
        if ($sent =~ $pattern) {
            my $trimmed = $sent;
            $trimmed =~ s/^\s+//;
            $trimmed =~ s/\s+$//;
            # Highlight the keyword
            $trimmed =~ s/($pattern)/[$1]/g;
            push @results, $trimmed if length($trimmed) > 2;
        }
    }
    return @results;
}

# ============================================================
# SENTENCE STATISTICS
# ============================================================

sub sentence_stats {
    my ($text) = @_;
    my @sentences = get_sentences($text);
    my @lengths;
    for my $sent (@sentences) {
        my @words = ($sent =~ /\b[a-z]+\b/gi);
        push @lengths, scalar @words if @words;
    }

    return () unless @lengths;

    my $mean_len   = mean(@lengths);
    my $median_len = median(@lengths);
    my $stddev_len = stddev(@lengths);
    my $max_len    = (sort { $b <=> $a } @lengths)[0];
    my $min_len    = (sort { $a <=> $b } @lengths)[0];

    # Distribution buckets
    my %dist;
    for my $l (@lengths) {
        my $bucket = int($l / 5) * 5;
        $dist{$bucket}++;
    }

    return {
        lengths    => \@lengths,
        count      => scalar @lengths,
        mean       => $mean_len,
        median     => $median_len,
        stddev     => $stddev_len,
        max        => $max_len,
        min        => $min_len,
        dist       => \%dist,
    };
}

# ============================================================
# DISPLAY HELPERS
# ============================================================

sub print_bar {
    my ($label, $count, $max_count, $bar_width) = @_;
    my $filled = $max_count > 0
        ? int($count / $max_count * $bar_width)
        : 0;
    my $bar = '#' x $filled;
    printf("  %-20s |%-${bar_width}s| %3d\n", $label, $bar, $count);
}

sub print_section {
    my ($title) = @_;
    print "\n$title\n";
    print '-' x 60, "\n";
}

# ============================================================
# MAIN PROGRAM
# ============================================================

print "=" x 60, "\n";
print "  TEXT ANALYTICS TOOLKIT — Perl Showcase\n";
print "  Readability * Frequency * N-grams * Concordance\n";
print "=" x 60, "\n";

for my $entry (
    ["Alice in Wonderland (excerpt)", $text_alice],
    ["Science Passage",               $text_science],
) {
    my ($name, $text) = @$entry;
    my @words    = tokenize($text);
    my %freq     = word_frequency(@words);

    print "\n";
    print "=" x 60, "\n";
    print "  TEXT: $name\n";
    print "=" x 60, "\n";

    # ---- Readability ----
    print_section("SECTION 1: FLESCH-KINCAID READABILITY");
    my $fk = flesch_kincaid($text);
    printf("  Words:            %d\n", $fk->{words});
    printf("  Sentences:        %d\n", $fk->{sentences});
    printf("  Syllables:        %d\n", $fk->{syllables});
    printf("  Avg sentence len: %.1f words\n", $fk->{avg_sent_len});
    printf("  Avg syllables:    %.2f per word\n", $fk->{avg_syllables});
    printf("  Reading Ease:     %.1f (0=hard, 100=easy)\n", $fk->{ease});
    printf("  Grade Level:      %.1f (%s)\n", $fk->{grade},
                                              grade_description($fk->{grade}));

    # ---- Word Frequency ----
    print_section("SECTION 2: TOP 20 WORDS BY FREQUENCY");
    my @stopwords = qw(the a an and or of to in it is was by she he with
                       not so as be from but for had at her its this on);
    my %stop; @stop{@stopwords} = (1) x @stopwords;
    my @content_words = grep { !$stop{$_} } keys %freq;
    my @top20 = (sort { $freq{$b} <=> $freq{$a} } @content_words)[0..19];
    my $max_freq = $freq{$top20[0]} || 1;

    for my $w (@top20) {
        print_bar($w, $freq{$w}, $max_freq, 30);
    }

    # ---- Vocabulary richness ----
    print_section("SECTION 3: VOCABULARY STATISTICS");
    my $total    = scalar @words;
    my $unique   = scalar keys %freq;
    my $ttr      = $unique / ($total || 1);   # type-token ratio
    my $hapax    = grep { $freq{$_} == 1 } keys %freq;
    my $avg_word_len = mean(map { length($_) } @words);

    printf("  Total words:      %d\n", $total);
    printf("  Unique words:     %d\n", $unique);
    printf("  Type-token ratio: %.3f (1.0 = all unique)\n", $ttr);
    printf("  Hapax legomena:   %d (words appearing once)\n", $hapax);
    printf("  Avg word length:  %.2f chars\n", $avg_word_len);

    # Letter frequency
    my %letters;
    for my $w (@words) {
        $letters{$_}++ for split //, $w;
    }
    my @top_letters = (sort { $letters{$b} <=> $letters{$a} } keys %letters)[0..9];
    print "  Top 10 letters:   ";
    print join(', ', map { "$_ ($letters{$_})" } @top_letters), "\n";

    # ---- N-gram analysis ----
    print_section("SECTION 4: N-GRAM ANALYSIS");

    print "  Top 10 bigrams:\n";
    my @bigrams = top_ngrams(2, 10, @words);
    for my $bg (@bigrams) {
        printf("    %-25s  %d\n", $bg->[0], $bg->[1]);
    }

    print "\n  Top 10 trigrams:\n";
    my @trigrams = top_ngrams(3, 10, @words);
    for my $tg (@trigrams) {
        printf("    %-35s  %d\n", $tg->[0], $tg->[1]);
    }

    # ---- Sentence statistics ----
    print_section("SECTION 5: SENTENCE LENGTH STATISTICS");
    my $ss = sentence_stats($text);
    if ($ss) {
        printf("  Sentence count:   %d\n", $ss->{count});
        printf("  Mean length:      %.1f words\n", $ss->{mean});
        printf("  Median length:    %.1f words\n", $ss->{median});
        printf("  Std deviation:    %.1f words\n", $ss->{stddev});
        printf("  Longest:          %d words\n", $ss->{max});
        printf("  Shortest:         %d words\n", $ss->{min});

        print "\n  Sentence length distribution (bucket = 5 words):\n";
        my $max_bucket = (sort { $ss->{dist}{$b} <=> $ss->{dist}{$a} } keys %{$ss->{dist}})[0];
        my $max_d = $ss->{dist}{$max_bucket} || 1;
        for my $bucket (sort { $a <=> $b } keys %{$ss->{dist}}) {
            print_bar("$bucket-" . ($bucket+4) . " words",
                      $ss->{dist}{$bucket}, $max_d, 25);
        }
    }

    # ---- Concordance ----
    print_section("SECTION 6: CONCORDANCE (KWIC)");
    my @keywords = $name =~ /Alice/ ? qw(alice rabbit thought) : qw(theory science evidence);
    for my $kw (@keywords) {
        printf("  Keyword: '%s'\n", $kw);
        my @ctx = concordance($text, $kw, 5);
        if (@ctx) {
            for my $c (@ctx) {
                my $trimmed = $c;
                $trimmed =~ s/^\s+|\s+$//g;
                printf("    ...%s...\n", substr($trimmed, 0, 70));
            }
        } else {
            print "    (not found)\n";
        }
        print "\n";
    }
}

print "=" x 60, "\n";
print "  Text Analytics complete!\n";
print "  Flesch-Kincaid * Frequency * N-grams * Concordance\n";
print "=" x 60, "\n";
