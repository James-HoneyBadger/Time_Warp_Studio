#!/usr/bin/env perl
# ============================================================
# DNA SEQUENCE ANALYZER — Perl Language Showcase
# Codon Translation * ORF Finder * GC Content * Statistics
# Reverse Complement * Pattern Search * Protein Analysis
# Time Warp Studio — Perl Language Demo
# ============================================================

use strict;
use warnings;

# ===== CODON TABLE =====

my %codon_table = (
    'TTT' => 'Phe', 'TTC' => 'Phe', 'TTA' => 'Leu', 'TTG' => 'Leu',
    'CTT' => 'Leu', 'CTC' => 'Leu', 'CTA' => 'Leu', 'CTG' => 'Leu',
    'ATT' => 'Ile', 'ATC' => 'Ile', 'ATA' => 'Ile', 'ATG' => 'Met',
    'GTT' => 'Val', 'GTC' => 'Val', 'GTA' => 'Val', 'GTG' => 'Val',
    'TCT' => 'Ser', 'TCC' => 'Ser', 'TCA' => 'Ser', 'TCG' => 'Ser',
    'CCT' => 'Pro', 'CCC' => 'Pro', 'CCA' => 'Pro', 'CCG' => 'Pro',
    'ACT' => 'Thr', 'ACC' => 'Thr', 'ACA' => 'Thr', 'ACG' => 'Thr',
    'GCT' => 'Ala', 'GCC' => 'Ala', 'GCA' => 'Ala', 'GCG' => 'Ala',
    'TAT' => 'Tyr', 'TAC' => 'Tyr', 'TAA' => 'STOP', 'TAG' => 'STOP',
    'CAT' => 'His', 'CAC' => 'His', 'CAA' => 'Gln', 'CAG' => 'Gln',
    'AAT' => 'Asn', 'AAC' => 'Asn', 'AAA' => 'Lys', 'AAG' => 'Lys',
    'GAT' => 'Asp', 'GAC' => 'Asp', 'GAA' => 'Glu', 'GAG' => 'Glu',
    'TGT' => 'Cys', 'TGC' => 'Cys', 'TGA' => 'STOP', 'TGG' => 'Trp',
    'CGT' => 'Arg', 'CGC' => 'Arg', 'CGA' => 'Arg', 'CGG' => 'Arg',
    'AGT' => 'Ser', 'AGC' => 'Ser', 'AGA' => 'Arg', 'AGG' => 'Arg',
    'GGT' => 'Gly', 'GGC' => 'Gly', 'GGA' => 'Gly', 'GGG' => 'Gly',
    'TGG' => 'Trp',
);

# 1-letter amino acid codes
my %aa_1letter = (
    'Ala' => 'A', 'Arg' => 'R', 'Asn' => 'N', 'Asp' => 'D',
    'Cys' => 'C', 'Gln' => 'Q', 'Glu' => 'E', 'Gly' => 'G',
    'His' => 'H', 'Ile' => 'I', 'Leu' => 'L', 'Lys' => 'K',
    'Met' => 'M', 'Phe' => 'F', 'Pro' => 'P', 'Ser' => 'S',
    'Thr' => 'T', 'Trp' => 'W', 'Tyr' => 'Y', 'Val' => 'V',
    'STOP' => '*',
);

# ===== SUBROUTINES =====

sub gc_content {
    my ($seq) = @_;
    my $len = length($seq);
    return 0 if $len == 0;
    my $gc = () = $seq =~ /[GC]/gi;
    return $gc * 100.0 / $len;
}

sub reverse_complement {
    my ($seq) = @_;
    my $rev = reverse $seq;
    $rev =~ tr/ACGTacgt/TGCAtgca/;
    return $rev;
}

sub translate_sequence {
    my ($dna, $frame) = @_;
    $frame //= 0;
    my @proteins;
    for (my $i = $frame; $i + 2 < length($dna); $i += 3) {
        my $codon = uc(substr($dna, $i, 3));
        my $aa3 = $codon_table{$codon} // '???';
        my $aa1 = $aa_1letter{$aa3}    // '?';
        push @proteins, $aa1;
        last if $aa1 eq '*';
    }
    return join('', @proteins);
}

sub find_orfs {
    my ($seq) = @_;
    my @orfs;
    $seq = uc($seq);

    for my $frame (0, 1, 2) {
        my $in_orf = 0;
        my $start_pos = 0;
        my @current_aas;

        for (my $i = $frame; $i + 2 < length($seq); $i += 3) {
            my $codon = substr($seq, $i, 3);
            my $aa3 = $codon_table{$codon} // '???';
            my $aa1 = $aa_1letter{$aa3}    // '?';

            if ($aa1 eq 'M' && !$in_orf) {
                $in_orf = 1;
                $start_pos = $i;
                @current_aas = ('M');
            } elsif ($in_orf) {
                if ($aa1 eq '*') {
                    push @orfs, {
                        frame   => $frame + 1,
                        start   => $start_pos,
                        end     => $i + 3,
                        length  => scalar(@current_aas),
                        protein => join('', @current_aas),
                    };
                    $in_orf = 0;
                    @current_aas = ();
                } else {
                    push @current_aas, $aa1;
                }
            }
        }
    }

    # Sort by ORF length descending
    return sort { $b->{length} <=> $a->{length} } @orfs;
}

sub nucleotide_freq {
    my ($seq) = @_;
    my %freq = (A => 0, T => 0, G => 0, C => 0);
    $freq{uc($_)}++ for split(//, $seq);
    return %freq;
}

sub find_motifs {
    my ($seq, $motif) = @_;
    my @positions;
    my $start = 0;
    while ((my $pos = index(uc($seq), uc($motif), $start)) != -1) {
        push @positions, $pos + 1;  # 1-based
        $start = $pos + 1;
    }
    return @positions;
}

sub kmer_analysis {
    my ($seq, $k) = @_;
    my %kmers;
    for (my $i = 0; $i <= length($seq) - $k; $i++) {
        $kmers{substr(uc($seq), $i, $k)}++;
    }
    return %kmers;
}

sub hamming_distance {
    my ($s1, $s2) = @_;
    return undef if length($s1) != length($s2);
    my $dist = 0;
    for my $i (0..length($s1)-1) {
        $dist++ if substr($s1,$i,1) ne substr($s2,$i,1);
    }
    return $dist;
}

sub format_sequence {
    my ($seq, $width) = @_;
    $width //= 60;
    my @lines;
    for (my $i = 0; $i < length($seq); $i += $width) {
        push @lines, sprintf("%4d  %s", $i+1, substr($seq, $i, $width));
    }
    return join("\n", @lines);
}

# ===== MAIN PROGRAM =====

print "=" x 60 . "\n";
print "  DNA SEQUENCE ANALYZER — Perl Showcase\n";
print "  Regex | Hashes | Arrays | Subroutines | Pattern Matching\n";
print "=" x 60 . "\n\n";

# ---- Section 1: Sequence Setup ----
my $dna = "ATGAAAGCAATTTTCGTACTGAAAGGTTTTGTTGGTTTTTTGTAAATGA" .
          "CTTGGGCAAATCTATGACCAAAGGTCAAGATAAGAATGTTTATGATGGC" .
          "CGTGAAATCGGTAAAGAAATTGCATTTGAAGTGGATGACTCTTTTGCG";

print "[ 1 ] SEQUENCE OVERVIEW\n\n";
print "  Length: " . length($dna) . " bp\n";
print "  Formatted:\n";
print format_sequence($dna) . "\n\n";

# ---- Section 2: Nucleotide Composition ----
print "[ 2 ] NUCLEOTIDE COMPOSITION\n\n";
my %freq = nucleotide_freq($dna);
my $total = length($dna);
for my $nuc (qw(A T G C)) {
    my $pct = sprintf("%.1f", $freq{$nuc} * 100 / $total);
    my $bar = "█" x int($freq{$nuc} * 40 / $total);
    printf("  %s: %3d (%5s%%) %s\n", $nuc, $freq{$nuc}, $pct, $bar);
}
printf("\n  GC Content: %.1f%%\n", gc_content($dna));
my $purine  = ($freq{A} + $freq{G}) * 100 / $total;
my $pyrim   = ($freq{T} + $freq{C}) * 100 / $total;
printf("  Purines (A+G): %.1f%%   Pyrimidines (T+C): %.1f%%\n\n", $purine, $pyrim);

# ---- Section 3: Reverse Complement ----
print "[ 3 ] REVERSE COMPLEMENT\n\n";
my $rc = reverse_complement($dna);
print "  Original (first 40): " . substr($dna, 0, 40) . "...\n";
print "  RevComp  (first 40): " . substr($rc,  0, 40) . "...\n";
printf("  GC content: %.1f%% (should match original)\n\n", gc_content($rc));

# ---- Section 4: Translation ----
print "[ 4 ] TRANSLATION (3 READING FRAMES)\n\n";
for my $frame (0, 1, 2) {
    my $protein = translate_sequence($dna, $frame);
    printf("  Frame +%d: %s\n", $frame+1, $protein);
}
print "\n";

# ---- Section 5: ORF Finder ----
print "[ 5 ] OPEN READING FRAME (ORF) FINDER\n\n";
my @orfs = find_orfs($dna);
if (@orfs) {
    printf("  Found %d ORF(s):\n", scalar @orfs);
    for my $orf (@orfs) {
        printf("    Frame +%d  pos %3d-%3d  length %2d aa  %s\n",
               $orf->{frame}, $orf->{start}+1, $orf->{end},
               $orf->{length}, $orf->{protein});
    }
} else {
    print "  No complete ORFs found in this fragment.\n";
}
print "\n";

# ---- Section 6: Motif Search ----
print "[ 6 ] MOTIF SEARCH\n\n";
my @motifs = ("ATG", "TAA", "TGA", "GCAAAT", "TTTG");
for my $motif (@motifs) {
    my @positions = find_motifs($dna, $motif);
    if (@positions) {
        printf("  %-10s found at positions: %s\n", $motif, join(", ", @positions));
    } else {
        printf("  %-10s not found\n", $motif);
    }
}
print "\n";

# ---- Section 7: k-mer Analysis ----
print "[ 7 ] 3-MER FREQUENCY ANALYSIS\n\n";
my %kmers = kmer_analysis($dna, 3);
my @sorted_kmers = sort { $kmers{$b} <=> $kmers{$a} || $a cmp $b } keys %kmers;
print "  Top 10 most frequent 3-mers:\n";
for my $km (@sorted_kmers[0..9]) {
    my $bar = "█" x $kmers{$km};
    printf("    %s: %2d  %s\n", $km, $kmers{$km}, $bar);
}
print "\n";

# ---- Section 8: Multiple Sequence Comparison ----
print "[ 8 ] SEQUENCE COMPARISON (HAMMING DISTANCE)\n\n";
my @sequences = (
    "ATGCGATACGCTTGATGCTAGCAT",
    "ATGCGATACGCTTGATGCTAGCAT",
    "ATGCAATACGCTTGATGCTAGCAT",
    "ATGCGATCCGCTTGATGCTCGCAT",
    "GCGCGATACGCTTGATGCTAGCAT",
);
my $ref_seq = $sequences[0];
print "  Reference: $ref_seq\n\n";
for my $i (1..$#sequences) {
    my $d = hamming_distance($ref_seq, $sequences[$i]);
    my $pct = sprintf("%.1f", $d * 100 / length($ref_seq));
    my $bar = "█" x (length($ref_seq) - $d);
    my $x   = "░" x $d;
    printf("  Seq %d:    %s  dist=%d (%.1f%% diff)  identity=[%s%s]\n",
           $i+1, $sequences[$i], $d, $pct, $bar, $x);
}
print "\n";

# ---- Section 9: Regex Feature Showcase ----
print "[ 9 ] PERL REGEX SHOWCASE\n\n";

# Find all start codons
my @starts;
while ($dna =~ /ATG/g) { push @starts, pos($dna) - 2; }
print "  All ATG positions (1-based): " . join(", ", @starts) . "\n";

# Find repeated elements
my @repeats;
while ($dna =~ /(.{3,})\1+/g) {
    push @repeats, $1;
}
if (@repeats) {
    print "  Repeated motifs: " . join(", ", @repeats) . "\n";
} else {
    print "  No direct tandem repeats found\n";
}

# Substitution: introduce point mutation (T→C) at position 10
my $mutant = $dna;
substr($mutant, 9, 1) = 'C';
my $mutations = () = ($dna ^ $mutant) =~ /[^\x00]/g;
printf("  Point mutation at pos 10: %s\n", substr($mutant, 5, 20) . "...");
printf("  Hamming distance from original: %d\n", hamming_distance($dna, $mutant));

# Composition via tr///
my $a_count = ($dna =~ tr/A//);
my $t_count = ($dna =~ tr/T//);
printf("\n  A-T base pairs possible: %d × %d → %d\n",
       $a_count, $t_count, $a_count < $t_count ? $a_count : $t_count);

print "\n";

# ---- Section 10: Statistics ----
print "[ 10 ] PROTEIN PROPERTY STATISTICS\n\n";

# Amino acid molecular weights (approximate Da)
my %aa_weight = (
    A => 89,  R => 174, N => 132, D => 133, C => 121,
    Q => 146, E => 147, G => 75,  H => 155, I => 131,
    L => 131, K => 146, M => 149, F => 165, P => 115,
    S => 105, T => 119, W => 204, Y => 181, V => 117,
);

my $protein_seq = "MKAIFVLKGFVGFLCKLSWQNMTKGQDKNVYDGAEIGKEIAFEVDDSFAA";
print "  Protein: $protein_seq\n";
print "  Length: " . length($protein_seq) . " aa\n";

my $mol_weight = 0;
my %aa_freq;
for my $aa (split //, $protein_seq) {
    next unless exists $aa_weight{$aa};
    $mol_weight += $aa_weight{$aa};
    $aa_freq{$aa}++;
}
$mol_weight -= (length($protein_seq) - 1) * 18; # water loss in peptide bonds

printf("  Molecular weight: ~%d Da\n", $mol_weight);

my @sorted_aa = sort { $aa_freq{$b} <=> $aa_freq{$a} } keys %aa_freq;
print "  Amino acid composition:\n";
for my $aa (@sorted_aa) {
    my $pct = sprintf("%.1f", $aa_freq{$aa} * 100 / length($protein_seq));
    my $bar = "█" x $aa_freq{$aa};
    printf("    %s: %2d (%5s%%) %s\n", $aa, $aa_freq{$aa}, $pct, $bar);
}

print "\n" . "=" x 60 . "\n";
print "  DNA Analyzer Complete!\n";
print "  Regex | Hashes | Arrays | References | Sorting | Substr\n";
print "=" x 60 . "\n";
