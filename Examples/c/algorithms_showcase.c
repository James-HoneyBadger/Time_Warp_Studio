/* ============================================================
   ALGORITHMS SHOWCASE — The Art of Computation in C
   Sorting * Primes * Matrix Algebra * String Analysis
   Time Warp Studio — C Language Demo
   ============================================================ */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

/* ===== DATA STRUCTURES ===== */

struct Student {
    char name[32];
    int grade;
    float gpa;
};

/* ===== SORTING ALGORITHMS ===== */

void bubble_sort(int arr[], int n) {
    int i, j, tmp;
    for (i = 0; i < n - 1; i++) {
        for (j = 0; j < n - i - 1; j++) {
            if (arr[j] > arr[j+1]) {
                tmp = arr[j];
                arr[j] = arr[j+1];
                arr[j+1] = tmp;
            }
        }
    }
}

void selection_sort(int arr[], int n) {
    int i, j, min_idx, tmp;
    for (i = 0; i < n - 1; i++) {
        min_idx = i;
        for (j = i + 1; j < n; j++) {
            if (arr[j] < arr[min_idx]) min_idx = j;
        }
        tmp = arr[min_idx];
        arr[min_idx] = arr[i];
        arr[i] = tmp;
    }
}

void merge(int arr[], int l, int m, int r) {
    int i, j, k;
    int n1 = m - l + 1;
    int n2 = r - m;
    int left[64], right[64];
    for (i = 0; i < n1; i++) left[i] = arr[l + i];
    for (j = 0; j < n2; j++) right[j] = arr[m + 1 + j];
    i = 0; j = 0; k = l;
    while (i < n1 && j < n2) {
        if (left[i] <= right[j]) { arr[k] = left[i]; i++; }
        else                      { arr[k] = right[j]; j++; }
        k++;
    }
    while (i < n1) { arr[k] = left[i]; i++; k++; }
    while (j < n2) { arr[k] = right[j]; j++; k++; }
}

void merge_sort(int arr[], int l, int r) {
    if (l < r) {
        int m = (l + r) / 2;
        merge_sort(arr, l, m);
        merge_sort(arr, m + 1, r);
        merge(arr, l, m, r);
    }
}

/* ===== PRIME SIEVE ===== */

void sieve_of_eratosthenes(int limit) {
    int i, j, count;
    int is_prime[512];
    for (i = 0; i <= limit; i++) is_prime[i] = 1;
    is_prime[0] = 0;
    is_prime[1] = 0;
    for (i = 2; i * i <= limit; i++) {
        if (is_prime[i]) {
            for (j = i * i; j <= limit; j += i)
                is_prime[j] = 0;
        }
    }
    printf("  Primes up to %d:\n  ", limit);
    count = 0;
    for (i = 2; i <= limit; i++) {
        if (is_prime[i]) {
            printf("%d ", i);
            count++;
        }
    }
    printf("\n\n  Total: %d primes\n", count);
    printf("  Largest: ");
    for (i = limit; i >= 2; i--) {
        if (is_prime[i]) { printf("%d\n", i); break; }
    }
    /* Twin primes */
    printf("  Twin primes (differ by 2): ");
    count = 0;
    for (i = 2; i <= limit - 2; i++) {
        if (is_prime[i] && is_prime[i+2]) {
            printf("(%d,%d) ", i, i+2);
            count++;
        }
    }
    printf("\n  Found %d twin prime pairs\n", count);
}

/* ===== MATRIX OPERATIONS ===== */

void matrix_multiply(int a[3][3], int b[3][3], int c[3][3]) {
    int i, j, k;
    for (i = 0; i < 3; i++)
        for (j = 0; j < 3; j++) {
            c[i][j] = 0;
            for (k = 0; k < 3; k++)
                c[i][j] += a[i][k] * b[k][j];
        }
}

void print_matrix(int m[3][3], char *label) {
    int i, j;
    printf("  %s:\n", label);
    for (i = 0; i < 3; i++) {
        printf("  |");
        for (j = 0; j < 3; j++)
            printf(" %4d", m[i][j]);
        printf(" |\n");
    }
}

int determinant_3x3(int m[3][3]) {
    return m[0][0]*(m[1][1]*m[2][2] - m[1][2]*m[2][1])
         - m[0][1]*(m[1][0]*m[2][2] - m[1][2]*m[2][0])
         + m[0][2]*(m[1][0]*m[2][1] - m[1][1]*m[2][0]);
}

/* ===== STRING ANALYSIS ===== */

void analyse_string(char *s) {
    int i, len, vowels, consonants, spaces, digits, uppers, lowers;
    int freq[26];
    char c;
    len = strlen(s);
    vowels = consonants = spaces = digits = uppers = lowers = 0;
    for (i = 0; i < 26; i++) freq[i] = 0;
    for (i = 0; i < len; i++) {
        c = s[i];
        if (c >= 'A' && c <= 'Z') { uppers++; freq[c - 'A']++; }
        if (c >= 'a' && c <= 'z') { lowers++; freq[c - 'a']++; }
        if (c == ' ') spaces++;
        if (c >= '0' && c <= '9') digits++;
        c = tolower(c);
        if (c=='a'||c=='e'||c=='i'||c=='o'||c=='u') vowels++;
        else if (c>='a' && c<='z') consonants++;
    }
    printf("  Text: \"%s\"\n", s);
    printf("  Length: %d  Words: ~%d\n", len, spaces + 1);
    printf("  Uppercase: %d  Lowercase: %d  Digits: %d\n",
           uppers, lowers, digits);
    printf("  Vowels: %d  Consonants: %d\n", vowels, consonants);
    printf("  Letter frequency (top 5): ");
    /* Simple selection for top 5 */
    int j, max_idx;
    for (i = 0; i < 5; i++) {
        max_idx = 0;
        for (j = 1; j < 26; j++)
            if (freq[j] > freq[max_idx]) max_idx = j;
        if (freq[max_idx] > 0)
            printf("'%c'=%d ", 'a' + max_idx, freq[max_idx]);
        freq[max_idx] = -1;
    }
    printf("\n");
}

/* ===== FIBONACCI WITH MEMOISATION ===== */

long long fib_memo[50];

long long fib(int n) {
    if (n <= 1) return n;
    if (fib_memo[n] != 0) return fib_memo[n];
    fib_memo[n] = fib(n-1) + fib(n-2);
    return fib_memo[n];
}

/* ===== STATISTICAL FUNCTIONS ===== */

float mean(int arr[], int n) {
    int i, sum = 0;
    for (i = 0; i < n; i++) sum += arr[i];
    return (float)sum / n;
}

float std_dev(int arr[], int n) {
    float m = mean(arr, n);
    float sum = 0;
    int i;
    for (i = 0; i < n; i++) sum += (arr[i] - m) * (arr[i] - m);
    return sqrt(sum / n);
}

/* ===== MAIN PROGRAM ===== */

int main() {
    int i;
    printf("============================================================\n");
    printf("  ALGORITHMS SHOWCASE — C Language Masterclass\n");
    printf("  Sorting | Primes | Matrices | Strings | Statistics\n");
    printf("============================================================\n\n");

    /* ---- Section 1: Sorting ---- */
    printf("[ 1 ] SORTING ALGORITHMS COMPARISON\n");
    printf("      Sorting the same 20-element array three ways\n\n");

    int data[20] = {64, 34, 25, 12, 22, 11, 90, 55, 44, 77,
                    33, 88, 66, 99, 15, 42, 71, 8, 53, 37};
    int a[20], b[20], c[20];

    for (i = 0; i < 20; i++) a[i] = b[i] = c[i] = data[i];

    printf("  Original: ");
    for (i = 0; i < 20; i++) printf("%d ", data[i]);
    printf("\n\n");

    bubble_sort(a, 20);
    printf("  Bubble:   ");
    for (i = 0; i < 20; i++) printf("%d ", a[i]);
    printf("  (O(n²) comparisons)\n\n");

    selection_sort(b, 20);
    printf("  Selection:");
    for (i = 0; i < 20; i++) printf("%d ", b[i]);
    printf("  (O(n²), minimal swaps)\n\n");

    merge_sort(c, 0, 19);
    printf("  Merge:    ");
    for (i = 0; i < 20; i++) printf("%d ", c[i]);
    printf("  (O(n log n), divide & conquer)\n\n");

    printf("  Stats on sorted array:\n");
    printf("  Mean: %.2f   Std Dev: %.2f\n",
           mean(a, 20), std_dev(a, 20));
    printf("  Min: %d   Max: %d   Median: %.1f\n\n",
           a[0], a[19], (a[9] + a[10]) / 2.0);

    /* ---- Section 2: Primes ---- */
    printf("[ 2 ] SIEVE OF ERATOSTHENES\n");
    printf("      Efficiently finding ALL primes up to 150\n\n");
    sieve_of_eratosthenes(150);
    printf("\n");

    /* ---- Section 3: Fibonacci ---- */
    printf("[ 3 ] FIBONACCI WITH MEMOISATION\n");
    printf("      O(n) time using cached subproblems\n\n");
    for (i = 0; i < 50; i++) fib_memo[i] = 0;
    printf("  F(n)  for n=0..20:\n  ");
    for (i = 0; i <= 20; i++) printf("F(%d)=%lld  ", i, fib(i));
    printf("\n\n");
    printf("  Golden ratio approximation:\n");
    for (i = 2; i <= 12; i++)
        printf("  F(%d)/F(%d) = %.6f\n", i, i-1,
               (double)fib(i) / fib(i-1));
    printf("\n");

    /* ---- Section 4: Matrix Algebra ---- */
    printf("[ 4 ] MATRIX ALGEBRA\n");
    printf("      3x3 multiplication and determinant\n\n");

    int A[3][3] = {{1, 2, 3}, {4, 5, 6}, {7, 8, 9}};
    int B[3][3] = {{9, 8, 7}, {6, 5, 4}, {3, 2, 1}};
    int C[3][3];
    matrix_multiply(A, B, C);
    print_matrix(A, "Matrix A");
    printf("\n");
    print_matrix(B, "Matrix B");
    printf("\n");
    print_matrix(C, "A × B");
    printf("\n");
    printf("  det(A) = %d\n\n", determinant_3x3(A));
    printf("  Identity check: det(A) × det(B) = det(A×B)?\n");
    printf("  det(A)=%d  det(B)=%d  det(A×B)=%d\n\n",
           determinant_3x3(A), determinant_3x3(B), determinant_3x3(C));

    /* ---- Section 5: String Analysis ---- */
    printf("[ 5 ] STRING ANALYSIS ENGINE\n\n");
    analyse_string("The quick brown fox jumps over the lazy dog");
    printf("\n");
    analyse_string("To be or not to be that is the question");
    printf("\n");

    /* ---- Section 6: Number Theory ---- */
    printf("[ 6 ] NUMBER THEORY — GCD, LCM, Perfect Numbers\n\n");

    /* GCD using Euclidean algorithm */
    printf("  Euclidean GCD:\n");
    int pairs[5][2] = {{48, 18}, {100, 75}, {252, 105}, {1071, 462}, {999, 777}};
    for (i = 0; i < 5; i++) {
        int x = pairs[i][0], y = pairs[i][1], tmp;
        int orig_x = x, orig_y = y;
        while (y != 0) { tmp = y; y = x % y; x = tmp; }
        printf("  GCD(%d, %d) = %d   LCM = %d\n",
               orig_x, orig_y, x,
               orig_x / x * orig_y);
    }
    printf("\n");

    /* Perfect numbers */
    printf("  Perfect numbers <= 10000 (sum of divisors = number):\n  ");
    for (i = 2; i <= 10000; i++) {
        int sum = 1, j;
        for (j = 2; j * j <= i; j++) {
            if (i % j == 0) {
                sum += j;
                if (j != i / j) sum += i / j;
            }
        }
        if (sum == i) printf("%d ", i);
    }
    printf("\n  (6=1+2+3, 28=1+2+4+7+14, 496, 8128)\n\n");

    printf("============================================================\n");
    printf("  C Language Showcase Complete!\n");
    printf("  From O(n log n) sorts to number theory — C is timeless.\n");
    printf("============================================================\n");
    return 0;
}
