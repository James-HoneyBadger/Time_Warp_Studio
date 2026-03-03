/*
 * ╔══════════════════════════════════════════════════════════╗
 * ║     SORTING ALGORITHMS — Complete C Implementation       ║
 * ║  Bubble, Selection, Insertion, Shell, Merge, Quick,      ║
 * ║  Heap, Counting, Radix sort with comparison counts       ║
 * ║  and timing benchmarks. Genuinely useful reference.      ║
 * ╚══════════════════════════════════════════════════════════╝
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#define MAX_SIZE 1000

typedef long long ll;

/* ─── UTILITY ──────────────────────────────────────────────── */
static ll comparisons = 0;
static ll swaps       = 0;

void reset_counters(void) { comparisons = 0; swaps = 0; }

void swap(int *a, int *b) {
    int tmp = *a; *a = *b; *b = tmp;
    swaps++;
}

int cmp(int a, int b) {
    comparisons++;
    return a - b;
}

void copy_array(const int *src, int *dst, int n) {
    memcpy(dst, src, n * sizeof(int));
}

void print_array(const char *label, const int *a, int n) {
    printf("  %-20s: [", label);
    for (int i = 0; i < n; i++) {
        printf("%d%s", a[i], i < n-1 ? ", " : "");
    }
    printf("]\n");
}

void print_stats(const char *algo, double secs) {
    printf("  %-16s comparisons:%-8lld swaps:%-8lld time:%.3fms\n",
           algo, comparisons, swaps, secs * 1000.0);
}

/* ─── BUBBLE SORT ──────────────────────────────────────────── */
void bubble_sort(int *a, int n) {
    for (int i = 0; i < n-1; i++) {
        int swapped = 0;
        for (int j = 0; j < n-1-i; j++) {
            if (cmp(a[j], a[j+1]) > 0) {
                swap(&a[j], &a[j+1]);
                swapped = 1;
            }
        }
        if (!swapped) break;  /* early exit */
    }
}

/* ─── SELECTION SORT ────────────────────────────────────────── */
void selection_sort(int *a, int n) {
    for (int i = 0; i < n-1; i++) {
        int min_idx = i;
        for (int j = i+1; j < n; j++)
            if (cmp(a[j], a[min_idx]) < 0) min_idx = j;
        if (min_idx != i) swap(&a[i], &a[min_idx]);
    }
}

/* ─── INSERTION SORT ────────────────────────────────────────── */
void insertion_sort(int *a, int n) {
    for (int i = 1; i < n; i++) {
        int key = a[i], j = i - 1;
        while (j >= 0 && cmp(a[j], key) > 0) {
            a[j+1] = a[j];
            j--;
            comparisons++;
        }
        a[j+1] = key;
    }
}

/* ─── SHELL SORT ────────────────────────────────────────────── */
void shell_sort(int *a, int n) {
    for (int gap = n/2; gap > 0; gap /= 2) {
        for (int i = gap; i < n; i++) {
            int tmp = a[i], j = i;
            while (j >= gap && cmp(a[j-gap], tmp) > 0) {
                a[j] = a[j-gap];
                j -= gap;
            }
            a[j] = tmp;
        }
    }
}

/* ─── MERGE SORT ────────────────────────────────────────────── */
static int temp_buf[MAX_SIZE];

void merge(int *a, int l, int m, int r) {
    int n1 = m - l + 1, n2 = r - m;
    int *L = temp_buf, *R = temp_buf + n1;
    for (int i = 0; i < n1; i++) L[i] = a[l+i];
    for (int j = 0; j < n2; j++) R[j] = a[m+1+j];
    int i = 0, j = 0, k = l;
    while (i < n1 && j < n2) {
        comparisons++;
        if (L[i] <= R[j]) a[k++] = L[i++];
        else               a[k++] = R[j++];
    }
    while (i < n1) a[k++] = L[i++];
    while (j < n2) a[k++] = R[j++];
}

void merge_sort_rec(int *a, int l, int r) {
    if (l < r) {
        int m = l + (r - l) / 2;
        merge_sort_rec(a, l, m);
        merge_sort_rec(a, m+1, r);
        merge(a, l, m, r);
    }
}

void merge_sort(int *a, int n) { merge_sort_rec(a, 0, n-1); }

/* ─── QUICK SORT ────────────────────────────────────────────── */
int partition(int *a, int lo, int hi) {
    /* median-of-three pivot */
    int mid = lo + (hi - lo) / 2;
    if (cmp(a[lo], a[mid]) > 0) swap(&a[lo], &a[mid]);
    if (cmp(a[lo], a[hi])  > 0) swap(&a[lo], &a[hi]);
    if (cmp(a[mid], a[hi]) > 0) swap(&a[mid], &a[hi]);
    int pivot = a[mid];
    swap(&a[mid], &a[hi-1]);
    int i = lo, j = hi - 1;
    while (1) {
        while (cmp(a[++i], pivot) < 0) ;
        while (cmp(a[--j], pivot) > 0) ;
        if (i >= j) break;
        swap(&a[i], &a[j]);
    }
    swap(&a[i], &a[hi-1]);
    return i;
}

void quick_sort_rec(int *a, int lo, int hi) {
    if (hi - lo < 2) return;
    if (hi - lo < 10) { /* insertion sort for small subarrays */
        for (int i = lo+1; i <= hi; i++) {
            int key = a[i], j = i-1;
            while (j >= lo && cmp(a[j], key) > 0) a[j+1] = a[j--];
            a[j+1] = key;
        }
        return;
    }
    int p = partition(a, lo, hi);
    quick_sort_rec(a, lo, p-1);
    quick_sort_rec(a, p+1, hi);
}

void quick_sort(int *a, int n) {
    if (n > 2) quick_sort_rec(a, 0, n-1);
}

/* ─── HEAP SORT ─────────────────────────────────────────────── */
void sift_down(int *a, int i, int n) {
    while (1) {
        int largest = i, l = 2*i+1, r = 2*i+2;
        if (l < n && cmp(a[l], a[largest]) > 0) largest = l;
        if (r < n && cmp(a[r], a[largest]) > 0) largest = r;
        if (largest == i) break;
        swap(&a[i], &a[largest]);
        i = largest;
    }
}

void heap_sort(int *a, int n) {
    for (int i = n/2 - 1; i >= 0; i--) sift_down(a, i, n);
    for (int i = n-1; i > 0; i--) { swap(&a[0], &a[i]); sift_down(a, 0, i); }
}

/* ─── COUNTING SORT (non-negative ints) ─────────────────────── */
void counting_sort(int *a, int n) {
    int max = a[0];
    for (int i = 1; i < n; i++) if (a[i] > max) max = a[i];
    int *count = calloc(max+1, sizeof(int));
    for (int i = 0; i < n; i++) count[a[i]]++;
    int k = 0;
    for (int i = 0; i <= max; i++)
        while (count[i]-- > 0) a[k++] = i;
    free(count);
}

/* ─── RADIX SORT (LSD, base 10) ─────────────────────────────── */
void radix_sort(int *a, int n) {
    int max = a[0];
    for (int i = 1; i < n; i++) if (a[i] > max) max = a[i];
    int *output = malloc(n * sizeof(int));
    for (int exp = 1; max/exp > 0; exp *= 10) {
        int count[10] = {0};
        for (int i = 0; i < n; i++) count[(a[i]/exp) % 10]++;
        for (int i = 1; i < 10; i++) count[i] += count[i-1];
        for (int i = n-1; i >= 0; i--)
            output[--count[(a[i]/exp) % 10]] = a[i];
        memcpy(a, output, n * sizeof(int));
    }
    free(output);
}

/* ─── BINARY SEARCH ─────────────────────────────────────────── */
int binary_search(const int *a, int n, int target) {
    int lo = 0, hi = n - 1;
    while (lo <= hi) {
        int mid = lo + (hi - lo) / 2;
        if (a[mid] == target) return mid;
        if (a[mid] < target) lo = mid + 1;
        else hi = mid - 1;
    }
    return -1;
}

/* ─── MAIN ──────────────────────────────────────────────────── */
#define N 20

typedef void (*sort_fn)(int*, int);
typedef struct { const char *name; sort_fn fn; } Sorter;

int main(void) {
    printf("╔══════════════════════════════════════════════════╗\n");
    printf("║     SORTING ALGORITHMS — C Implementation        ║\n");
    printf("╚══════════════════════════════════════════════════╝\n\n");

    /* Generate random array */
    int original[N];
    srand(42);
    for (int i = 0; i < N; i++) original[i] = rand() % 100 + 1;
    print_array("Original", original, N);
    printf("\n");

    int arr[N];
    Sorter sorters[] = {
        {"Bubble Sort",    bubble_sort},
        {"Selection Sort", selection_sort},
        {"Insertion Sort", insertion_sort},
        {"Shell Sort",     shell_sort},
        {"Merge Sort",     merge_sort},
        {"Quick Sort",     quick_sort},
        {"Heap Sort",      heap_sort},
        {"Counting Sort",  counting_sort},
        {"Radix Sort",     radix_sort},
    };
    int num_sorters = sizeof(sorters) / sizeof(sorters[0]);

    printf("  Algorithm        Comparisons       Swaps            Time\n");
    printf("  ─────────────────────────────────────────────────────────\n");

    for (int i = 0; i < num_sorters; i++) {
        copy_array(original, arr, N);
        reset_counters();
        clock_t t0 = clock();
        sorters[i].fn(arr, N);
        clock_t t1 = clock();
        print_stats(sorters[i].name, (double)(t1 - t0) / CLOCKS_PER_SEC);
    }

    /* Show a final sorted result */
    copy_array(original, arr, N);
    merge_sort(arr, N);
    printf("\n");
    print_array("Sorted result", arr, N);

    /* Binary search demo */
    int target = arr[N/2];
    int idx = binary_search(arr, N, target);
    printf("\n  Binary search for %d → index %d\n", target, idx);
    printf("  Binary search for 999 → index %d\n", binary_search(arr, N, 999));

    /* Performance test on larger array */
    printf("\n  ─── Large-N Performance (N=%d) ───\n", MAX_SIZE);
    int big[MAX_SIZE], tmp[MAX_SIZE];
    for (int i = 0; i < MAX_SIZE; i++) big[i] = rand() % 10000;

    Sorter fast_sorters[] = {
        {"Merge Sort",  merge_sort},
        {"Quick Sort",  quick_sort},
        {"Heap Sort",   heap_sort},
        {"Radix Sort",  radix_sort},
    };
    for (int i = 0; i < 4; i++) {
        copy_array(big, tmp, MAX_SIZE);
        reset_counters();
        clock_t t0 = clock();
        fast_sorters[i].fn(tmp, MAX_SIZE);
        clock_t t1 = clock();
        print_stats(fast_sorters[i].name, (double)(t1 - t0) / CLOCKS_PER_SEC);
    }

    printf("\n  ✓ All sorting algorithms complete!\n");
    return 0;
}
