/* ==========================================================
   SORTING ALGORITHMS SHOWCASE
   Demonstrates: bubble sort, selection sort, insertion sort,
   shell sort — all inlined in main() using arrays.
   ========================================================== */
#include <stdio.h>

int main() {
    /* ── Banner ─────────────────────────────────────────── */
    printf("╔═══════════════════════════════════════════════╗\n");
    printf("║     SORTING ALGORITHMS SHOWCASE v1.0          ║\n");
    printf("╚═══════════════════════════════════════════════╝\n\n");

    int original[10] = {64, 34, 25, 12, 22, 11, 90, 45, 78, 33};
    int arr[10];
    int n = 10;
    int i, j, tmp;

    /* Print original array */
    printf("  Original array: ");
    for (i = 0; i < n; i++) printf("%d ", original[i]);
    printf("\n\n");

    /* ── 1. Bubble Sort ───────────────────────────────── */
    printf("  1. BUBBLE SORT\n");
    printf("  ─────────────\n");
    for (i = 0; i < n; i++) arr[i] = original[i];
    int swaps = 0;
    int comps = 0;
    for (i = 0; i < n - 1; i++) {
        for (j = 0; j < n - 1 - i; j++) {
            comps++;
            if (arr[j] > arr[j+1]) {
                tmp = arr[j]; arr[j] = arr[j+1]; arr[j+1] = tmp;
                swaps++;
            }
        }
    }
    printf("  Result: ");
    for (i = 0; i < n; i++) printf("%d ", arr[i]);
    printf("\n  Comparisons: %d  Swaps: %d\n\n", comps, swaps);

    /* ── 2. Selection Sort ────────────────────────────── */
    printf("  2. SELECTION SORT\n");
    printf("  ────────────────\n");
    for (i = 0; i < n; i++) arr[i] = original[i];
    swaps = 0; comps = 0;
    for (i = 0; i < n - 1; i++) {
        int minIdx = i;
        for (j = i + 1; j < n; j++) {
            comps++;
            if (arr[j] < arr[minIdx]) minIdx = j;
        }
        if (minIdx != i) {
            tmp = arr[i]; arr[i] = arr[minIdx]; arr[minIdx] = tmp;
            swaps++;
        }
    }
    printf("  Result: ");
    for (i = 0; i < n; i++) printf("%d ", arr[i]);
    printf("\n  Comparisons: %d  Swaps: %d\n\n", comps, swaps);

    /* ── 3. Insertion Sort ────────────────────────────── */
    printf("  3. INSERTION SORT\n");
    printf("  ────────────────\n");
    for (i = 0; i < n; i++) arr[i] = original[i];
    swaps = 0; comps = 0;
    for (i = 1; i < n; i++) {
        int key = arr[i];
        j = i - 1;
        while (j >= 0) {
            comps++;
            if (arr[j] > key) {
                arr[j + 1] = arr[j];
                swaps++;
                j--;
            } else {
                break;
            }
        }
        arr[j + 1] = key;
    }
    printf("  Result: ");
    for (i = 0; i < n; i++) printf("%d ", arr[i]);
    printf("\n  Comparisons: %d  Swaps: %d\n\n", comps, swaps);

    /* ── 4. Shell Sort ────────────────────────────────── */
    printf("  4. SHELL SORT\n");
    printf("  ─────────────\n");
    for (i = 0; i < n; i++) arr[i] = original[i];
    swaps = 0; comps = 0;
    int gap = n / 2;
    while (gap > 0) {
        for (i = gap; i < n; i++) {
            int temp = arr[i];
            j = i;
            while (j >= gap) {
                comps++;
                if (arr[j - gap] > temp) {
                    arr[j] = arr[j - gap];
                    swaps++;
                    j -= gap;
                } else {
                    break;
                }
            }
            arr[j] = temp;
        }
        gap = gap / 2;
    }
    printf("  Result: ");
    for (i = 0; i < n; i++) printf("%d ", arr[i]);
    printf("\n  Comparisons: %d  Swaps: %d\n\n", comps, swaps);

    /* ── Binary Search ────────────────────────────────── */
    printf("  BINARY SEARCH (on sorted array)\n");
    printf("  ──────────────────────────────\n");
    /* arr is already sorted from shell sort */
    int target = 45;
    int lo = 0;
    int hi = n - 1;
    int found = -1;
    while (lo <= hi) {
        int mid = (lo + hi) / 2;
        if (arr[mid] == target) { found = mid; break; }
        if (arr[mid] < target) lo = mid + 1;
        else hi = mid - 1;
    }
    if (found >= 0) printf("  Found %d at index %d\n", target, found);
    else printf("  %d not found\n", target);

    printf("\n  Sorting algorithms showcase complete.\n");
    return 0;
}
