/* ==========================================================
   ARRAY DATA STRUCTURES SHOWCASE
   Since the interpreter does not support structs or pointers,
   this demo showcases array-based data management:
   stack, queue, circular buffer, sorted insert, search.
   ========================================================== */
#include <stdio.h>
#include <string.h>

int main() {
    printf("╔═══════════════════════════════════════════════╗\n");
    printf("║     ARRAY DATA STRUCTURES  v1.0               ║\n");
    printf("╚═══════════════════════════════════════════════╝\n\n");

    int i, j, tmp;

    /* ── 1. Stack (LIFO) using array ──────────────────── */
    printf("  1. STACK (LIFO)\n");
    printf("  ──────────────\n");
    int stack[20];
    int stop = 0;

    /* Push values */
    stack[stop] = 10; stop++;
    stack[stop] = 20; stop++;
    stack[stop] = 30; stop++;
    stack[stop] = 40; stop++;
    printf("  Pushed: 10, 20, 30, 40\n");
    printf("  Stack size: %d\n", stop);

    /* Pop values */
    stop--; printf("  Pop: %d\n", stack[stop]);
    stop--; printf("  Pop: %d\n", stack[stop]);
    printf("  Stack size after 2 pops: %d\n\n", stop);

    /* ── 2. Queue (FIFO) using circular buffer ────────── */
    printf("  2. QUEUE (FIFO)\n");
    printf("  ──────────────\n");
    int queue[10];
    int qfront = 0;
    int qrear = 0;
    int qcount = 0;
    int qsize = 10;

    /* Enqueue */
    queue[qrear] = 100; qrear = (qrear + 1) % qsize; qcount++;
    queue[qrear] = 200; qrear = (qrear + 1) % qsize; qcount++;
    queue[qrear] = 300; qrear = (qrear + 1) % qsize; qcount++;
    queue[qrear] = 400; qrear = (qrear + 1) % qsize; qcount++;
    printf("  Enqueued: 100, 200, 300, 400\n");

    /* Dequeue */
    printf("  Dequeue: %d\n", queue[qfront]);
    qfront = (qfront + 1) % qsize; qcount--;
    printf("  Dequeue: %d\n", queue[qfront]);
    qfront = (qfront + 1) % qsize; qcount--;
    printf("  Queue size: %d\n\n", qcount);

    /* ── 3. Sorted insertion ──────────────────────────── */
    printf("  3. SORTED ARRAY\n");
    printf("  ──────────────\n");
    int sorted[20];
    int sn = 0;
    int values[8] = {45, 12, 78, 3, 56, 91, 23, 67};

    printf("  Inserting in sorted order: ");
    for (i = 0; i < 8; i++) {
        int val = values[i];
        printf("%d ", val);
        /* Find insert position */
        j = sn;
        while (j > 0) {
            if (sorted[j-1] > val) {
                sorted[j] = sorted[j-1];
                j--;
            } else {
                break;
            }
        }
        sorted[j] = val;
        sn++;
    }
    printf("\n  Sorted result: ");
    for (i = 0; i < sn; i++) printf("%d ", sorted[i]);
    printf("\n\n");

    /* ── 4. Linear and Binary Search ──────────────────── */
    printf("  4. SEARCH ALGORITHMS\n");
    printf("  ───────────────────\n");
    int target = 56;

    /* Linear search on unsorted */
    int found = -1;
    for (i = 0; i < 8; i++) {
        if (values[i] == target) { found = i; break; }
    }
    printf("  Linear search for %d: found at index %d\n", target, found);

    /* Binary search on sorted array */
    int lo = 0;
    int hi = sn - 1;
    found = -1;
    while (lo <= hi) {
        int mid = (lo + hi) / 2;
        if (sorted[mid] == target) { found = mid; break; }
        if (sorted[mid] < target) lo = mid + 1;
        else hi = mid - 1;
    }
    printf("  Binary search for %d: found at index %d\n", target, found);
    printf("\n");

    /* ── 5. Array Statistics ──────────────────────────── */
    printf("  5. ARRAY STATISTICS\n");
    printf("  ──────────────────\n");
    int sum = 0;
    int minv = sorted[0];
    int maxv = sorted[0];
    for (i = 0; i < sn; i++) {
        sum += sorted[i];
        if (sorted[i] < minv) minv = sorted[i];
        if (sorted[i] > maxv) maxv = sorted[i];
    }
    printf("  Count: %d\n", sn);
    printf("  Sum:   %d\n", sum);
    printf("  Min:   %d\n", minv);
    printf("  Max:   %d\n", maxv);
    printf("  Mean:  %.2f\n", (double)sum / sn);

    /* Median (array already sorted) */
    double median;
    if (sn % 2 == 1) {
        median = sorted[sn / 2];
    } else {
        median = (sorted[sn / 2 - 1] + sorted[sn / 2]) / 2.0;
    }
    printf("  Median: %.1f\n\n", median);

    /* ── 6. Array Rotation ────────────────────────────── */
    printf("  6. ARRAY ROTATION\n");
    printf("  ────────────────\n");
    int rot[8] = {1, 2, 3, 4, 5, 6, 7, 8};
    int rn = 8;
    printf("  Before: ");
    for (i = 0; i < rn; i++) printf("%d ", rot[i]);
    printf("\n");

    /* Rotate left by 3 positions */
    int k = 3;
    for (int r = 0; r < k; r++) {
        tmp = rot[0];
        for (i = 0; i < rn - 1; i++) rot[i] = rot[i + 1];
        rot[rn - 1] = tmp;
    }
    printf("  After left-rotate by %d: ", k);
    for (i = 0; i < rn; i++) printf("%d ", rot[i]);
    printf("\n\n");

    printf("  Array data structures demo complete.\n");
    return 0;
}
