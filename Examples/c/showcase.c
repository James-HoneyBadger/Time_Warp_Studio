/* Time Warp C Showcase
 * Demonstrates: functions, arrays, pointers, sorting, and simple IO
 */

#include <stdio.h>
#include <stdlib.h>
#include <time.h>

void fill_random(int *arr, int n) {
    for (int i = 0; i < n; ++i) arr[i] = rand() % 100 + 1;
}

void print_array(int *arr, int n) {
    for (int i = 0; i < n; ++i) printf("%3d ", arr[i]);
    printf("\n");
}

void sort_asc(int *arr, int n) {
    for (int i = 0; i < n-1; ++i) {
        for (int j = i+1; j < n; ++j) {
            if (arr[i] > arr[j]) {
                int t = arr[i]; arr[i] = arr[j]; arr[j] = t;
            }
        }
    }
}

int sum_array(int *arr, int n) {
    int s = 0;
    for (int i = 0; i < n; ++i) s += arr[i];
    return s;
}

int main(void) {
    srand((unsigned)time(NULL));
    const int N = 12;
    int nums[N];
    printf("Time Warp C Showcase\n");
    fill_random(nums, N);
    printf("Original: "); print_array(nums, N);
    sort_asc(nums, N);
    printf("Sorted:   "); print_array(nums, N);
    int total = sum_array(nums, N);
    printf("Sum = %d, Avg = %d\n", total, total / N);

    printf("Pick a threshold to display numbers >= threshold: ");
    int thr = 50;
    if (scanf("%d", &thr) == 1) {
        printf("Numbers >= %d:", thr);
        for (int i = 0; i < N; ++i) if (nums[i] >= thr) printf(" %d", nums[i]);
        printf("\n");
    } else {
        printf("No threshold entered — skipping.\n");
    }
    return 0;
}
