/*
 * Arrays - Working with Collections
 * 
 * Demonstrates:
 * - Array declaration and initialization
 * - Accessing array elements
 * - Iterating through arrays
 * - Multi-dimensional arrays
 * - Common array operations
 */

#include <stdio.h>

int main() {
    int i, j;
    
    // ARRAY BASICS
    printf("=== Array Basics ===\n");
    int numbers[5] = {10, 20, 30, 40, 50};
    
    printf("Array elements:\n");
    for (i = 0; i < 5; i++) {
        printf("numbers[%d] = %d\n", i, numbers[i]);
    }
    printf("\n");
    
    // ARRAY OPERATIONS
    printf("=== Array Operations ===\n");
    
    // Sum
    int sum = 0;
    for (i = 0; i < 5; i++) {
        sum += numbers[i];
    }
    printf("Sum: %d\n", sum);
    
    // Average
    float average = (float)sum / 5;
    printf("Average: %.1f\n", average);
    
    // Find maximum
    int max = numbers[0];
    for (i = 1; i < 5; i++) {
        if (numbers[i] > max) {
            max = numbers[i];
        }
    }
    printf("Maximum: %d\n\n", max);
    
    // STORING SCORES
    printf("=== Student Scores ===\n");
    int scores[10];
    
    printf("Enter 10 test scores:\n");
    for (i = 0; i < 10; i++) {
        printf("Score %d: ", i + 1);
        scanf("%d", &scores[i]);
    }
    
    // Calculate average
    sum = 0;
    for (i = 0; i < 10; i++) {
        sum += scores[i];
    }
    printf("\nClass average: %.1f\n\n", (float)sum / 10);
    
    // 2D ARRAY - Matrix
    printf("=== 2D Array: Multiplication Table ===\n");
    int table[5][5];
    
    // Fill table
    for (i = 0; i < 5; i++) {
        for (j = 0; j < 5; j++) {
            table[i][j] = (i + 1) * (j + 1);
        }
    }
    
    // Print table
    printf("     1   2   3   4   5\n");
    printf("   -------------------\n");
    for (i = 0; i < 5; i++) {
        printf("%d | ", i + 1);
        for (j = 0; j < 5; j++) {
            printf("%3d ", table[i][j]);
        }
        printf("\n");
    }
    
    return 0;
}
