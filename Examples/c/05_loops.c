/*
 * Loops - for, while, do-while
 * 
 * Demonstrates:
 * - for loop (counting)
 * - while loop (condition-based)
 * - do-while loop (execute at least once)
 * - break and continue
 */

#include <stdio.h>

int main() {
    int i, sum;
    
    // FOR LOOP - counting 1 to 10
    printf("=== For Loop: Count to 10 ===\n");
    for (i = 1; i <= 10; i++) {
        printf("%d ", i);
    }
    printf("\n\n");
    
    // FOR LOOP - countdown
    printf("=== Countdown ===\n");
    for (i = 10; i >= 1; i--) {
        printf("%d... ", i);
    }
    printf("Liftoff!\n\n");
    
    // FOR LOOP - sum of numbers
    printf("=== Sum of 1 to 100 ===\n");
    sum = 0;
    for (i = 1; i <= 100; i++) {
        sum += i;
    }
    printf("Sum: %d\n\n", sum);
    
    // WHILE LOOP - multiplication table
    printf("=== 7 Times Table ===\n");
    i = 1;
    while (i <= 12) {
        printf("7 × %d = %d\n", i, 7 * i);
        i++;
    }
    printf("\n");
    
    // DO-WHILE LOOP - input validation
    int number;
    printf("=== Input Validation ===\n");
    do {
        printf("Enter a positive number: ");
        scanf("%d", &number);
        if (number <= 0) {
            printf("That's not positive! Try again.\n");
        }
    } while (number <= 0);
    printf("Thank you! You entered: %d\n\n", number);
    
    // BREAK and CONTINUE
    printf("=== Even Numbers (using continue) ===\n");
    for (i = 1; i <= 20; i++) {
        if (i % 2 != 0) {
            continue;  // Skip odd numbers
        }
        printf("%d ", i);
    }
    printf("\n\n");
    
    printf("=== Find First Multiple of 7 (using break) ===\n");
    for (i = 1; i <= 100; i++) {
        if (i % 7 == 0) {
            printf("Found: %d\n", i);
            break;  // Stop after finding first one
        }
    }
    
    return 0;
}
