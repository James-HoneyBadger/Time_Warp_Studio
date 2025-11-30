/*
 * Simple Calculator
 * 
 * Complete program demonstrating:
 * - Menu-driven interface
 * - Functions for operations
 * - Input validation
 * - Switch statement
 * - Loops for continuous operation
 */

#include <stdio.h>

// Function prototypes
float add(float a, float b);
float subtract(float a, float b);
float multiply(float a, float b);
float divide(float a, float b);
void display_menu();

int main() {
    int choice;
    float num1, num2, result;
    char continue_calc;
    
    printf("=================================\n");
    printf("   SIMPLE CALCULATOR PROGRAM\n");
    printf("=================================\n\n");
    
    do {
        display_menu();
        printf("Enter your choice (1-4): ");
        scanf("%d", &choice);
        
        if (choice < 1 || choice > 4) {
            printf("Invalid choice! Please try again.\n\n");
            continue;
        }
        
        printf("Enter first number: ");
        scanf("%f", &num1);
        printf("Enter second number: ");
        scanf("%f", &num2);
        
        switch (choice) {
            case 1:
                result = add(num1, num2);
                printf("\n%.2f + %.2f = %.2f\n", num1, num2, result);
                break;
            case 2:
                result = subtract(num1, num2);
                printf("\n%.2f - %.2f = %.2f\n", num1, num2, result);
                break;
            case 3:
                result = multiply(num1, num2);
                printf("\n%.2f × %.2f = %.2f\n", num1, num2, result);
                break;
            case 4:
                if (num2 == 0) {
                    printf("\nError: Cannot divide by zero!\n");
                } else {
                    result = divide(num1, num2);
                    printf("\n%.2f ÷ %.2f = %.2f\n", num1, num2, result);
                }
                break;
        }
        
        printf("\nContinue? (y/n): ");
        scanf(" %c", &continue_calc);
        printf("\n");
        
    } while (continue_calc == 'y' || continue_calc == 'Y');
    
    printf("Thank you for using the calculator!\n");
    return 0;
}

void display_menu() {
    printf("Choose an operation:\n");
    printf("1. Addition (+)\n");
    printf("2. Subtraction (-)\n");
    printf("3. Multiplication (×)\n");
    printf("4. Division (÷)\n");
}

float add(float a, float b) {
    return a + b;
}

float subtract(float a, float b) {
    return a - b;
}

float multiply(float a, float b) {
    return a * b;
}

float divide(float a, float b) {
    return a / b;
}
