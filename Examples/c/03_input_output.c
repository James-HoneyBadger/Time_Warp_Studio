/*
 * Input and Output
 * 
 * Demonstrates:
 * - Reading user input with scanf()
 * - Formatted output with printf()
 * - Working with different data types
 */

#include <stdio.h>

int main() {
    char name[50];
    int age;
    float height;
    
    printf("=== Personal Information ===\n\n");
    
    // Get user input
    printf("Enter your name: ");
    scanf("%s", name);  // Note: no & for strings
    
    printf("Enter your age: ");
    scanf("%d", &age);  // & is required for int
    
    printf("Enter your height in feet: ");
    scanf("%f", &height);
    
    // Display formatted output
    printf("\n=== Your Information ===\n");
    printf("Name: %s\n", name);
    printf("Age: %d years old\n", age);
    printf("Height: %.1f feet\n", height);
    
    // Calculate and display
    int age_in_months = age * 12;
    printf("Age in months: %d\n", age_in_months);
    
    return 0;
}
