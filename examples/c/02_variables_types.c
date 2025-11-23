/*
 * Variables and Data Types
 * 
 * Demonstrates:
 * - Different data types (int, float, char, string)
 * - Variable declaration and initialization
 * - Format specifiers in printf()
 */

#include <stdio.h>

int main() {
    // Integer variables
    int age = 25;
    int year = 2025;
    
    // Floating point
    float price = 19.99;
    float temperature = 72.5;
    
    // Characters and strings
    char grade = 'A';
    char name[50] = "Alice";
    
    // Display all variables
    printf("=== Variable Examples ===\n\n");
    
    printf("Name: %s\n", name);
    printf("Age: %d years\n", age);
    printf("Grade: %c\n", grade);
    printf("Year: %d\n", year);
    printf("Price: $%.2f\n", price);
    printf("Temperature: %.1f°F\n", temperature);
    
    // Arithmetic with variables
    int next_year = year + 1;
    float total = price * 2;
    
    printf("\nCalculations:\n");
    printf("Next year: %d\n", next_year);
    printf("Two items: $%.2f\n", total);
    
    return 0;
}
