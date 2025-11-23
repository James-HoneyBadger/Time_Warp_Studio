/*
 * Conditional Statements
 * 
 * Demonstrates:
 * - if, else if, else statements
 * - Comparison operators
 * - Logical operators
 * - switch statement
 */

#include <stdio.h>

int main() {
    int score;
    char grade;
    
    printf("=== Grade Calculator ===\n\n");
    printf("Enter your score (0-100): ");
    scanf("%d", &score);
    
    // Validate input
    if (score < 0 || score > 100) {
        printf("Error: Score must be between 0 and 100!\n");
        return 1;
    }
    
    // Determine letter grade
    if (score >= 90) {
        grade = 'A';
        printf("Excellent! Grade: %c\n", grade);
    } else if (score >= 80) {
        grade = 'B';
        printf("Good job! Grade: %c\n", grade);
    } else if (score >= 70) {
        grade = 'C';
        printf("Average. Grade: %c\n", grade);
    } else if (score >= 60) {
        grade = 'D';
        printf("Below average. Grade: %c\n", grade);
    } else {
        grade = 'F';
        printf("Failing. Grade: %c\n", grade);
    }
    
    // Switch statement example
    printf("\nFeedback: ");
    switch (grade) {
        case 'A':
            printf("Outstanding work!\n");
            break;
        case 'B':
            printf("Keep it up!\n");
            break;
        case 'C':
            printf("Room for improvement.\n");
            break;
        case 'D':
            printf("Need more study time.\n");
            break;
        case 'F':
            printf("Please see instructor.\n");
            break;
        default:
            printf("Unknown grade.\n");
    }
    
    return 0;
}
