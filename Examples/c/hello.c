/* =============================================
 *  C Comprehensive Demo - Time Warp Studio
 * ============================================= */
#include <stdio.h>

int main() {
    /* --- Hello World --- */
    printf("===== HELLO WORLD =====\n");
    printf("Welcome to C!\n");
    printf("\n");

    /* --- Variables and Types --- */
    printf("===== VARIABLES & TYPES =====\n");
    int x = 10;
    int y = 3;
    float pi = 3.14159;
    char ch = 'A';
    printf("int: %d\n", x);
    printf("float: %f\n", pi);
    printf("char: %c\n", ch);
    printf("\n");

    /* --- Arithmetic --- */
    printf("===== ARITHMETIC =====\n");
    printf("x + y = %d\n", x + y);
    printf("x - y = %d\n", x - y);
    printf("x * y = %d\n", x * y);
    printf("x / y = %d\n", x / y);
    printf("x %% y = %d\n", x % y);
    x++;
    printf("x++ = %d\n", x);
    x--;
    printf("x-- = %d\n", x);
    printf("\n");

    /* --- Conditionals --- */
    printf("===== CONDITIONALS =====\n");
    if (x > 5) {
        printf("x is greater than 5\n");
    } else {
        printf("x is not greater than 5\n");
    }

    if (y == 3) {
        printf("y equals 3\n");
    }
    printf("\n");

    /* --- For Loop --- */
    printf("===== FOR LOOP =====\n");
    for (int i = 1; i <= 5; i++) {
        printf("%d\n", i);
    }
    printf("\n");

    /* --- While Loop --- */
    printf("===== WHILE LOOP =====\n");
    int count = 0;
    while (count < 3) {
        printf("count = %d\n", count);
        count++;
    }
    printf("\n");

    /* --- Do-While Loop --- */
    printf("===== DO-WHILE LOOP =====\n");
    int n = 1;
    do {
        printf("n = %d\n", n);
        n++;
    } while (n <= 3);
    printf("\n");

    /* --- Format Specifiers --- */
    printf("===== FORMAT SPECIFIERS =====\n");
    printf("Hex: %x\n", 255);
    printf("Oct: %o\n", 255);
    printf("Sci: %e\n", 12345.6789);
    printf("Percent: 100%%\n");
    printf("\n");

    /* --- String Functions --- */
    printf("===== STRINGS =====\n");
    printf("strlen: %d\n", strlen("Hello"));
    char dest[30];
    strcpy(dest, "Hello");
    strcat(dest, " World");
    printf("strcat: %s\n", dest);
    printf("\n");

    /* --- Math Functions --- */
    printf("===== MATH =====\n");
    printf("abs(-5) = %d\n", abs(-5));
    printf("sqrt(16) = %f\n", sqrt(16.0));
    printf("pow(2, 10) = %f\n", pow(2.0, 10.0));
    printf("\n");

    printf("===== DONE =====\n");
    return 0;
}
