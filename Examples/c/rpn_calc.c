/* ==========================================================
   RPN (Reverse Polish Notation) CALCULATOR
   Demonstrates: stack-based evaluation using arrays,
   arithmetic operations, and string parsing.
   ========================================================== */
#include <stdio.h>
#include <math.h>
#include <string.h>

int main() {
    printf("╔═══════════════════════════════════════════════╗\n");
    printf("║      RPN CALCULATOR SHOWCASE  v1.0            ║\n");
    printf("╚═══════════════════════════════════════════════╝\n\n");

    /* Stack for RPN evaluation (inline) */
    double stack[64];
    int top = 0;
    double a, b, result;

    /* ── Test 1: Basic arithmetic: 3 4 + ────────────── */
    printf("  1. Expression: 3 4 +\n");
    top = 0;
    stack[top] = 3; top++;
    stack[top] = 4; top++;
    top--; b = stack[top]; top--; a = stack[top];
    stack[top] = a + b; top++;
    top--; result = stack[top];
    printf("     Result: %.2f\n\n", result);

    /* ── Test 2: Complex: 5 1 2 + 4 * + 3 - ────────── */
    printf("  2. Expression: 5 1 2 + 4 * + 3 -\n");
    top = 0;
    stack[top] = 5; top++;
    stack[top] = 1; top++;
    stack[top] = 2; top++;
    top--; b = stack[top]; top--; a = stack[top];
    stack[top] = a + b; top++;
    stack[top] = 4; top++;
    top--; b = stack[top]; top--; a = stack[top];
    stack[top] = a * b; top++;
    top--; b = stack[top]; top--; a = stack[top];
    stack[top] = a + b; top++;
    stack[top] = 3; top++;
    top--; b = stack[top]; top--; a = stack[top];
    stack[top] = a - b; top++;
    top--; result = stack[top];
    printf("     Result: %.2f  (expected 14.00)\n\n", result);

    /* ── Test 3: Division and modulo: 100 7 / ────────── */
    printf("  3. Expression: 100 7 /\n");
    top = 0;
    stack[top] = 100; top++;
    stack[top] = 7; top++;
    top--; b = stack[top]; top--; a = stack[top];
    stack[top] = a / b; top++;
    top--; result = stack[top];
    printf("     Result: %.4f\n\n", result);

    /* ── Test 4: Power: 2 10 ^ (2^10 = 1024) ────────── */
    printf("  4. Expression: 2 10 ^ (power)\n");
    top = 0;
    stack[top] = 2; top++;
    stack[top] = 10; top++;
    top--; b = stack[top]; top--; a = stack[top];
    stack[top] = pow(a, b); top++;
    top--; result = stack[top];
    printf("     Result: %.0f\n\n", result);

    /* ── Test 5: Square root: 144 sqrt ───────────────── */
    printf("  5. Expression: 144 sqrt\n");
    top = 0;
    stack[top] = 144; top++;
    top--; a = stack[top];
    stack[top] = sqrt(a); top++;
    top--; result = stack[top];
    printf("     Result: %.2f\n\n", result);

    /* ── Test 6: Temperature conversion: (F - 32) * 5/9     */
    printf("  6. Fahrenheit 212 to Celsius\n");
    printf("     Formula: 212 32 - 5 * 9 /\n");
    top = 0;
    stack[top] = 212; top++;
    stack[top] = 32; top++;
    top--; b = stack[top]; top--; a = stack[top];
    stack[top] = a - b; top++;
    stack[top] = 5; top++;
    top--; b = stack[top]; top--; a = stack[top];
    stack[top] = a * b; top++;
    stack[top] = 9; top++;
    top--; b = stack[top]; top--; a = stack[top];
    stack[top] = a / b; top++;
    top--; result = stack[top];
    printf("     Result: %.2f C\n\n", result);

    /* ── Test 7: Pythagorean: sqrt(3^2 + 4^2) ───────── */
    printf("  7. Pythagorean: sqrt(3^2 + 4^2)\n");
    printf("     Formula: 3 2 ^ 4 2 ^ + sqrt\n");
    top = 0;
    stack[top] = 3; top++;
    stack[top] = 2; top++;
    top--; b = stack[top]; top--; a = stack[top];
    stack[top] = pow(a, b); top++;
    stack[top] = 4; top++;
    stack[top] = 2; top++;
    top--; b = stack[top]; top--; a = stack[top];
    stack[top] = pow(a, b); top++;
    top--; b = stack[top]; top--; a = stack[top];
    stack[top] = a + b; top++;
    top--; a = stack[top];
    stack[top] = sqrt(a); top++;
    top--; result = stack[top];
    printf("     Result: %.2f\n\n", result);

    printf("  RPN calculator demo complete.\n");
    return 0;
}
