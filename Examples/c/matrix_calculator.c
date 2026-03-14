/* Matrix Calculator — Demonstrates arrays, functions, formatted output */
#include <stdio.h>

/* 3x3 matrix operations: add, multiply, transpose, determinant */

int A[3][3];
int B[3][3];
int C[3][3];

void print_matrix(char *name, int M[3][3]) {
    printf("\n  %s =\n", name);
    int i, j;
    for (i = 0; i < 3; i++) {
        printf("    [ ");
        for (j = 0; j < 3; j++) {
            printf("%5d ", M[i][j]);
        }
        printf("]\n");
    }
}

void matrix_add(int A[3][3], int B[3][3], int C[3][3]) {
    int i, j;
    for (i = 0; i < 3; i++)
        for (j = 0; j < 3; j++)
            C[i][j] = A[i][j] + B[i][j];
}

void matrix_multiply(int A[3][3], int B[3][3], int C[3][3]) {
    int i, j, k;
    for (i = 0; i < 3; i++)
        for (j = 0; j < 3; j++) {
            C[i][j] = 0;
            for (k = 0; k < 3; k++)
                C[i][j] = C[i][j] + A[i][k] * B[k][j];
        }
}

void matrix_transpose(int A[3][3], int T[3][3]) {
    int i, j;
    for (i = 0; i < 3; i++)
        for (j = 0; j < 3; j++)
            T[i][j] = A[j][i];
}

int determinant(int M[3][3]) {
    return M[0][0] * (M[1][1] * M[2][2] - M[1][2] * M[2][1])
         - M[0][1] * (M[1][0] * M[2][2] - M[1][2] * M[2][0])
         + M[0][2] * (M[1][0] * M[2][1] - M[1][1] * M[2][0]);
}

int main() {
    printf("╔══════════════════════════════════════╗\n");
    printf("║     🧮 Matrix Calculator (3×3)      ║\n");
    printf("╚══════════════════════════════════════╝\n");

    /* Initialize matrices */
    A[0][0] = 1;  A[0][1] = 2;  A[0][2] = 3;
    A[1][0] = 4;  A[1][1] = 5;  A[1][2] = 6;
    A[2][0] = 7;  A[2][1] = 8;  A[2][2] = 9;

    B[0][0] = 9;  B[0][1] = 8;  B[0][2] = 7;
    B[1][0] = 6;  B[1][1] = 5;  B[1][2] = 4;
    B[2][0] = 3;  B[2][1] = 2;  B[2][2] = 1;

    print_matrix("A", A);
    print_matrix("B", B);

    /* Addition */
    printf("\n──── Addition: A + B ────\n");
    matrix_add(A, B, C);
    print_matrix("A + B", C);

    /* Multiplication */
    printf("\n──── Multiplication: A × B ────\n");
    matrix_multiply(A, B, C);
    print_matrix("A × B", C);

    /* Transpose */
    printf("\n──── Transpose of A ────\n");
    matrix_transpose(A, C);
    print_matrix("Aᵀ", C);

    /* Determinant */
    printf("\n──── Determinants ────\n");
    printf("  det(A) = %d\n", determinant(A));
    printf("  det(B) = %d\n", determinant(B));

    /* Identity check */
    printf("\n──── Identity Matrix ────\n");
    int I[3][3];
    int i, j;
    for (i = 0; i < 3; i++)
        for (j = 0; j < 3; j++)
            I[i][j] = (i == j) ? 1 : 0;
    print_matrix("I", I);

    printf("\n──── A × I (should equal A) ────\n");
    matrix_multiply(A, I, C);
    print_matrix("A × I", C);

    printf("\n✅ All matrix operations complete.\n");
    return 0;
}
