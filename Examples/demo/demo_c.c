#include <stdio.h>

int main() {
    printf("Time Warp Studio - C Demo\n");
    printf("\n");
    
    printf("1. Variables and Arithmetic:\n");
    int x = 10;
    int y = 20;
    int sum = x + y;
    printf("x=%d, y=%d, sum=%d\n", x, y, sum);
    printf("\n");
    
    printf("2. Loop Example:\n");
    printf("Counting: ");
    for (int i = 1; i <= 5; i++) {
        printf("%d ", i);
    }
    printf("\n");
    printf("\n");
    
    printf("3. Conditionals:\n");
    int score = 85;
    if (score >= 80) {
        printf("Good score!\n");
    } else {
        printf("Try again\n");
    }
    printf("\n");
    
    printf("4. String:\n");
    printf("Language: C\n");
    printf("\n");
    
    printf("Demo complete!\n");
    return 0;
}
