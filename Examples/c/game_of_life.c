/* ============================================================ */
/* CONWAY'S GAME OF LIFE — C Language Showcase                  */
/* Classic cellular automaton: gliders, oscillators, patterns   */
/* Time Warp Studio — C Language Demo                           */
/* ============================================================ */

#define ROWS 22
#define COLS 60
#define GENERATIONS 25

int grid[ROWS][COLS];
int next[ROWS][COLS];

void clear_grid() {
    int r, c;
    for (r = 0; r < ROWS; r++)
        for (c = 0; c < COLS; c++)
            grid[r][c] = 0;
}

int count_neighbors(int r, int c) {
    int count = 0;
    int dr, dc;
    for (dr = -1; dr <= 1; dr++) {
        for (dc = -1; dc <= 1; dc++) {
            if (dr == 0 && dc == 0) continue;
            int nr = (r + dr + ROWS) % ROWS;
            int nc = (c + dc + COLS) % COLS;
            count += grid[nr][nc];
        }
    }
    return count;
}

void step() {
    int r, c, n;
    for (r = 0; r < ROWS; r++) {
        for (c = 0; c < COLS; c++) {
            n = count_neighbors(r, c);
            if (grid[r][c] == 1)
                next[r][c] = (n == 2 || n == 3) ? 1 : 0;
            else
                next[r][c] = (n == 3) ? 1 : 0;
        }
    }
    for (r = 0; r < ROWS; r++)
        for (c = 0; c < COLS; c++)
            grid[r][c] = next[r][c];
}

int count_alive() {
    int r, c, count = 0;
    for (r = 0; r < ROWS; r++)
        for (c = 0; c < COLS; c++)
            count += grid[r][c];
    return count;
}

void print_grid(int gen) {
    int r, c;
    char line[COLS + 5];
    printf("Generation %d  (alive: %d)\n", gen, count_alive());
    printf("+");
    for (c = 0; c < COLS; c++) printf("-");
    printf("+\n");
    for (r = 0; r < ROWS; r++) {
        printf("|");
        for (c = 0; c < COLS; c++)
            printf("%c", grid[r][c] ? '#' : ' ');
        printf("|\n");
    }
    printf("+");
    for (c = 0; c < COLS; c++) printf("-");
    printf("+\n");
}

/* ===== Pattern setters ===== */

void set_glider(int r, int c) {
    /* Classic glider — travels diagonally */
    grid[r+0][c+1] = 1;
    grid[r+1][c+2] = 1;
    grid[r+2][c+0] = 1;
    grid[r+2][c+1] = 1;
    grid[r+2][c+2] = 1;
}

void set_blinker(int r, int c) {
    /* Period-2 oscillator */
    grid[r][c]   = 1;
    grid[r][c+1] = 1;
    grid[r][c+2] = 1;
}

void set_toad(int r, int c) {
    /* Period-2 oscillator */
    grid[r+0][c+1] = 1;
    grid[r+0][c+2] = 1;
    grid[r+0][c+3] = 1;
    grid[r+1][c+0] = 1;
    grid[r+1][c+1] = 1;
    grid[r+1][c+2] = 1;
}

void set_beacon(int r, int c) {
    /* Period-2 oscillator */
    grid[r+0][c+0] = 1;
    grid[r+0][c+1] = 1;
    grid[r+1][c+0] = 1;
    grid[r+2][c+3] = 1;
    grid[r+3][c+2] = 1;
    grid[r+3][c+3] = 1;
}

void set_block(int r, int c) {
    /* Still life */
    grid[r+0][c+0] = 1;
    grid[r+0][c+1] = 1;
    grid[r+1][c+0] = 1;
    grid[r+1][c+1] = 1;
}

void set_r_pentomino(int r, int c) {
    /* Classic chaotic pattern — runs for 1103 gens */
    grid[r+0][c+1] = 1;
    grid[r+0][c+2] = 1;
    grid[r+1][c+0] = 1;
    grid[r+1][c+1] = 1;
    grid[r+2][c+1] = 1;
}

void set_pulsar(int r, int c) {
    /* Period-3 oscillator, large and beautiful */
    int d;
    for (d = 0; d <= 2; d++) {
        grid[r+0][c+2+d] = 1; grid[r+0][c+8+d] = 1;
        grid[r+5][c+2+d] = 1; grid[r+5][c+8+d] = 1;
        grid[r+7][c+2+d] = 1; grid[r+7][c+8+d] = 1;
        grid[r+12][c+2+d] = 1; grid[r+12][c+8+d] = 1;
    }
    for (d = 0; d <= 2; d++) {
        grid[r+2+d][c+0] = 1; grid[r+2+d][c+5] = 1;
        grid[r+2+d][c+7] = 1; grid[r+2+d][c+12] = 1;
        grid[r+8+d][c+0] = 1; grid[r+8+d][c+5] = 1;
        grid[r+8+d][c+7] = 1; grid[r+8+d][c+12] = 1;
    }
}

void run_simulation(char *name, int pattern_fn) {
    int g;
    printf("\n=== %s ===\n", name);
    print_grid(0);
    for (g = 1; g <= GENERATIONS; g++) {
        step();
        if (g == 1 || g == 5 || g == 10 || g == 15 || g == 20 || g == GENERATIONS)
            print_grid(g);
    }
}

/* ===== Statistics ===== */

void analyze_pattern(char *name, int alive_start, int alive_end) {
    printf("  %-20s alive_start=%3d  alive_end=%3d  delta=%+d\n",
        name, alive_start, alive_end, alive_end - alive_start);
}

/* ===== Main ===== */

int main() {
    int g, c, alive_start, alive_end;

    printf("============================================================\n");
    printf("  CONWAY'S GAME OF LIFE — C Language Demo\n");
    printf("  Cellular automata: emergent complexity from simple rules\n");
    printf("============================================================\n\n");

    printf("RULES:\n");
    printf("  1. Live cell with 2 or 3 neighbors → survives\n");
    printf("  2. Live cell with < 2 or > 3 neighbors → dies\n");
    printf("  3. Dead cell with exactly 3 neighbors → becomes alive\n\n");

    /* ---- Simulation 1: Glider ---- */
    printf("PATTERN 1: GLIDER\n");
    printf("The classic spaceship — travels diagonally across the grid.\n\n");
    clear_grid();
    set_glider(2, 2);
    set_glider(2, 20);
    set_glider(10, 40);
    alive_start = count_alive();
    print_grid(0);
    for (g = 1; g <= 30; g++) {
        step();
        if (g == 10 || g == 20 || g == 30) print_grid(g);
    }
    alive_end = count_alive();

    /* ---- Simulation 2: Oscillators ---- */
    printf("\nPATTERN 2: OSCILLATORS (Blinker, Toad, Beacon)\n");
    printf("These patterns return to their initial state periodically.\n\n");
    clear_grid();
    set_blinker(3, 5);
    set_blinker(3, 15);
    set_toad(8, 5);
    set_beacon(8, 20);
    set_beacon(14, 40);
    print_grid(0);
    step(); print_grid(1);
    step(); print_grid(2);
    step(); print_grid(3);
    step(); print_grid(4);

    /* ---- Simulation 3: R-Pentomino (chaotic) ---- */
    printf("\nPATTERN 3: R-PENTOMINO (Chaotic)\n");
    printf("5 cells that produce 1103 generations of chaos before stabilizing.\n\n");
    clear_grid();
    set_r_pentomino(9, 28);
    print_grid(0);
    for (g = 1; g <= 50; g++) {
        step();
        if (g == 10 || g == 25 || g == 50) print_grid(g);
    }

    /* ---- Simulation 4: Mixed garden ---- */
    printf("\nPATTERN 4: LIFE GARDEN (Mixed patterns)\n");
    printf("Still lives, oscillators, and a glider coexist.\n\n");
    clear_grid();
    set_block(1, 1);
    set_block(1, 55);
    set_block(18, 1);
    set_blinker(10, 28);
    set_glider(1, 25);
    set_toad(15, 35);
    print_grid(0);
    for (g = 1; g <= 20; g++) step();
    print_grid(20);

    /* ---- Statistics Summary ---- */
    printf("\n============================================================\n");
    printf("  CELLULAR AUTOMATA FACTS\n");
    printf("============================================================\n");
    printf("  Grid size:        %d x %d = %d cells\n", ROWS, COLS, ROWS*COLS);
    printf("  Boundary:         Toroidal (wraps around edges)\n");
    printf("  Neighborhood:     Moore (8 adjacent cells)\n");
    printf("  Rules:            B3/S23 (Conway's classic)\n\n");

    printf("  Known patterns:\n");
    printf("    Still lifes:    Block, beehive, loaf, boat (don't change)\n");
    printf("    Oscillators:    Blinker(p2), Toad(p2), Beacon(p2), Pulsar(p3)\n");
    printf("    Spaceships:     Glider(p4), LWSS(p4), MWSS, HWSS\n");
    printf("    Methuselahs:    R-pentomino(1103 gen), Diehard(130 gen)\n");
    printf("    Infinite:       Gosper Glider Gun (creates gliders forever)\n\n");

    printf("  Game of Life is Turing-complete — anything computable\n");
    printf("  can be computed with the right initial configuration!\n");
    printf("============================================================\n");

    return 0;
}
