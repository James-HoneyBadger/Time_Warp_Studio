// ============================================================
// GAME OF LIFE — Rust Language Showcase
// Conway's cellular automaton: struct/impl, patterns, statistics
// Time Warp Studio — Rust Language Demo
// ============================================================

use std::fmt;

// ============================================================
// GRID STRUCT
// ============================================================

#[derive(Clone)]
struct Grid {
    cells: Vec<Vec<bool>>,
    width: usize,
    height: usize,
}

impl Grid {
    fn new(width: usize, height: usize) -> Self {
        Grid {
            cells: vec![vec![false; width]; height],
            width,
            height,
        }
    }

    fn set(&mut self, row: usize, col: usize, alive: bool) {
        if row < self.height && col < self.width {
            self.cells[row][col] = alive;
        }
    }

    fn get(&self, row: isize, col: isize) -> bool {
        // Toroidal boundary conditions
        let r = ((row % self.height as isize) + self.height as isize) as usize % self.height;
        let c = ((col % self.width as isize) + self.width as isize) as usize % self.width;
        self.cells[r][c]
    }

    fn count_neighbors(&self, row: usize, col: usize) -> u8 {
        let mut count = 0u8;
        for dr in -1..=1isize {
            for dc in -1..=1isize {
                if dr == 0 && dc == 0 {
                    continue;
                }
                if self.get(row as isize + dr, col as isize + dc) {
                    count += 1;
                }
            }
        }
        count
    }

    fn step(&self) -> Grid {
        let mut next = Grid::new(self.width, self.height);
        for row in 0..self.height {
            for col in 0..self.width {
                let n = self.count_neighbors(row, col);
                let alive = self.cells[row][col];
                next.cells[row][col] = match (alive, n) {
                    (true,  2) | (true,  3) => true,  // survival
                    (false, 3)               => true,  // birth
                    _                        => false, // death
                };
            }
        }
        next
    }

    fn count_alive(&self) -> usize {
        self.cells.iter().flatten().filter(|&&c| c).count()
    }

    fn is_equal(&self, other: &Grid) -> bool {
        self.cells == other.cells
    }

    fn clear(&mut self) {
        for row in self.cells.iter_mut() {
            for cell in row.iter_mut() {
                *cell = false;
            }
        }
    }
}

impl fmt::Display for Grid {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for row in &self.cells {
            write!(f, "  |")?;
            for &cell in row {
                write!(f, "{}", if cell { '#' } else { ' ' })?;
            }
            writeln!(f, "|")?;
        }
        Ok(())
    }
}

// ============================================================
// PATTERN PLACER
// ============================================================

fn place_pattern(grid: &mut Grid, pattern: &[(usize, usize)], row_off: usize, col_off: usize) {
    for &(r, c) in pattern {
        grid.set(r + row_off, c + col_off, true);
    }
}

// Classic patterns
const GLIDER: &[(usize, usize)] = &[(0,1),(1,2),(2,0),(2,1),(2,2)];
const BLINKER: &[(usize, usize)] = &[(1,0),(1,1),(1,2)];
const TOAD: &[(usize, usize)] = &[(0,1),(0,2),(0,3),(1,0),(1,1),(1,2)];
const BEACON: &[(usize, usize)] = &[(0,0),(0,1),(1,0),(2,3),(3,2),(3,3)];
const R_PENTOMINO: &[(usize, usize)] = &[(0,1),(0,2),(1,0),(1,1),(2,1)];
const LWSS: &[(usize, usize)] = &[  // Lightweight spaceship
    (0,1),(0,4),(1,0),(2,0),(2,4),(3,0),(3,1),(3,2),(3,3)
];
const PULSAR_QUARTER: &[(usize, usize)] = &[  // One quadrant of pulsar
    (0,2),(0,3),(0,4),(2,0),(3,0),(4,0),(2,5),(3,5),(4,5),(5,2),(5,3),(5,4)
];

// ============================================================
// STATISTICS
// ============================================================

#[derive(Debug)]
struct SimStats {
    pattern_name: String,
    generations: usize,
    initial_alive: usize,
    final_alive: usize,
    max_alive: usize,
    period: Option<usize>,
}

impl fmt::Display for SimStats {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "  Pattern:      {}", self.pattern_name)?;
        writeln!(f, "  Generations:  {}", self.generations)?;
        writeln!(f, "  Initial alive:{}", self.initial_alive)?;
        writeln!(f, "  Final alive:  {}", self.final_alive)?;
        writeln!(f, "  Max alive:    {}", self.max_alive)?;
        if let Some(p) = self.period {
            writeln!(f, "  Period:       {}", p)?;
        } else {
            writeln!(f, "  Period:       (not detected)")?;
        }
        Ok(())
    }
}

fn run_simulation(
    name: &str,
    initial: Grid,
    generations: usize,
    print_gens: &[usize],
) -> SimStats {
    let mut grid = initial.clone();
    let initial_alive = grid.count_alive();
    let mut max_alive = initial_alive;
    let mut period: Option<usize> = None;

    // Print generation 0
    if print_gens.contains(&0) {
        let w = grid.width;
        println!("  Generation 0  (alive: {})", initial_alive);
        println!("  +{}+", "-".repeat(w));
        print!("{}", grid);
        println!("  +{}+", "-".repeat(w));
    }

    // Store history for period detection (up to 16 steps)
    let mut history: Vec<Grid> = vec![grid.clone()];

    for gen in 1..=generations {
        grid = grid.step();
        let alive = grid.count_alive();
        if alive > max_alive {
            max_alive = alive;
        }

        if print_gens.contains(&gen) {
            let w = grid.width;
            println!("  Generation {}  (alive: {})", gen, alive);
            println!("  +{}+", "-".repeat(w));
            print!("{}", grid);
            println!("  +{}+", "-".repeat(w));
        }

        // Period detection
        if period.is_none() {
            for (h_gen, h_grid) in history.iter().enumerate() {
                if grid.is_equal(h_grid) {
                    period = Some(gen - h_gen);
                    break;
                }
            }
        }
        if history.len() < 20 {
            history.push(grid.clone());
        }
    }

    SimStats {
        pattern_name: name.to_string(),
        generations,
        initial_alive,
        final_alive: grid.count_alive(),
        max_alive,
        period,
    }
}

// ============================================================
// MAIN
// ============================================================

fn main() {
    println!("============================================================");
    println!("  GAME OF LIFE — Rust Showcase");
    println!("  Structs, generics, iterators, cellular automaton");
    println!("============================================================");
    println!();
    println!("Rules: Live with 2-3 neighbors survives; dead with 3 born.");
    println!("       Grid is toroidal (wraps edges).");
    println!();

    let mut all_stats: Vec<SimStats> = Vec::new();

    // ---- Glider ----
    println!("PATTERN 1: GLIDER");
    println!("------------------------------------------------------------");
    println!("Classic spaceship — travels diagonally.");
    println!();
    let mut g1 = Grid::new(20, 14);
    place_pattern(&mut g1, GLIDER, 1, 1);
    let stats = run_simulation("Glider", g1, 20, &[0, 4, 8, 12, 16, 20]);
    println!();
    all_stats.push(stats);

    // ---- Blinker ----
    println!("PATTERN 2: BLINKER (period-2 oscillator)");
    println!("------------------------------------------------------------");
    let mut g2 = Grid::new(12, 5);
    place_pattern(&mut g2, BLINKER, 2, 2);
    place_pattern(&mut g2, BLINKER, 2, 7);
    let stats = run_simulation("Blinker", g2, 6, &[0, 1, 2, 3, 4]);
    println!();
    all_stats.push(stats);

    // ---- Toad ----
    println!("PATTERN 3: TOAD (period-2 oscillator)");
    println!("------------------------------------------------------------");
    let mut g3 = Grid::new(14, 6);
    place_pattern(&mut g3, TOAD, 2, 4);
    let stats = run_simulation("Toad", g3, 6, &[0, 1, 2, 3]);
    println!();
    all_stats.push(stats);

    // ---- Beacon ----
    println!("PATTERN 4: BEACON (period-2 oscillator)");
    println!("------------------------------------------------------------");
    let mut g4 = Grid::new(14, 8);
    place_pattern(&mut g4, BEACON, 2, 3);
    let stats = run_simulation("Beacon", g4, 4, &[0, 1, 2]);
    println!();
    all_stats.push(stats);

    // ---- R-Pentomino ----
    println!("PATTERN 5: R-PENTOMINO (methuselah)");
    println!("------------------------------------------------------------");
    println!("5 cells producing 1103 generations of chaos.");
    println!();
    let mut g5 = Grid::new(40, 24);
    place_pattern(&mut g5, R_PENTOMINO, 10, 18);
    let stats = run_simulation("R-Pentomino", g5, 50, &[0, 10, 25, 50]);
    println!();
    all_stats.push(stats);

    // ---- LWSS (Lightweight Spaceship) ----
    println!("PATTERN 6: LIGHTWEIGHT SPACESHIP (LWSS)");
    println!("------------------------------------------------------------");
    println!("Travels horizontally, period 4.");
    println!();
    let mut g6 = Grid::new(30, 8);
    place_pattern(&mut g6, LWSS, 2, 1);
    let stats = run_simulation("LWSS", g6, 12, &[0, 4, 8, 12]);
    println!();
    all_stats.push(stats);

    // ---- Mixed garden ----
    println!("PATTERN 7: MIXED GARDEN");
    println!("------------------------------------------------------------");
    println!("Multiple patterns coexisting: still lifes + oscillators + glider");
    println!();
    let mut g7 = Grid::new(36, 18);
    // block (still life)
    g7.set(1, 1, true); g7.set(1, 2, true);
    g7.set(2, 1, true); g7.set(2, 2, true);
    // blinker
    place_pattern(&mut g7, BLINKER, 8, 15);
    // toad
    place_pattern(&mut g7, TOAD, 12, 20);
    // glider
    place_pattern(&mut g7, GLIDER, 1, 25);
    let stats = run_simulation("Mixed Garden", g7, 20, &[0, 10, 20]);
    println!();
    all_stats.push(stats);

    // ---- Summary ----
    println!("============================================================");
    println!("  SIMULATION SUMMARY");
    println!("============================================================");
    println!();
    for stats in &all_stats {
        print!("{}", stats);
        println!();
    }

    // Count total unique periods found
    let periods: Vec<usize> = all_stats.iter()
        .filter_map(|s| s.period)
        .collect();
    println!("  Periods detected: {:?}", periods);

    // Find most alive at peak
    let most_alive = all_stats.iter().max_by_key(|s| s.max_alive).unwrap();
    println!("  Most alive at peak: {} (in {} pattern)",
             most_alive.max_alive, most_alive.pattern_name);

    println!();
    println!("============================================================");
    println!("  FACTS ABOUT GAME OF LIFE");
    println!("============================================================");
    println!("  Creator:          John Horton Conway (1970)");
    println!("  Type:             Outer totalistic cellular automaton");
    println!("  Turing-complete:  Yes (via Gosper Glider Gun patterns)");
    println!("  Rule notation:    B3/S23 (born with 3, survive with 2-3)");
    println!("  Still lifes:      Block (min 4), beehive, loaf, boat");
    println!("  Oscillators:      Blinker(p2), Toad(p2), Pulsar(p3)...");
    println!("  Spaceships:       Glider(p4), LWSS/MWSS/HWSS(p4)");
    println!("  Methuselahs:      R-pentomino, Diehard, Acorn");
    println!("============================================================");
}
