// ============================================================
// ALGORITHMS LIBRARY — Rust Language Showcase
// Stack * Queue * BST * HashMap * Sorting * Number Theory
// Time Warp Studio — Rust Language Demo
// ============================================================

use std::collections::HashMap;

// ===== STACK =====

struct Stack<T> {
    data: Vec<T>,
}

impl<T> Stack<T> {
    fn new() -> Self { Stack { data: Vec::new() } }
    fn push(&mut self, val: T) { self.data.push(val); }
    fn pop(&mut self) -> Option<T> { self.data.pop() }
    fn peek(&self) -> Option<&T> { self.data.last() }
    fn is_empty(&self) -> bool { self.data.is_empty() }
    fn size(&self) -> usize { self.data.len() }
}

// ===== QUEUE =====

struct Queue<T> {
    data: Vec<T>,
    head: usize,
}

impl<T> Queue<T> {
    fn new() -> Self { Queue { data: Vec::new(), head: 0 } }
    fn enqueue(&mut self, val: T) { self.data.push(val); }
    fn dequeue(&mut self) -> Option<&T> {
        if self.head >= self.data.len() { return None; }
        let r = &self.data[self.head];
        self.head += 1;
        Some(r)
    }
    fn is_empty(&self) -> bool { self.head >= self.data.len() }
    fn size(&self) -> usize { self.data.len() - self.head }
}

// ===== BINARY SEARCH TREE =====

enum BST {
    Empty,
    Node { val: i64, left: Box<BST>, right: Box<BST> },
}

impl BST {
    fn new() -> Self { BST::Empty }

    fn insert(self, v: i64) -> Self {
        match self {
            BST::Empty => BST::Node {
                val: v,
                left: Box::new(BST::Empty),
                right: Box::new(BST::Empty),
            },
            BST::Node { val, left, right } => {
                if v < val {
                    BST::Node { val, left: Box::new(left.insert(v)), right }
                } else if v > val {
                    BST::Node { val, left, right: Box::new(right.insert(v)) }
                } else {
                    BST::Node { val, left, right }
                }
            }
        }
    }

    fn contains(&self, v: i64) -> bool {
        match self {
            BST::Empty => false,
            BST::Node { val, left, right } => {
                if v == *val { true }
                else if v < *val { left.contains(v) }
                else { right.contains(v) }
            }
        }
    }

    fn inorder(&self) -> Vec<i64> {
        match self {
            BST::Empty => vec![],
            BST::Node { val, left, right } => {
                let mut result = left.inorder();
                result.push(*val);
                result.extend(right.inorder());
                result
            }
        }
    }

    fn height(&self) -> usize {
        match self {
            BST::Empty => 0,
            BST::Node { left, right, .. } => {
                1 + left.height().max(right.height())
            }
        }
    }
}

// ===== SORTING ALGORITHMS =====

fn quicksort(mut arr: Vec<i32>) -> Vec<i32> {
    if arr.len() <= 1 { return arr; }
    let pivot = arr.remove(arr.len() / 2);
    let left:  Vec<i32> = arr.iter().filter(|&&x| x < pivot).cloned().collect();
    let mid:   Vec<i32> = arr.iter().filter(|&&x| x == pivot).cloned().collect();
    let right: Vec<i32> = arr.iter().filter(|&&x| x > pivot).cloned().collect();
    let mut result = quicksort(left);
    result.extend(mid);
    result.extend(quicksort(right));
    result
}

fn merge_sort(arr: Vec<i32>) -> Vec<i32> {
    if arr.len() <= 1 { return arr; }
    let mid = arr.len() / 2;
    let left  = merge_sort(arr[..mid].to_vec());
    let right = merge_sort(arr[mid..].to_vec());
    merge(left, right)
}

fn merge(left: Vec<i32>, right: Vec<i32>) -> Vec<i32> {
    let mut result = Vec::with_capacity(left.len() + right.len());
    let (mut i, mut j) = (0, 0);
    while i < left.len() && j < right.len() {
        if left[i] <= right[j] { result.push(left[i]); i += 1; }
        else                    { result.push(right[j]); j += 1; }
    }
    result.extend_from_slice(&left[i..]);
    result.extend_from_slice(&right[j..]);
    result
}

// ===== NUMBER THEORY =====

fn gcd(a: u64, b: u64) -> u64 {
    if b == 0 { a } else { gcd(b, a % b) }
}

fn lcm(a: u64, b: u64) -> u64 { a / gcd(a, b) * b }

fn is_prime(n: u64) -> bool {
    if n < 2 { return false; }
    if n == 2 { return true; }
    if n % 2 == 0 { return false; }
    let mut i = 3u64;
    while i * i <= n { if n % i == 0 { return false; } i += 2; }
    true
}

fn sieve(limit: usize) -> Vec<usize> {
    let mut composite = vec![false; limit + 1];
    composite[0] = true;
    composite[1] = true;
    let mut i = 2;
    while i * i <= limit {
        if !composite[i] {
            let mut j = i * i;
            while j <= limit { composite[j] = true; j += i; }
        }
        i += 1;
    }
    (2..=limit).filter(|&n| !composite[n]).collect()
}

fn fibonacci_iter(n: u32) -> u64 {
    if n == 0 { return 0; }
    let (mut a, mut b) = (0u64, 1u64);
    for _ in 1..n { let tmp = a + b; a = b; b = tmp; }
    b
}

fn totient(mut n: u64) -> u64 {
    let orig = n;
    let mut result = n;
    let mut p = 2u64;
    while p * p <= n {
        if n % p == 0 {
            while n % p == 0 { n /= p; }
            result -= result / p;
        }
        p += 1;
    }
    if n > 1 { result -= result / n; }
    result
}

// ===== MATRIX OPERATIONS =====

fn mat_mul(a: &[[i64; 3]; 3], b: &[[i64; 3]; 3]) -> [[i64; 3]; 3] {
    let mut c = [[0i64; 3]; 3];
    for i in 0..3 {
        for j in 0..3 {
            for k in 0..3 {
                c[i][j] += a[i][k] * b[k][j];
            }
        }
    }
    c
}

fn mat_det(m: &[[i64; 3]; 3]) -> i64 {
    m[0][0] * (m[1][1]*m[2][2] - m[1][2]*m[2][1])
  - m[0][1] * (m[1][0]*m[2][2] - m[1][2]*m[2][0])
  + m[0][2] * (m[1][0]*m[2][1] - m[1][1]*m[2][0])
}

// ===== STATISTICS =====

fn statistics(data: &[f64]) -> (f64, f64, f64, f64, f64) {
    let n = data.len() as f64;
    let mean = data.iter().sum::<f64>() / n;
    let variance = data.iter().map(|x| (x - mean).powi(2)).sum::<f64>() / n;
    let stddev = variance.sqrt();
    let mut sorted = data.to_vec();
    sorted.sort_by(|a, b| a.partial_cmp(b).unwrap());
    let median = if sorted.len() % 2 == 0 {
        (sorted[sorted.len()/2 - 1] + sorted[sorted.len()/2]) / 2.0
    } else {
        sorted[sorted.len() / 2]
    };
    (mean, median, stddev, sorted[0], sorted[sorted.len()-1])
}

// ===== MAIN =====

fn main() {
    println!("{}", "=".repeat(60));
    println!("  ALGORITHMS LIBRARY — Rust");
    println!("  Generics | Enums | Pattern Matching | Ownership");
    println!("{}", "=".repeat(60));

    // ---- Section 1: Stack ----
    println!("\n[ 1 ] GENERIC STACK");
    let mut stack: Stack<i32> = Stack::new();
    for v in [10, 20, 30, 40, 50] { stack.push(v); }
    println!("  Pushed 5 items. Size: {}", stack.size());
    println!("  Peek: {:?}", stack.peek());
    while !stack.is_empty() {
        print!("  Pop: {} ", stack.pop().unwrap());
    }
    println!("\n  Stack is now empty: {}", stack.is_empty());

    // ---- Section 2: Queue ----
    println!("\n[ 2 ] GENERIC QUEUE");
    let mut queue: Queue<&str> = Queue::new();
    for word in ["alpha", "beta", "gamma", "delta", "epsilon"] {
        queue.enqueue(word);
    }
    println!("  Enqueued 5 items. Size: {}", queue.size());
    while !queue.is_empty() {
        print!("  Dequeue: {} ", queue.dequeue().unwrap());
    }
    println!();

    // ---- Section 3: BST ----
    println!("\n[ 3 ] BINARY SEARCH TREE (Recursive Enum)");
    let values = [50i64, 30, 70, 20, 40, 60, 80, 10, 25, 35, 45];
    let mut tree = BST::new();
    for &v in &values { tree = tree.insert(v); }
    println!("  Inserted: {:?}", values);
    println!("  In-order: {:?}", tree.inorder());
    println!("  Height:   {}", tree.height());
    println!("  Contains 40: {}   Contains 55: {}", tree.contains(40), tree.contains(55));

    // ---- Section 4: Sorting ----
    println!("\n[ 4 ] SORTING ALGORITHMS");
    let data = vec![64, 25, 12, 22, 11, 90, 55, 44, 77, 33, 88, 66, 99];
    println!("  Input:     {:?}", data);
    println!("  QuickSort: {:?}", quicksort(data.clone()));
    println!("  MergeSort: {:?}", merge_sort(data.clone()));
    let mut std_sort = data.clone();
    std_sort.sort();
    println!("  std::sort: {:?}", std_sort);

    // ---- Section 5: HashMap ----
    println!("\n[ 5 ] HASHMAP — Word Frequency");
    let text = "the quick brown fox jumps over the lazy dog the fox";
    let mut freq: HashMap<&str, u32> = HashMap::new();
    for word in text.split_whitespace() {
        *freq.entry(word).or_insert(0) += 1;
    }
    let mut freq_pairs: Vec<(&&str, &u32)> = freq.iter().collect();
    freq_pairs.sort_by(|a, b| b.1.cmp(a.1).then(a.0.cmp(b.0)));
    println!("  Text: \"{}\"", text);
    for (word, count) in freq_pairs {
        println!("    {:12} : {} {}", word, count, "█".repeat(*count as usize));
    }

    // ---- Section 6: Number Theory ----
    println!("\n[ 6 ] NUMBER THEORY");
    println!("  Primes up to 100:");
    let primes = sieve(100);
    println!("    {:?}", primes);
    println!("  Count: {}  Largest: {}", primes.len(), primes.last().unwrap());

    let twin_primes: Vec<(usize, usize)> = primes.windows(2)
        .filter(|w| w[1] - w[0] == 2)
        .map(|w| (w[0], w[1]))
        .collect();
    println!("  Twin primes: {:?}", twin_primes);

    println!("  GCD(252, 105) = {}", gcd(252, 105));
    println!("  LCM(12, 18)   = {}", lcm(12, 18));
    println!("  φ(30)         = {} (Euler's totient)", totient(30));

    print!("  Fibonacci F0..F15: ");
    for i in 0..=15 { print!("{} ", fibonacci_iter(i)); }
    println!();

    // ---- Section 7: Matrix ----
    println!("\n[ 7 ] 3×3 MATRIX OPERATIONS");
    let a: [[i64; 3]; 3] = [[1, 2, 3], [4, 5, 6], [7, 8, 9]];
    let b: [[i64; 3]; 3] = [[9, 8, 7], [6, 5, 4], [3, 2, 1]];
    let c = mat_mul(&a, &b);
    println!("  A × B =");
    for row in &c { println!("    {:?}", row); }
    println!("  det(A) = {}", mat_det(&a));

    // ---- Section 8: Statistics ----
    println!("\n[ 8 ] STATISTICS");
    let dataset: Vec<f64> = vec![4.0, 8.0, 6.0, 5.0, 3.0, 2.0, 8.0, 9.0, 2.0, 5.0,
                                  7.0, 4.0, 8.0, 3.0, 6.0, 7.0, 5.0, 8.0, 3.0, 4.0];
    let (mean, median, stddev, min, max) = statistics(&dataset);
    println!("  n={}  mean={:.3}  median={:.1}  stddev={:.3}", dataset.len(), mean, median, stddev);
    println!("  min={}  max={}  range={}", min as i32, max as i32, (max - min) as i32);

    // Distribution
    println!("  Histogram:");
    let mut buckets = [0u32; 5];
    for &x in &dataset {
        let b = ((x - min) / ((max - min + 1.0) / 5.0)) as usize;
        buckets[b.min(4)] += 1;
    }
    for (i, &cnt) in buckets.iter().enumerate() {
        let lo = min + i as f64 * (max - min) / 5.0;
        let hi = lo + (max - min) / 5.0;
        println!("    {:.0}-{:.0}: {}", lo, hi, "█".repeat(cnt as usize));
    }

    // ---- Section 9: Pattern matching ----
    println!("\n[ 9 ] PATTERN MATCHING — Number Classification");
    let numbers = [0i64, 1, 2, 3, 7, 12, 28, 496, -5, 100, 1000, 8128];
    for &n in &numbers {
        let desc = match n {
            0 => "zero",
            1 => "one",
            n if n < 0 => "negative",
            n if n > 1000 => "large",
            n if is_prime(n as u64) => "prime",
            _ => "composite",
        };
        println!("    {:5} → {}", n, desc);
    }

    println!("\n{}", "=".repeat(60));
    println!("  Rust Algorithms Library Complete!");
    println!("  Generics | Enums | Pattern match | Vec | HashMap | Ownership");
    println!("{}", "=".repeat(60));
}
