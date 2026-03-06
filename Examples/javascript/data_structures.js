// ═══════════════════════════════════════════════════════════════
//  DATA STRUCTURES — JavaScript Demo
//  Demonstrates: arrays, stack/queue patterns, sorting, searching
// ═══════════════════════════════════════════════════════════════

console.log("== ARRAY-BASED STACK (LIFO) ==");
let stack = [];
stack.push(10);
stack.push(20);
stack.push(30);
console.log("Pushed 10, 20, 30");
let popped = stack.pop();
console.log(`Popped: ${popped}`);
console.log(`Stack size: ${stack.length}`);
console.log("");

console.log("== ARRAY-BASED QUEUE (FIFO) ==");
let queue = [];
queue.push("A");
queue.push("B");
queue.push("C");
console.log("Enqueued A, B, C");
let dequeued = queue.shift();
console.log(`Dequeued: ${dequeued}`);
console.log(`Queue size: ${queue.length}`);
console.log("");

console.log("== BUBBLE SORT ==");
let nums = [64, 34, 25, 12, 22, 11, 90];
let n = nums.length;
for (let i = 0; i < n; i++) {
    for (let j = 0; j < n - i - 1; j++) {
        if (nums[j] > nums[j + 1]) {
            let temp = nums[j];
            nums[j] = nums[j + 1];
            nums[j + 1] = temp;
        }
    }
}
console.log(`Sorted: ${nums.join(", ")}`);
console.log("");

console.log("== BINARY SEARCH ==");
let sorted = [2, 5, 8, 12, 16, 23, 38, 56, 72, 91];
let target = 23;
let lo = 0;
let hi = sorted.length - 1;
let found = -1;
while (lo <= hi) {
    let mid = Math.floor((lo + hi) / 2);
    if (sorted[mid] === target) {
        found = mid;
        break;
    } else if (sorted[mid] < target) {
        lo = mid + 1;
    } else {
        hi = mid - 1;
    }
}
console.log(`Target ${target} found at index ${found}`);
console.log("");

console.log("== SET OPERATIONS ==");
let setA = [1, 2, 3, 4, 5];
let setB = [3, 4, 5, 6, 7];
let union = [];
let i;
for (i = 0; i < setA.length; i++) {
    union.push(setA[i]);
}
for (i = 0; i < setB.length; i++) {
    let exists = false;
    for (let j = 0; j < union.length; j++) {
        if (union[j] === setB[i]) { exists = true; }
    }
    if (!exists) { union.push(setB[i]); }
}
let inter = [];
for (i = 0; i < setA.length; i++) {
    for (let j = 0; j < setB.length; j++) {
        if (setA[i] === setB[j]) { inter.push(setA[i]); }
    }
}
console.log(`Union: ${union.join(", ")}`);
console.log(`Intersection: ${inter.join(", ")}`);
console.log("");

console.log("== FREQUENCY COUNTER ==");
let words = ["apple", "banana", "apple", "cherry", "banana", "apple"];
let unique = [];
let counts = [];
for (i = 0; i < words.length; i++) {
    let idx = -1;
    for (let j = 0; j < unique.length; j++) {
        if (unique[j] === words[i]) { idx = j; }
    }
    if (idx === -1) {
        unique.push(words[i]);
        counts.push(1);
    } else {
        counts[idx] = counts[idx] + 1;
    }
}
for (i = 0; i < unique.length; i++) {
    console.log(`  ${unique[i]}: ${counts[i]}`);
}
console.log("");
console.log("Data structures demo complete.");
