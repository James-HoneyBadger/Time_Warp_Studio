// =============================================
//  JavaScript Comprehensive Demo - Time Warp Studio
// =============================================

// --- Hello World ---
console.log("===== HELLO WORLD =====");
console.log("Hello from JavaScript");
console.log(`Template literal: ${2 + 3} is five`);
console.log("");

// --- Variables ---
console.log("===== VARIABLES =====");
let x = 10;
let y = 3;
const PI = 3.14159;
let greeting = "Hello";
console.log(`x = ${x}`);
console.log(`y = ${y}`);
console.log(`PI = ${PI}`);
console.log(`greeting = ${greeting}`);
console.log("");

// --- Arithmetic ---
console.log("===== ARITHMETIC =====");
console.log(`x + y = ${x + y}`);
console.log(`x - y = ${x - y}`);
console.log(`x * y = ${x * y}`);
console.log(`x / y = ${x / y}`);
console.log(`x % y = ${x % y}`);
console.log(`2 ** 10 = ${2 ** 10}`);
console.log("");

// --- Strings ---
console.log("===== STRINGS =====");
let s = "Hello World";
console.log(`upper: ${s.toUpperCase()}`);
console.log(`lower: ${s.toLowerCase()}`);
console.log(`includes: ${s.includes("World")}`);
console.log(`repeat: ${"ab".repeat(3)}`);
console.log(`split: ${s.split(" ")}`);
console.log(`slice: ${s.slice(0, 5)}`);
console.log(`length: ${s.length}`);
console.log("");

// --- Arrays ---
console.log("===== ARRAYS =====");
let nums = [5, 2, 8, 1, 9, 3];
console.log(`array: ${nums}`);
console.log(`length: ${nums.length}`);
console.log(`first: ${nums[0]}`);
console.log(`map *2: ${[5, 2, 8, 1, 9, 3].map(x => x * 2)}`);
console.log(`filter >3: ${[5, 2, 8, 1, 9, 3].filter(x => x > 3)}`);
console.log(`reduce sum: ${[5, 2, 8, 1, 9, 3].reduce((a, b) => a + b, 0)}`);
console.log("");

// --- Objects ---
console.log("===== OBJECTS =====");
let person = {name: "Alice", age: 30, city: "Wonderland"};
console.log(`name: ${person.name}`);
console.log(`age: ${person.age}`);
console.log(`JSON: ${JSON.stringify(person)}`);
console.log("");

// --- Control Flow ---
console.log("===== CONTROL FLOW =====");
for (let i = 1; i <= 5; i++) {
    if (i % 2 === 0) {
        console.log(`${i} is even`);
    } else {
        console.log(`${i} is odd`);
    }
}
console.log("");

// --- While Loop ---
console.log("===== WHILE LOOP =====");
let n = 1;
while (n <= 3) {
    console.log(`n = ${n}`);
    n++;
}
console.log("");

// --- For...of ---
console.log("===== FOR-OF =====");
for (let fruit of ["apple", "banana", "cherry"]) {
    console.log(fruit);
}
console.log("");

// --- Switch ---
console.log("===== SWITCH =====");
let day = 3;
switch (day) {
    case 1: console.log("Monday"); break;
    case 2: console.log("Tuesday"); break;
    case 3: console.log("Wednesday"); break;
    default: console.log("Other");
}
console.log("");

// --- Functions ---
console.log("===== FUNCTIONS =====");
function factorial(n) {
    if (n <= 1) return 1;
    return n * factorial(n - 1);
}
console.log("5 factorial = " + String(factorial(5)));
console.log("10 factorial = " + String(factorial(10)));

const double = x => x * 2;
console.log("double(5) = " + String(double(5)));

function greet(who, greeting = "Hello") {
    return greeting + ", " + who;
}
console.log(greet("World"));
console.log(greet("JS", "Welcome"));
console.log("");

// --- Classes ---
console.log("===== CLASSES =====");
class Animal {
    constructor(name) {
        this.name = name;
    }
    speak() {
        return `${this.name} speaks`;
    }
}

class Dog extends Animal {
    bark() {
        return `${this.name} says Woof`;
    }
}

let dog = new Dog("Rex");
console.log(dog.speak());
console.log(dog.bark());
console.log("");

// --- Error Handling ---
console.log("===== ERRORS =====");
try {
    let result = JSON.parse("{invalid}");
} catch (e) {
    console.log("Caught error: " + String(e));
}
console.log("");

// --- Math ---
console.log("===== MATH =====");
console.log(`sqrt(16) = ${Math.sqrt(16)}`);
console.log(`PI = ${Math.PI}`);
console.log(`floor(3.7) = ${Math.floor(3.7)}`);
console.log(`ceil(3.2) = ${Math.ceil(3.2)}`);
console.log(`abs(-5) = ${Math.abs(-5)}`);
console.log(`max(3, 7) = ${Math.max(3, 7)}`);
console.log(`min(3, 7) = ${Math.min(3, 7)}`);
console.log("");

console.log("===== DONE =====");
