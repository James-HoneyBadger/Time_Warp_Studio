# JavaScript Programming Tutorial

JavaScript was created in 1995 by Brendan Eich and has become the world's most widely deployed programming language. Time Warp Studio provides a sandboxed educational JavaScript executor.

## Hello World

```javascript
console.log("Hello from JavaScript!");
console.log("Time Warp Studio - JS Edition");
```

## Variables

```javascript
// Three kinds of variable declarations
let name = "Alice";        // block-scoped, reassignable
const PI  = 3.14159;       // block-scoped, constant
var count = 0;             // function-scoped (older style)

console.log(name, PI, count);

// Template literals (backtick strings)
console.log(`Hello, ${name}! PI is approximately ${PI.toFixed(4)}`);
```

## Data Types

```javascript
let num   = 42;           // number
let str   = "hello";      // string
let bool  = true;         // boolean
let arr   = [1, 2, 3];    // array
let obj   = {x: 10, y: 20}; // object
let nothing = null;       // null
let undef;                // undefined

console.log(typeof num, typeof str, typeof bool, typeof arr);
```

## Control Flow

```javascript
// If / else if / else
let score = 85;
if (score >= 90)       console.log("A");
else if (score >= 80)  console.log("B");
else if (score >= 70)  console.log("C");
else                   console.log("F");

// For loop
for (let i = 1; i <= 5; i++) {
    console.log("Count:", i);
}

// While
let n = 1;
while (n <= 64) {
    process.stdout.write(n + " ");
    n *= 2;
}
console.log();

// For...of (iterate array)
let colors = ["red", "green", "blue"];
for (let color of colors) {
    console.log(color);
}
```

## Functions

```javascript
// Function declaration
function factorial(n) {
    return n <= 1 ? 1 : n * factorial(n - 1);
}

// Arrow function
const square  = x => x ** 2;
const add     = (a, b) => a + b;

// Higher-order functions
const numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
const evens   = numbers.filter(x => x % 2 === 0);
const doubled = numbers.map(x => x * 2);
const sum     = numbers.reduce((acc, x) => acc + x, 0);

console.log("Evens:",   evens);
console.log("Doubled:", doubled);
console.log("Sum:",     sum);
```

## Arrays

```javascript
let fruits = ["apple", "banana", "cherry"];
fruits.push("date");            // add end
fruits.unshift("avocado");      // add start
fruits.splice(2, 1);            // remove index 2

console.log(fruits);
console.log("Length:", fruits.length);
console.log("Sorted:", [...fruits].sort());
console.log("Joined:", fruits.join(", "));
```

## Objects

```javascript
const person = {
    name: "Bob",
    age: 28,
    greet() {
        return `Hi, I'm ${this.name}, aged ${this.age}`;
    }
};

console.log(person.greet());
console.log(Object.keys(person));
console.log(Object.entries(person));

// Spread / destructure
const { name, age } = person;
console.log(name, age);
```

## Classes (ES6+)

```javascript
class Animal {
    constructor(name, sound) {
        this.name  = name;
        this.sound = sound;
    }
    speak() {
        return `${this.name} says ${this.sound}!`;
    }
}

class Dog extends Animal {
    constructor(name, breed) {
        super(name, "Woof");
        this.breed = breed;
    }
    describe() {
        return `${this.name} is a ${this.breed}`;
    }
}

const rex = new Dog("Rex", "Labrador");
console.log(rex.speak());
console.log(rex.describe());
```

## Further Reading

- [Examples/javascript/](../Examples/javascript/) — 10 JavaScript example programs
- [Language Guide: JavaScript](LANGUAGE_GUIDE.md#javascript)
