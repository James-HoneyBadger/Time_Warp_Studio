# SQL Programming Tutorial

SQL (Structured Query Language) was developed by IBM in the 1970s and standardized by ANSI/ISO. It is the universal language for relational databases — used in MySQL, PostgreSQL, Oracle, SQL Server, SQLite, and more.

## Quick Start

```sql
-- Create a table
CREATE TABLE students (
    id        INTEGER PRIMARY KEY,
    name      VARCHAR(50)  NOT NULL,
    grade     CHAR(1),
    score     DECIMAL(5,2),
    enrolled  DATE
);

-- Insert rows
INSERT INTO students VALUES (1, 'Alice',  'A', 95.5, '2025-09-01');
INSERT INTO students VALUES (2, 'Bob',    'B', 82.0, '2025-09-01');
INSERT INTO students VALUES (3, 'Carol',  'A', 91.0, '2025-09-03');
INSERT INTO students VALUES (4, 'Dave',   'C', 74.5, '2025-09-02');
INSERT INTO students VALUES (5, 'Eve',    'B', 88.0, '2025-09-01');

-- Query
SELECT * FROM students;
SELECT name, score FROM students WHERE grade = 'A';
SELECT name, score FROM students ORDER BY score DESC;
```

## SELECT Basics

```sql
-- All columns
SELECT * FROM students;

-- Specific columns with alias
SELECT name AS student_name,
       score * 10 AS points_out_of_1000
FROM students;

-- DISTINCT values
SELECT DISTINCT grade FROM students;

-- LIMIT / TOP / FETCH FIRST
SELECT * FROM students LIMIT 3;                   -- MySQL, SQLite, PG
SELECT TOP 3 * FROM students;                     -- SQL Server
SELECT * FROM students FETCH FIRST 3 ROWS ONLY;  -- IBM DB2 / Standard
```

## WHERE Clause

```sql
-- Comparison operators  =  <>  <  >  <=  >=
SELECT * FROM students WHERE score >= 90;
SELECT * FROM students WHERE grade <> 'A';

-- BETWEEN
SELECT * FROM students WHERE score BETWEEN 80 AND 90;

-- IN / NOT IN
SELECT * FROM students WHERE grade IN ('A', 'B');
SELECT * FROM students WHERE name NOT IN ('Dave', 'Eve');

-- LIKE
SELECT * FROM students WHERE name LIKE 'A%';      -- starts with A
SELECT * FROM students WHERE name LIKE '%ve';     -- ends with ve
SELECT * FROM students WHERE name LIKE '_a%';     -- second char is a

-- NULL checks
SELECT * FROM students WHERE grade IS NULL;
SELECT * FROM students WHERE grade IS NOT NULL;

-- AND / OR / NOT
SELECT * FROM students
WHERE grade = 'A' OR (grade = 'B' AND score > 85);
```

## Aggregate Functions

```sql
SELECT COUNT(*)           AS total_students,
       AVG(score)         AS average_score,
       MAX(score)         AS highest_score,
       MIN(score)         AS lowest_score,
       SUM(score)         AS total_points
FROM students;

-- GROUP BY
SELECT grade,
       COUNT(*) AS count,
       AVG(score) AS avg_score
FROM students
GROUP BY grade
ORDER BY grade;

-- HAVING (filter groups)
SELECT grade, COUNT(*) AS cnt
FROM students
GROUP BY grade
HAVING COUNT(*) >= 2;
```

## JOIN

```sql
-- Setup
CREATE TABLE courses (
    id    INTEGER PRIMARY KEY,
    title VARCHAR(50),
    credits INTEGER
);
CREATE TABLE enrollments (
    student_id INTEGER REFERENCES students(id),
    course_id  INTEGER REFERENCES courses(id),
    semester   CHAR(6)
);

INSERT INTO courses VALUES (1, 'Intro to COBOL',  3);
INSERT INTO courses VALUES (2, 'Mainframe Ops',   2);
INSERT INTO courses VALUES (3, 'CICS Development',4);

-- INNER JOIN
SELECT s.name, c.title, e.semester
FROM enrollments e
    INNER JOIN students s ON e.student_id = s.id
    INNER JOIN courses  c ON e.course_id  = c.id
ORDER BY s.name;

-- LEFT JOIN (all students, even if not enrolled)
SELECT s.name, c.title
FROM students s
    LEFT JOIN enrollments e ON s.id = e.student_id
    LEFT JOIN courses     c ON e.course_id = c.id;
```

## Subqueries

```sql
-- Scalar subquery
SELECT name, score
FROM students
WHERE score > (SELECT AVG(score) FROM students);

-- EXISTS
SELECT name FROM students s
WHERE EXISTS (
    SELECT 1 FROM enrollments e
    WHERE e.student_id = s.id
);
```

## Window Functions

```sql
SELECT name,
       score,
       grade,
       RANK() OVER (ORDER BY score DESC) AS rank_overall,
       RANK() OVER (PARTITION BY grade ORDER BY score DESC) AS rank_in_grade,
       AVG(score) OVER (PARTITION BY grade) AS avg_grade_score
FROM students;
```

## DDL: CREATE, ALTER, DROP

```sql
-- Create with constraints
CREATE TABLE products (
    id          SERIAL PRIMARY KEY,
    name        VARCHAR(100) NOT NULL UNIQUE,
    price       DECIMAL(10,2) CHECK (price >= 0),
    category_id INTEGER REFERENCES categories(id) ON DELETE SET NULL
);

-- Add column
ALTER TABLE products ADD COLUMN stock INTEGER DEFAULT 0;

-- Drop table
DROP TABLE IF EXISTS products;
```

## Further Reading

- [Examples/sql/](../Examples/sql/) — 5 SQL example programs
- [Language Guide: SQL](LANGUAGE_GUIDE.md#sql)
