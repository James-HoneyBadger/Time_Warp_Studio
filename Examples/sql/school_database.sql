-- ══════════════════════════════════════
--   🏫 School Database - SQL Demo
-- ══════════════════════════════════════
-- Demonstrates: DDL, DML, JOINs, subqueries,
-- aggregation, views, CASE expressions

-- === Create Tables ===

CREATE TABLE departments (
    dept_id    INTEGER PRIMARY KEY,
    dept_name  VARCHAR(50) NOT NULL,
    building   VARCHAR(30),
    budget     DECIMAL(10,2)
);

CREATE TABLE teachers (
    teacher_id   INTEGER PRIMARY KEY,
    first_name   VARCHAR(30),
    last_name    VARCHAR(30),
    dept_id      INTEGER,
    hire_date    DATE,
    salary       DECIMAL(10,2)
);

CREATE TABLE students (
    student_id   INTEGER PRIMARY KEY,
    first_name   VARCHAR(30),
    last_name    VARCHAR(30),
    grade_level  INTEGER,
    gpa          DECIMAL(3,2),
    enrolled_date DATE
);

CREATE TABLE courses (
    course_id    INTEGER PRIMARY KEY,
    course_name  VARCHAR(60),
    dept_id      INTEGER,
    teacher_id   INTEGER,
    credits      INTEGER,
    max_students INTEGER DEFAULT 30
);

CREATE TABLE enrollments (
    enrollment_id INTEGER PRIMARY KEY,
    student_id    INTEGER,
    course_id     INTEGER,
    semester      VARCHAR(20),
    grade         VARCHAR(2)
);

-- === Populate Data ===

INSERT INTO departments VALUES (1, 'Mathematics', 'Newton Hall', 150000);
INSERT INTO departments VALUES (2, 'Science', 'Einstein Lab', 200000);
INSERT INTO departments VALUES (3, 'English', 'Shakespeare Bldg', 120000);
INSERT INTO departments VALUES (4, 'History', 'Heritage Center', 100000);
INSERT INTO departments VALUES (5, 'Computer Science', 'Turing Wing', 180000);

INSERT INTO teachers VALUES (1, 'Ada', 'Lovelace', 5, '2018-08-15', 72000);
INSERT INTO teachers VALUES (2, 'Isaac', 'Newton', 1, '2015-01-10', 78000);
INSERT INTO teachers VALUES (3, 'Marie', 'Curie', 2, '2016-03-22', 80000);
INSERT INTO teachers VALUES (4, 'William', 'Shakespeare', 3, '2014-09-01', 75000);
INSERT INTO teachers VALUES (5, 'Alan', 'Turing', 5, '2019-06-15', 82000);
INSERT INTO teachers VALUES (6, 'Albert', 'Einstein', 2, '2017-01-08', 85000);
INSERT INTO teachers VALUES (7, 'Euclid', 'Alexandria', 1, '2020-08-20', 68000);

INSERT INTO students VALUES (1, 'Alice', 'Chen', 11, 3.85, '2022-09-01');
INSERT INTO students VALUES (2, 'Bob', 'Smith', 10, 3.42, '2023-09-01');
INSERT INTO students VALUES (3, 'Carol', 'Jones', 12, 3.91, '2021-09-01');
INSERT INTO students VALUES (4, 'David', 'Kim', 11, 2.98, '2022-09-01');
INSERT INTO students VALUES (5, 'Eve', 'Garcia', 10, 3.67, '2023-09-01');
INSERT INTO students VALUES (6, 'Frank', 'Lee', 12, 3.15, '2021-09-01');
INSERT INTO students VALUES (7, 'Grace', 'Wilson', 11, 3.78, '2022-09-01');
INSERT INTO students VALUES (8, 'Hank', 'Brown', 10, 2.85, '2023-09-01');

INSERT INTO courses VALUES (1, 'Calculus I', 1, 2, 4, 25);
INSERT INTO courses VALUES (2, 'Physics', 2, 3, 4, 30);
INSERT INTO courses VALUES (3, 'English Literature', 3, 4, 3, 35);
INSERT INTO courses VALUES (4, 'Intro to Programming', 5, 1, 3, 20);
INSERT INTO courses VALUES (5, 'Data Structures', 5, 5, 4, 20);
INSERT INTO courses VALUES (6, 'Chemistry', 2, 6, 4, 28);
INSERT INTO courses VALUES (7, 'Geometry', 1, 7, 3, 30);

INSERT INTO enrollments VALUES (1, 1, 1, 'Fall 2024', 'A');
INSERT INTO enrollments VALUES (2, 1, 4, 'Fall 2024', 'A+');
INSERT INTO enrollments VALUES (3, 2, 2, 'Fall 2024', 'B+');
INSERT INTO enrollments VALUES (4, 2, 7, 'Fall 2024', 'B');
INSERT INTO enrollments VALUES (5, 3, 5, 'Fall 2024', 'A');
INSERT INTO enrollments VALUES (6, 3, 3, 'Fall 2024', 'A-');
INSERT INTO enrollments VALUES (7, 4, 1, 'Fall 2024', 'C+');
INSERT INTO enrollments VALUES (8, 4, 6, 'Fall 2024', 'B-');
INSERT INTO enrollments VALUES (9, 5, 4, 'Fall 2024', 'A');
INSERT INTO enrollments VALUES (10, 5, 3, 'Fall 2024', 'A-');
INSERT INTO enrollments VALUES (11, 6, 5, 'Fall 2024', 'B');
INSERT INTO enrollments VALUES (12, 7, 2, 'Fall 2024', 'A');
INSERT INTO enrollments VALUES (13, 7, 4, 'Fall 2024', 'B+');
INSERT INTO enrollments VALUES (14, 8, 7, 'Fall 2024', 'C');
INSERT INTO enrollments VALUES (15, 8, 6, 'Fall 2024', 'C+');

-- === Queries ===

-- 1. Course roster with teacher names
SELECT '📋 Course Roster';
SELECT c.course_name, t.first_name || ' ' || t.last_name AS teacher,
       COUNT(e.student_id) AS enrolled
FROM courses c
JOIN teachers t ON c.teacher_id = t.teacher_id
LEFT JOIN enrollments e ON c.course_id = e.course_id
GROUP BY c.course_id
ORDER BY c.course_name;

-- 2. Student transcripts
SELECT '📝 Student Grades';
SELECT s.first_name || ' ' || s.last_name AS student,
       c.course_name, e.grade
FROM students s
JOIN enrollments e ON s.student_id = e.student_id
JOIN courses c ON e.course_id = c.course_id
ORDER BY s.last_name, c.course_name;

-- 3. Department summary
SELECT '🏛️ Department Summary';
SELECT d.dept_name,
       COUNT(DISTINCT t.teacher_id) AS teachers,
       COUNT(DISTINCT c.course_id) AS courses,
       d.budget
FROM departments d
LEFT JOIN teachers t ON d.dept_id = t.dept_id
LEFT JOIN courses c ON d.dept_id = c.dept_id
GROUP BY d.dept_id
ORDER BY d.dept_name;

-- 4. Honor roll (GPA >= 3.7)
SELECT '🏆 Honor Roll (GPA >= 3.7)';
SELECT first_name || ' ' || last_name AS student,
       grade_level, gpa
FROM students
WHERE gpa >= 3.7
ORDER BY gpa DESC;

-- 5. CASE expression: salary bands
SELECT '💰 Teacher Salary Bands';
SELECT first_name || ' ' || last_name AS teacher,
       salary,
       CASE
           WHEN salary >= 80000 THEN 'Senior'
           WHEN salary >= 70000 THEN 'Mid-Level'
           ELSE 'Junior'
       END AS band
FROM teachers
ORDER BY salary DESC;

-- 6. Subquery: above-average GPA students
SELECT '⭐ Above-Average GPA';
SELECT first_name || ' ' || last_name AS student, gpa
FROM students
WHERE gpa > (SELECT AVG(gpa) FROM students)
ORDER BY gpa DESC;

SELECT '✅ School database queries complete!';
