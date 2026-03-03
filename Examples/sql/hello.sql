-- =============================================
--  SQL Comprehensive Demo - Time Warp Studio
-- =============================================

-- Direct output
PRINT '===== HELLO WORLD ====='
PRINT 'Welcome to SQL!'
PRINT ''

-- Variables
PRINT '===== VARIABLES ====='
DECLARE @x INT
DECLARE @y INT
DECLARE @name VARCHAR(50)
SET @x = 10
SET @y = 3
SET @name = 'SQL'
PRINT @x
PRINT @y
PRINT @name
PRINT ''

-- Select expressions
PRINT '===== SELECT EXPRESSIONS ====='
SELECT 1 + 1
SELECT @x + @y
SELECT @x * @y
PRINT ''

-- String functions
PRINT '===== STRINGS ====='
PRINT LEN('Hello')
PRINT UPPER('hello')
PRINT LOWER('HELLO')
PRINT ''

-- Conditionals
PRINT '===== CONDITIONALS ====='
SELECT CASE WHEN @x > 5 THEN 'x is greater than 5' ELSE 'x is not greater than 5' END
SELECT CASE WHEN @y = 3 THEN 'y equals 3' ELSE 'y is not 3' END
PRINT ''

-- While loop
PRINT '===== WHILE LOOP ====='
DECLARE @i INT
SET @i = 1
WHILE @i <= 5
BEGIN
  PRINT @i
  SET @i = @i + 1
END
PRINT ''

-- Create table and DML
PRINT '===== TABLE OPERATIONS ====='
CREATE TABLE employees (id INT, name VARCHAR(50), department VARCHAR(50), salary INT)

INSERT INTO employees VALUES (1, 'Alice', 'Engineering', 85000)
INSERT INTO employees VALUES (2, 'Bob', 'Marketing', 72000)
INSERT INTO employees VALUES (3, 'Charlie', 'Engineering', 90000)
INSERT INTO employees VALUES (4, 'Diana', 'Marketing', 68000)
INSERT INTO employees VALUES (5, 'Eve', 'Engineering', 95000)

SELECT * FROM employees
PRINT ''

-- Filtered query
PRINT '===== FILTERED QUERY ====='
SELECT * FROM employees WHERE department = 'Engineering'
PRINT ''

-- Ordered query
PRINT '===== ORDERED QUERY ====='
SELECT * FROM employees ORDER BY salary
PRINT ''

-- Aggregates
PRINT '===== AGGREGATES ====='
SELECT COUNT(*) FROM employees
SELECT SUM(salary) FROM employees
PRINT ''

-- Update
PRINT '===== UPDATE ====='
UPDATE employees SET salary = 75000 WHERE name = 'Bob'
SELECT * FROM employees WHERE name = 'Bob'
PRINT ''

-- Delete
PRINT '===== DELETE ====='
DELETE FROM employees WHERE name = 'Diana'
SELECT COUNT(*) FROM employees
PRINT ''

-- Batch separator
PRINT '===== BATCH ====='
PRINT 'batch 1'
GO
PRINT 'batch 2'
PRINT ''

PRINT '===== DONE ====='
