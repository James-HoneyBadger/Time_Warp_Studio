/* REXX - Arrays and Collections */
/* Demonstrates stem variables (REXX arrays) and compound variables */

SAY '=== REXX Arrays and Stems ==='
SAY ''

/* Simple stem array */
fruits.1 = 'Apple'
fruits.2 = 'Banana'
fruits.3 = 'Cherry'
fruits.4 = 'Date'
fruits.5 = 'Elderberry'
fruits.0 = 5   /* convention: .0 holds the count */

SAY 'Fruit list:'
DO i = 1 TO fruits.0
    SAY '  ' i'. 'fruits.i
END

/* Reverse the array */
SAY ''
SAY 'Reversed:'
DO i = fruits.0 TO 1 BY -1
    SAY '  'fruits.i
END

/* Sorting with bubble sort */
SAY ''
SAY '=== Bubble Sort ==='
nums.1 = 64;  nums.2 = 25;  nums.3 = 12
nums.4 = 90;  nums.5 = 3;   nums.6 = 77
nums.7 = 44;  nums.8 = 18
nums.0 = 8

SAY 'Before sort:'
line = ''
DO i = 1 TO nums.0
    line = line nums.i
END
SAY ' 'STRIP(line)

/* Bubble sort */
DO pass = 1 TO nums.0 - 1
    DO i = 1 TO nums.0 - pass
        j = i + 1
        IF nums.i > nums.j THEN DO
            temp   = nums.i
            nums.i = nums.j
            nums.j = temp
        END
    END
END

SAY 'After sort:'
line = ''
DO i = 1 TO nums.0
    line = line nums.i
END
SAY ' 'STRIP(line)

/* Associative array via compound variables */
SAY ''
SAY '=== Associative Array ==='
capitals.France    = 'Paris'
capitals.Japan     = 'Tokyo'
capitals.Germany   = 'Berlin'
capitals.Brazil    = 'Brasilia'
capitals.Australia = 'Canberra'

countries = 'France Japan Germany Brazil Australia'
DO WHILE countries \= ''
    PARSE VAR countries country countries
    SAY '  'country': 'capitals.country
END
