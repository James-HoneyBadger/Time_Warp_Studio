      IDENTIFICATION DIVISION.
      PROGRAM-ID. HELLO-DEMO.

      DATA DIVISION.
      WORKING-STORAGE SECTION.
      01 WS-NAME PIC X(20) VALUE "COBOL".
      01 WS-X PIC 9(5) VALUE 10.
      01 WS-Y PIC 9(5) VALUE 3.
      01 WS-RESULT PIC 9(5) VALUE 0.
      01 WS-I PIC 9(3) VALUE 0.
      01 WS-STR PIC X(30) VALUE "HELLO WORLD HELLO".
      01 WS-CNT PIC 9(3) VALUE 0.
      01 WS-OUT PIC X(30).

      PROCEDURE DIVISION.

      DISPLAY "===== HELLO WORLD =====".
      DISPLAY "Welcome to COBOL!".
      DISPLAY " ".

      DISPLAY "===== VARIABLES =====".
      DISPLAY WS-NAME.
      DISPLAY WS-X.
      DISPLAY WS-Y.
      DISPLAY " ".

      DISPLAY "===== ARITHMETIC =====".
      ADD WS-X TO WS-Y GIVING WS-RESULT.
      DISPLAY WS-RESULT.
      SUBTRACT WS-Y FROM WS-X GIVING WS-RESULT.
      DISPLAY WS-RESULT.
      MULTIPLY WS-X BY WS-Y GIVING WS-RESULT.
      DISPLAY WS-RESULT.
      DIVIDE WS-X BY WS-Y GIVING WS-RESULT.
      DISPLAY WS-RESULT.
      COMPUTE WS-RESULT = WS-X + WS-Y * 2.
      DISPLAY WS-RESULT.
      DISPLAY " ".

      DISPLAY "===== CONDITIONALS =====".
      IF WS-X > 5
        DISPLAY "X is greater than 5"
      END-IF.
      IF WS-Y = 3
        DISPLAY "Y equals 3"
      ELSE
        DISPLAY "Y does not equal 3"
      END-IF.
      DISPLAY " ".

      DISPLAY "===== EVALUATE =====".
      EVALUATE WS-Y
        WHEN 1 DISPLAY "one"
        WHEN 2 DISPLAY "two"
        WHEN 3 DISPLAY "three"
        WHEN OTHER DISPLAY "other"
      END-EVALUATE.
      DISPLAY " ".

      DISPLAY "===== LOOPS =====".
      PERFORM 5 TIMES
        DISPLAY "X"
      END-PERFORM.
      DISPLAY " ".

      DISPLAY "===== PERFORM VARYING =====".
      PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 5
        DISPLAY WS-I
      END-PERFORM.
      DISPLAY " ".

      DISPLAY "===== STRINGS =====".
      INSPECT WS-STR TALLYING WS-CNT FOR ALL "L".
      DISPLAY WS-CNT.

      STRING "Hello" DELIMITED SIZE
             " " DELIMITED SIZE
             "World" DELIMITED SIZE
             INTO WS-OUT.
      DISPLAY WS-OUT.
      DISPLAY " ".

      DISPLAY "===== DONE =====".
      STOP RUN.
