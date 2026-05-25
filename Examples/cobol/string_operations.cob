       IDENTIFICATION DIVISION.
       PROGRAM-ID. STRING-OPERATIONS.
       AUTHOR. Time Warp Studio.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-FIRST-NAME    PIC X(10) VALUE 'Hello'.
       01  WS-LAST-NAME     PIC X(10) VALUE 'World'.
       01  WS-FULL-NAME     PIC X(25).
       01  WS-POINTER       PIC 99 VALUE 1.
       01  WS-SOURCE        PIC X(30) VALUE 'Time Warp Studio is great!'.
       01  WS-DEST          PIC X(20).
       01  WS-LENGTH        PIC 99 VALUE 0.
       01  WS-COUNT         PIC 99 VALUE 1.

       PROCEDURE DIVISION.
       MAIN-PARA.
           DISPLAY 'COBOL String Operations'.
           DISPLAY '======================='.

           MOVE SPACES TO WS-FULL-NAME.
           STRING WS-FIRST-NAME DELIMITED SPACE
                  ', '
                  WS-LAST-NAME DELIMITED SPACE
                  '!'
               INTO WS-FULL-NAME.
           DISPLAY 'String: ' WS-FULL-NAME.

           UNSTRING WS-SOURCE DELIMITED BY SPACE
               INTO WS-DEST
               WITH POINTER WS-POINTER.
           DISPLAY 'First word: ' WS-DEST.

           INSPECT WS-SOURCE TALLYING WS-COUNT FOR ALL 'a'.
           DISPLAY 'Count of a: ' WS-COUNT.

           MOVE WS-SOURCE TO WS-DEST.
           INSPECT WS-DEST REPLACING ALL 'a' BY '*'.
           DISPLAY 'Masked: ' WS-DEST.

           STOP RUN.
