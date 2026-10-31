10 REM ============================================================
20 REM HB-1: THE MAP, READ RATHER THAN BELIEVED
30 REM The self-test says the machine has 32K, a screen and eleven
40 REM ROMs. This asks the address space instead: four PEEKs, one
50 REM from each region the map documents, and a line about what
60 REM answered. A hole in the map answers zero, which is a thing
70 REM the machine does on purpose.
80 REM ============================================================
90 CLS
100 PRINT "HB-1 MEMORY MAP, FROM THE OUTSIDE IN"
110 PRINT "-----------------------------------"
120 PRINT ""
130 REM --- free RAM below the screen -----------------------------
140 PRINT "00000  FREE RAM";
150 POKE 1000, 165
160 LET RAMBYTE = PEEK(1000)
170 PRINT "     WROTE 165, READ "; RAMBYTE;
180 REM --- the screen cells ---------------------------------------
190 PRINT ""
200 PRINT "32768  SCREEN CELLS";
210 POKE 32768, 65
220 LET CELL = PEEK(32768)
230 PRINT "   'A' IS 65, READ "; CELL;
240 REM --- the colour attributes ----------------------------------
250 PRINT ""
260 PRINT "36864  ATTRIBUTES";
270 POKE 36864, 240
280 LET ATTR = PEEK(36864)
290 PRINT "     INK 15 ON PAPER 0 IS 240, READ "; ATTR;
300 REM --- the I/O page: the cursor is a register -----------------
310 PRINT ""
320 PRINT "40960  I/O REGISTERS";
330 LOCATE 14, 3
340 LET COLREG = PEEK(40960)
350 PRINT "  CURSOR COLUMN REGISTER READS "; COLREG;
360 REM --- ROM: readable, and it refuses to be written ------------
370 PRINT ""
380 PRINT "61440  ROM SPACE";
390 LET ROMBYTE = PEEK(61440)
400 PRINT "    A ROM BYTE READS "; ROMBYTE;
410 REM --- a hole: an address no region claims -------------------
420 PRINT ""
430 PRINT "20000  A HOLE IN THE MAP";
440 LET HOLE = PEEK(20000)
450 PRINT "  ANSWERS "; HOLE;
460 REM --- and the one thing memory refuses ----------------------
470 PRINT ""
480 PRINT "WRITING TO ROM IS REFUSED, NOT IGNORED."
490 POKE 61440, 1
500 PRINT "THE POKE ABOVE WAS TOLD SO, IN THE MACHINE'S VOICE."
510 END
