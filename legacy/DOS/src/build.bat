@echo off
rem Build Time Warp (DOS) with DJGPP
rem Prerequisites: set DJGPP and PATH to your DJGPP install

gcc -O2 -ffast-math -s -o TLAND.EXE tland_fixed.c -lm
if errorlevel 1 goto :err
echo Build OK: TLAND.EXE
exit /b 0
:err
echo Build failed.
exit /b 1
