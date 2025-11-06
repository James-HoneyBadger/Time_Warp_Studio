#!/bin/bash
# DOS Version Test Plan
# Run this inside DOSBox with DJGPP to validate the DOS build

echo "========================================="
echo "Time Warp DOS - Build & Test Script"
echo "========================================="
echo ""

# Prerequisites check
if [ -z "$DJGPP" ]; then
    echo "ERROR: DJGPP environment variable not set"
    echo "Set it to your DJGPP.ENV path, e.g.:"
    echo "  set DJGPP=C:\\DJGPP\\DJGPP.ENV"
    exit 1
fi

echo "DJGPP: $DJGPP"
echo ""

# Build
echo "Building TLAND.EXE..."
cd src
gcc -O2 -ffast-math -s -o TLAND.EXE tland_fixed.c -lm
if [ $? -ne 0 ]; then
    echo "Build FAILED"
    exit 1
fi
echo "Build OK"
echo ""

# Test 1: Usage message
echo "Test 1: Usage message"
./TLAND.EXE
echo ""

# Test 2: Run SQUARE.TC (graphics + wait for key)
echo "Test 2: SQUARE.TC with graphics"
echo "Expected: Graphics window with cyan square, press any key to continue"
./TLAND.EXE ../examples/SQUARE.TC
echo ""

# Test 3: HELLO_LINE.TC with graphics
echo "Test 3: HELLO_LINE.TC with graphics"
echo "Expected: Graphics window with yellow L-shape, press any key"
./TLAND.EXE ../examples/HELLO_LINE.TC
echo ""

# Test 4: Headless BMP save
echo "Test 4: Headless BMP save"
./TLAND.EXE ../examples/SQUARE.TC --save square_out.bmp
if [ -f square_out.bmp ]; then
    echo "BMP created: square_out.bmp"
    ls -lh square_out.bmp
else
    echo "ERROR: BMP not created"
    exit 1
fi
echo ""

# Test 5: Verify old tland.c is blocked
echo "Test 5: Verify deprecated tland.c fails"
gcc -o TLAND_OLD.EXE tland.c 2>&1 | grep -i error
if [ $? -eq 0 ]; then
    echo "OK: tland.c correctly emits error"
else
    echo "WARNING: tland.c did not fail as expected"
fi
echo ""

echo "========================================="
echo "All tests completed!"
echo "Check square_out.bmp in an image viewer"
echo "========================================="
