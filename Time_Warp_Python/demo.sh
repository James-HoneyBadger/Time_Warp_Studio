#!/bin/bash
# Demo script showing Time Warp IDE Python version capabilities

echo "=============================="
echo "Time Warp IDE - Python Version"
echo "=============================="
echo ""

echo "1. Running Logo Square Example:"
echo "--------------------------------"
python3 run_time_warp.py examples/logo_square.logo --turtle
echo ""

echo "2. Running PILOT Screen Demo:"
echo "------------------------------"
python3 run_time_warp.py examples/pilot_screen_demo.pilot
echo ""

echo "3. Running Logo Starburst:"
echo "--------------------------"
python3 run_time_warp.py examples/logo_starburst.logo --turtle
echo ""

echo "4. Quick Expression Test:"
echo "-------------------------"
python3 -c "
from time_warp.utils.expression_evaluator import ExpressionEvaluator
eval = ExpressionEvaluator({'X': 10, 'Y': 5})
print('Expression: X * 2 + Y =', eval.evaluate('X * 2 + Y'))
print('Expression: sqrt(144) =', eval.evaluate('sqrt(144)'))
print('Expression: sin(0) + cos(0) =', eval.evaluate('sin(0) + cos(0)'))
"
echo ""

echo "5. Error Hint Test:"
echo "-------------------"
python3 -c "
from time_warp.utils.error_hints import suggest_command
print('Typo: PRITN -> Suggestion:', suggest_command('PRITN'))
print('Typo: FORWAD -> Suggestion:', suggest_command('FORWAD'))
print('Typo: GOTP -> Suggestion:', suggest_command('GOTP'))
"
echo ""

echo "=============================="
echo "✅ All demos completed!"
echo "=============================="
