#!/usr/bin/env python3
"""
Simple test to check if Time Warp Studio interpreter works.
"""
import sys
import os
sys.path.insert(0, os.path.join(os.path.dirname(__file__), 'Platforms', 'Python'))

try:
    from time_warp.core.interpreter import Interpreter
    from time_warp.graphics.turtle_state import TurtleState
    print("✅ Imports successful")
    
    # Test basic interpreter
    interpreter = Interpreter()
    turtle = TurtleState()
    
    # Test a simple BASIC program
    basic_code = '10 PRINT "Hello World"'
    result = interpreter.execute('basic', basic_code, turtle)
    print(f"✅ BASIC execution result: {result[:100]}...")
    
    print("✅ Basic functionality test passed")
    
except Exception as e:
    print(f"❌ Error: {e}")
    import traceback
    traceback.print_exc()