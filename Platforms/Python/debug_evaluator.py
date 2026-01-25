from time_warp.utils.expression_evaluator import ExpressionEvaluator

evaluator = ExpressionEvaluator({"CHOICE": 5.0})
result = evaluator.evaluate("CHOICE < 1")
print(f"5.0 < 1 => {result}")

tokens = evaluator._tokenize("CHOICE < 1")
print(f"Tokens: {[t.value for t in tokens]}")
rpn = evaluator._to_rpn(tokens)
print(f"RPN: {[t.value for t in rpn]}")
val = evaluator._evaluate_rpn(rpn)
print(f"Val: {val}")
