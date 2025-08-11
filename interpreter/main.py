#!/usr/bin/env python3
"""
Functional Language Interpreter - Main Entry Point

Usage:
    python main.py                     # Interactive mode
    python main.py file.func           # Execute file
    python main.py -test               # Run tests
    python main.py -demo               # Run demonstrations
"""

import sys
import os
from interpreter import Interpreter
from values import IntegerValue, FunctionValue, RecordValue

def run_interactive():
    """Run interactive interpreter"""
    print("Functional Language Interpreter v1.0")
    print("Type 'help' for examples, 'quit' to exit")
    print()
    
    interpreter = Interpreter()
    
    while True:
        try:
            text = input(">>> ").strip()
            
            if text.lower() in ['quit', 'exit', 'q']:
                print("Goodbye!")
                break
            elif text.lower() in ['help', 'h']:
                show_help()
                continue
            elif not text:
                continue
            
            result = interpreter.eval_text(text)
            print_result(result)
            
        except KeyboardInterrupt:
            print("\nUse 'quit' to exit")
        except Exception as e:
            print(f"Error: {e}")

def run_file(filename):
    """Execute a file"""
    try:
        with open(filename, 'r') as f:
            content = f.read()
        
        interpreter = Interpreter()
        result = interpreter.eval_text(content)
        print_result(result)
        
    except FileNotFoundError:
        print(f"Error: File '{filename}' not found")
    except Exception as e:
        print(f"Error: {e}")

def run_tests():
    """Run the test suite"""
    try:
        from test_comprehensive import main as test_main
        test_main()
    except ImportError:
        print("Comprehensive test suite not found. Running basic tests...")
        import test_interpreter
        import unittest
        unittest.main(module=test_interpreter, argv=[''], exit=False)

def run_demonstrations():
    """Run demonstration programs"""
    print("=" * 50)
    print("FUNCTIONAL LANGUAGE DEMONSTRATIONS")
    print("=" * 50)
    
    interpreter = Interpreter()
    
    demos = [
        ("Integer literal", "42", "Basic value"),
        ("Identity function", "(x.x) 5", "Simple lambda"),
        ("Square function", "(x.mult x x) 4", "Function with computation"),
        ("Curried addition", "(x.y.plus x y) 3 4", "Multi-parameter function"),
        ("Partial application", "(plus 5) 3", "Currying in action"),
        ("Eager record", "[a=10, b=20] plus a b", "Record as environment"),
        ("Lazy record", "{a=5, b=mult a 2} b", "Lazy evaluation"),
        ("Higher-order function", "(f.x.f (f x)) (y.plus y 1) 0", "Function as argument"),
        ("Nested records", "[outer={inner=42}] outer inner", "Structured data"),
    ]
    
    for name, expr, description in demos:
        print(f"\n{name}: {description}")
        print(f"Expression: {expr}")
        try:
            result = interpreter.eval_text(expr)
            print(f"Result: ", end="")
            print_result(result)
        except Exception as e:
            print(f"Error: {e}")
    
    print(f"\n{'='*50}")
    print("Demonstrations complete!")

def show_help():
    """Show help information"""
    print("""
Functional Language Help:
========================

Basic Syntax:
  42                           # Integer literal
  x.expr                       # Lambda function (param x, body expr)
  func arg                     # Function application
  {a=1, b=2}                   # Lazy record
  [a=1, b=2]                   # Eager record
  record field                 # Record access

Built-in Functions:
  plus x y                     # Addition
  minus x y                    # Subtraction
  mult x y                     # Multiplication
  div x y                      # Division

Examples:
  (x.x) 5                      # Identity function → 5
  (x.mult x x) 4               # Square function → 16
  plus 3 4                     # Addition → 7
  (plus 5) 3                   # Partial application → 8
  [a=10, b=20] plus a b        # Record environment → 30
  {x=5, y=mult x 2} y          # Lazy evaluation → 10

Advanced:
  (x.y.plus x y) 3 4           # Curried function → 7
  (f.x.f (f x)) (y.plus y 1) 0 # Higher-order function → 2
""")

def print_result(result):
    """Pretty print evaluation results"""
    if isinstance(result, IntegerValue):
        print(result.value)
    elif isinstance(result, FunctionValue):
        print(f"<function {result.param}.{type(result.body).__name__}(...)>")
    elif isinstance(result, RecordValue):
        record_type = "eager" if result.eager else "lazy"
        field_count = len(result.vals) + len(result.exprs)
        print(f"<{record_type} record with {field_count} fields>")
    else:
        print(f"<{type(result).__name__}>")

def main():
    """Main entry point"""
    if len(sys.argv) == 1:
        # Interactive mode
        run_interactive()
    elif len(sys.argv) == 2:
        arg = sys.argv[1]
        if arg == '-test':
            run_tests()
        elif arg == '-demo':
            run_demonstrations()
        elif arg in ['-h', '--help']:
            print(__doc__)
        else:
            # Treat as filename
            run_file(arg)
    else:
        print("Usage: python main.py [file.func | -test | -demo | -h]")
        sys.exit(1)

if __name__ == "__main__":
    main()