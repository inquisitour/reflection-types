# OCaml Reflection Types: GADTs and Runtime Introspection

**MSc Research Project** | Logic and Computation | TU Wien  
*Exploring compile-time and runtime reflection in OCaml through practical implementations*

## Overview

This repository demonstrates the practical application of reflection types in OCaml, particularly Generalized Algebraic Data Types (GADTs), through two interconnected systems:

1. **Functional Language Interpreter** (Python) - A complete implementation showcasing the language features that benefit from reflection
2. **Syntax-Aware Editor** (OCaml) - Real-world GADT applications with integrated proof checking

## Key Contributions

### 🔬 Theoretical Foundations
- **Type equality witnesses** for safe runtime casting
- **Phantom types** for compile-time state verification  
- **Type-indexed values** eliminating runtime errors
- **GL modal logic proof checker** using GADTs for soundness

### 🛠️ Practical Implementations

#### GADT Applications in Editor
- **Type-safe AST evaluation** with preserved type information
- **Command history** with reversible operations and type-safe undo/redo
- **Syntax highlighting** with compile-time color guarantees
- **Brace matching** verified at type level

#### Functional Language Features
- Lambda calculus with curried functions
- Eager and lazy record evaluation  
- Record-as-environment pattern
- Integrated interpreter execution from editor

## Research Focus

This project investigates three key aspects of reflection types:

1. **Compile-time vs Runtime Trade-offs**: GADTs provide zero-runtime cost but require extensive annotations, while runtime reflection offers flexibility at performance cost

2. **Practical Applicability**: Moving beyond toy examples to production-ready patterns (inspired by Jane Street's usage)

3. **Formal Verification Integration**: Connection to theorem proving through GL logic implementation and HOL Light-style proof terms

## Quick Start

```bash
# Run the interpreter
cd interpreter
python3 main.py examples/square.func

# Build and run the editor
cd editor
dune build
dune exec editor test/example.func
```

### Editor Commands
- `Ctrl-R`: Run current file in interpreter
- `Ctrl-L`: Check GL syntax  
- `Ctrl-E`: Run GL proof demonstration
- `Ctrl-S`: Save | `Ctrl-X`: Quit

## Project Structure

```
├── interpreter/          # Functional language implementation
│   ├── evaluator.py     # Core evaluation with lazy/eager records
│   └── examples/        # Language feature demonstrations
│
├── editor/src/          # OCaml editor with GADT examples
│   ├── practical_gadts.ml    # 4 practical GADT patterns
│   ├── gl_proof_checker.ml   # Complete GL modal logic
│   └── interpreter_integration.ml # Cross-language execution
│
└── Research.md          # Comprehensive reflection types survey
```

## Key Insights

1. **GADTs eliminate entire bug classes** by making invalid states unrepresentable
2. **Type witnesses enable safe heterogeneous collections** while preserving type information
3. **Reflection bridges static and dynamic typing** within a single type-safe framework
4. **Production systems benefit** from reduced memory usage and eliminated runtime checks

## Technical Highlights

- **Zero-cost abstractions**: GADTs compile to efficient pattern matching
- **Formal soundness**: GL proof checker validates modal logic theorems
- **Practical patterns**: Every GADT example solves real editor problems
- **Cross-language integration**: OCaml editor executes Python interpreter

## Related Work

- [Full Research Survey](docs/Research.md) - Detailed analysis of OCaml reflection types (2023-2025)
- Jane Street's `Base` library - Production GADT usage
- `refl` library - Universal PPX deriver for runtime types
- HOL Light - OCaml-based theorem prover leveraging type safety

## Dependencies

**OCaml**: 4.14+ with Dune build system  
**Python**: 3.8+ (no external dependencies)  
**OCaml libraries**: ANSITerminal, Unix

---

*This research explores how reflection types can transform OCaml from a practical ML dialect into a language capable of theorem proving, zero-cost abstractions, and compile-time verification—while maintaining its core simplicity and performance.*