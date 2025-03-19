# 🧩 Klotski Puzzle Solver  
*A Search-Based Solver for the Classic Sliding-Block Puzzle Game*

## 📌 Overview
Klotski is a well-known sliding-block puzzle game where players move blocks within a confined space to achieve a goal state. Some variations of Klotski are notoriously difficult, requiring dozens of moves to solve.

This project implements an **automatic Klotski solver** using **search algorithms** (BFS and DFS) in **OCaml**. The solver efficiently finds the shortest solution to a given Klotski board configuration and provides an **interactive mode** for manual solving.

---

## 🚀 Features
- **Breadth-First Search (BFS) and Depth-First Search (DFS)** for automated solving  
- **Optimized state-space exploration** using functional programming concepts  
- **Interactive Mode** for manually solving puzzles via command-line  
- **Pre-loaded Test Cases** with multiple challenging Klotski boards  
- **Modular Design** implemented using OCaml functors for extensibility  

---

## ⚙️ Installation & Setup
This project uses **Dune** as its build system.

### 1️⃣ Install Dependencies
Ensure that you have **OCaml** and **Dune** installed:
```bash
opam install dune
```

Build and Run Tests 
```make test```

To solve teh Board 
```make solve```

To Interact with the Board 
```make interact```

Other Commands: 

```
klotski.ml         # Main module for game logic and board representation
klotski.mli        # Interface for the Klotski module
gsearch.ml         # Search algorithms for solving puzzles (BFS/DFS)
gsearch.mli        # Interface for the search module
klotski_boards.ml  # Pre-defined Klotski boards for testing
klotski_tests.ml   # Unit tests for verifying functionality
klotski_solve.ml   # Command-line solver program
klotski_interact.ml# Interactive player mode
dune               # Dune build configuration file
Makefile           # Makefile for building and running the project
```
