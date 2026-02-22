# SquareLang

An experimental C-like language and compiler written in C.
`square` compiles `.square` files into LLVM IR (`.ll`), which you can then build with `clang`.

## Quick Start

```bash
cmake -S . -B build
cmake --build build -j
```

## Example

```bash
cd example
../bin/square example.square out.ll
clang out.ll -o out
./out
```

## Project Structure

- `src/` - compiler source code
- `example/` - SquareLang example programs
- `bin/` - built `square` binary
