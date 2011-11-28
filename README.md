Arithmetic Trees
================

## Purpose
This is just the port of Java practicals at École Centrale de
Nantes. The goal is to describe polynomials as trees, then evaluate them or derive
them. Since I'm fairly new to Haskell, this code has been written just for
fun. Comments are welcome !

## Usage

### Define a tree

    let plus = Binary Plus (Leaf (Val 2)) (Leaf X) -- (2 + x)
    let cos = Unary Cos (Leaf X)  -- cos(x)

The following binary operators are supported

    Plus, Minus, Times, Div, Pow

The following unary operators are supported

    Cos, Sin, Tan

`Tree` is an instance of `Show`. The expressions are displayed in a nice infix
notation.

   ((42 + 10) * x)

### Evaluate a tree

   eval (Binary Pow (Leaf (Val x)) (Leaf (Val 2))) -- 4

### Evaluate a tree for a given value

   eval (replace (Binary Pow (Leaf (Val x)) (Leaf (Val 2))) 2) -- 4


### Derive a tree

   derive (Binary Plus (Binary Times (Leaf (X)) (Leaf (Val 12))) (Leaf (X)))

### Clean a tree
The derived tree is often unnecessarily cluttered. You can clean it by
simplifying (`1*x`, `0*x`, `0+x` and so on).

   clean (Binary Times (Val (Leaf 0)) (Val (Leaf 1)))


Again, this is just me playing with Haskell. If you have comments /
suggestions, I'll be glad to hear them.
