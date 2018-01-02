# Matrix-chain multiplication

My attempt to work through the exercises in chapter 16.1 of Introduction to
Algorithms, by Cormen, Leiserson, and Rivest (first edition).

## Background

A few weeks ago I came across a blog post called [Sudoku in Coders At
Work](http://pindancing.blogspot.com/2009/09/sudoku-in-coders-at-work.html). It
includes a passage where Peter Norvig mentions the importance of dynamic
programming, and I realized I didn't know what dynamic programming was.

I read
[the Wikipedia article](http://en.wikipedia.org/wiki/Dynamic_programming), but
that didn't really help me understand it. Later, I found [a Stack Overflow
question](https://stackoverflow.com/q/1065433/25637) that mentioned that "the
Cormen Algorithms book has a great chapter about dynamic programming", so I dug
my copy out.

In this essay and accompanying source code, I will try to explain the problem
and solution as best I can.

## Problem Overview

Find the most efficient way to multiply a chain of matrices.

Matrix multiplication is associative but not commutative: `(AB)C = A(BC)`, but
`AB != BA`. The order of multiplication can be specified by adding parentheses,
and specifying an order for the complete expression makes the expression *fully
parenthesized*. The order matters because multiplying two large matrices
requires much more arithmetic than multiplying a large matrix by a small
matrix, so if `A` is small and `B` and `C` are large, the expression `(AB)C` is
much more efficient to calculate.

"Given a chain of matrices, ... fully parenthesize the product that minimizes
the number of scalar multiplications."

## Solution

This problem is ideally solved by dynamic programming because it has *optimal
substructure* - it can be split into subproblems which each have optimal
solutions whose composition is an optimal solution - and *overlapping
subproblems* - each subproblem may be encountered many times during solving.

Recording the answers to these subproblems (called *memoizing*)  allows each
to only be calculated once, trading space to save time.

The algorithm in the book uses a bottom-up approach, calculating the cost of
each matrix subchain, starting with length 1 and working up to length n, taking
advantage of the optimal substructure.

The algorithm, as listed in the book:

```
Matrix-Multiply(A, B)
  if columns[A] != columns[B]
    then error "incompatible dimensions"
    else for i <- 1 to rows[A]
      do for j <- 1 to columns[B]
        do C[i,j] <- 0
          for k <- 1 to columns[A]
            do C[i,j] <- C[i,j] + A[i,k] * B[k,j]
      return C

Matrix-Chain-Order(p)
  n <- length[p] - 1
  for i <- 1 to n
    do m[i,i] <- 0
  for l <- 2 to n
    do for i <- 1 to n - l + 1
      do j <- i + l - 1
        m[i,j] <- infinity
        for k <- i to j - 1
          do q <- m[i,k] + m[k + 1,j] + (p_i-1)(p_k)(p_j)
            if q < m[i,j]
              then m[i,j] <- q
                   s[i,j] <- k
  return m and s

Matrix-Chain-Multiply(A, s, i, j)
  if j > i
    then X <- Matrix-Chain-Multiply(A, s, i, s[i,j])
         Y <- Matrix-Chain-Multiply(A, s, s[i,j] + 1, j)
         return Matrix-Multiply(X, Y)
    else return A_i
```

## Exercises

16.1-1: Find an optimal parenthesization of a matrix-chain product whose
sequence of dimensions is `[5, 10, 3, 12, 5, 50, 6]`.

[Solution](solution1.md)

16.1-2: Give an efficient algorithm `Print-Optimal-Parens` to print the optimal
parenthesization of a matrix chain given the table `s` computed by
`Matrix-Chain-Order`. Analyze your algorithm.

[Solution](solution2.md)

16.1-3: Let R(i,j) be the number of times that table entry `m[i,j]` is
referenced by `Matrix-Chain-Order` in computing other table entries. Show that
the total number of references for the entire table is

```
 n   n           n^3-n
sum sum R(i,j) = -----
i=1 j=1            3
```

(Hint: you may find the identity sum i=1..n i^2 = n(n+1)(2n+1)/6 useful.)

16.1-4: Show that a full parenthesization of an n-element expression has
exactly n-1 pairs of parentheses.


## License

![cc-by-sa](https://i.creativecommons.org/l/by-sa/4.0/88x31.png)
This essay is shared under the [Creative Commons Attribution-ShareAlike 4.0
International License](http://creativecommons.org/licenses/by-sa/4.0/).

![GPL v3.0](https://www.gnu.org/graphics/gplv3-88x31.png)
The associated source code is shared under the GNU General Public License,
v3.0.
