The algorithm to print optimal parens is identical the algorithm to
calculate the final matrix chain product, `Matrix-Chain-Multiply`, which
recursively calls itself:

```
Print-Optimal-Parens(A, s, i, j):
  if j > i
    then X <- Print-Optimal-Parens(A, s, i, s[i,j])
         Y <- Print-Optimal-Parens(A, s, s[i,j] + 1, j)
         return "(X, Y)"
    else return "A_i"
```

Effectively, we have built a tree defined by the pivots in `s`. Walking a tree
is linear with the number of nodes, so its complexity is `O(n)`.
