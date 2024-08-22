# chem-balance

## How to use it

To build this project, I recommend installing GHCup and running `cabal build` inside of this repository. You can then use it by running `cabal run`. The purpose of this program is to balance chemical equations, e.g. you input "C2H6 + O2 -> CO2 + H2O" and it outputs "2C2H6 + 7O2 -> 4CO2 + 6H2O".
```
chem-balance$ cabal build
...
chem-balance$ cabal run
Please enter the chemical equation to be balanced: C2H6 + O2 -> CO2 + H2O
Balanced equation: 2C2H6 + 7O2 -> 4CO2 + 6H2O
chem-balance$ cabal run
Please enter the chemical equation to be balanced: Fe2O3 + HCl -> FeCl3 + H2O
Balanced equation: Fe2O3 + 6HCl -> 2FeCl3 + 3H2O
```

## How it works

This program consists of three Haskell files: `app/Main.hs`, `src/ChemicalEquation.hs`, and `src/LinearAlgebra.hs`. It might seem weird that there is a module called `LinearAlgebra` in this program, but as it turns out, the problem of balancing a chemical equation can be encoded as a system of linear equations. For example if you are trying to balance the chemical equation "CH4 + O2 -> CO2 + H20", i.e. find `w`, `x`, `y`, and `z` such that "`w`CH4 + `x`O2 -> `y`CO2 + `z`H20" is balanced, then the conservation of matter for each chemical becomes a linear equation. For carbon we have `w = y`, for hydrogen we have `4w = 2z`, and for oxygen we have `2x = 2y + z`. This as an infinite family of solutions, but we can add a constraint such as `w = 1` so that there is a unique solution and then multiply the values of `w`, `x`, `y`, and `z` by their least common denominator to get a solution in simplest terms. In this case, the solution at which we arrive is `w = 1`, `x = 2`, `y = 1`, and `z = 2`, so the balanced chemical equation is "CH4 + 2O2 -> CO2 + 2H20".

I initially looked for a Haskell package that would take care of solving systems of linear equations for me, but they all ended up having problems: can't handle Rational values, can't handle overdetermined systems, IO return type due to using external program, you name it. In the end, I decided to create my own module for the job. I was able to achieve `O(m^2n)` time complexity for solving an `m x n` system of linear equations by using the `Data.Array` module. `Data.Array` uses `ST` to encapsulate stateful computation, allowing `k` updates to be applied to an array of length `l` in `O(k+l)` time by "unfreezing" the underlying mutable array, applying all of the updates, then "freezing" it again. By grouping together updates to be applied all at once so as to minimize the freezing and unfreezing, I was able to avoid the time complexity overhead that is normally associated with using a functional language.

I also needed a way to parse the input. For this I wrote the `ChemicalEquation` module, which defines a data type for chemical equations and makes use of `ReadP` to build up a monadic parser for this data type. Once a chemical equation is parsed, it can be easily processed by the functions in `Main` to yield a solution.
