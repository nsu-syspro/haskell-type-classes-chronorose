[![Review Assignment Due Date](https://classroom.github.com/assets/deadline-readme-button-22041afd0340ce965d47ae6ef1cefeee28c7c493a6346c4f15d667ab976d596c.svg)](https://classroom.github.com/a/YLLy2KL8)
# Haskell: Type classes

<img alt="points bar" align="right" height="36" src="../../blob/badges/.github/badges/points-bar.svg" />

<details>
<summary>Guidelines</summary>

## Guidelines

When solving the homework, strive to create not just code that works, but code that is readable and concise.
Try to write small functions which perform just a single task, and then combine those smaller
pieces to create more complex functions.

Don’t repeat yourself: write one function for each logical task, and reuse functions as necessary.

Don't be afraid to introduce new functions where you see fit.

### Sources

Each task has corresponding source file in [src](src) directory where you should implement the solution.

### Building

All solutions should compile without warnings with following command:

```bash
stack build
```

### Testing

You can and should run automated tests before pushing solution to GitHub via

```bash
stack test --test-arguments "-p TaskX"
```

where `X` in `TaskX` should be number of corresponding Task to be tested.

So to run all test for the first task you should use following command:

```bash
stack test --test-arguments "-p Task1"
```

You can also run tests for all tasks with just

```bash
stack test
```

### Debugging

For debugging you should use GHCi via stack:

```bash
stack ghci
```

You can then load your solution for particular task using `:load TaskX` command.

Here is how to load Task1 in GHCi:

```bash
$ stack ghci
ghci> :load Task1
[1 of 1] Compiling Task1 ( .../src/Task1.hs, interpreted )
Ok, one module loaded.
```

> **Note:** if you updated solution, it can be quickly reloaded in the same GHCi session with `:reload` command
> ```bash
> ghci> :reload
> ```

</details>

## Preface

In this assignment you will implement generalized representation for arbitrary expressions.

> It is recommended for tasks to be implemented in order.

## Task 1 (3 points)

Before attempting to generalize anything, you should always
first write something simple and concrete as a prototype,
and then see what can be abstracted and generalized.

### Integer arithmetic expressions

Your first task is to implement representation for integer arithmetic expressions
with just two binary operations: addition and multiplication.

In fact the representation for this task is already provided in [src/Task1.hs](src/Task1.hs):

```haskell
-- | Representation of integer arithmetic expressions comprising
-- - Literals of type 'a'
-- - Binary operations 'Add' and 'Mul'
data IExpr =
    Lit Integer
  | Add IExpr IExpr
  | Mul IExpr IExpr
  deriving Show
```

Your goal will be to implement evaluation of these expressions
and parsing from string representation in
[Reverse Polish Notation](https://en.wikipedia.org/wiki/Reverse_Polish_notation).

### Representation

In [src/Task1.hs](src/Task1.hs) you will find following definition
for integer arithmetic expressions:

```haskell
data IExpr =
    Lit Integer
  | Add IExpr IExpr
  | Mul IExpr IExpr
  deriving Show
```

> Feel free to derive any other type classes like `Eq` if necessary.

### Evaluation

Using provided representation implement function `evalIExpr`
which fully evaluates given expression.

```haskell
evalIExpr :: IExpr -> Integer
```

**Example:**

```haskell
>>> evalIExpr (Lit 2)
2
>>> evalIExpr (Add (Lit 2) (Lit 3))
5
>>> evalIExpr (Add (Mul (Lit 3) (Lit 2)) (Lit 3))
9
```

### Parsing

Next you need to implement parsing of `IExpr` from
[Reverse Polish Notation](https://en.wikipedia.org/wiki/Reverse_Polish_notation)
(RPN).

Since RPN is just one of many possible string representations
for arithmetic expressions, it makes sense to abstract
parsing into a dedicated type class (like `Read` from `Prelude`).

However, instead of using built-in type class `Read` you should
use a much simpler type class `Parse` provided in [src/Task1.hs](src/Task1.hs).

```haskell
-- | Class of parseable types
class Parse a where
  -- | Parses value 'a' from given string
  -- wrapped in 'Maybe' with 'Nothing' indicating failure to parse
  parse :: String -> Maybe a
```

Define an instance of `Parse` for `IExpr` which will parse
arithmetic expression in RPN with symbols `+` and `*`
representing addition and multiplication respectively.

```haskell
instance Parse IExpr where
  parse = ...
```

> [!TIP]
>
> For simple parsing like in this assignment you can use standard function
> [words](https://hackage.haskell.org/package/base-4.21.0.0/docs/Prelude.html#v:words)
> from `Prelude` to split given string by spaces into a list of words.

**Example:**

```haskell
>>> parse "2" :: Maybe IExpr
Just (Lit 2)
>>> parse "2 3 +" :: Maybe IExpr
Just (Add (Lit 2) (Lit 3))
>>> parse "3 2 * 3 +" :: Maybe IExpr
Just (Add (Mul (Lit 3) (Lit 2)) (Lit 3))
>>> parse "2 +" :: Maybe IExpr
Nothing
>>> parse "2 3" :: Maybe IExpr
Nothing
```

### Evaluation with parsing

Finally, combine both evaluation and parsing into
single function `evaluateIExpr`, which parses RPN
and immediately evaluates it.

```haskell
evaluateIExpr :: String -> Maybe Integer
```

**Example:**

```haskell
>>> evaluateIExpr "2"
Just 2
>>> evaluateIExpr "2 3 +"
Just 5
>>> evaluateIExpr "3 2 * 3 +"
Just 9
>>> evaluateIExpr "2 +"
Nothing
>>> evaluateIExpr "2 3"
Nothing
```

> Note that result is wrapped into `Maybe` with `Nothing`
> indicating that given expression could not be parsed as RPN.

## Task 2 (4 points)

Now that you have a prototype for `Integer` expressions
with couple of operations, it is time to extend its functionality
in a more general representation.

In particular, new representation should
- support expressions with variables (like `x 2 +`)
- support arbitrary domain type (not only `Integer`)
- support different binary operations for different domain types

### Generalized expressions

Based on outlined requirements, prototype from first task needs to be
extended with additional constructor `Var String` and become parameterized
with domain type `a` and type of binary operations (enumeration) `op`.

Again, this new representation is already provided in [src/Task2.hs](src/Task2.hs):

```haskell
-- | Generalized representation of expressions comprising
-- - Literals of type 'a'
-- - Variables with arbitrary 'String' names
-- - Binary operations of type 'op'
data Expr a op =
    Lit a
  | Var String
  | BinOp op (Expr a op) (Expr a op)
  deriving Show
```

Also provided is enumeration of integer binary operations, this time with subtraction:

```haskell
-- | Integer binary operations
data IntOp = Add | Mul | Sub
  deriving Show
```

### Parsing

Define an instance of `Parse` for `Expr` that parses expression from RPN.
Note that because `Expr` is parameterized with types `a` and `op`, you will need to
require these types to be instances of `Parse` as well:

```haskell
instance (Parse a, Parse op) => Parse (Expr a op) where
  parse = ...
```

**Example:**

```haskell
>>> parse "2" :: Maybe (Expr Integer IntOp)
Just (Lit 2)
>>> parse "2 3 -" :: Maybe (Expr Integer IntOp)
Just (BinOp Sub (Lit 2) (Lit 3))
>>> parse "3 2 * 3 +" :: Maybe (Expr Integer IntOp)
Just (BinOp Add (BinOp Mul (Lit 3) (Lit 2)) (Lit 3))
>>> parse "2 +" :: Maybe (Expr Integer IntOp)
Nothing
>>> parse "2 3" :: Maybe (Expr Integer IntOp)
Nothing
```

### Evaluation of operations

Evaluating these generalized expressions is not as simple as it was in first task,
because semantics of the same binary operation may be different depending on domain type
(e.g. addition operation must be evaluated completely differently in `Integer` expressions
and in `Float` expressions).

In order to solve this problem we can introduce yet another type class `Eval a op`,
parameterized with both the domain type `a` and binary operations `op`, allowing us
to define separate implementations for operations in different contexts:

```haskell
-- | Class of evaluatable types
class Eval a op where
  -- | Evaluates given binary operation with provided arguments
  evalBinOp :: op -> a -> a -> a
```

Implement instance of `Eval` for `Integer` with `IntOp` operations.

> [!NOTE]
>
> Haskell 2010 prohibits type classes with multiple parameters.
> However, this restriction can be lifted with following
> [pragma](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/multi_param_type_classes.html)
>
> ```haskell
> {-# LANGUAGE MultiParamTypeClasses #-}
> ```
>
> In modern Haskell versions (`GHC2021` and `GHC2024`) this pragma is enabled by default.

### Evaluation of expressions

Next, implement function `evalExpr` which evaluates given expression
using given association list of variable values:

```haskell
evalExpr :: (Eval a op) => [(String, a)] -> Expr a op -> Maybe a
```

> Returns `Nothing` in case appropriate variable value is missing.

**Example:**

```haskell
>>> evalExpr [] (Lit 2 :: Expr Integer IntOp)
Just 2
>>> evalExpr [("x", 3)] (BinOp Add (Lit 2) (Var "x")) :: Maybe Integer
Just 5
>>> evalExpr [("x", 3)] (BinOp Add (Lit 2) (Var "y")) :: Maybe Integer
Nothing
```

### Evaluation with parsing

Finally, we need to combine both generalized evaluation and parsing
into single function.

In fact, this function is already provided in [src/Task2.hs](src/Task2.hs):

```haskell
evaluate :: forall a op. (Eval a op, Parse a, Parse op) => [(String, a)] -> String -> Maybe a
evaluate m s = case parse s of
  Just e -> evalExpr m (e :: Expr a op)
  Nothing -> Nothing
```

> Check out the explanation below to understand why this generic function is the way it is
> and how to use it to implement `evaluateInteger`.

So your job is to only implement the `Integer`-specific version in `evaluateInteger`
using this predefined function `evaluate`:

```haskell
evaluateInteger :: [(String, Integer)] -> String -> Maybe Integer
```

**Example:**

```haskell
>>> evaluateInteger [] "2"
Just 2
>>> evaluateInteger [("x", 3)] "2 x -"
Just (-1)
>>> evaluateInteger [("x", 3)] "2 y -"
Nothing
>>> evaluateInteger [] "3 2 * 3 +"
Just 9
>>> evaluateInteger [] "2 +"
Nothing
>>> evaluateInteger [] "2 3"
Nothing
```

> [!IMPORTANT]
>
> Ideally we would have the following function that will use `parse` and `evalExpr`
> in its definition:
> 
> ```haskell
> evaluate :: (Eval a op, Parse a, Parse op) => [(String, a)] -> String -> Maybe a
> evaluate m s = case parse s of
>   Just e -> evalExpr m e
>   Nothing -> Nothing
> ```
> 
> However, if you try to implement it as shown, the compiler will start complaining:
> 
> ```
> • Could not deduce (Parse op0) arising from a use of ‘parse’
>   from the context: (Eval a op, Parse a, Parse op)
>   ...
> • Could not deduce (Eval a op0) arising from a use of ‘evalExpr’
>   from the context: (Eval a op, Parse a, Parse op)
>   ...
> ```
> 
> We accidentally defined way too general function,
> such that compiler cannot prove that the expression `e`
> has type `Expr a op` with the same `a` and `op` which are used in constraints.
>
> Even specifying explicit type as `e :: Expr a op` will not work,
> because `a` and `op` in this type signature have no relation
> to `a` and `op` in constraints. In this case they are just some random
> names for generic type parameters (we might as well have specified it
> as `e :: Expr foo bar` to the same result).
>
> There is, however, a way to reference generic type parameter names from inside of
> function definition, using
> [Lexically scoped type variables](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/scoped_type_variables.html#scoped-type-variables)
> provided by following pragma (enabled by default in `GHC2021` and `GHC2024`):
>
> ```haskell
> {-# LANGUAGE ScopedTypeVariables #-}
> ```
>
> This extension allows us to specify explicit `forall a op.` clause
> which brings type variables `a` and `op` into scope and thus
> makes explicit type `Expr a op` work inside of function definition.
>
> Unfortunately in order to actually make this function compilable we also need to allow
> defer [Ambiguous type check](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/ambiguous_types.html)
> until actual use of this function using following pragma:
>
> ```haskell
> {-# LANGUAGE AllowAmbiguousTypes #-}
> ```
>
> Now if you try to implement `evaluateInteger` like this:
>
> ```haskell
> evaluateInteger :: [(String, Integer)] -> String -> Maybe Integer
> evaluateInteger = evaluate
> ```
>
> you would get the following error from compiler:
>
> ```
> • Ambiguous type variable ‘op0’ arising from a use of ‘evaluate’
>   prevents the constraint ‘(Eval Integer op0)’ from being solved.
>   Probable fix: use a type annotation to specify what ‘op0’ should be.
>   Potentially matching instance:
>     instance Eval Integer IntOp
> ```
> 
> This means that compiler could not figure out that type variable `op` should
> correspond to `IntOp` (indeed there might be many different `Expr Integer _` instances defined).
> And as described in "Probable fix" compiler suggests to somehow specify that ambiguous type variable.
>
> There is a very elegant way of specifying type variable values for expression using
> [Visible type application](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/type_applications.html#visible-type-application)
> provided by following pragma and enabled by default in `GHC2021` and `GHC2024`:
>
> ```haskell
> {-# LANGUAGE TypeApplications #-}
> ```
>
> This extension allows us to specify explicit values for both `a` and `op` type variables:
> 
> ```haskell
> evaluateInteger = evaluate @Integer @IntOp
> ```
>
> Or even just for `op` type variable, since compiler is able to figure out that `a` must be `Integer` anyway:
>
> ```haskell
> evaluateInteger = evaluate @_ @IntOp
> ```

## Task 3 (3 points)

The last task is to implement brute force [SAT solver](https://en.wikipedia.org/wiki/SAT_solver)
using representation `Expr` (from previous task) for Boolean formulas in Reverse Polish Notation.

> In computer science and formal methods, a SAT solver is a computer program which aims to solve the
> [Boolean satisfiability problem](https://en.wikipedia.org/wiki/Boolean_satisfiability_problem) (SAT).
> On input a formula over Boolean variables, such as "(x or y) and (x or not y)", a SAT solver outputs
> whether the formula is satisfiable, meaning that there are possible values of x and y which make the formula true,
> or unsatisfiable, meaning that there are no such values of x and y.
> In this case, the formula is satisfiable when x is true, so the solver should return "satisfiable".
>
> -- [SAT solver, Wikipedia](https://en.wikipedia.org/wiki/SAT_solver)

### SAT solver

You should implement function `solveSAT` which parses given Boolean formula in Reverse Polish Notation
and returns whether this formula is satisfiable wrapped into `Maybe` with `Nothing` indicating
parsing failure:

```haskell
solveSAT :: String -> Maybe Bool
```

Input formula will consist of variables and following binary operations:
- `and`
- `or`
- `xor`

**Example:**

```haskell
>>> solveSAT "x y and"
Just True
>>> solveSAT "x y xor x y and and"
Just False
>>> solveSAT "x y xor x y and or"
Just True
>>> solveSAT "x xor"
Nothing
```

> [!TIP]
>
> When implementing enumeration of all possible variable mappings
> it might be helpful to use [list comprehensions](https://wiki.haskell.org/List_comprehension) in some way.
>
> Here is example of generating all pairs, where first element is from list `[1,2,3]` and second from `['a','b']`:
>
> ```haskell
> >>> [(x, y) | x <- [1,2,3], y <- "ab"]
> [(1,'a'),(1,'b'),(2,'a'),(2,'b'),(3,'a'),(3,'b')]
> ```
