# Lambda Calculus Evaluator

## Overview

This project is divided into 5 components:

1. Lexer
2. Parser
3. Compiler
4. Evaluator
5. Interpreter

## Lexer

Given a string, the `LambdaLexer` class generates a list of tokens of type `LambdaToken` that the `LambdaParser` uses to construct an abstract syntax tree of type `LambdaAST`.

There are eight distinct tokens that the lexer recognizes:

1. DOT

    The symbol separating a function args and function body.

2. LAMBDA

    The symbol indicating the start of a function.

3. OPEN

    The symbol indicating the opening parenthesis.

4. CLOSE

    The symbol indicating the closing parenthesis.

5. LET

    The symbol indicating the start of a let expression.

6. EQUAL

    The symbol separating the key and value in a let expression.

7. IN

    The symbol separating the value and the body in a let expression.

8. ID

    The symbol indicating a variable.

## Parser

Given a sequence of `LambdaToken`, the `LambdaParser` generates an abstract syntax tree of type `LambdaAST` that represents the lambda expression.

The grammar for the lambda expression is defined as:

    <expr> ::= ID
            |   <expr>, <expr>
            |   OPEN, <expr>, CLOSE
            |   LAMBDA, ID+, DOT, <expr>
            |   LET, ID, EQUAL, <expr>, IN, <expr>


Because `<expr> ::= <expr>, <expr>` we have left recursion in the grammar and we need to eliminate it. Hence, we simplify it as follows:

    <expr> ::= <simpleExpr>+

    simpleExpr ::= ID
                |   OPEN, <expr>, CLOSE
                |   LAMBDA, ID+, DOT, <expr>
                |   LET, ID, EQUAL, <expr>, IN, <expr>


Now, `<expr>` doesn't have left recursion anymore because to match `<expr>`, we need to first match atleast one simpleExpr.

We also note that since, FApp is left associative, we get a list of `<expr>` and then reduce that list from left to right with FApp. I.e. `<expr> = rep1(<simpleExpr>).reduce(FApp)`.

Additionally, we note that the let expression is redundant for the language as it can be simplified to an application expression. I.e. `LET, x, EQUAL, y, IN, z` can be parsed as `FApp(Fun(x, z), y)`. However, we continue to parse the let expression as a Let node.

## Compiler

Given a string representing a lambda expression, the `LambdaCompiler` makes use of `LambdaLexer` and `LambdaParser` to generate an abstract syntax tree of type `LambdaAST`.

## Evaluator

Given, a string representing a lambda expression, the `LambdaEvaluator` makes use of `LambdaCompiler` to obtain the abstract syntax tree of the lambda expression. It then evaluates the abstract syntax tree in a call-by-value evaluation order. It does so using alpha-renaming and beta-reduction.

`LambdaEvaluator` has two import methods, `eval` and `substitute`. `eval` does the beta-reduction and `substitute` does the alpha-renaming.


`eval` repeatedly makes use of `substitute` to reduce the abstract syntax tree according to the following rules:

1. If the abstract syntax tree is of type `Fun` (a function) or `Var` (a variable), then result is the same abstract syntax tree. I.e. do not beta-reduce the function body.
2. If the abstract syntax tree is of type `Let` (a let expression), then rewrite the abstract syntax tree into a `FApp` type as follows:
`Let(x, y, z)` is re-written as `FApp(Fun(x, z), y)`.
3. If the abstract syntax tree is of type `FApp` then:
    1. Evaluate the caller
    2. Evaluate the callee
    3. If the caller evaluates to a function (`Fun` type), then substitute the callee into the body of the evaluated caller.
    4. If the caller does not evaluate to a function, then return the abstract syntax tree representing the application of the evalued caller to the evaluated callee.

As, the above routine can be difficult to follow, here is the relevant code for `eval`:
    
``` scala

    def eval(ast: LambdaAST): LambdaAST = {
        ast match {
          case FApp(f, v) => {
            val ef = eval(f)
            val ev = eval(v)
            ef match {
              case Fun(arg, body) => eval(substitute(body, arg, ev))
              case _ => FApp(ef, ev)
            }
          }
          case Let(key, value, body) => {
            eval(FApp(Fun(key, body), value))
          }
          case Var(_) => ast
          case Fun(_, _) => ast
          case _ => throw new Exception("Invalid expression.")
        }
      }

```

As mentioned above, `eval` makes use of `substitute` to do alpha-renaming. `substitute` takes three arguments `ast`, `key` and `alue`. It replaces all occurrences of `key` in `ast` with `value` using the following rules:

1. If `ast` is a variable then:
    1. If the variable has the same name as the key, return the `value`.
    2. If the variable has a different name as the key, return the variable instead.
2. If `ast` is an application then, substitute `key` with `value` in both the caller and the caller of the application.
3. If `ast` is a function then:
    3. If the argument of the function is the same as the key, don't substitute anything and return the function instead.
    3. If the argument of the function is different from the key, substitute the `key` in the body of the function with the `value`.

Again, here's the code for `substitute`:

``` scala

    def substitute(ast: LambdaAST, key: String, value: LambdaAST): LambdaAST = {
        ast match {
          case Var(x) => {
            if (x == key) value
            else ast
          }
          case FApp(f, v) => {
            val f_sub = substitute(f, key, value)
            val v_sub = substitute(v, key, value)
            FApp(f_sub, v_sub)
          }
          case Fun(arg, body) => {
            if (arg == key) ast
            else Fun(arg, substitute(body, key, value))
          }
        }
      }
```

## Interpreter

The `LambdaInterpreter` is a REPL that makes use of `LambdaEvaluator` to evaluate the expressions read from the standard input.

`LambdaInterpreter` calls `LambdaEvaluator` on every line read from the standard input.
