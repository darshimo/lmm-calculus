# Reduction Machine in λ-calculus
<!-- # Reduction Machine in λ̅μμ̃-calculus-->

## Overview

head reduction machine in λ-calculus.

## Usage

```sh
$ cd lmm-calculus
$ cargo run
```

After this, please input λ-term to reduce.

## Grammar of λ-term

```
t ::=               terms
      x             variable
      λx.t          abstraction
      t t           application
```

Note that consecutive characters are regarded as a token. You should put whitespace between separate variables.

## Example

In reduction process, variable names are changed properly in order to avoid *variable capture*.

```
$ cargo run
λ-calculus head reduction machine
please input λ-term
(λx.λy.x y)(λz.z)(λw.w)

(λx. λy. x y) (λz. z) (λw. w)
(λx₁. (λz. z) x₁) (λw. w)
(λx₂. x₂) (λw. w)
λw. w
```

## Requirement
cargo 1.35.0
