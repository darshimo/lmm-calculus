# Reduction Machine in λ̅μ-calculus
<!-- # Reduction Machine in λ̅μμ̃-calculus-->

## Overview

head reduction machine in λ̅μ-calculus.

## Usage

```sh
$ cd lmm-calculus
$ cargo run
```

After this, please input λ̅μ-term to reduce.

## Syntax of λ̅μ-calculus

```
c ::=               Commands
      ⟨v|E⟩

E ::=               Contexts
      α
      v⋅E

v ::=               Terms
      x
      μβ.c
      λx.v
```

Note that consecutive characters are regarded as a token. You should put whitespace between separate variables.

## Example

In reduction process, variable names are changed properly in order to avoid *variable capture*.

```
$ cargo run
head reduction machine in λ̅μ-calculus
please input λ̅μ-term
⟨λx.μβ.⟨x|β⟩|λy.y⋅z⋅α⟩

  ⟨λx.μβ.⟨x|β⟩|λy.y⋅z⋅α⟩
→ ⟨μβ.⟨λy.y|β⟩|z⋅α⟩
→ ⟨λy.y|z⋅α⟩
→ ⟨z|α⟩
```

## Requirement
cargo 1.35.0
