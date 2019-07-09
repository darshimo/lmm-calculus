# Reduction Machine in λ̅μμ̃-calculus

## Overview

head reduction machine in λ̅μμ̃-calculus.

## Usage

```sh
$ cd lmm-calculus
$ cargo run
```

After this, please input λ̅μμ̃-term to reduce.

## Syntax of λ̅μμ̃-calculus

```
c ::=               Commands
      ⟨v|E⟩

v ::=               Terms
      x
      μβ.c
      λx.v

E ::=               Contexts
      α
      μ̃x.c
      v⋅E
```

Note that consecutive characters are regarded as a token. You should put whitespace between separate variables.

## Example

In reduction process, variable names are changed properly in order to avoid *variable capture*.

```
$ cargo run
head reduction machine in λ̅μμ̃-calculus
please input λ̅μμ̃-term
⟨z|μ̃x.⟨λy.y|μβ.⟨λw.λt.t|z⋅β⟩⋅x⋅α⟩⟩

  ⟨z|μ̃x.⟨λy.y|μβ.⟨λw.λt.t|z⋅β⟩⋅x⋅α⟩⟩
→ ⟨λx₁.x₁|μβ.⟨λx₂.λx₄.x₄|z⋅β⟩⋅z⋅α⟩
→ ⟨μβ.⟨λx₂.λx₄.x₄|z⋅β⟩|μ̃x₅.⟨x₅|z⋅α⟩⟩
→ ⟨μβ.⟨λx₂.λx₄.x₄|z⋅β⟩|z⋅α⟩
→ ⟨λx₂.λx₄.x₄|z⋅z⋅α⟩
→ ⟨z|μ̃x₆.⟨λx₇.x₇|z⋅α⟩⟩
→ ⟨λx₈.x₈|z⋅α⟩
→ ⟨z|μ̃x₉.⟨x₉|α⟩⟩
→ ⟨z|α⟩
```

## Requirement
cargo 1.35.0
