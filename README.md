Implementation of the Typed Call-by-Value λ-calculus using a Stack of Regions
---

> My implementation and related notes

## Problems about paper

1. Where will `at` appear and why?
2. What does "size of region" means?
3. Why is "effect" encoded in the type scheme?
4. Is their any special restriction over the recursive `letrec`?
5. The type et cetera is just a temporary thing? Or should I pass them down in the implementation?

## Idea
First, we will implement the algorithms introduced in the paper. Then the *target language* will go through a second pass into a simplified subset of Rust with same-level annotation. Then, the simplified version will be transformed into real Rust code.


