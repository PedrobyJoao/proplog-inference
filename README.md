# kb |= q?

This is an inference engine tool based on **propositional logic**.

Given a knowledge base `kb` with a set of given statements, the inference engine is capable of
telling if a given statement `q` is entailed by the knowledge base.

In summary, this engine checks if `kb |= q`

## v1: Recursive enumeration and validation of models

A **recursive** algorithm that, **enumerating all models** given knowledge base's (`kb`) and query's (`q`) **symbols**,
checks if for _all_ models in which the knowledge base is _true_, if query `q` is also true for the given model.

**Time complexity**: `O(2^n)`

### Example:

Modus Ponens

```
knowledge(
    p -> q
    p
)

query(q)
```

Enumerate all possible models:

```
| p | q | kb |
|---|---|----|
| T | T | T  | # the only true model
| T | F | F  | # modus ponens, true -> q, therefore q must be also true. Invalid model
| F | T | F  | # p is true on the kb, so this model is invalid
| F | F | F  | # p is true on the kb, so this model is invalid
```

For all models in which kb is true, meaning just one of the models, is the query (q=q) also true?
Yes, `q` is true for this model, therefore kb entails q.

## v2: Search by inference rules

## v3: Resolution Algorithm

## TODOs

Inference engine:

- [ ] Algorithm v2: generate all possible inference rules and check if query is between them
  - Note: is this really more efficient?
- [ ] do not check trees until the end when we can already discard it on the beginning

Parser:

- [ ] use yaml

```

```
