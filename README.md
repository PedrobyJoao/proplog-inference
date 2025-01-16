**WIP**

# kb |= q?

This is an inference engine tool based on **propositional logic**.

Given a knowledge base `kb` with a set of statements, the inference engine is capable of
telling if a given statement `q` is entailed by the knowledge base.

Basically, the engine checks if `kb |= q`

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

Given the initial knowledge base, new propositions will be generated by
inference rules upon the already existent information. The new
propositions will be added to the knowledge base.

The algorithm will then check if the query's proposition is within
the knowledge base. If not, it continues to apply inference rules,
now using also the new propositions included to the knowledge base.

If the query's proposition is within the knowledge base, then `kb` entails `q`.

Otherwise, after applying all possible inference rules, if the knowledge base
does not contain the query's proposition, then `kb` does _not_ entail `q`.

Types of inference rules to be applied:

- Immediate inference: applied upon one proposition
- Syllogisms: applied upon two propositions
- Compound syllogisms: applied upon three or more propositions

I'm afraid the algorithm might not be complete, meaning that it might
not derive all possible conclusions, and therefore might incorrectly
assert false for a valid entailment.

> Maybe, for it to be a complete algorithm, it might have to apply
> _all the possible inference rules_. Or maybe that is not necessary? I don't know.
>
> And even if we can manually enumerate all known inference rules,
> do we really know all possible inference rules? Or are there some not discovered yet?

**Time complexity**: ?

## v3: Search by inference rules with, as last resource, recursive enumeration

## v4: Resolution Algorithm

## v5: all of them elegantly combined

TODO

## TODOs

Inference engine:

- [ ] move algorithm out of Logic.hs, and keep only types and inference rules stuffs
- [ ] Algorithm v2: generate all possible inference rules and check if query is between them
- [ ] Algorithm v3: Resolution algorithm
- [ ] Algorithm v1: do not check trees until the end when we can already discard it on the beginning
  - if query isn't true for branch
  - if knowledge base isn't true for branch
- [ ] (maybe) Parser: use yaml instead of txt
