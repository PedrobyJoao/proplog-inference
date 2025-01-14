This is an inference engine tool based on propositional logic.

Given a knowledge base `kb` with a set of given statements, the inference engine is capable of
telling if a given statement `q` is entailed by the knowledge base.

TL;DR: this engine checks if `kb |= q`

## TODOs

- [ ] represent propositional logic
- [ ] Simple recursive algorithm checking all enumerable truth values
- [ ] do not check trees until the end when we can already discard it on the beginning
