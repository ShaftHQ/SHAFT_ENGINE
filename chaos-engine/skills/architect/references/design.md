# Design the code

## 6 API contracts

- Contract first: request, response, errors, limits written before code.
- Additive only on a published API: new optional fields, new endpoints.
  Removal or meaning change = new version + deprecation window.
- Hyrum's law: any observable behaviour becomes a dependency. Change
  observables (order, timing, error text) deliberately.
- Mutations idempotent (key or natural id); list endpoints paginated;
  errors typed with stable codes. Follow repo style, else Google AIP.

## 7 Boundaries

- Domain logic pure and inward; I/O (DB, HTTP, files, clock) at edges.
- Dependencies point to the more stable module. Cycles = design bug.
- Interface only at an I/O seam or with 2+ real implementations today
  (Ponytail wins otherwise).
- One module, one reason to change. Big module touched often: split along
  the change axis, in its own step.

## 8 Domain language

- Name types and functions with the users' words; one term, one meaning.
- Different meanings across areas = different bounded contexts; translate
  at the boundary, do not share one bloated model.

## 9 Principles

- KISS and YAGNI first; DRY only for knowledge, not look-alike code.
- Rule of three: duplicate twice, abstract on the third real use.
- Wrong abstraction costs more than duplication; inline it back.
- SOLID as smell detectors, not a target: god class (S), switch on type
  growing (O), surprising subclass (L), fat interface (I), I/O in core (D).

## 10 Patterns

Pick by a force present now:

| Force | Pattern |
| --- | --- |
| Many variants of one algorithm | Strategy (or a map of functions) |
| Complex construction | Builder / factory function |
| Incompatible interface | Adapter |
| Cross-cutting add-on | Decorator / middleware |
| Notify many, decoupled | Observer / events |
| State-dependent behaviour | State machine (explicit enum + table) |

No force, no pattern. Prefer language features over class ceremony.
