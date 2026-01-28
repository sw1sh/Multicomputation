# Multicomputation

[Documentation](https://resources.wolframcloud.com/PacletRepository/resources/Wolfram/Multicomputation/)

## Multi Object

`Multi` is a lazy non-deterministic container with tensor product semantics. It represents expressions with multiple possible values and supports multiway rewriting systems.

### Basic Usage

```wolfram
(* Multiple alternatives *)
Multi[{1, 2, 3}]                        (* 3 alternatives *)

(* Tensor product - lazy combination *)
Multi[{1, 2}] + Multi[{10, 20}]         (* 4 combinations: {11, 21, 12, 22} *)

(* Rule-based binding *)
Multi[5, x_ :> {x, x+1, x^2}]           (* 5 → {5, 6, 25} *)

(* Automatic chaining via UpValues *)
f[Multi[{a, b}]]                        (* → Multi with {f[a], f[b]} *)
```

### Constructors

| Syntax | Description |
|--------|-------------|
| `Multi[{a, b, c}]` | Alternatives from list |
| `Multi[expr, rule]` | Apply replacement rule to expression |
| `Multi[expr, {rules...}]` | Apply multiple rules |
| `Multi[assoc]` | Create from association (internal) |

---

## Properties Reference

Access properties via `multi["PropertyName"]` or `multi["PropertyName", args...]`.

### Core Data Properties

| Property | Description |
|----------|-------------|
| `"Expression"` | The underlying expression with placeholders |
| `"HoldExpression"` | Expression wrapped in `HoldComplete` |
| `"Data"` | Full internal association |
| `"ValidQ"` | Whether the Multi is well-formed |
| `"Values"` | Association mapping positions to alternative values |
| `"Matches"` | Association mapping positions to rule match results |
| `"Rules"` | List of rewrite rules used |
| `"Keys"` | Combined keys from Values and Matches |
| `"Positions"` | Position specifications for all alternatives |
| `"Placeholders"` | Association of placeholder symbols to values |
| `"Bindings"` | Pattern variable bindings from rule matches |
| `"Size"` | Number of alternative positions |
| `"ValueCount"` | Product of alternative counts (total combinations) |
| `"MatchCount"` | Total number of rule matches |

### Values & Tuples

| Property | Description |
|----------|-------------|
| `"ListValues"` | List of possible values at each position |
| `"HoldListValues"` | Same, wrapped in `HoldForm` |
| `"Tuples"` | All combinations (Cartesian product) of values |
| `"Tuples", n` | First `n` tuples |
| `"HoldTuples"` | Tuples with held values |
| `"MatchBindings"` | Pattern bindings for each match |

### Evaluation Properties

These evaluate the expression by substituting alternatives:

| Property | Description |
|----------|-------------|
| `"EvaluateList"` | List of all evaluated alternatives |
| `"EvaluateList", n` | First `n` alternatives |
| `"HoldEvaluateList"` | Evaluated alternatives, held |
| `"EvaluateListOnce"` | One step of evaluation per position |
| `"HoldEvaluateListOnce"` | One step, held |

### Multi-Returning Evaluation

These return new `Multi` objects:

| Property | Args | Description |
|----------|------|-------------|
| `"Evaluate"` | `n` | Apply rules `n` times, return Multi |
| `"EvaluateOnce"` | `n, m, k` | One-step eval, `n` iterations, `m` positions, `k` tuples |
| `"HoldEvaluate"` | | Evaluate with held results |
| `"HoldEvaluateOnce"` | | One step, held |
| `"MultiEvaluate"` | `n` | Return list of Multi (one per branch) |
| `"HoldMultiEvaluate"` | `n` | Same, held |
| `"MultiEvaluateList"` | | Expressions from MultiEvaluate |

### List/Multi Hybrid Evaluation

| Property | Description |
|----------|-------------|
| `"MultiList"` | Split into list of Multis (one per list element) |
| `"ListEvaluate"` | Evaluate each list element's Multi separately |
| `"HoldListEvaluate"` | Same, held |
| `"MultiListEvaluate"` | Flatten ListEvaluate back to single Multi |
| `"HoldMultiListEvaluate"` | Same, held |
| `"ListEvaluateWithKeys"` | ListEvaluate with position keys |

### Transformation Properties

| Property | Args | Description |
|----------|------|-------------|
| `"Apply"` | `f, pos` | Apply function `f` at position(s) |
| `"HoldApply"` | `f, pos` | Apply without evaluating argument |
| `"ListApply"` | `f` | Apply to expression as list |
| `"DeleteDuplicates"` | | Remove duplicate alternatives |
| `"ReplacePart"` | `rules` | Replace parts of expression |
| `"RematchRules"` | | Re-apply stored rules |
| `"ModifyData"` | `f` | Transform internal data with function |

### Graph Properties

Generate visualizations of multiway evolution:

| Property | Args | Description |
|----------|------|-------------|
| `"Graph"` | `n, opts` | Evolution graph for `n` steps |
| `"HoldGraph"` | `n, opts` | Evolution graph with held expressions |
| `"CausalGraph"` | `n, opts` | Causal relationships between events |
| `"BranchialGraph"` | `n, opts` | Connections between concurrent states |
| `"TokenEventGraph"` | `n, opts` | Token-event relationships |
| `"EvolutionCausalGraph"` | `n, opts` | Combined evolution + causal |
| `"CausalBranchialGraph"` | `n, opts` | Combined causal + branchial |
| `"CausalStatesGraph"` | `n, opts` | Causal graph of states |

### Event & Edge Properties

| Property | Description |
|----------|-------------|
| `"Events"` | Tagged events from evaluation |
| `"HoldEvents"` | Events with held expressions |
| `"Edges"` | Directed edges for graph construction |
| `"HoldEdges"` | Edges with held expressions |
| `"Branches"` | Output states from each event |
| `"HoldBranches"` | Branches with held expressions |
| `"BranchPairs"` | Pairs of concurrent branches |
| `"HoldBranchPairs"` | Branch pairs, held |
| `"Foliations"` | Layer-by-layer evolution with counts |
| `"HoldFoliations"` | Foliations with held expressions |

---

## Examples

### Tensor Product (Lazy Cartesian Product)

```wolfram
m = Multi[{1, 2, 3}] + Multi[{10, 20}];
m["Expression"]      (* $MultiPlaceholder$1 + $MultiPlaceholder$2 *)
m["Tuples"]          (* {{1,10}, {1,20}, {2,10}, {2,20}, {3,10}, {3,20}} *)
m["EvaluateList"]    (* {11, 21, 12, 22, 13, 23} *)
```

### Rule-Based Binding

```wolfram
(* Each x produces variable alternatives based on value *)
m = Multi[{1, 2, 3}, x_ :> Range[x]];
m["EvaluateList"]    (* 1×2×3 = 6 combinations *)

(* Conditional branching *)
m = Multi[Range[5], {
  x_ /; EvenQ[x] :> {x, x/2},
  x_ /; OddQ[x] :> {x, 3x + 1}
}];
m["EvaluateList"]    (* 2^5 = 32 combinations *)
```

### Multiway Evolution Graph

```wolfram
m = Multi[1, x_ :> {x + 1, x * 2}];
m["Graph", 4]        (* Visualize 4 steps of evolution *)
```

### Subset Sum Algorithm

```wolfram
(* Each element: include (element) or exclude (0) *)
subsetSums[nums_] := Total[Multi[{0, #}] & /@ nums]

ss = subsetSums[{3, 7, 1, 8, 4}];
Pick[ss["Tuples"], ss["EvaluateList"], 12]  (* Subsets summing to 12 *)
```

---

## Automatic Chaining

Multi has `UpValues` that propagate through expressions:

```wolfram
m = Multi[{1, 2, 3}];
m + 10                 (* → Multi, evaluates to {11, 12, 13} *)
Sin[m]                 (* → Multi, evaluates to Sin values *)
f[g[m]]                (* → Multi, expression f[g[$placeholder]] *)

(* Rule-based Multi chains too *)
m2 = Multi[5, x_ :> {x, x+1}] * 10;
m2["EvaluateList"]     (* {50, 60} *)
```

No explicit `["EvaluateList"]` needed for intermediate steps—just use Multi in expressions naturally.