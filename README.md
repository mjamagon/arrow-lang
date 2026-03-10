# Arrow Lang

A dataflow pipeline language embedded in Org Babel. Define computation graphs in a concise text syntax, visualize them as ASCII flowcharts, and execute them as real pipelines — all from an Org file.

Arrow chains named code blocks together, threading each block's `output` as the next block's `input`. Blocks can be written in Python, shell, Ruby, R, Julia, or JavaScript. Python blocks communicate via pickle serialization; others pass strings through stdout.

## Quick Start

```elisp
(load-file "/path/to/arrow-lang.el")
```

In any Org file, create an arrow source block and press `C-c C-c`:

```
#+begin_src arrow
Flow := LoadData > Process > Analyze > Plot
Flow
#+end_src
```

This renders an ASCII flowchart in the `*arrow*` buffer and scaffolds stub `#+name:` blocks for any nodes that don't yet exist. Fill in the blocks:

```
#+name: LoadData
#+begin_src python :results output
import pandas as pd
output = pd.read_csv("data.csv")
#+end_src

#+name: Process
#+begin_src python :results output
df = input  # receives LoadData's output
output = df.dropna()
#+end_src
```

Then press `C-c C-c` on the arrow block again to execute the pipeline.

## Syntax

```
A > B                     sequence: B receives A's output
(A, B, C)                 parallel fork: all branches run concurrently, results merge into a dict
A > B >? C                or-fork: first branch to succeed wins
A > B*                    parallel map: B runs once per element of A's output list
name := A > B > C         module definition (reusable sub-pipeline)
A > B : label             labeled transition
A > B / Facilitator       facilitator annotation on a transition
flow1 ; flow2             independent parallel tracks
-- comment                line comment
```

### Suffixes

Append suffixes to node or module references to control caching and re-execution:

```
Flow#                     cache all nodes in Flow (skip re-runs when input+code unchanged)
Flow!                     force re-run all nodes in Flow (bypass cache)
Node#                     cache a single node
Node!                     force re-run a single node
```

### Secondary Arrows

When a node needs input from a non-adjacent node, add a secondary arrow on its own line. All referenced nodes must already be defined:

```
A > B > C > D
A > D
```

`D` receives a merged dict: `{"C": <C's output>, "A": <A's output>}`.

Secondary arrows also work inside mapped modules using dotted notation:

```
PerItem := (LoadMeta, LoadData) > Merge > Analyze > Report
Pipeline := Prepare > PerItem*
PerItem.LoadMeta > PerItem.Report
```

## Shared Blocks

Blocks tagged `:arrow shared` are assembled into a shared module that is automatically imported by every pipeline block. Use this for common imports and utility functions:

```
#+name: Imports
#+begin_src python :results output :arrow shared
import numpy as np
import pandas as pd
#+end_src

#+name: Utils
#+begin_src python :results output :arrow shared
def normalize(x):
    return (x - x.mean()) / x.std()
#+end_src
```

All symbols from shared blocks (functions, classes, imported modules) are available in every pipeline block without explicit imports.

## Block Convention

Every pipeline block follows the same pattern — read from `input`, write to `output`:

```
#+name: MyNode
#+begin_src python :results output
data = input                    # whatever the previous node produced
result = do_something(data)
output = result                 # passed to the next node
#+end_src
```

For the first node in a pipeline, `input` is `None`. For nodes after a fork, `input` is a dict keyed by branch names. Print statements go to stderr (visible in the exec log) so they don't interfere with the serialization protocol.

## Execution & Visualization

When you execute a pipeline (`C-c C-c` on the arrow block, or `M-x arrow-run`), the `*arrow*` buffer shows a live-updating flowchart:

- **Gray** — pending
- **Yellow** — running
- **Green** — done
- **Cyan** — cache hit (skipped)
- **Red** — error

![arrow](https://github.com/user-attachments/assets/7e163243-ec65-4a64-9cc0-57a3f20ad559)

Keybindings in the `*arrow*` buffer:

| Key | Action |
|-----|--------|
| `C-c C-l` | Show the execution log (`*arrow-exec*`) |
| `RET` | Open the REPL with all node outputs loaded |

## Caching

Append `#` to a node or module name to enable caching:

```
Flow := LoadData > ExpensiveFit# > Plot
Flow
```

Or cache an entire flow:

```
Flow := LoadData > Fit > Plot
Flow#
```

Cache keys are derived from `SHA256(block body + shared module + input hash)`. A cache hit copies the stored pickle and skips the subprocess entirely. Cache entries are stored in `.arrow-cache/` next to the Org file.

Use `!` to force a re-run even when a cache hit exists:

```
Flow!    -- force re-run everything
Fit!     -- force re-run just Fit
```

## Parallel Map

When a node's output is a list, the next node can be mapped over each element in parallel:

```
ListItems > ProcessItem* > Collect
```

`ProcessItem` runs as an independent subprocess for each element of `ListItems`'s output. Results are collected in order and merged back into a list.

If `ProcessItem` is a module, the entire sub-pipeline is expanded and executed per element:

```
PerItem := Load > Transform > Analyze
Pipeline := MakeList > PerItem* > Summarize
```

## REPL

After a pipeline completes, press `RET` in the `*arrow*` buffer to open an interactive Python REPL. All node outputs are available in a `nodes` dictionary:

```python
>>> nodes["Analyze"]        # access any node's output
>>> nodes["LoadData"].head() # inspect intermediate results
```

Shared module symbols (functions, imports) are also available in the REPL namespace. The REPL auto-refreshes when you re-run the pipeline.

## Multiple Pipelines

A single Org file can contain multiple named arrow blocks. Use `M-x arrow-run` to pick which one to execute:

```
#+name: QuickCheck
#+begin_src arrow
Load > Validate > Report
#+end_src

#+name: FullAnalysis
#+begin_src arrow
Load > Clean > Fit > Diagnose > Plot
#+end_src
```

## Supported Languages

Python is the primary language (with pickle-based data threading), but blocks can be written in any supported language. Non-Python blocks receive `input` as a shell/language variable containing the previous node's stdout string.

| Language | Interpreter |
|----------|-------------|
| python / python3 | `python3` |
| sh / bash | `bash` |
| ruby | `ruby` |
| perl | `perl` |
| R | `Rscript` |
| julia | `julia` |
| javascript / js | `node` |
