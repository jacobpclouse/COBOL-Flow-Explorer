# COBOL Flow Explorer

COBOL Flow Explorer is a local, dependency-free Python tool for creating an interactive flowchart from COBOL source. It maps paragraph fall-through, `PERFORM` calls, and `GO TO` targets, then lets you highlight paragraphs that read or change a selected data item. It does not use AI, send source anywhere, or execute the COBOL program.

## Run

Requires Python 3.10 or later. From this folder:

```sh
python3 cobol_flow.py examples/billing.cbl
```

This creates `examples/billing.flow.html`. Open that file in a browser. To choose another output path:

```sh
python3 cobol_flow.py path/to/program.cob --output flow.html
```

Click a data item to highlight its readers and writers. Use the **Flowchart** and **Data history** tabs to switch views. Data history lists detected initial values, assignments, clears, arithmetic/computed updates, reads, output writes, and arguments that an external `CALL` may modify, with source line, paragraph, and expression. Click a history row to jump back to its paragraph in the flowchart. Click a paragraph to see its source statements. The paragraph search and data-item filter narrow their respective lists.

## What it analyzes

The analyzer recognizes common fixed-format source lines, data declarations, paragraphs, `PERFORM`, `GO TO` (including `DEPENDING ON`), common arithmetic and assignment statements, `READ ... INTO`, `DISPLAY`, and basic condition expressions. The HTML flowchart is self-contained and works offline.

This is static source analysis, not a COBOL interpreter or runtime trace. History is ordered by source line, not by actual execution, and does not calculate values or determine which conditional branch or loop iteration occurs. It may not resolve copybooks, macros, complex qualification/subscripts, embedded SQL, dynamic calls, or every dialect-specific syntax. `CALL ... USING` arguments are marked as possible updates because an external program may change them. Treat inferred edges and data events as navigation help and verify them against the source. Unknown paragraph targets are reported in the generated page.

## Tests

```sh
python3 -m unittest discover -s tests -v
```