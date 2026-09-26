#!/usr/bin/env python3
"""Build a local, interactive flow and data-reference map from COBOL source."""

from __future__ import annotations

import argparse
import html
import json
import re
import sys
from dataclasses import asdict, dataclass, field
from pathlib import Path


IDENTIFIER = r"[A-Z0-9][A-Z0-9-]*"
DECLARATION_RE = re.compile(rf"^\s*(?:0[1-9]|[1-4][0-9]|66|77|88)\s+({IDENTIFIER})\b", re.I)
PARAGRAPH_RE = re.compile(rf"^\s*({IDENTIFIER})\s*\.\s*(?:\*>.*)?$", re.I)
SECTION_RE = re.compile(rf"^\s*({IDENTIFIER})\s+SECTION\s*\.\s*(?:\*>.*)?$", re.I)
TOKEN_RE = re.compile(rf"\b{IDENTIFIER}\b", re.I)
LITERAL_RE = re.compile(r"'(?:''|[^'])*'|\"(?:\"\"|[^\"])*\"")

COBOL_WORDS = set("""
ACCEPT ADD ADDRESS ALL AND ALSO ALTER ALPHABET ALPHABETIC ALPHANUMERIC
ALPHANUMERIC-EDITED ALPHANUMERIC-TO-UTF-8 ALTERNATE ANY ARE AREA AREAS
ASCENDING ASSIGN AT AUTHOR BEFORE BINARY BY CALL CANCEL CD CF CH CHARACTER
CHARACTERS CLASS CLOSE CODE-SET COLLATING COLUMN COMMA COMMON COMP COMP-1
COMP-2 COMP-3 COMP-4 COMP-5 COMPUTATIONAL COMPUTATIONAL-1 COMPUTATIONAL-2
COMPUTATIONAL-3 COMPUTATIONAL-4 COMPUTATIONAL-5 COMPUTE CONFIGURATION
CONTAINS CONTENT CONTINUE CONTROL CONTROLS CONVERTING COPY CORR CORRESPONDING
COUNT CURRENCY DATA DATE-COMPILED DATE-WRITTEN DAY DAY-OF-WEEK DB DEBUGGING
DECIMAL-POINT DECLARATIVES DELETE DELIMITED DELIMITER DEPENDING DESCENDING
DISPLAY DIVIDE DIVISION DOWN DUPLICATES DYNAMIC ELSE END END-ACCEPT END-ADD
END-CALL END-COMPUTE END-DELETE END-DISPLAY END-DIVIDE END-EVALUATE END-IF
END-MULTIPLY END-OF-PAGE END-PERFORM END-READ END-RECEIVE END-RETURN END-REWRITE
END-SEARCH END-START END-STRING END-SUBTRACT END-UNSTRING END-WRITE ENTRY
ENVIRONMENT EOP EQUAL EQUALS ERROR EVALUATE EXCEPTION EXIT EXTEND EXTERNAL
FALSE FD FILE FILE-CONTROL FILLER FINAL FIRST FOOTING FOR FROM FUNCTION GENERATE
GIVING GLOBAL GO GOBACK GREATER GROUP HEADING HIGH-VALUE HIGH-VALUES I-O
I-O-CONTROL ID IDENTIFICATION IF IN INDEX INDEXED INDICATE INITIAL INITIALIZE
INITIATE INPUT INPUT-OUTPUT INSPECT INSTALLATION INTO INVALID IS JUST JUSTIFIED
KEY LABEL LEADING LEFT LENGTH LESS LIMIT LIMITS LINAGE LINAGE-COUNTER LINE
LINES LINKAGE LOCAL-STORAGE LOCK MEMORY MERGE MODE MODULES MORE MOVE MULTIPLY
NATIVE NEGATIVE NEXT NO NOT NULL NULLS NUMBER NUMERIC NUMERIC-EDITED OBJECT
OCCURS OF OFF OMITTED ON ONLY OPEN OPTIONAL OR ORDER ORGANIZATION OTHER OUTPUT
OVERFLOW PACKED-DECIMAL PADDING PAGE PAGE-COUNTER PASSWORD PERFORM PF PH PIC
PICTURE PLUS POINTER POSITIVE PRESENT PRINTING PROCEDURE PROCEDURES PROCEED
PROGRAM PROGRAM-ID PURGE QUEUE QUOTE QUOTES RANDOM RD READ RECEIVE RECORD
RECORDING REDEFINES REEL REFERENCE REFERENCES RELATIVE RELEASE REMAINDER REMARKS
REMOVAL RENAMES REPLACING REPORT REPORTING REPORTS RERUN RESERVE RESET RETURN
RETURNING REVERSE-VIDEO REWIND REWRITE RF RIGHT ROUNDED RUN SAME SD SEARCH
SECTION SECURITY SEGMENT SEGMENT-LIMIT SELECT SEND SENTENCE SEPARATE SEQUENCE
SEQUENTIAL SET SIGN SIZE SORT SOURCE-COMPUTER SPACE SPACES SPECIAL-NAMES
STANDARD START STATUS STOP STRING SUB-QUEUE-1 SUB-QUEUE-2 SUB-QUEUE-3 SUBTRACT
SUPPRESS SYMBOLIC SYNC SYNCHRONIZED TABLE TALLYING TAPE TERMINAL TEST THAN THEN
THROUGH THRU TIMES TO TOP TRAILING TRUE TYPE UNIT UNTIL UP UPON USAGE USE USING
VALUE VALUES VARYING WHEN WITH WORDS WORKING-STORAGE WRITE ZERO ZEROES ZEROS
TRUE FALSE HIGH-VALUES LOW-VALUES ZERO ZEROES ZEROS
""".split())


@dataclass
class DataEvent:
    line: int
    paragraph: str
    variable: str
    action: str
    operation: str
    expression: str
    source: str


@dataclass
class Paragraph:
    name: str
    lines: list[str] = field(default_factory=list)
    line_numbers: list[int] = field(default_factory=list)
    line_number: int = 0
    reads: set[str] = field(default_factory=set)
    writes: set[str] = field(default_factory=set)
    events: list[DataEvent] = field(default_factory=list)
    conditions: list[str] = field(default_factory=list)
    calls: list[tuple[str, str]] = field(default_factory=list)
    gotos: list[tuple[str, str]] = field(default_factory=list)
    terminal: bool = False


def clean_source(source: str) -> list[tuple[int, str]]:
    """Remove common fixed-format sequence/comment areas and inline comments."""
    cleaned: list[tuple[int, str]] = []
    for number, original in enumerate(source.splitlines(), start=1):
        line = original.expandtabs(8)
        if len(line) >= 7 and line[:6].strip().isdigit():
            if line[6:7] in {"*", "/", "D", "d"}:
                continue
            line = line[7:72]
        elif len(line) >= 7 and line[6:7] in {"*", "/"}:
            continue
        elif line.lstrip().startswith("*>"):
            continue
        line = re.split(r"\*>", line, maxsplit=1)[0].rstrip()
        if line.strip():
            cleaned.append((number, line.strip()))
    return cleaned


def parse_program(source: str) -> tuple[list[Paragraph], set[str], list[str]]:
    lines = clean_source(source)
    declared: set[str] = set()
    initial_values: list[DataEvent] = []
    paragraph_names: set[str] = set()
    procedure_start = False
    procedure_lines: list[tuple[int, str]] = []
    for number, line in lines:
        match = DECLARATION_RE.match(line)
        if not procedure_start and match:
            variable = match.group(1).upper()
            declared.add(variable)
            value = re.search(r"\bVALUE(?:S)?\s+(.+?)\s*\.?$", line, re.I)
            if value and not re.match(r"^\s*88\b", line):
                initial_values.append(DataEvent(
                    line=number,
                    paragraph="DATA DIVISION",
                    variable=variable,
                    action="update",
                    operation="INITIAL VALUE",
                    expression=value.group(1).strip(),
                    source=line,
                ))
        if re.match(r"^PROCEDURE\s+DIVISION\b", line, re.I):
            procedure_start = True
            continue
        if procedure_start:
            procedure_lines.append((number, line))

    paragraphs: list[Paragraph] = []
    current: Paragraph | None = None
    previous_continues = False
    for number, line in procedure_lines:
        section = SECTION_RE.match(line)
        paragraph = PARAGRAPH_RE.match(line)
        if section:
            previous_continues = False
            continue
        if paragraph and paragraph.group(1).upper() not in COBOL_WORDS and not previous_continues:
            name = paragraph.group(1).upper()
            paragraph_names.add(name)
            current = Paragraph(name=name, line_number=number)
            paragraphs.append(current)
            previous_continues = False
            continue
        if current is None:
            current = Paragraph(name="MAIN", line_number=number)
            paragraphs.append(current)
        current.lines.append(line)
        current.line_numbers.append(number)
        upper = line.upper().rstrip()
        previous_continues = bool(
            re.search(r"(?:[=+*/,-]|\b(?:TO|FROM|BY|INTO|GIVING|AND|OR))\s*$", upper)
            or (re.match(r"^MOVE\b", upper) and not re.search(r"\bTO\b", upper))
        )

    if not paragraphs:
        return [], declared, ["No PROCEDURE DIVISION was found."]

    diagnostics: list[str] = []
    paragraphs[0].events.extend(initial_values)
    if not declared:
        diagnostics.append("No data declarations were recognized; variable matching is inferred from procedure statements.")
    for paragraph in paragraphs:
        _analyze_paragraph(paragraph, declared, paragraph_names)
    for paragraph in paragraphs:
        for target, _ in paragraph.calls + paragraph.gotos:
            if target not in paragraph_names:
                diagnostics.append(f"{paragraph.name} references unknown paragraph {target}.")
    return paragraphs, declared, diagnostics


def _candidate_variables(text: str, declared: set[str], paragraph_names: set[str]) -> set[str]:
    without_literals = LITERAL_RE.sub(" ", text)
    tokens = {token.upper() for token in TOKEN_RE.findall(without_literals)}
    excluded = COBOL_WORDS | paragraph_names | {"END-IF", "END-EVALUATE", "END-PERFORM"}
    return {token for token in tokens if token not in excluded and not token.isdigit()}


def _analyze_paragraph(paragraph: Paragraph, declared: set[str], paragraph_names: set[str]) -> None:
    for line_number, line in _logical_lines(paragraph):
        upper = line.upper().strip().rstrip(".")
        condition = re.match(r"^(?:IF|WHEN|UNTIL|WHILE)\s+(.+)$", upper)
        if condition:
            paragraph.conditions.append(condition.group(1).strip())
            paragraph.reads.update(_candidate_variables(condition.group(1), declared, paragraph_names))

        perform = re.search(rf"\bPERFORM\s+({IDENTIFIER})(?:\s+(?:THRU|THROUGH)\s+({IDENTIFIER}))?", upper)
        if perform and perform.group(1) not in {"UNTIL", "VARYING", "WITH", "TEST"}:
            paragraph.calls.append((perform.group(1), "PERFORM"))
            if perform.group(2):
                paragraph.calls.append((perform.group(2), "PERFORM THRU"))

        goto = re.search(rf"\bGO\s+TO\s+(.+)$", upper)
        if goto:
            target_text, separator, selector = goto.group(1).partition(" DEPENDING ON ")
            targets = re.match(rf"({IDENTIFIER}(?:\s*,?\s+{IDENTIFIER})*)$", target_text)
            if targets:
                names = [name.upper() for name in TOKEN_RE.findall(targets.group(1))]
                for index, name in enumerate(names):
                    label = f"GO TO ({index + 1})" if len(names) > 1 else "GO TO"
                    paragraph.gotos.append((name, label))
                if separator:
                    paragraph.reads.update(_candidate_variables(selector, declared, paragraph_names))

        if re.search(r"\b(?:GOBACK|STOP\s+RUN|EXIT\s+PROGRAM)\b", upper):
            paragraph.terminal = True

        _analyze_data_statement(upper, paragraph, declared, paragraph_names, line_number)
        clause = re.match(r"^(?:AT\s+END|NOT\s+AT\s+END|INVALID\s+KEY|NOT\s+INVALID\s+KEY)\s+(.+)$", upper)
        if clause:
            _analyze_data_statement(clause.group(1), paragraph, declared, paragraph_names, line_number)


def _logical_lines(paragraph: Paragraph) -> list[tuple[int, str]]:
    logical: list[tuple[int, str]] = []
    pending = ""
    start_line = 0
    for line_number, line in zip(paragraph.line_numbers, paragraph.lines):
        if pending:
            pending = f"{pending} {line}"
        else:
            pending = line
            start_line = line_number

        upper = pending.upper().rstrip()
        incomplete = bool(re.search(r"(?:[=+*/,-]|\b(?:TO|FROM|BY|INTO|GIVING|AND|OR))\s*$", upper))
        move_missing_to = bool(re.match(r"^MOVE\b", upper) and not re.search(r"\bTO\b", upper))
        if incomplete or move_missing_to:
            continue
        logical.append((start_line, pending))
        pending = ""
    if pending:
        logical.append((start_line, pending))
    return logical


def _analyze_data_statement(
    text: str,
    paragraph: Paragraph,
    declared: set[str],
    paragraph_names: set[str],
    line_number: int,
) -> None:
    candidates = lambda part: _candidate_variables(part, declared, paragraph_names)
    patterns: list[tuple[str, str, str]] = [
        (r"^MOVE\s+(.+?)\s+TO\s+(.+)$", "MOVE", "move"),
        (r"^COMPUTE\s+([A-Z0-9-]+)(?:\s+ROUNDED)?\s*=\s*(.+)$", "COMPUTE", "compute"),
        (r"^ADD\s+(.+?)\s+TO\s+([A-Z0-9-]+)(?:\s+GIVING\s+([A-Z0-9-]+))?", "ADD", "add"),
        (r"^SUBTRACT\s+(.+?)\s+FROM\s+([A-Z0-9-]+)(?:\s+GIVING\s+([A-Z0-9-]+))?", "SUBTRACT", "subtract"),
        (r"^MULTIPLY\s+(.+?)\s+BY\s+([A-Z0-9-]+)(?:\s+GIVING\s+([A-Z0-9-]+))?", "MULTIPLY", "multiply"),
        (r"^DIVIDE\s+(.+?)\s+INTO\s+([A-Z0-9-]+)(?:\s+GIVING\s+([A-Z0-9-]+))?", "DIVIDE", "divide"),
    ]
    for pattern, operation, action in patterns:
        match = re.match(pattern, text)
        if not match:
            continue
        groups = match.groups()
        if action == "move":
            expression, target_text = groups
            reads = candidates(expression)
            writes = candidates(target_text)
            paragraph.reads.update(reads)
            paragraph.writes.update(writes)
            operation = "CLEAR" if expression.strip() in {"ZERO", "ZEROS", "ZEROES", "SPACE", "SPACES", "LOW-VALUES", "HIGH-VALUES"} else "MOVE"
            _add_events(paragraph, line_number, operation, expression, text, reads, writes)
        elif action == "compute":
            target, expression = groups
            reads = candidates(expression)
            writes = candidates(target)
            paragraph.writes.update(writes)
            paragraph.reads.update(reads)
            _add_events(paragraph, line_number, operation, expression, text, reads, writes)
        else:
            source, receiver, giving = groups
            reads = candidates(source) | candidates(receiver)
            writes = candidates(giving or receiver)
            paragraph.reads.update(reads)
            paragraph.writes.update(writes)
            symbols = {"ADD": "+", "SUBTRACT": "-", "MULTIPLY": "*", "DIVIDE": "/"}
            expression = f"{receiver} {symbols[operation]} ({source})"
            _add_events(paragraph, line_number, operation, expression, text, reads, writes)
        return

    match = re.match(r"^(INITIALIZE|ACCEPT|SET)\s+(.+)$", text)
    if match:
        action, rest = match.groups()
        target_text = rest.split(" TO ", 1)[0] if action == "SET" else rest
        writes = candidates(target_text)
        reads: set[str] = set()
        paragraph.writes.update(writes)
        if action == "SET" and " TO " in rest:
            expression = rest.split(" TO ", 1)[1]
            reads = candidates(expression)
            paragraph.reads.update(reads)
        else:
            expression = "COBOL initial/default values" if action == "INITIALIZE" else "input value" if action == "ACCEPT" else rest
        _add_events(paragraph, line_number, action, expression, text, reads, writes)
        return

    match = re.match(r"^READ\s+\S+(?:\s+INTO\s+(.+?))?(?:\s+AT\s+END|$)", text)
    if match and match.group(1):
        writes = candidates(match.group(1))
        paragraph.writes.update(writes)
        _add_events(paragraph, line_number, "READ INTO", "input record", text, set(), writes)
        return

    match = re.match(r"^DISPLAY\s+(.+)$", text)
    if match:
        reads = candidates(match.group(1))
        paragraph.reads.update(reads)
        _add_events(paragraph, line_number, "DISPLAY", match.group(1), text, reads, set())
        return

    match = re.match(r"^WRITE\s+([A-Z0-9-]+)", text)
    if match:
        reads = candidates(match.group(1))
        paragraph.reads.update(reads)
        _add_events(paragraph, line_number, "WRITE OUTPUT", "written to output file", text, reads, set())
        return

    match = re.match(r"^CALL\s+.+?\s+USING\s+(.+)$", text)
    if match:
        possible_writes = candidates(match.group(1))
        _add_events(paragraph, line_number, "CALL BY REFERENCE?", "external program may modify argument", text, set(), possible_writes, "possible update")


def _add_events(
    paragraph: Paragraph,
    line_number: int,
    operation: str,
    expression: str,
    source: str,
    reads: set[str],
    writes: set[str],
    write_action: str = "update",
) -> None:
    for variable in sorted(reads):
        paragraph.events.append(DataEvent(line_number, paragraph.name, variable, "read", operation, expression, source))
    for variable in sorted(writes):
        paragraph.events.append(DataEvent(line_number, paragraph.name, variable, write_action, operation, expression, source))


def build_graph(paragraphs: list[Paragraph]) -> dict:
    names = {paragraph.name for paragraph in paragraphs}
    nodes = []
    edges = []
    for index, paragraph in enumerate(paragraphs):
        nodes.append({
            "id": paragraph.name,
            "line": paragraph.line_number,
            "reads": sorted(paragraph.reads),
            "writes": sorted(paragraph.writes),
            "conditions": paragraph.conditions,
            "source": paragraph.lines,
        })
        for target, kind in paragraph.calls:
            if target in names:
                edges.append({"from": paragraph.name, "to": target, "kind": kind, "label": kind})
        for target, kind in paragraph.gotos:
            if target in names:
                edges.append({"from": paragraph.name, "to": target, "kind": "goto", "label": kind})
        has_unconditional_goto = any(kind == "GO TO" for _, kind in paragraph.gotos)
        if index + 1 < len(paragraphs) and not paragraph.terminal and not has_unconditional_goto:
            edges.append({"from": paragraph.name, "to": paragraphs[index + 1].name, "kind": "next", "label": "next"})
    return {
        "nodes": nodes,
        "edges": edges,
        "entry": paragraphs[0].name if paragraphs else None,
        "variables": sorted(
            {variable for node in nodes for variable in node["reads"] + node["writes"]}
            | {event.variable for paragraph in paragraphs for event in paragraph.events}
        ),
        "events": [asdict(event) for paragraph in paragraphs for event in paragraph.events],
    }


def render_html(graph: dict, title: str, diagnostics: list[str]) -> str:
    payload = json.dumps(graph, ensure_ascii=True).replace("</", "<\\/")
    diagnostics_html = "".join(f"<li>{html.escape(item)}</li>" for item in diagnostics)
    safe_title = html.escape(title)
    return f'''<!doctype html>
<html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width,initial-scale=1">
<title>{safe_title} | COBOL Flow Explorer</title>
<style>
:root{{--ink:#182327;--muted:#5a6a6c;--paper:#f3f4ee;--panel:#fff;--line:#bdc9c5;--teal:#087e75;--orange:#cf633c;--yellow:#f3c85b;--mono:"IBM Plex Mono","Cascadia Code",monospace;--sans:"Aptos","Segoe UI",sans-serif}}
*{{box-sizing:border-box}}body{{margin:0;background:var(--paper);color:var(--ink);font-family:var(--sans)}}
header{{display:flex;align-items:center;justify-content:space-between;padding:18px 24px;border-bottom:1px solid var(--line);background:#fbfcf8}}
h1{{font-size:19px;margin:0;font-weight:650}}header small{{display:block;color:var(--muted);font:12px var(--mono);margin-top:5px}}
main{{display:grid;grid-template-columns:270px minmax(0,1fr);min-height:calc(100vh - 67px)}}aside{{padding:20px;border-right:1px solid var(--line);background:#e9eee8;overflow:auto}}
.control{{margin-bottom:20px}}label,.caption{{display:block;font:700 11px var(--mono);text-transform:uppercase;color:#526160;margin-bottom:8px}}
input,select{{width:100%;padding:10px 11px;border:1px solid #aebbb6;border-radius:3px;background:#fff;color:var(--ink);font:13px var(--sans)}}
#variables{{max-height:40vh;overflow:auto;display:grid;gap:4px}}.var{{width:100%;text-align:left;border:0;padding:8px;background:transparent;font:12px var(--mono);cursor:pointer;color:var(--ink);border-radius:2px}}.var:hover,.var.active{{background:#d0e7df;color:#07564f}}
.legend{{font:11px var(--mono);line-height:1.9;color:var(--muted)}}.key{{display:inline-block;width:9px;height:9px;border-radius:50%;margin-right:7px;background:var(--teal)}}.key.write{{background:var(--orange)}}
.workspace{{min-width:0;display:flex;flex-direction:column}}.toolbar{{display:flex;gap:14px;align-items:center;padding:12px 18px;border-bottom:1px solid var(--line);background:#fbfcf8}}.toolbar input{{max-width:360px}}#count{{margin-left:auto;color:var(--muted);font:11px var(--mono)}}
.view-switch{{display:flex;border:1px solid var(--line);border-radius:3px;overflow:hidden;flex:none}}.view-switch button{{border:0;border-right:1px solid var(--line);padding:9px 12px;background:#fff;color:var(--ink);font:11px var(--mono);cursor:pointer}}.view-switch button:last-child{{border-right:0}}.view-switch button.active{{background:#d0e7df;color:#07564f}}
#canvas{{overflow:auto;flex:1;min-height:430px;background-color:#f7f8f3;background-image:radial-gradient(#d8dfd8 0.7px,transparent 0.7px);background-size:18px 18px}}svg{{display:block}}.edge{{stroke:#82918e;stroke-width:1.6;fill:none;opacity:.8}}.edge.perform{{stroke:var(--teal);stroke-dasharray:5 4}}.edge.goto{{stroke:var(--orange);stroke-width:2.2}}.edge-label{{font:10px var(--mono);fill:#536360}}
.node rect{{fill:#fff;stroke:#aebbb6;stroke-width:1.4;rx:4}}.node.entry rect{{stroke:var(--teal);stroke-width:2.4}}.node.selected rect{{stroke:var(--yellow);stroke-width:4}}.node.dim{{opacity:.2}}.node-title{{font:700 13px var(--mono);fill:var(--ink)}}.node-meta{{font:10px var(--mono);fill:#536360}}.node-code{{font:10px var(--mono);fill:#253438}}.node-hit{{cursor:pointer}}
#history{{flex:1;min-height:430px;overflow:auto;padding:22px;background:#f7f8f3}}#history[hidden]{{display:none}}#history h2{{font:700 15px var(--mono);margin:0 0 6px}}#history .history-summary{{font-size:12px;color:var(--muted);margin:0 0 16px}}.event-table{{width:100%;border-collapse:collapse;background:#fff;font-size:12px}}.event-table th{{position:sticky;top:0;text-align:left;background:#e9eee8;font:700 10px var(--mono);text-transform:uppercase;color:#526160}}.event-table th,.event-table td{{padding:9px 10px;border-bottom:1px solid #dce3de;vertical-align:top}}.event-table tr{{cursor:pointer}}.event-table tbody tr:hover{{background:#f3f7f2}}.event-type{{font:700 10px var(--mono);white-space:nowrap}}.event-type.update{{color:#a33f26}}.event-type.read{{color:#087e75}}.event-type.possible-update{{color:#806300}}.event-expression{{font-family:var(--mono);overflow-wrap:anywhere}}.empty-history{{padding:20px;background:#fff;color:var(--muted);font-size:13px}}
#details{{border-top:1px solid var(--line);background:#fff;padding:14px 20px;min-height:100px;max-height:210px;overflow:auto}}#details h2{{margin:0 0 8px;font:700 13px var(--mono)}}#details pre{{margin:6px 0;white-space:pre-wrap;font:11px/1.55 var(--mono);color:#344346}}
#details[hidden]{{display:none}}
.note{{font-size:11px;color:var(--muted);line-height:1.5}}#diagnostics{{padding-left:17px;font-size:11px;color:#775638;line-height:1.5}}
@media(max-width:760px){{header{{padding:14px}}main{{grid-template-columns:1fr}}aside{{border-right:0;border-bottom:1px solid var(--line);padding:13px 14px}}#variables{{display:flex;flex-wrap:wrap;max-height:95px}}.var{{width:auto}}.legend,.note,#diagnostics{{display:none}}.control{{margin-bottom:9px}}}}
</style></head><body>
<header><div><h1>COBOL Flow Explorer</h1><small>{safe_title}</small></div><small>STATIC ANALYSIS · NO RUNTIME EXECUTION</small></header>
<main><aside>
<div class="control"><label for="paragraphFilter">Find paragraph</label><input id="paragraphFilter" placeholder="Paragraph name..."></div>
<div class="control"><label>Data items</label><div id="variables"></div></div>
<div class="control legend"><span><i class="key"></i>reads data</span><br><span><i class="key write"></i>writes data</span><br>Solid green: PERFORM<br>Orange: GO TO<br>Gray: paragraph flow</div>
<p class="note">Choose a data item to highlight paragraphs that read or change it. Select a paragraph for its statements and source lines.</p>
{f'<ul id="diagnostics">{diagnostics_html}</ul>' if diagnostics else ''}
</aside><section class="workspace"><div class="toolbar"><div class="view-switch" role="tablist" aria-label="Analysis view"><button id="flowTab" class="active" role="tab" aria-selected="true">Flowchart</button><button id="historyTab" role="tab" aria-selected="false">Data history</button></div><input id="dataFilter" placeholder="Filter data items..."><span id="count"></span></div><div id="canvas"></div><section id="history" hidden></section><section id="details"><h2>Select a paragraph</h2><div class="note">Branches and paragraph calls are inferred from source; this is not an execution trace.</div></section></section></main>
<script>
const graph={payload};
const ns="http://www.w3.org/2000/svg";
const canvas=document.getElementById("canvas"), historyPanel=document.getElementById("history"), details=document.getElementById("details");
const positions=new Map(), nodeEls=new Map(), edgeEls=[];
const colCount=Math.max(1,Math.min(3,Math.ceil(Math.sqrt(graph.nodes.length||1))));
const cardW=300,cardH=152,gapX=84,gapY=74,pad=46;
const rows=Math.ceil(graph.nodes.length/colCount);
const width=pad*2+colCount*cardW+(colCount-1)*gapX,height=pad*2+rows*cardH+(rows-1)*gapY;
function el(name,attrs={{}}){{const item=document.createElementNS(ns,name);for(const [key,val] of Object.entries(attrs))item.setAttribute(key,val);return item}}
const svg=el("svg",{{width,height,viewBox:`0 0 ${{width}} ${{height}}`,role:"img","aria-label":"COBOL paragraph flowchart"}});
const defs=el("defs"), marker=el("marker",{{id:"arrow",viewBox:"0 0 10 10",refX:"9",refY:"5",markerWidth:"7",markerHeight:"7",orient:"auto-start-reverse"}});marker.appendChild(el("path",{{d:"M 0 0 L 10 5 L 0 10 z",fill:"context-stroke"}}));defs.appendChild(marker);svg.appendChild(defs);
graph.nodes.forEach((node,index)=>positions.set(node.id,{{x:pad+(index%colCount)*(cardW+gapX),y:pad+Math.floor(index/colCount)*(cardH+gapY)}}));
for(const edge of graph.edges){{const a=positions.get(edge.from),b=positions.get(edge.to);if(!a||!b)continue;const x1=a.x+cardW/2,y1=a.y+cardH/2,x2=b.x+cardW/2,y2=b.y+cardH/2;
 const path=el("path",{{d:`M ${{x1}} ${{y1}} C ${{x1}} ${{(y1+y2)/2}}, ${{x2}} ${{(y1+y2)/2}}, ${{x2}} ${{y2}}`,class:`edge ${{edge.kind.toLowerCase().replaceAll(" ","-")}}`,"marker-end":"url(#arrow)"}});svg.appendChild(path);edgeEls.push({{element:path,edge}});
 const label=el("text",{{x:(x1+x2)/2+5,y:(y1+y2)/2-4,class:"edge-label"}});label.textContent=edge.label;svg.appendChild(label);
}}
function showNode(node){{const code=node.source.length?node.source.join("\\n"):"(no statements)";details.innerHTML=`<h2>${{node.id}} <span class="note">· source line ${{node.line}}</span></h2><div class="note">Reads: ${{node.reads.join(", ")||"none detected"}} &nbsp; Writes: ${{node.writes.join(", ")||"none detected"}}</div>${{node.conditions.length?`<div class="note">Conditions: ${{node.conditions.map(s=>escapeHtml(s)).join(" · ")}}</div>`:""}}<pre>${{escapeHtml(code)}}</pre>`}}
function escapeHtml(value){{return value.replace(/[&<>"']/g,ch=>({{"&":"&amp;","<":"&lt;",">":"&gt;",'"':"&quot;","'":"&#39;"}}[ch]))}}
for(const node of graph.nodes){{const p=positions.get(node.id),group=el("g",{{class:`node ${{node.id===graph.entry?"entry":""}}`,transform:`translate(${{p.x}} ${{p.y}})`}});group.appendChild(el("rect",{{width:cardW,height:cardH}}));
 const title=el("text",{{x:14,y:23,class:"node-title"}});title.textContent=node.id;group.appendChild(title);
 const line=el("text",{{x:cardW-12,y:22,class:"node-meta","text-anchor":"end"}});line.textContent=`L${{node.line}}`;group.appendChild(line);
 let meta=el("text",{{x:14,y:45,class:"node-meta"}});meta.textContent=`R: ${{node.reads.slice(0,5).join(", ")||"-"}}`;group.appendChild(meta);
 meta=el("text",{{x:14,y:61,class:"node-meta"}});meta.textContent=`W: ${{node.writes.slice(0,5).join(", ")||"-"}}`;group.appendChild(meta);
 const snippets=node.source.slice(0,5);snippets.forEach((text,i)=>{{const t=el("text",{{x:14,y:86+i*13,class:"node-code"}});t.textContent=text.length>43?text.slice(0,40)+"...":text;group.appendChild(t)}});
 group.addEventListener("click",()=>showNode(node));svg.appendChild(group);nodeEls.set(node.id,{{element:group,node}});
}}
canvas.appendChild(svg);
function drawVariables(filter=""){{const list=document.getElementById("variables");list.innerHTML="";for(const variable of graph.variables.filter(v=>v.includes(filter.toUpperCase()))){{const button=document.createElement("button");button.type="button";button.className="var";button.textContent=variable;button.addEventListener("click",()=>selectVariable(variable,button));list.appendChild(button)}}}}
let activeVariable=null;
function renderHistory(variable){{historyPanel.replaceChildren();const title=document.createElement("h2");title.textContent=`Data history: ${{variable}}`;historyPanel.appendChild(title);const events=graph.events.filter(event=>event.variable===variable).sort((a,b)=>a.line-b.line);const summary=document.createElement("p");summary.className="history-summary";summary.textContent=`${{events.filter(event=>event.action!=="read").length}} possible updates · ${{events.filter(event=>event.action==="read").length}} reads. Source order only; branches and loops may change which events execute.`;historyPanel.appendChild(summary);if(!events.length){{const empty=document.createElement("div");empty.className="empty-history";empty.textContent="No recognized updates or reads for this data item.";historyPanel.appendChild(empty);return}}const table=document.createElement("table");table.className="event-table";table.innerHTML="<thead><tr><th>Line</th><th>Paragraph</th><th>Kind</th><th>Operation</th><th>Expression / source</th></tr></thead>";const body=document.createElement("tbody");for(const event of events){{const row=document.createElement("tr");for(const value of [event.line,event.paragraph,event.action,event.operation,event.expression||event.source]){{const cell=document.createElement("td");cell.textContent=value;row.appendChild(cell)}}row.children[2].className=`event-type ${{event.action.replaceAll(" ","-")}}`;row.title=event.source;row.addEventListener("click",()=>{{const node=nodeEls.get(event.paragraph);if(node){{switchView("flow");showNode(node.node)}}}});body.appendChild(row)}}table.appendChild(body);historyPanel.appendChild(table)}}
function switchView(view){{const showHistory=view==="history";canvas.hidden=showHistory;historyPanel.hidden=!showHistory;details.hidden=showHistory;document.getElementById("flowTab").classList.toggle("active",!showHistory);document.getElementById("historyTab").classList.toggle("active",showHistory);document.getElementById("flowTab").setAttribute("aria-selected",String(!showHistory));document.getElementById("historyTab").setAttribute("aria-selected",String(showHistory));if(showHistory){{if(!activeVariable&&graph.variables.length){{const first=graph.variables[0];selectVariable(first,document.querySelector(".var"))}}else if(activeVariable)renderHistory(activeVariable)}}}}
function selectVariable(variable,button){{activeVariable=variable;document.querySelectorAll(".var").forEach(item=>item.classList.toggle("active",item===button));for(const {{element,node}} of nodeEls.values()){{const matches=node.reads.includes(variable)||node.writes.includes(variable);element.classList.toggle("selected",matches);element.classList.toggle("dim",!matches)}}for(const {{element,edge}} of edgeEls){{const from=nodeEls.get(edge.from).node,to=nodeEls.get(edge.to).node;element.style.opacity=(from.reads.includes(variable)||from.writes.includes(variable)||to.reads.includes(variable)||to.writes.includes(variable))?"1":".12"}}if(!historyPanel.hidden)renderHistory(variable)}}
drawVariables();document.getElementById("dataFilter").addEventListener("input",event=>drawVariables(event.target.value));
document.getElementById("flowTab").addEventListener("click",()=>switchView("flow"));document.getElementById("historyTab").addEventListener("click",()=>switchView("history"));
document.getElementById("paragraphFilter").addEventListener("input",event=>{{const query=event.target.value.toUpperCase();for(const {{element,node}} of nodeEls.values())element.classList.toggle("dim",!!query&&!node.id.includes(query));if(!query&&activeVariable)selectVariable(activeVariable,document.querySelector(".var.active"))}});
document.getElementById("count").textContent=`${{graph.nodes.length}} paragraphs · ${{graph.edges.length}} links · ${{graph.variables.length}} data items`;
</script></body></html>'''


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description="Create a static COBOL flowchart with data-item highlighting.")
    parser.add_argument("source", type=Path, help="COBOL source file (.cbl, .cob, or text)")
    parser.add_argument("-o", "--output", type=Path, help="HTML output path (default: <source>.flow.html)")
    args = parser.parse_args(argv)
    try:
        source = args.source.read_text(encoding="utf-8", errors="replace")
    except OSError as error:
        parser.error(str(error))
    paragraphs, _, diagnostics = parse_program(source)
    if not paragraphs:
        print("Could not build a flowchart: " + " ".join(diagnostics), file=sys.stderr)
        return 2
    graph = build_graph(paragraphs)
    output = args.output or args.source.with_suffix(".flow.html")
    output.write_text(render_html(graph, args.source.name, diagnostics), encoding="utf-8")
    print(f"Wrote {output} ({len(graph['nodes'])} paragraphs, {len(graph['edges'])} links, {len(graph['variables'])} data items)")
    for diagnostic in diagnostics:
        print(f"Note: {diagnostic}", file=sys.stderr)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())