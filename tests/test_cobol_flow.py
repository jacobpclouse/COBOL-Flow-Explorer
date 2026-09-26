import unittest

from cobol_flow import build_graph, parse_program, render_html


SAMPLE = """IDENTIFICATION DIVISION.
PROGRAM-ID. DEMO.
DATA DIVISION.
WORKING-STORAGE SECTION.
01 AMOUNT PIC 9(4).
01 FLAG PIC 9.
PROCEDURE DIVISION.
MAIN.
 MOVE 5 TO AMOUNT
 PERFORM CHECK-AMOUNT
 GO TO LOW-PATH HIGH-PATH DEPENDING ON FLAG
LOW-PATH.
 ADD 1 TO AMOUNT
 GOBACK.
HIGH-PATH.
 COMPUTE AMOUNT = AMOUNT * 2
 GOBACK.
CHECK-AMOUNT.
 IF AMOUNT > 0
 MOVE 1 TO FLAG
 END-IF.
"""


class CobolFlowTests(unittest.TestCase):
    def test_finds_paragraphs_and_computed_goto_targets(self):
        paragraphs, _, diagnostics = parse_program(SAMPLE)
        graph = build_graph(paragraphs)

        self.assertEqual([item.name for item in paragraphs], [
            "MAIN", "LOW-PATH", "HIGH-PATH", "CHECK-AMOUNT",
        ])
        self.assertEqual(diagnostics, [])
        self.assertIn(
            {"from": "MAIN", "to": "HIGH-PATH", "kind": "goto", "label": "GO TO (2)"},
            graph["edges"],
        )
        self.assertIn(
            {"from": "MAIN", "to": "CHECK-AMOUNT", "kind": "PERFORM", "label": "PERFORM"},
            graph["edges"],
        )

    def test_tracks_reads_and_writes(self):
        paragraphs, _, _ = parse_program(SAMPLE)
        graph = build_graph(paragraphs)
        by_name = {node["id"]: node for node in graph["nodes"]}

        self.assertEqual(by_name["MAIN"]["writes"], ["AMOUNT"])
        self.assertIn("FLAG", by_name["MAIN"]["reads"])
        self.assertEqual(by_name["CHECK-AMOUNT"]["reads"], ["AMOUNT"])

    def test_history_identifies_arithmetic_expression_and_line(self):
        paragraphs, _, _ = parse_program(SAMPLE)
        graph = build_graph(paragraphs)
        event = next(
            item for item in graph["events"]
            if item["variable"] == "AMOUNT" and item["operation"] == "ADD" and item["action"] == "update"
        )

        self.assertEqual(event["paragraph"], "LOW-PATH")
        self.assertEqual(event["line"], 13)
        self.assertEqual(event["expression"], "AMOUNT + (1)")

    def test_handles_multiline_compute_and_clear(self):
        source = """IDENTIFICATION DIVISION.
PROGRAM-ID. WRAPPED.
DATA DIVISION.
WORKING-STORAGE SECTION.
01 AMOUNT PIC 9(4).
PROCEDURE DIVISION.
MAIN.
 COMPUTE AMOUNT = AMOUNT +
   1.
      * comment divider ----------------
NEXT-PARA.
 MOVE ZEROES TO AMOUNT.
"""
        paragraphs, _, _ = parse_program(source)
        graph = build_graph(paragraphs)

        self.assertEqual([item.name for item in paragraphs], ["MAIN", "NEXT-PARA"])
        compute = next(item for item in graph["events"] if item["operation"] == "COMPUTE")
        clear = next(item for item in graph["events"] if item["operation"] == "CLEAR")
        self.assertEqual(compute["expression"], "AMOUNT + 1")
        self.assertEqual(clear["paragraph"], "NEXT-PARA")

    def test_tracks_copybook_fields_and_read_at_end_assignment(self):
        source = """IDENTIFICATION DIVISION.
PROGRAM-ID. EXTERNAL-FIELDS.
DATA DIVISION.
WORKING-STORAGE SECTION.
01 AMOUNT PIC 9(4).
PROCEDURE DIVISION.
MAIN.
 COMPUTE AMOUNT = COPYBOOK-INCOME + 1.
 READ INPUT-FILE INTO AMOUNT
   AT END MOVE 'Y' TO EOF-FLAG.
"""
        paragraphs, _, _ = parse_program(source)
        graph = build_graph(paragraphs)

        self.assertTrue(any(
            event["variable"] == "COPYBOOK-INCOME" and event["action"] == "read"
            for event in graph["events"]
        ))
        self.assertTrue(any(
            event["variable"] == "EOF-FLAG" and event["operation"] == "MOVE"
            for event in graph["events"]
        ))

    def test_renders_standalone_html(self):
        paragraphs, _, diagnostics = parse_program(SAMPLE)
        document = render_html(build_graph(paragraphs), "demo.cbl", diagnostics)

        self.assertIn("COBOL Flow Explorer", document)
        self.assertIn('const graph={', document)
        self.assertIn("Data history", document)
        self.assertIn("selectVariable", document)
        self.assertIn("renderHistory", document)


if __name__ == "__main__":
    unittest.main()