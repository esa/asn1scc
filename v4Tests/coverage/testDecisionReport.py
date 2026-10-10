"""Strict decision / MC/DC accounting of statementCollector.read_decisions (host, stdlib only)."""
from pathlib import Path
import tempfile
import unittest

from statementCollector import read_decisions, report_levels, report_dir_name

WORK = Path("/work")


def stats(kind, total, covered, partial, uncovered, exempted=0):
    return (f'<obligation_stats kind="{kind}">'
            f'<metric kind="total_obligations_of_relevance" count="{total}"/>'
            f'<metric kind="fully_covered" count="{covered}"/>'
            f'<metric kind="partially_covered" count="{partial}"/>'
            f'<metric kind="not_covered" count="{uncovered}"/>'
            f'<metric kind="exempted" count="{exempted}"/></obligation_stats>')


def decision(sco, status, conditions, line, message=None):
    body = "".join(f'<condition id="{c}" text="c{c}" coverage="{s}"><src><line num="{line}"/></src></condition>'
                   for c, s in conditions)
    msg = f'<message kind="violation" SCO="SCO #{sco}: DECISION" message="{message}"/>' if message else ""
    return (f'<src_mapping><src><line num="{line}" src="if (ret)"/></src>'
            f'<decision id="{sco}" text="ret)" coverage="{status}"><src><line num="{line}"/></src>{body}</decision>'
            f'{msg}</src_mapping>')


def write_report(directory, level, mappings, file_stats, global_stats=None):
    xml = directory / "xml"
    xml.mkdir(parents=True)
    (xml / "sample1.c.xml").write_text(
        f'<source file="{WORK / "sample1.c"}" coverage_level="{level}">{"".join(mappings)}</source>')
    (xml / "index.xml").write_text(
        '<document xmlns:xi="http://www.w3.org/2001/XInclude">'
        f'<coverage_report coverage_level="{level}"><coverage_summary>{global_stats or file_stats}'
        f'<file name="{WORK / "sample1.c"}">{file_stats}</file></coverage_summary>'
        '<sources><xi:include parse="xml" href="sample1.c.xml"/></sources></coverage_report></document>')


class DecisionReport(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.root = Path(self.temp.name)

    def tearDown(self):
        self.temp.cleanup()

    MAPPINGS = [decision(1, "+", [(2, ".")], 10),
                decision(3, "!", [(4, ".")], 11, "outcome FALSE never exercised"),
                decision(5, ".", [(6, "."), (7, ".")], 12)]

    def test_decision_level_counts_and_messages(self):
        write_report(self.root, "stmt+decision", self.MAPPINGS, stats("Decision", 2, 1, 1, 0))
        files = read_decisions(self.root, WORK, "stmt+decision", {"sample1.c"})
        self.assertEqual(files["sample1.c"]["counts"], {"total": 2, "covered": 1, "partial": 1, "uncovered": 0})
        partial = files["sample1.c"]["decisions"][1]
        self.assertEqual(partial["messages"], ["outcome FALSE never exercised"])
        self.assertEqual(files["sample1.c"]["decisions"][2]["status"], ".")

    def test_mcdc_level_counts_conditions(self):
        mappings = [decision(1, "+", [(2, "+")], 10), decision(3, "-", [(4, "."), (5, "-")], 11)]
        write_report(self.root, "stmt+mcdc", mappings, stats("MCDC", 3, 1, 0, 2))
        files = read_decisions(self.root, WORK, "stmt+mcdc", {"sample1.c"})
        self.assertEqual(files["sample1.c"]["counts"], {"total": 3, "covered": 1, "partial": 0, "uncovered": 2})

    def test_summary_mismatch_is_rejected(self):
        write_report(self.root, "stmt+decision", self.MAPPINGS, stats("Decision", 2, 2, 0, 0))
        with self.assertRaisesRegex(ValueError, "accounting differs"):
            read_decisions(self.root, WORK, "stmt+decision", {"sample1.c"})

    def test_exempted_obligations_are_rejected(self):
        write_report(self.root, "stmt+decision", self.MAPPINGS, stats("Decision", 2, 1, 1, 0, exempted=1))
        with self.assertRaisesRegex(ValueError, "exempted"):
            read_decisions(self.root, WORK, "stmt+decision", {"sample1.c"})

    def test_exempted_status_is_rejected(self):
        write_report(self.root, "stmt+decision", [decision(1, "*", [], 10)], stats("Decision", 0, 0, 0, 0))
        with self.assertRaisesRegex(ValueError, "Unresolved decision status"):
            read_decisions(self.root, WORK, "stmt+decision", {"sample1.c"})

    def test_wrong_level_is_rejected(self):
        write_report(self.root, "stmt+mcdc", self.MAPPINGS, stats("Decision", 2, 1, 1, 0))
        with self.assertRaisesRegex(ValueError, "Expected a GNATcoverage stmt\\+decision report"):
            read_decisions(self.root, WORK, "stmt+decision", {"sample1.c"})

    def test_unknown_source_is_rejected(self):
        write_report(self.root, "stmt+decision", self.MAPPINGS, stats("Decision", 2, 1, 1, 0))
        with self.assertRaisesRegex(ValueError, "missing from the statement report"):
            read_decisions(self.root, WORK, "stmt+decision", {"other.c"})

    def test_levels(self):
        self.assertEqual(report_levels("stmt"), ("stmt",))
        self.assertEqual(report_levels("stmt+mcdc"), ("stmt", "stmt+decision", "stmt+mcdc"))
        self.assertEqual(report_dir_name("stmt"), "report")


if __name__ == "__main__":
    unittest.main()
