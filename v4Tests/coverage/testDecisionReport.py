"""Strict statement / decision / MC/DC accounting of statementCollector.read_report and read_decisions,
including exemption regions (host, stdlib only)."""
from pathlib import Path
import tempfile
import unittest

from statementCollector import read_decisions, read_report, report_levels, report_dir_name

WORK = Path("/work")


def stats(kind, total, covered, partial, uncovered, exempted=0, exempted_no_violation=0):
    return (f'<obligation_stats kind="{kind}">'
            f'<metric kind="total_obligations_of_relevance" count="{total}"/>'
            f'<metric kind="fully_covered" count="{covered}"/>'
            f'<metric kind="partially_covered" count="{partial}"/>'
            f'<metric kind="not_covered" count="{uncovered}"/>'
            f'<metric kind="exempted_no_violation" count="{exempted_no_violation}"/>'
            f'<metric kind="exempted" count="{exempted}"/></obligation_stats>')


def decision(sco, status, conditions, line, message=None, region=None):
    body = "".join(f'<condition id="{c}" text="c{c}" coverage="{s}"><src><line num="{line}"/></src></condition>'
                   for c, s in conditions)
    msg = f'<message kind="violation" SCO="SCO #{sco}: DECISION" message="{message}"/>' if message else ""
    coverage = f' coverage="{region}"' if region else ""
    return (f'<src_mapping{coverage}><src><line num="{line}" src="if (ret)"/></src>'
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
        self.assertEqual(files["sample1.c"]["counts"], {"total": 2, "covered": 1, "partial": 1, "uncovered": 0,
                                                         "exempted": 0})
        partial = files["sample1.c"]["decisions"][1]
        self.assertEqual(partial["messages"], ["outcome FALSE never exercised"])
        self.assertEqual(files["sample1.c"]["decisions"][2]["status"], ".")

    def test_mcdc_level_counts_conditions(self):
        mappings = [decision(1, "+", [(2, "+")], 10), decision(3, "-", [(4, "."), (5, "-")], 11)]
        write_report(self.root, "stmt+mcdc", mappings, stats("MCDC", 3, 1, 0, 2))
        files = read_decisions(self.root, WORK, "stmt+mcdc", {"sample1.c"})
        self.assertEqual(files["sample1.c"]["counts"], {"total": 3, "covered": 1, "partial": 0, "uncovered": 2,
                                                         "exempted": 0})

    def test_unique_cause_leaves_out_unassessed_conditions(self):
        mappings = [decision(1, "+", [(2, "+")], 10), decision(3, "-", [(4, "."), (5, "-")], 11)]
        write_report(self.root, "stmt+uc_mcdc", mappings, stats("UC_MCDC", 2, 1, 0, 1))
        files = read_decisions(self.root, WORK, "stmt+uc_mcdc", {"sample1.c"})
        self.assertEqual(files["sample1.c"]["counts"], {"total": 2, "covered": 1, "partial": 0, "uncovered": 1,
                                                         "exempted": 0})

    def test_summary_mismatch_is_rejected(self):
        write_report(self.root, "stmt+decision", self.MAPPINGS, stats("Decision", 2, 2, 0, 0))
        with self.assertRaisesRegex(ValueError, "accounting differs"):
            read_decisions(self.root, WORK, "stmt+decision", {"sample1.c"})

    def test_exempted_metric_without_region_is_rejected(self):
        write_report(self.root, "stmt+decision", self.MAPPINGS, stats("Decision", 2, 1, 1, 0, exempted=1))
        with self.assertRaisesRegex(ValueError, "Inconsistent Decision"):
            read_decisions(self.root, WORK, "stmt+decision", {"sample1.c"})

    # An exemption region (`*` lines, a violation inside): the missing outcome of the
    # COV-002 guard is exempted, an executed decision in another region counts as covered.
    EXEMPTED = [decision(1, "+", [(2, "+")], 10, region="*"),
                decision(3, "-", [(4, "."), (5, ".")], 11, "outcome TRUE never exercised", region="*"),
                decision(6, "!", [(7, "-")], 12)]

    def test_exempted_decisions_are_counted_apart(self):
        write_report(self.root, "stmt+decision", self.EXEMPTED,
                     stats("Decision", 3, 0, 1, 0, exempted=1, exempted_no_violation=1))
        files = read_decisions(self.root, WORK, "stmt+decision", {"sample1.c"})
        self.assertEqual(files["sample1.c"]["counts"], {"total": 3, "covered": 1, "partial": 1, "uncovered": 0,
                                                         "exempted": 1})
        self.assertEqual([d["exempted"] for d in files["sample1.c"]["decisions"]], [True, True, False])

    def test_exempted_conditions_masking_and_unique_cause(self):
        write_report(self.root, "stmt+mcdc", self.EXEMPTED, stats("MCDC", 4, 0, 0, 1, exempted=2, exempted_no_violation=1))
        files = read_decisions(self.root, WORK, "stmt+mcdc", {"sample1.c"})
        self.assertEqual(files["sample1.c"]["counts"], {"total": 4, "covered": 1, "partial": 0, "uncovered": 1,
                                                         "exempted": 2})
        self.temp.cleanup()
        self.temp = tempfile.TemporaryDirectory()
        self.root = Path(self.temp.name)
        write_report(self.root, "stmt+uc_mcdc", self.EXEMPTED, stats("UC_MCDC", 2, 0, 0, 1, exempted_no_violation=1))
        files = read_decisions(self.root, WORK, "stmt+uc_mcdc", {"sample1.c"})
        self.assertEqual(files["sample1.c"]["counts"], {"total": 2, "covered": 1, "partial": 0, "uncovered": 1,
                                                         "exempted": 0})

    def test_region_without_violation_is_a_broken_proof(self):
        write_report(self.root, "stmt+decision", [decision(1, "+", [(2, "+")], 10, region="#")],
                     stats("Decision", 1, 0, 0, 0, exempted_no_violation=1))
        with self.assertRaisesRegex(ValueError, "proof broken"):
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

    def statement_report(self, regions, metrics):
        work = self.root / "work"
        work.mkdir()
        (work / "sample1.c").write_text("int x;\n")
        mappings = "".join(
            f'<src_mapping{f" coverage=\"{r}\"" if r else ""}><src><line num="{n}" src="s"/></src>'
            f'<statement id="{n}" text="s{n}" coverage="{st}"><src><line num="{n}"/></src></statement></src_mapping>'
            for n, (st, r) in enumerate(regions, 1))
        xml = self.root / "report" / "xml"
        xml.mkdir(parents=True)
        (xml / "sample1.c.xml").write_text(f'<source file="{work / "sample1.c"}" coverage_level="stmt">{mappings}</source>')
        total, covered, missed, exempted, no_violation = metrics
        st = (f'<obligation_stats kind="Stmt"><metric kind="total_obligations_of_relevance" count="{total}"/>'
              f'<metric kind="fully_covered" count="{covered}"/><metric kind="not_covered" count="{missed}"/>'
              f'<metric kind="exempted" count="{exempted}"/><metric kind="exempted_no_violation" count="{no_violation}"/>'
              '</obligation_stats>')
        (xml / "index.xml").write_text(
            '<document xmlns:xi="http://www.w3.org/2001/XInclude"><coverage_report coverage_level="stmt">'
            f'<coverage_summary>{st}<file name="{work / "sample1.c"}">{st}</file></coverage_summary>'
            '<sources><xi:include parse="xml" href="sample1.c.xml"/></sources></coverage_report></document>')
        return read_report(self.root / "report", work)

    def test_exempted_statements_are_counted_apart(self):
        # COV-002 guard line: the `if` runs (+), its body is exempted; one ordinary miss.
        files = self.statement_report([("+", None), ("+", "*"), ("-", "*"), ("-", "*"), ("-", None)], (5, 1, 1, 2, 1))
        f = files["sample1.c"]
        self.assertEqual((f["statements_total"], f["statements_covered"], f["statements_uncovered"],
                          f["statements_exempted"]), (5, 2, 1, 2))
        self.assertEqual([o["exempted"] for o in f["obligations"]], [False, True, True, True, False])

    def test_executed_exempted_statements_are_a_broken_proof(self):
        with self.assertRaisesRegex(ValueError, "proof broken"):
            self.statement_report([("+", "#")], (1, 0, 0, 0, 1))

    def test_levels(self):
        self.assertEqual(report_levels("stmt"), ("stmt",))
        self.assertEqual(report_levels("stmt+mcdc"), ("stmt", "stmt+decision", "stmt+mcdc"))
        self.assertEqual(report_levels("stmt+uc_mcdc")[-2:], ("stmt+mcdc", "stmt+uc_mcdc"))
        self.assertEqual(report_dir_name("stmt"), "report")


if __name__ == "__main__":
    unittest.main()
