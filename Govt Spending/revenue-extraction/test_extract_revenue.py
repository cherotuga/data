"""
Regression tests for RevenueExtractor._analyze_headers() header-to-column mapping.

The OCOB county revenue table ("Table 2.1: Own Source Revenue Collection") has used
at least four distinct header layouts across FY2014/15-2024/25. Fixtures below are
captured verbatim (via pdfplumber.extract_tables()) from real source PDFs so the
mapping logic is validated against actual formats, not synthetic guesses.

Known-broken behavior (as of the import commit) that this suite pins down:
  - osr_actual_realised is NEVER populated, in any format that has it (category B, C).
  - category C (single-row header) additionally mislabels fif_aia_target or
    ordinary_osr_target with the *actual* value instead of leaving it correct,
    depending on header wording (see CATEGORY_C1 vs CATEGORY_C2).

Run with: pytest test_extract_revenue.py -v
"""
import sys
from pathlib import Path

import pytest

sys.path.insert(0, str(Path(__file__).parent))
from extract_revenue import RevenueExtractor  # noqa: E402

# Any real PDF path works here - RevenueExtractor.__init__ only checks existence,
# these tests exercise _analyze_headers() directly and never call extract().
_SAMPLE_PDF = Path(__file__).parent.parent / "2023_24" / "01" / "county" / "2023_24_01_county.pdf"


def _extractor():
    assert _SAMPLE_PDF.exists(), f"fixture PDF missing: {_SAMPLE_PDF}"
    return RevenueExtractor(str(_SAMPLE_PDF), year="2023_24", quarter="01")


# ---------------------------------------------------------------------------
# Category A: no OSR/FIF breakdown in the source (2015_16-2018_19, 2020_21-2022_23)
# Captured from 2020_21 Q1 page 22.
# ---------------------------------------------------------------------------
CATEGORY_A_HEADERS = [
    "County",
    "Annual Own Source Revenue (OSR) Target for\nFY 2020/21 (Kshs.)",
    "First Quarter of FY 2020/21 OSR\nCollection (Kshs.)",
    "% of Collection of\nOSR Against Annual\nTarget",
]

# ---------------------------------------------------------------------------
# Category B: two-row header, breakdown present (2019_20, all 4 quarters).
# Captured from 2019_20 Q1 page 26 (main header row + sub-header row).
# ---------------------------------------------------------------------------
CATEGORY_B_MAIN = [
    "County",
    "Annual Own Source Revenue (OSR) Target for FY\n2019/20 (Kshs.)",
    None,
    None,
    "First Qurter of FY 2019/20 OSR Performance\n(Kshs.)",
    None,
    None,
    "% of Collection\nof OSR Against\nAnnual Target",
]
CATEGORY_B_SUB = [
    None,
    "Ordinary OSR",
    "Appropriations\nIn Aid (A-I-A)",
    "Total Annual\nTarget",
    "Q1 Actual\nOSR",
    "Q1 Actual AIA",
    "Q1 Actual Total",
    None,
]

# ---------------------------------------------------------------------------
# Category C1: single-row header, "OSR Actual" / "FIF/AIA Actual" wording.
# Captured from 2023_24 Q1 page 25 (also matches 2024_25 Q1 wording).
# ---------------------------------------------------------------------------
CATEGORY_C1_HEADERS = [
    "County",
    "Ordinary OSR\nTarget\n(Kshs.)",
    "FIF/A-I-A\nTarget\n(Kshs.)",
    "Total Target",
    "OSR Actual",
    "FIF/AIA\nActual",
    "Total Actual",
    "Performance\n(%)",
]

# ---------------------------------------------------------------------------
# Category C2: single-row header, "Ordinary OSR Actual Realised" wording.
# Captured from 2024_25 Q3 page 32.
# ---------------------------------------------------------------------------
CATEGORY_C2_HEADERS = [
    "County",
    "Ordinary OSR\nTarget (Kshs.\nMillion)",
    "FIF/ AIA\nTarget (Kshs.\nMillion)",
    "Total OSR\nRevenue Target\n(Kshs.Million)",
    "Ordinary OSR\nActual Realised\n(Kshs.Million)",
    "FIF/AIA Actual\n(Kshs.Million)",
    "Total OSR Reve-\nnue (Kshs.Million)",
    "Perfor-\nmance (%)",
]


class TestCategoryA_NoBreakdown:
    """Simple target/actual/performance format - breakdown columns must stay None."""

    def test_column_mapping(self):
        col_map = _extractor()._analyze_headers(CATEGORY_A_HEADERS)
        assert col_map.get("county") == 0
        assert col_map.get("total_revenue_target") == 1
        assert col_map.get("actual_revenue") == 2
        assert col_map.get("performance_percent") == 3
        # No breakdown exists in this format - must not be fabricated
        assert "ordinary_osr_target" not in col_map
        assert "fif_aia_target" not in col_map
        assert "osr_actual_realised" not in col_map
        assert "fif_aia_actual" not in col_map


class TestCategoryA_ActualOsrCollectionWording:
    """
    No-breakdown format where the actual-collection column header is worded
    "Actual OSR Collection" (word order reversed vs. category C1's bare "OSR
    Actual"). Must map to actual_revenue, not osr_actual_realised - there is no
    real breakdown in this format. Captured from 2020_21 Q4 page 24 and
    2022_23 Q4 page 28.
    """

    HEADERS = [
        "County",
        "Annual Own Source Revenue Target for\nFY 2020/21 (Kshs.)",
        "Actual OSR Collection (Kshs.)",
        "% of Collection of OSR\nAgainst Annual Target",
    ]

    def test_column_mapping(self):
        col_map = _extractor()._analyze_headers(self.HEADERS)
        assert col_map.get("county") == 0
        assert col_map.get("total_revenue_target") == 1
        assert col_map.get("actual_revenue") == 2, (
            "'Actual OSR Collection' must map to actual_revenue, not be captured "
            "by osr_actual's 'actual osr' keyword (reversed word order vs. the "
            "true breakdown column's 'OSR Actual' wording)"
        )
        assert col_map.get("performance_percent") == 3
        assert "osr_actual_realised" not in col_map
        assert "ordinary_osr_target" not in col_map
        assert "fif_aia_target" not in col_map
        assert "fif_aia_actual" not in col_map


class TestCategoryB_TwoRowHeaderBreakdown:
    """2019_20 two-row header - all 8 fields should resolve once fixed."""

    def test_column_mapping(self):
        col_map = _extractor()._analyze_headers(CATEGORY_B_MAIN, CATEGORY_B_SUB)
        assert col_map.get("county") == 0
        assert col_map.get("ordinary_osr_target") == 1
        assert col_map.get("fif_aia_target") == 2
        assert col_map.get("total_revenue_target") == 3
        assert col_map.get("osr_actual_realised") == 4, (
            "column D ('Q1 Actual OSR') must map to osr_actual_realised, not be "
            "swallowed by the 'performance' keyword collision"
        )
        assert col_map.get("fif_aia_actual") == 5
        assert col_map.get("actual_revenue") == 6
        assert col_map.get("performance_percent") == 7


class TestCategoryB_HalfYearWording:
    """
    2019_20 Q2 two-row header: same shape as category B but with "Half Year Actual
    OSR"/"Half Year Actual AIA" sub-header wording, and critically a main/group-title
    header that itself contains the word "Collection" ("First Half of FY 2019/20 OSR
    Collection") - the exact phrase that must NOT leak into column D's classification
    via the main+sub combination. Captured from 2019_20 Q2 page 21.
    """

    MAIN = [
        "County",
        "Annual Own Source Revenue (OSR) Target for FY 2019/20\n(Kshs. Million)",
        None,
        None,
        "First Half of FY 2019/20 OSR Collection (Kshs.\nMillion)",
        None,
        None,
        "% of Collection\nAgainst Annual\nTarget",
    ]
    SUB = [
        None,
        "Ordinary OSR",
        "Appropriations In\nAid (A-I-A)",
        "Total Annual\nTarget",
        "Half Year Actual\nOSR",
        "Half Year\nActual AIA",
        "Half Year Actual\nTotal",
        None,
    ]

    def test_column_mapping(self):
        col_map = _extractor()._analyze_headers(self.MAIN, self.SUB)
        assert col_map.get("county") == 0
        assert col_map.get("ordinary_osr_target") == 1
        assert col_map.get("fif_aia_target") == 2
        assert col_map.get("total_revenue_target") == 3
        assert col_map.get("osr_actual_realised") == 4, (
            "column D ('Half Year Actual OSR') must map to osr_actual_realised even "
            "though the shared group-title main header contains 'Collection'"
        )
        assert col_map.get("fif_aia_actual") == 5, (
            "column E ('Half Year Actual AIA') - 'actual aia' word order must be "
            "recognized, not just 'aia actual'"
        )
        assert col_map.get("actual_revenue") == 6
        assert col_map.get("performance_percent") == 7


class TestCategoryB_ActualOsrCollectionGroupTitle:
    """
    2019_20 Q3/Q4 two-row header: sub-headers are bare "Actual OSR"/"Actual AIA"/
    "Actual Total" (matching category C1's single-row wording exactly), but the
    shared main/group-title header says "... Actual OSR Collection ..." (2019_20 Q4)
    or "... OSR Collection ..." (Q3) - the single-row "collection" guard must not
    fire here since these columns DO have their own sub-header. Captured from
    2019_20 Q4 page 22.
    """

    MAIN = [
        "County",
        "Own Source Revenue (OSR) Target for FY 2019/20\n(Kshs. Million)",
        None,
        None,
        "FY 2019/20 Actual OSR Collection\n(Kshs. Million)",
        None,
        None,
        "% of Collec-\ntion Against\nAnnual\nTarget",
    ]
    SUB = [
        None,
        "Ordinary OSR",
        "Appropriations In\nAid (A-I-A)",
        "Total Annual\nTarget",
        "Actual OSR",
        "Actual AIA",
        "Actual Total",
        None,
    ]

    def test_column_mapping(self):
        col_map = _extractor()._analyze_headers(self.MAIN, self.SUB)
        assert col_map.get("county") == 0
        assert col_map.get("ordinary_osr_target") == 1
        assert col_map.get("fif_aia_target") == 2
        assert col_map.get("total_revenue_target") == 3
        assert col_map.get("osr_actual_realised") == 4, (
            "column D ('Actual OSR') must map to osr_actual_realised - the "
            "single-row 'collection' guard must not apply here since this column "
            "has its own sub-header, unlike category A's true single-row format"
        )
        assert col_map.get("fif_aia_actual") == 5
        assert col_map.get("actual_revenue") == 6
        assert col_map.get("performance_percent") == 7


class TestCategoryC1_SingleRowHeaderOsrActualWording:
    """2023_24 / 2024_25 Q1 single-row header - all 8 fields should resolve once fixed."""

    def test_column_mapping(self):
        col_map = _extractor()._analyze_headers(CATEGORY_C1_HEADERS)
        assert col_map.get("county") == 0
        assert col_map.get("ordinary_osr_target") == 1
        assert col_map.get("fif_aia_target") == 2, (
            "column B ('FIF/A-I-A Target') must not be overwritten by column E "
            "('FIF/AIA Actual') - currently fif_aia_target silently ends up "
            "pointing at the actual value instead of the target"
        )
        assert col_map.get("total_revenue_target") == 3
        assert col_map.get("osr_actual_realised") == 4, (
            "column D ('OSR Actual') must map to osr_actual_realised, not be "
            "swallowed by the 'osr actual' substring in the actual_revenue keyword list"
        )
        assert col_map.get("fif_aia_actual") == 5
        assert col_map.get("actual_revenue") == 6
        assert col_map.get("performance_percent") == 7


class TestCategoryC2_SingleRowHeaderOrdinaryOsrActualWording:
    """2024_25 Q3 single-row header wording - all 8 fields should resolve once fixed."""

    def test_column_mapping(self):
        col_map = _extractor()._analyze_headers(CATEGORY_C2_HEADERS)
        assert col_map.get("county") == 0
        assert col_map.get("ordinary_osr_target") == 1, (
            "column A ('Ordinary OSR Target') must not be overwritten by column D "
            "('Ordinary OSR Actual Realised') - both contain the substring "
            "'ordinary osr', and D is currently processed later, silently "
            "corrupting ordinary_osr_target with the actual value"
        )
        assert col_map.get("fif_aia_target") == 2
        assert col_map.get("total_revenue_target") == 3
        assert col_map.get("osr_actual_realised") == 4
        assert col_map.get("fif_aia_actual") == 5
        assert col_map.get("actual_revenue") == 6
        assert col_map.get("performance_percent") == 7


class TestKnownGoodDataRow:
    """
    Cross-check against a real, fully-populated data row (Kirinyaga, 2024_25 Q3,
    page 32) so a correct column_map is validated against real numbers, not just
    header text. A=430.45 B=218.00 C=648.45 D=258.57 E=244.11 F=502.68 G=78,
    with C=A+B, F=D+E, G=F/C*100 all holding in the source.
    """

    ROW = ["Kirinyaga", "430.45", "218.00", "648.45", "258.57", "244.11", "502.68", "78"]

    def test_formula_consistency_sanity_check(self):
        a, b, c, d, e, f, g = (float(x) for x in self.ROW[1:])
        assert c == pytest.approx(a + b)
        assert f == pytest.approx(d + e)
        assert g == pytest.approx(f / c * 100, abs=0.5)

    def test_mapped_values_match_expected_columns(self):
        col_map = _extractor()._analyze_headers(CATEGORY_C2_HEADERS)
        ex = _extractor()
        assert ex._safe_get_column_value(self.ROW, col_map, "ordinary_osr_target") == "430.45"
        assert ex._safe_get_column_value(self.ROW, col_map, "fif_aia_target") == "218.00"
        assert ex._safe_get_column_value(self.ROW, col_map, "total_revenue_target") == "648.45"
        assert ex._safe_get_column_value(self.ROW, col_map, "osr_actual_realised") == "258.57"
        assert ex._safe_get_column_value(self.ROW, col_map, "fif_aia_actual") == "244.11"
        assert ex._safe_get_column_value(self.ROW, col_map, "actual_revenue") == "502.68"
        assert ex._safe_get_column_value(self.ROW, col_map, "performance_percent") == "78"


class TestCategoryA_QuarterlyBreakdownVariant:
    """
    2018_19 Q4 single-row header: annual target + 4 quarterly OSR columns + FY total +
    performance. No ordinary/FIF split exists in this wording either - the quarterly
    columns must be ignored, not misclassified. Captured from 2018_19 Q4 page 25.
    """

    HEADERS = [
        "County",
        "Annual Own\nSource Revenue\nTarget FY 2018/19\n(Kshs.)",
        "1st Quarter, FY\n2018/19 Own\nSource Revenue\n(Kshs.)",
        "2nd Quarter,\nFY 2018/19 Own\nSource Revenue\n(Kshs.)",
        "3rd Quarter, FY\n2018/19 Own\nSource Revenue\n(Kshs.)",
        "4th Quarter, FY\n2018/19 Own\nSource Revenue\n(Kshs.)",
        "FY 2018/19 Total\nOwn Source Rev-\nenue (Kshs.)",
        "% of Own\nSource\nRevenue\nAgainst\nAnnual\nRevenue\nTarget",
    ]

    def test_column_mapping(self):
        col_map = _extractor()._analyze_headers(self.HEADERS)
        assert col_map.get("county") == 0
        assert col_map.get("total_revenue_target") == 1
        assert col_map.get("actual_revenue") == 6
        assert col_map.get("performance_percent") == 7
        assert "ordinary_osr_target" not in col_map
        assert "fif_aia_target" not in col_map
        assert "osr_actual_realised" not in col_map
        assert "fif_aia_actual" not in col_map


class TestCategoryC1Variant_OsrActualRealisedWording:
    """
    2024_25 Q1 single-row header: same shape as Category C1 but with "OSR Actual
    Realised" wording (a third distinct actual-column phrasing alongside C1's bare
    "OSR Actual" and C2's "Ordinary OSR Actual Realised"). Captured from 2024_25 Q1
    page 26.
    """

    HEADERS = [
        "County",
        "Ordinary OSR\nTarget (Kshs.)",
        "FIF/ AIA Target\n(Kshs.)",
        "Total Revenue\nTarget (Kshs.)",
        "OSR Actual\nRealised (Kshs.)",
        "FIF/AIA Actual\n(Kshs.)",
        "Actual Revenue\n(Kshs.)",
        "Perfor-\nmance (%)",
    ]

    def test_column_mapping(self):
        col_map = _extractor()._analyze_headers(self.HEADERS)
        assert col_map.get("county") == 0
        assert col_map.get("ordinary_osr_target") == 1
        assert col_map.get("fif_aia_target") == 2
        assert col_map.get("total_revenue_target") == 3
        assert col_map.get("osr_actual_realised") == 4
        assert col_map.get("fif_aia_actual") == 5
        assert col_map.get("actual_revenue") == 6
        assert col_map.get("performance_percent") == 7


class TestCategory2015_16_UnitOnlySubHeader:
    """
    2015_16 Q1 two-row header where the sub-header for the target column is just a
    bare currency unit ("Kshs"), not a real column name - the main header ("Local
    Revenue Target for FY 2015/16") carries the actual meaning. Using the sub-header
    alone (as the naive sub-header-only design does) loses the target column
    entirely; the main header must be used as a fallback whenever the sub-header is
    just unit/formula noise. Captured from 2015_16 Q1 page 27.
    """

    MAIN_HEADERS = [
        "First Quarter of FY 2015/16 Revenue (Kshs)",
        None,
        None,
        None,
        None,
        "Local Revenue\nTarget for FY\n2015/16",
        "% of First\nQuarter revenue\nagainst Annual\ntargets",
    ]
    SUB_HEADERS = ["County", "July", "August", "September", "Total (Q1)", "Kshs", ""]

    def test_column_mapping(self):
        col_map = _extractor()._analyze_headers(self.MAIN_HEADERS, self.SUB_HEADERS)
        assert col_map.get("county") == 0
        assert col_map.get("actual_revenue") == 4
        assert col_map.get("total_revenue_target") == 5
        assert col_map.get("performance_percent") == 6


class TestCategory2017_18_FormulaSubHeader:
    """
    2017_18 Q3 two-row header where the second row is a spreadsheet-style formula/
    column-letter row ("A", "B", "C", "D=A+B+C") rather than a real sub-header - the
    real column names live entirely in the main header. Using the sub-header alone
    would classify the actual-revenue column by "d=a+b+c", matching nothing. Captured
    from 2017_18 Q3 page 24.
    """

    MAIN_HEADERS = [
        "County Title",
        "First Quarter\nof FY 2017/18\n( Kshs.Mil)",
        "First Half of FY\n2017/18 (Kshs.Mil)",
        "First Nine Months\nof FY 2017/18\n(Kshs.Mil)",
        "Total Local\nRevenue\nCollection\n(Kshs.Mil)",
        "Annual Local\nRevenue Target\nFor FY 2017/18\n(Kshs.Mil)",
        "Percentage\nof total Local\nRevenue\nCollection\nto Annual\nTarget (%)",
    ]
    SUB_HEADERS = ["", "A", "B", "C", "D=A+B+C", "", ""]

    def test_column_mapping(self):
        col_map = _extractor()._analyze_headers(self.MAIN_HEADERS, self.SUB_HEADERS)
        assert col_map.get("county") == 0
        assert col_map.get("actual_revenue") == 4
        assert col_map.get("total_revenue_target") == 5
        assert col_map.get("performance_percent") == 6


class TestCategory2014_15Unaffected:
    """
    2014_15 uses a wholly separate transposed/reversed-text code path
    (_process_2014_15_transposed_table / _is_2014_15_transposed_table) that never
    calls _analyze_headers(). Breakdown fields are None there by design (the
    source format has no OSR/FIF split). This test guards against the fix
    accidentally being applied to that path.
    """

    def test_2014_15_detection_untouched_by_header_fix(self):
        ex = _extractor()
        # A transposed-format table snippet with reversed-text markers - detection
        # must still work independent of any _analyze_headers changes.
        reversed_table = [
            ["eunever", "ht4", "dr3", "3q2q1q", "", "", "", ""],
            ["", "", "", "", "", "", "", ""],
            ["", "", "", "", "", "", "", "aipikiaL"],
        ]
        assert ex._is_2014_15_transposed_table(reversed_table) is True
