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
import os
import re
import sys
import tempfile
from pathlib import Path

import pandas as pd
import pytest

sys.path.insert(0, str(Path(__file__).parent))
from extract_revenue import RevenueExtractor  # noqa: E402

# RevenueExtractor.__init__ only checks the path exists (it never opens/parses
# the PDF there) - these tests exercise _analyze_headers() directly against
# synthetic/captured header fixtures and never call extract(). A self-created
# placeholder keeps the suite runnable on any branch/checkout regardless of
# whether the (large, not-always-present) real PDF corpus is checked out.
with tempfile.NamedTemporaryFile(suffix=".pdf", delete=False) as _f:
    _f.write(b"%PDF-1.4\n%%EOF")
    _SAMPLE_PDF = Path(_f.name)


def _extractor():
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


class TestCorpusPopulationBaseline:
    """
    Full-corpus regression guard against revenue_data.csv (the checked-in output
    of running extract_revenue.py over every available PDF).

    These floors were captured from the full 1974-record corpus as of the
    _analyze_headers() sub-header-noise fix (commit 84bec97). They are floors,
    not exact-match assertions, because re-running extraction over a growing set
    of PDFs (newer quarterly reports) is expected to add rows and can only raise
    these counts - a drop below the floor means a real regression, not just
    "more data with more gaps". If you intentionally change parsing behavior in
    a way that legitimately lowers a count, update the floor here and explain
    why in revenue-extraction/TODO.md.

    ordinary_osr_target / fif_aia_target are populated only in Category B/C table
    formats (they don't exist in every year's table), so their floors are much
    lower than the near-universal fields - this is a genuine property of the
    source data, not a bug (verified against real PDFs; see README.md).
    """

    CSV_PATH = os.path.join(os.path.dirname(__file__), "revenue_data.csv")

    @pytest.fixture(scope="class")
    @classmethod
    def df(cls):
        if not os.path.exists(cls.CSV_PATH):
            pytest.skip("revenue_data.csv not present - run extraction first")
        return pd.read_csv(cls.CSV_PATH, dtype=str)

    def test_total_records(self, df):
        assert len(df) >= 1974

    def test_core_field_population(self, df):
        assert df['total_revenue_target'].notna().sum() >= 1970
        assert df['actual_revenue'].notna().sum() >= 1974
        assert df['performance_percent'].notna().sum() >= 1970

    def test_breakdown_field_population(self, df):
        assert df['osr_actual_realised'].notna().sum() >= 517
        assert df['fif_aia_actual'].notna().sum() >= 342
        assert df['ordinary_osr_target'].notna().sum() >= 517
        assert df['fif_aia_target'].notna().sum() >= 344


class TestCorpusPopulationByYear:
    """
    Per-county, per-year/quarter version of TestCorpusPopulationBaseline.

    A flat corpus-wide floor can hide a real regression in one year behind
    healthy population in another. This class checks each year/quarter group
    against what's actually true of the source PDFs, so the tests stay
    "context aware" instead of just checking an aggregate number:

    - Every year/quarter should have exactly 47 counties (one row per county),
      except 2017_18 Q2, whose county-level OCOB report was never published
      (only the national report exists for that quarter - confirmed by
      checking the PDF corpus directory, not an extraction bug).
    - total_revenue_target/performance_percent should be present for all 47
      counties in every year/quarter except two confirmed-genuine PDF gaps
      (see README.md): 2020_21 Q1 (missing kitui/mandera/wajir) and 2021_22 Q1
      (missing vihiga).
    - The OSR/FIF-AIA breakdown columns (ordinary_osr_target, fif_aia_target,
      osr_actual_realised, fif_aia_actual) only exist in some years' table
      format:
        * 2014_15-2018_19 and 2020_21-2022_23: no breakdown at all (0/47) -
          this table format has no OSR/FIF-AIA split.
        * 2019_20: ordinary_osr_target/osr_actual_realised are universal
          (47/47), but fif_aia_target/fif_aia_actual are reported by only a
          small, *consistent* set of counties each quarter (~7-9/47:
          bungoma, elgeyo marakwet, embu, homa bay, meru, nairobi city,
          nakuru, nyandarua, plus one of turkana/west pokot depending on
          quarter) - a stable subset, not scattered/random, consistent with
          only some counties having FIF/AIA revenue to report that year
          rather than a parsing defect.
        * 2023_24 onwards: ordinary_osr_target/osr_actual_realised are
          universal (47/47); fif_aia_target/fif_aia_actual are populated for
          most but not all counties per quarter (as low as 41/47), with the
          handful of gaps varying county-by-county across quarters (not the
          same counties every time) - the signature of genuine per-county
          reporting gaps, not a systematic extraction bug. This lines up with
          Kenya's Facilities Improvement Financing Act, 2023 (assented
          2023-10-19, in force 2023-11-02 - right at the start of FY2023/24),
          which made FIF accounts/reporting a legal requirement for county
          health facilities. See README.md.
    """

    CSV_PATH = os.path.join(os.path.dirname(__file__), "revenue_data.csv")

    NO_BREAKDOWN_YEARS = {
        '2014_15', '2015_16', '2016_17', '2017_18', '2018_19',
        '2020_21', '2021_22', '2022_23',
    }

    # 2017_18 Q2: county-level report never published for that quarter.
    MISSING_QUARTERS = {('2017_18', '02')}

    # Confirmed-genuine gaps (verified against the source PDF - see README.md).
    KNOWN_TARGET_GAPS = {
        ('2020_21', '01'): {'kitui', 'mandera', 'wajir'},
        ('2021_22', '01'): {'vihiga'},
    }

    @pytest.fixture(scope="class")
    @classmethod
    def df(cls):
        if not os.path.exists(cls.CSV_PATH):
            pytest.skip("revenue_data.csv not present - run extraction first")
        return pd.read_csv(cls.CSV_PATH, dtype=str)

    def test_every_year_quarter_has_47_counties_except_known_missing_report(self, df):
        counts = df.groupby(['year', 'quarter'])['county'].nunique()
        for (year, quarter), n in counts.items():
            if (year, quarter) in self.MISSING_QUARTERS:
                continue
            assert n == 47, f"{year} Q{quarter}: expected 47 counties, got {n}"

    def test_missing_quarters_have_no_source_pdf(self, df):
        for year, quarter in self.MISSING_QUARTERS:
            assert df[(df.year == year) & (df.quarter == quarter)].empty

    def test_target_and_performance_gaps_match_known_pdf_gaps(self, df):
        for (year, quarter), group in df.groupby(['year', 'quarter']):
            expected_missing = self.KNOWN_TARGET_GAPS.get((year, quarter), set())
            actual_missing = set(group[group['total_revenue_target'].isna()]['county'])
            assert actual_missing == expected_missing, (
                f"{year} Q{quarter}: total_revenue_target missing counties "
                f"{actual_missing} != expected {expected_missing}"
            )
            # performance_percent has always tracked total_revenue_target 1:1
            # in this corpus - assert that stays true rather than drifting.
            actual_missing_perf = set(group[group['performance_percent'].isna()]['county'])
            assert actual_missing_perf == expected_missing, (
                f"{year} Q{quarter}: performance_percent missing counties "
                f"{actual_missing_perf} != expected {expected_missing}"
            )

    def test_no_breakdown_years_have_zero_breakdown_population(self, df):
        breakdown_cols = ['ordinary_osr_target', 'fif_aia_target', 'osr_actual_realised', 'fif_aia_actual']
        subset = df[df['year'].isin(self.NO_BREAKDOWN_YEARS)]
        for col in breakdown_cols:
            n = subset[col].notna().sum()
            assert n == 0, f"{col}: expected 0 in no-breakdown years, got {n}"

    def test_2019_20_ordinary_osr_universal_fif_partial(self, df):
        subset = df[df['year'] == '2019_20']
        for quarter, group in subset.groupby('quarter'):
            assert group['ordinary_osr_target'].notna().sum() == 47, (
                f"2019_20 Q{quarter}: ordinary_osr_target should be universal"
            )
            assert group['osr_actual_realised'].notna().sum() == 47, (
                f"2019_20 Q{quarter}: osr_actual_realised should be universal"
            )
            fif_target_n = group['fif_aia_target'].notna().sum()
            assert 7 <= fif_target_n <= 9, (
                f"2019_20 Q{quarter}: fif_aia_target should be a small, stable "
                f"subset (7-9/47), got {fif_target_n}"
            )

    def test_2023_24_onwards_ordinary_osr_universal_fif_mostly_populated(self, df):
        subset = df[df['year'].isin(['2023_24', '2024_25'])]
        for (year, quarter), group in subset.groupby(['year', 'quarter']):
            assert group['ordinary_osr_target'].notna().sum() == 47, (
                f"{year} Q{quarter}: ordinary_osr_target should be universal"
            )
            assert group['osr_actual_realised'].notna().sum() == 47, (
                f"{year} Q{quarter}: osr_actual_realised should be universal"
            )
            fif_target_n = group['fif_aia_target'].notna().sum()
            assert fif_target_n >= 41, (
                f"{year} Q{quarter}: fif_aia_target should be near-universal "
                f"(>=41/47), got {fif_target_n}"
            )


def _to_number(value):
    """Strip currency formatting (commas, 'Kshs', stray %) down to a float."""
    if pd.isna(value):
        return None
    cleaned = re.sub(r'[^0-9.\-]', '', str(value))
    try:
        return float(cleaned)
    except ValueError:
        return None


class TestCorpusValueConsistency:
    """
    Population counts alone can't catch a header-mapping bug that keeps every
    cell filled but swaps values between columns (e.g. writes the FIF value
    into the ordinary OSR column). These tests instead check the arithmetic
    relationships between columns that must hold given how the source table
    is structured, across every row in the corpus where the relevant fields
    are present:

    - performance_percent must equal actual_revenue / total_revenue_target * 100
    - ordinary_osr_target + fif_aia_target must equal total_revenue_target
      (wherever the corpus has the OSR/FIF-AIA breakdown)
    - osr_actual_realised + fif_aia_actual must equal actual_revenue (ditto)

    All three held for every applicable row (1970 / 344 / 349 rows respectively)
    in the full corpus as of commit 84bec97 - see README.md.
    """

    CSV_PATH = os.path.join(os.path.dirname(__file__), "revenue_data.csv")
    TOLERANCE_PCT = 0.02  # 2% relative tolerance for rounding in the source PDFs
    TOLERANCE_ABS = 2.0   # plus a small absolute floor for near-zero values

    @pytest.fixture(scope="class")
    @classmethod
    def df(cls):
        if not os.path.exists(cls.CSV_PATH):
            pytest.skip("revenue_data.csv not present - run extraction first")
        df = pd.read_csv(cls.CSV_PATH, dtype=str)
        for col in ['ordinary_osr_target', 'fif_aia_target', 'total_revenue_target',
                    'osr_actual_realised', 'fif_aia_actual', 'actual_revenue',
                    'performance_percent']:
            df[col + '_n'] = df[col].apply(_to_number)
        return df

    def _assert_no_outliers(self, df, lhs_col, rhs_col, label):
        subset = df.dropna(subset=[lhs_col, rhs_col])
        diff = (subset[lhs_col] - subset[rhs_col]).abs()
        tolerance = subset[rhs_col].abs() * self.TOLERANCE_PCT + self.TOLERANCE_ABS
        outliers = subset[diff > tolerance]
        assert outliers.empty, (
            f"{label}: {len(outliers)} row(s) violate the expected relationship:\n"
            f"{outliers[['county', 'year', 'quarter', lhs_col, rhs_col]].to_string()}"
        )
        return len(subset)

    def test_performance_percent_matches_formula(self, df):
        subset = df.dropna(subset=['total_revenue_target_n', 'actual_revenue_n', 'performance_percent_n'])
        subset = subset[subset['total_revenue_target_n'] != 0].copy()
        subset['calc'] = subset['actual_revenue_n'] / subset['total_revenue_target_n'] * 100
        n = self._assert_no_outliers(subset, 'calc', 'performance_percent_n', 'performance_percent')
        assert n >= 1970

    def test_target_breakdown_sums_to_total(self, df):
        subset = df.dropna(subset=['ordinary_osr_target_n', 'fif_aia_target_n', 'total_revenue_target_n']).copy()
        subset['sum'] = subset['ordinary_osr_target_n'] + subset['fif_aia_target_n']
        n = self._assert_no_outliers(subset, 'sum', 'total_revenue_target_n', 'ordinary_osr_target + fif_aia_target')
        assert n >= 344

    def test_actual_breakdown_sums_to_total(self, df):
        subset = df.dropna(subset=['osr_actual_realised_n', 'fif_aia_actual_n', 'actual_revenue_n']).copy()
        subset['sum'] = subset['osr_actual_realised_n'] + subset['fif_aia_actual_n']
        n = self._assert_no_outliers(subset, 'sum', 'actual_revenue_n', 'osr_actual_realised + fif_aia_actual')
        assert n >= 349

    def test_no_duplicate_county_quarter_rows(self, df):
        dupes = df[df.duplicated(subset=['county', 'year', 'quarter'], keep=False)]
        assert dupes.empty, f"duplicate (county, year, quarter) rows found:\n{dupes[['county', 'year', 'quarter']]}"

    def test_exactly_47_canonical_counties(self, df):
        assert df['county'].nunique() == 47, sorted(df['county'].unique())
