# OSR (Own Source Revenue) Extraction

Extracts "Table 2.1: Own Source Revenue Collection" from OCOB (Office of the
Controller of Budget) quarterly county budget implementation review PDFs,
covering FY2014/15 through the present. Output: `revenue_data.csv`.

For the resolved-issue changelog and any currently open work, see `TODO.md`.
This file is the stable reference: how to run it, what the table formats
look like, and what the output data actually means.

## Setup & running

Run from `Govt Spending/` (one level up from this directory) - the extractor
searches the current directory tree for `*county*.pdf` files using the
`<year>/<quarter>/county/*.pdf` layout, and its default output path
(`revenue-extraction/revenue_data.csv`) assumes that working directory:

```bash
cd "Govt Spending"
python3 revenue-extraction/extract_revenue.py --all
```

Other useful flags (see `extract_revenue.py --help`):
- `--year 2023_24 --quarter 01` - process a single PDF instead of the whole corpus
- `--output <path>` - write elsewhere
- `--debug` - enable header/table-detection debug logging

Run the regression suite from `revenue-extraction/`:
```bash
cd revenue-extraction
python3 -m pytest test_extract_revenue.py -v
```

## Table format categories

The source table's header layout has changed at least four times across
FY2014/15-2024/25. `RevenueExtractor._analyze_headers()` (and, for 2014/15
specifically, a separate transposed-table code path) has to recognize all of
them:

- **Category A** - single total-only header, no OSR/FIF-AIA breakdown
  columns at all (most years: 2014_15-2018_19, 2020_21-2022_23).
- **Category B** - two-row header with a shared group-title row (e.g. "OSR
  Performance") spanning Ordinary-OSR/FIF-AIA/Total sub-columns.
- **Category C1/C2** - single-row header whose column text itself names
  Ordinary OSR vs. FIF-AIA vs. Total, worded differently between C1 and C2.
- **2014_15** - a wholly separate transposed/reversed-text table layout,
  handled by `_process_2014_15_transposed_table` /
  `_is_2014_15_transposed_table`, which never calls `_analyze_headers()` and
  has no OSR/FIF-AIA breakdown by format.
- **Sub-header noise** - some formats add a second header row that's not a
  real column name at all: a bare currency-unit label ("Kshs"), or a
  spreadsheet formula/column-letter artifact ("D=A+B+C"). These must be
  detected from the *raw* header text before normalization strips the `=`/`+`
  characters the pattern depends on (see `sub_header_is_noise` in
  `_analyze_headers()`).

Each category (and the noise-detection cases) has a dedicated regression test
class in `test_extract_revenue.py`, built from real header/row fixtures
captured verbatim via `pdfplumber.extract_tables()` against the actual source
PDFs - not synthetic guesses.

## Output columns & population baseline

As of the full corpus (1974 records, all 47 counties, FY2014/15-2024/25 to
date, except one missing quarter - see below):

| Column | Populated | Why |
|---|---|---|
| `county`, `year`, `quarter` | 1974/1974 | identifiers; 47 canonical county names, no duplicate (county, year, quarter) rows |
| `total_revenue_target` | 1970/1974 (99.8%) | 4 genuine PDF gaps (target column literally `-` in source): Kitui/Mandera/Wajir 2020_21 Q1, Vihiga 2021_22 Q1 |
| `actual_revenue` | 1974/1974 (100%) | |
| `performance_percent` | 1970/1974 (99.8%) | same 4 gaps as `total_revenue_target` (tracks it 1:1) |
| `ordinary_osr_target` | 517/1974 (26.2%) | only years/counties with the OSR/FIF-AIA breakdown - see below |
| `osr_actual_realised` | 517/1974 (26.2%) | ditto |
| `fif_aia_target` | 344/1974 (17.4%) | ditto, further gated by FIF reporting adoption - see below |
| `fif_aia_actual` | 349/1974 (17.7%) | ditto |

**2017_18 Q2 has zero rows for any county** - not an extraction bug; no
county-level PDF was ever published for that quarter (only the national
report exists in the source corpus for `2017_18/02/`).

### Why `ordinary_osr_target`/`fif_aia_target` are sparse

- **Category A years** (2014_15-2018_19, 2020_21-2022_23): 0 populated - the
  table format has no breakdown at all, by design.
- **2019_20**: `ordinary_osr_target`/`osr_actual_realised` are universal
  (47/47 every quarter), but `fif_aia_target`/`fif_aia_actual` are reported
  by only a small, *consistent* set of ~7-9 counties each quarter (bungoma,
  elgeyo marakwet, embu, homa bay, meru, nairobi city, nakuru, nyandarua,
  plus one of turkana/west pokot depending on quarter). The same counties
  recur every quarter, which is the signature of a genuine reporting
  difference rather than a random parsing failure.
- **2023_24 onward**: `ordinary_osr_target`/`osr_actual_realised` stay
  universal; `fif_aia_target`/`fif_aia_actual` jump to near-universal
  (41-47/47, reaching full 47/47 by 2024_25 Q2), with the remaining
  per-quarter gaps varying county-by-county rather than repeating.

**Legal basis:** Kenya's *Facilities Improvement Financing Act, 2023* was
assented 2023-10-19 and came into force 2023-11-02 - right at the start of
FY2023/24 - making FIF accounts/reporting a legal requirement for public
health facilities at the county level. This is why FIF reporting goes from a
sparse, stable handful of early-adopter counties (2019_20, e.g. Nyandarua had
a local FIF framework ahead of the national Act) to near-universal from
2023_24 onward, closing further as more counties finalize their accounts.

### Value-correctness guarantees

Beyond population, the values themselves satisfy these relationships across
every applicable row in the full corpus (checked exactly, not sampled):

- `performance_percent == actual_revenue / total_revenue_target * 100` (1970/1970 rows, within 2% rounding tolerance)
- `ordinary_osr_target + fif_aia_target == total_revenue_target` (344/344 rows)
- `osr_actual_realised + fif_aia_actual == actual_revenue` (349/349 rows)

All of the above - population floors, per-year/county population patterns,
and value-consistency formulas - are enforced by `TestCorpusPopulationBaseline`,
`TestCorpusPopulationByYear`, and `TestCorpusValueConsistency` in
`test_extract_revenue.py`. If you regenerate `revenue_data.csv` from a wider
PDF corpus and a count legitimately needs to change, update both this table
and the corresponding test.

## The validation loop

The methodology used to find and fix every header-format regression in this
pipeline's history (and the one to reuse for the next one):

1. Run full extraction (`extract_revenue.py --all`) and compare the
   resulting population % per column against the baseline table above.
2. If a column's population drops for some year/quarter, don't guess - pull
   the actual table for that year/quarter with `pdfplumber.extract_tables()`
   and inspect the raw header row(s) that got misclassified.
3. Identify the specific header wording/format variant causing the
   misclassification (bare unit label, formula artifact, reworded column
   title, keyword collision with another column, etc.).
4. Fix `_analyze_headers()` to handle that variant, using the raw
   (pre-normalization) text where the normalization regex would otherwise
   destroy the signal (e.g. it strips `=` and `+`).
5. Add a regression test class using the *real* captured header/row fixture
   for that format (not a synthetic guess).
6. Re-run full extraction, confirm the population % recovers and nothing
   else regressed, and update the baseline table above and the
   `TestCorpusPopulationBaseline`/`TestCorpusPopulationByYear` floors if the
   corpus grew.
