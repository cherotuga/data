# OSR Extraction - Status & Changelog

For setup/usage, table format reference, the population baseline, and the
validation methodology, see `README.md`. This file tracks open work and the
dated resolved-issue history, mirroring `department/TODO.md`'s convention.

## Current Status

- ✅ All known header-format regressions fixed and covered by regression tests (28/28 passing).
- ✅ Every output column verified for both population (which cells are filled) and value correctness (arithmetic relationships between columns) - see `README.md`.
- ✅ All previously-open investigations closed (see Resolved Issues below).
- 🔄 No open extraction-code issues at present.

## Notes

- This module (`extract_revenue.py`) has zero dependency on the
  department-extraction code (`../department/*.py`) - it only imports
  `pdfplumber`, `pandas`, `pathlib`, `re`, `logging`, `argparse`, `warnings`,
  `typing`. It can be merged/deployed independently of department-extraction
  work.
- `revenue_data.csv` is checked in; the test suite's corpus-level tests
  (`TestCorpusPopulationBaseline`, `TestCorpusPopulationByYear`,
  `TestCorpusValueConsistency`) validate it directly rather than
  re-running extraction, so they run fast and don't need the PDF corpus
  present.

---

## ✅ RESOLVED ISSUES

### ✅ Sub-header noise and keyword-collision regressions (Resolved 2026-09-16)
**Previous Problem:** Three regressions in `_analyze_headers()`:
1. A sub-header normalization regex stripped `=`/`+`, so a spreadsheet
   formula sub-header like `"D=A+B+C"` normalized to `"dabc"` and was
   treated as a real column name instead of noise.
2. Bare-unit sub-headers (e.g. `"Kshs"`) were likewise treated as real
   column names, silently overwriting the correct main-header mapping.
3. Three keyword-matching bugs in the `elif` chain: wrong word order shadowed
   `osr_actual_realised` behind the generic `actual_revenue` catch-all;
   bare `'ordinary osr'`/`'fif'` keywords matched both a target and its
   actual column, so whichever was processed later overwrote the earlier
   `column_map` entry; and a bare `'performance'` keyword false-matched a
   shared "OSR Performance" group-title header.
**Root Cause:** noise-detection ran on already-normalized text (losing the
signal characters), and the keyword `elif` chain had no protection against
one keyword being a substring of another column's header.
**Solution:** compute sub-header noise from raw pre-normalization text; add
an `is_actual_column` gate before the target-column branches; fix keyword
word order and specificity. See commits `595224d`, `84bec97`.
**Result:** all affected columns recover to their expected population; 28
regression tests (including corpus-wide population and value-consistency
checks) now guard against recurrence.

### ✅ 4 missing `total_revenue_target` rows (Resolved 2026-09-16)
**Previous Problem:** Kitui/Mandera/Wajir 2020_21 Q1 and Vihiga 2021_22 Q1
have no `total_revenue_target`.
**Root Cause:** none - verified via direct pdfplumber read of the source
PDFs that the target column literally contains `-` in each case.
**Solution:** no code change; confirmed as a genuine source-data gap.
**Result:** `TestCorpusPopulationByYear` asserts this exact missing-county
set, so a future change that silently drops *more* rows will fail loudly.

### ✅ `ordinary_osr_target`/`fif_aia_target` sparse population (Resolved 2026-09-16)
**Previous Problem:** these two fields are populated far less than
`total_revenue_target`/`actual_revenue` (26%/17% vs. 99.8%/100%), raising the
question of whether this was a parsing bug.
**Root Cause:** none - genuine property of the source data. See `README.md`
for the full per-year breakdown and the legal basis (Kenya's *Facilities
Improvement Financing Act, 2023*, in force 2023-11-02) for the sparsity
pattern changing at FY2023/24.
**Solution:** no code change; confirmed via systematic per-year/county
population analysis plus spot-checks against real PDFs.
**Result:** `TestCorpusPopulationByYear` encodes the expected pattern
per year (zero in no-breakdown years, a stable partial set in 2019_20,
near-universal from 2023_24) so a real regression can't hide behind
"it's always been sparse."

### ✅ 2017_18 Q2 entirely missing from the corpus (Resolved 2026-09-16)
**Previous Problem:** no rows exist for any county in 2017_18 Q2.
**Root Cause:** none - confirmed via the source PDF directory listing that
`Govt Spending/2017_18/02/` only contains a national report PDF; no county
report PDF was ever published for that quarter.
**Solution:** no code change.
**Result:** `TestCorpusPopulationByYear::test_missing_quarters_have_no_source_pdf`
guards this stays the only missing quarter (a new gap elsewhere would fail
`test_every_year_quarter_has_47_counties_except_known_missing_report`).
