# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is a data extraction pipeline for processing Kenyan government spending data from Office of the Controller of Budget (OCOB) quarterly reports. The system extracts financial data from PDF reports, converts them to structured CSV files, and performs semantic analysis to identify health-related spending.

## Architecture

The codebase follows a two-stage pipeline:

### Stage 1: PDF to CSV Extraction (`program.py`)
- **Sequential Processing**: Processes PDF elements (text + tables) in document order for accurate county boundary detection
- **Title-Bounded Sections**: Uses county headings and "Recommendations" sections as definitive start/end markers
- **Multi-page Table Support**: Handles programme tables that span multiple pages within county sections
- **Fuzzy Matching**: Normalizes county names against official 47 counties list using similarity threshold
- **Cross-Quarter Compatibility**: Handles different PDF formats (Q2 2023_24 patterns, etc.)
- **Outputs**: Structured CSV files at `program/{year}/{quarter}/county/{county}_programme_table.csv`

### Stage 2: Health Data Analysis (`baringo-scraping.py`)
- Main driver for health spending analysis using hybrid classification approach
- Combines keyword screening, semantic similarity (sentence-transformers), and contextual analysis
- Validates subtotals against calculated values to ensure data integrity
- Uses HealthBudgetAnalyzer class for comprehensive health spending extraction

## Key Data Flow

```
PDF Reports → program.py → CSV Files → baringo-scraping.py → Health Budget Excel Report
```

### Data Processing Pipeline

```
┌─────────────────┐    ┌─────────────────┐    ┌─────────────────┐    ┌─────────────────┐
│   PDF Reports   │───►│   program.py    │───►│   CSV Files     │───►│ baringo-scraping│
│    (Source)     │    │  (Extraction)   │    │   (Interim)     │    │    (Analysis)   │
│ 47 Counties x   │    │                 │    │ 47 Counties x   │    │ Health Budget   │
│ 4 Quarters x    │    │ • TOC Parsing   │    │ 4 Quarters x    │    │ Classification  │
│ Multiple Years  │    │ • Table Extract │    │ Multiple Years  │    │ & Validation    │
│                 │    │ • Fuzzy Match   │    │ = 1,100+ files  │    │                 │
└─────────────────┘    └─────────────────┘    └─────────────────┘    └─────────────────┘
```

### Stage 1: Sequential PDF Processing Architecture

#### Document Structure Understanding
Each PDF contains:
- **Table of Contents**: Maps counties to table locations
- **County Sections**: Title-bounded sections for all 47 counties
- **Programme Tables**: Budget vs actual expenditure by programme/sub-programme
- **Section Markers**: "Key Observations and Recommendations" end each county

#### Sequential Processing Flow
```
┌─────────────────────────────────────────────────────────────────────────────────┐
│ SEQUENTIAL ELEMENT PROCESSING (by Y-position, top to bottom)                   │
├─────────────────────────────────────────────────────────────────────────────────┤
│ 1. Extract & Sort Elements: text_lines + tables → sorted by Y-position         │
│ 2. Process in Document Order:                                                  │
│    • County Title (font_size >= 10) → START new county section                 │
│    • Programme Tables → Assign to current_county                               │
│    • "Recommendations" section → END county section                            │
│ 3. Multi-page Continuity: headers_match() preserves table spans               │
│ 4. Buffer Management: finalize_buffer() merges tables per county              │
└─────────────────────────────────────────────────────────────────────────────────┘
```

#### Key Advantages of Sequential Processing
- **✅ No Spatial Validation**: Eliminates Y-coordinate geometry dependencies
- **✅ Document Order Processing**: Trust PDF structure over position heuristics  
- **✅ Clear Section Boundaries**: County titles and recommendations provide definitive markers
- **✅ Cross-County Data Integrity**: Prevents table misattribution (Kakamega-Kajiado fix)
- **✅ Multi-page Table Support**: Preserves table continuity within county sections

### Stage 2: Health Classification Methods

```
┌─────────────────┐  ┌─────────────────┐  ┌─────────────────┐  ┌─────────────────┐
│ Keyword         │  │ Semantic        │  │ Contextual      │  │ Hybrid          │
│ Screening       │─►│ Similarity      │─►│ Analysis        │─►│ Classification  │
│                 │  │ (Transformers)  │  │                 │  │ + Validation    │
└─────────────────┘  └─────────────────┘  └─────────────────┘  └─────────────────┘
```

## Commands

### Extract Programme Data from PDFs
```bash
# Process specific year and quarter
python program.py --year 2022 --quarter 4

# Process all available PDFs
python program.py --all

# Process a range
python program.py --year 2020 --quarter 2 --end-year 2021 --end-quarter 3
```

### Analyze Health Spending Data
```bash
# Process all available health data (primary command)
python baringo-scraping.py

# Process specific years/quarters/counties
python baringo-scraping.py --years 2019_20 2020_21 --quarters 01 02 --counties baringo nakuru

# Specify output file
python baringo-scraping.py --output "custom_health_analysis.xlsx"
```

## Directory Structure

```
├── program/                    # Extracted CSV files
│   ├── 2019_20/01/county/     # Financial year 2019-20, Q1, county data
│   │   ├── baringo_programme_table.csv
│   │   ├── bomet_programme_table.csv
│   │   └── ... (47 counties)
│   └── .../                   # Other years/quarters
├── 2019_20/                   # Source PDF files by financial year
│   ├── 01/county/             # Quarter folders
│   └── .../
├── jan2025/                   # Latest quarterly extracts
├── sep2024/                   # Previous quarterly extracts
└── baringo-scraping.py        # Main health analysis driver
```

## PDF Structure and Data Organization

The source PDFs follow this structure:
- **Filename format**: `{year}_{yy}_{qq}_county.pdf` (e.g., `2019_20_01_county.pdf`)
- **Content**: Each PDF contains budget execution tables for all 47 counties
- **Table types**: Programme budgets, departmental budgets, and specialized health programme data
- **Organization**: County-by-county with table of contents for navigation

The extracted CSVs maintain this hierarchy:
- **Standard columns**: Programme, Sub-Programme, Approved Budget, Actual Payments
- **One file per county per quarter**: Enables granular analysis
- **Preserved metadata**: Year, quarter, and county information maintained

## Key Dependencies

- `pdfplumber` - PDF table extraction with handling for complex layouts
- `sentence-transformers` - Semantic analysis using 'all-MiniLM-L6-v2' model
- `fuzzywuzzy` - County name matching against official list
- `pandas` - Data manipulation and analysis
- `openpyxl` - Excel export functionality

## Health Analysis Methodology

The HealthBudgetAnalyzer uses a multi-step approach:

1. **Keyword Screening**: Identifies obvious health/non-health terms
2. **Contextual Analysis**: Examines programme structure and boundaries  
3. **Semantic Similarity**: Uses embeddings to classify uncertain cases
4. **Subtotal Validation**: Verifies data integrity by comparing reported vs calculated totals
5. **Hybrid Classification**: Combines all methods for robust health spending identification

## Data Schema

Standard CSV schema from PDF extraction:
- `programme` - Main programme/department name
- `sub_programme` - Sub-programme or line item description  
- `approved_budget` - Approved budget amount (Kshs)
- `actual_payments` - Actual expenditure (Kshs)

Enhanced schema in health analysis:
- Adds metadata: `county`, `year`, `quarter`
- Classification details: `method`, `confidence`, `validation_status`
- Data quality flags: completeness indicators and validation results

## Testing Framework

Regression testing ensures code changes don't break existing functionality.

### Test program.py CSV Extraction
```bash
# Run tests
python test_program.py

# Add verified baseline line counts
python add_baseline.py --year 2019_20 --quarter 01 --county baringo --lines 134

# Discover existing CSV line counts
python add_baseline.py --discover
```

### Pre-commit Hooks
```bash
# Install pre-commit (optional)
pip install pre-commit
pre-commit install

# Tests run automatically on git commit when program.py changes
```

### Test Structure
- **`test_program.py`**: Validates CSV line counts match expected baselines
- **`add_baseline.py`**: Helper to add verified line counts after manual PDF verification  
- **`.pre-commit-config.yaml`**: Runs tests automatically on code changes

## Testing/Debugging

Configure debug flags in `program.py`:
```python
TARGET_COUNTY = ["nakuru"]     # Focus on specific county
toc_debug = True               # Table of contents parsing
header_debug = True            # Table header matching  
match_line_debug = True        # County name matching
```

Enable detailed logging in `baringo-scraping.py`:
```python
# In HealthBudgetAnalyzer.__init__()
self.debug_semantic = True
self.debug_contextual = True  
self.debug_check_health_context_ahead = True
```

## Important Notes

- The 47 counties are hardcoded and validated using fuzzy matching
- Health analysis requires internet connection on first run to download the transformer model
- Subtotal validation ensures data integrity before aggregation
- CSV column mapping uses semantic similarity to handle header variations
- Multi-file analysis automatically discovers available data files

---

# IMPLEMENTATION STATUS ✅ COMPLETE

**Status**: All phases completed (2025-09-02)  
**Current Status**: Production-ready system with sequential processing architecture

## Key Features Implemented
- **Sequential PDF Processing**: Document-order element processing eliminates spatial validation complexity
- **Title-Bounded County Sections**: Clear START/END markers using county headings and "Recommendations"
- **Multi-year Support**: Handles 2019_20 through 2024_25 (6 years) with robust column matching
- **Cross-County Data Integrity**: Prevents table misattribution (Kakamega-Kajiado issue resolved)
- **Multi-page Table Continuity**: Preserves table spans within county sections using header matching
- **Optimized Model Loading**: Semantic similarity model loaded once per multi-file analysis  
- **Empty File Handling**: Graceful skip of invalid/empty CSV files without crashes
- **Column Normalization**: Exact word matching with substring fallback handles all naming variations
- **Health Classification**: Contextual understanding (e.g., "LAN installation at hospital" = health infrastructure)
- **Subtotal Validation**: Cross-quarter compatibility with high validation rates

## Multi-Year Test Results (Baringo)
- **2019_20**: 9 health records, 4 perfect subtotal matches
- **2020_21**: 46 health records across 4 quarters, excellent validation  
- **2021_22**: 32 health records, strong subtotal validation
- **2022_23**: 22 health records, robust cross-quarter processing
- **2023_24**: 24 health records (Q2 empty file handled gracefully)
- **2024_25**: 18 health records, 6+ perfect subtotal matches per quarter

---

# ISSUE 1: HEALTH CLASSIFICATION FIXES - COMPLETED ✅
**Problem**: HEALTH_DEPARTMENT_CONTEXT method causing false positives (e.g., Agricultural Development admin classified as health)
**Solution**: Removed 6 contextual analysis functions, added simple 3-row adjacency check, applied text normalization for line breaks
**Result**: Eliminates false positives while preserving legitimate health admin detection

# ISSUE 2: SUBTOTAL VALIDATION - COMPLETED ✅
**Problem**: After normalization, subtotal validation shows `no_explicit_subtotal` and blank `calculated_approved` values
**Solution**: Fixed case-sensitive subtotal detection, updated position detection logic, added health-only validation filtering
**Result**: Baringo 2019_20 Q1: 4 perfect matches (vs 0 before), Q4: 25 perfect matches with cross-quarter compatibility

# ISSUE 3: Q2 2023_24 TABLE EXTRACTION FAILURE - RESOLVED ✅
**Problem**: Empty 1-byte CSV files for all counties in 2023_24 Q2 due to county detection failure
**Solution**: Added Q2-specific patterns for "County Government [County]" and "County Government of [County]" formats
**Result**: Q2 extraction now works for all counties with proper headers and budget data

# ISSUE 4: SPATIAL COUNTY BOUNDARY DETECTION - RESOLVED ✅
**Problem**: Kakamega 2019_20 Q1 showing Kajiado's programme data due to incorrect table assignment above county heading
**Solution**: Added Y-coordinate tracking and spatial validation to assign tables only when spatially below county headings
**Result**: Eliminates cross-county data contamination, Kakamega empty (correct), Kajiado retains 104+ rows

# ISSUE 5: SEQUENTIAL PROCESSING ARCHITECTURE - COMPLETED ✅
**Problem**: Spatial validation approach was complex and relied on Y-coordinate geometry instead of document structure
**Solution**: Implemented document-order element processing with title-bounded sections using county headings and "Recommendations"
**Result**: Cleaner code, better performance, eliminates spatial validation complexity while preserving all functionality

# ISSUE 6: ISIOLO TABLE POSITION ESTIMATION - RESOLVED ✅
**Problem**: Isiolo 2019_20 Q1 extracting revenue table instead of programme table, missing 1.2B Kshs health spending on page 101
**Solution**: Enhanced programme detection with negative filtering and flexible header combinations (Programme/Program/Sector + Approved/Actual)
**Result**: Health data recovered (1.2B Kshs), correct programme table with 159 rows, cross-county compatible positioning

# ISSUE 7: 4-CATEGORY HEADER MATCHING BROKE ALL COUNTIES - RESOLVED ✅
**Problem**: Strict 4-category requirement (programme + sub-programme + budget + payment) broke 42 counties due to header variations like "Sub- Programme" vs "sub-programme"
**Solution**: Added header normalization to handle spacing around hyphens: "Sub- Programme" → "sub-programme", "Sub -Programme" → "sub-programme"
**Result**: All 47 counties now extract successfully with robust 4-category programme table detection

# ISSUE 8: ELGEYO MARAKWET SPACE-SEPARATED SUB PROGRAMME HEADER - RESOLVED ✅
**Problem**: Elgeyo Marakwet 2019_20 Q1 extraction failing with blank CSV despite having correct 4-category table structure on pages 67-69
**Root Cause**: Table headers used "Sub Programme" (with space) instead of "Sub-Programme" (with hyphen), causing sub-programme keyword matching to fail
**Solution**: Added "sub programme" to sub_programme_keywords list to handle both space and hyphen formats: `["sub-programme", "sub-program", "sub programme", "description"]`
**Result**: ✅ Elgeyo Marakwet now extracts 129 rows of programme data including health spending ✅ Compatible with other counties using space format

# ISSUE 9: KERICHO HYPHENATED LINE BREAK HEADERS - RESOLVED ✅
**Problem**: Kericho 2019_20 Q1 failing extraction due to "Actual Pay-\nments" header (hyphenated line break)
**Root Cause**: Header normalized to "actual pay-ments" but payment_keywords looked for "payments" without hyphen
**Solution**: Enhanced header normalization to handle hyphenated line breaks with regex: `header_no_hyphens = re.sub(r'(\w)-\s*(\w)', r'\1\2', header_normalized)`
**Result**: ✅ "Actual Pay-\nments" → "actual payments" matches keywords ✅ Handles all hyphenated word breaks systematically

# ISSUE 10: HEADERLESS TABLE CONTINUATION - RESOLVED ✅
**Problem**: Programme tables spanning multiple pages with headerless continuations (e.g., Kericho pages 117-120) were missed, only capturing ~27 rows from page 116
**Root Cause**: `is_program_table()` required headers with keywords, failing on continuation tables that only contained data rows
**Solution**: Added headerless continuation detection layer:
- `is_programme_table_continuation()` - detects content patterns (programme codes, budget amounts, service descriptions)
- `looks_like_revenue_table()` & `looks_like_department_table()` - negative filters to prevent false positives
- `validate_programme_content_patterns()` - content-based validation using regex patterns
- Zero disruption: `elif` branch only triggers when header-based detection fails
**Result**: ✅ Kericho 2019_20 Q1: 27 → 112 rows (4x improvement) ✅ Captures pages 116-120 programme data ✅ Maintains 100% backward compatibility

# ISSUE 11: FALSE COUNTY DETECTION POSITIVES - RESOLVED ✅
**Problem**: Baringo 2019_20 Q4 showing wrong data from page 128 instead of pages 36-38
**Root Cause**: Fuzzy county matching caused false positives - text fragments like `'opment'`, `'ing'`, `'Farming'` matched county names (`bomet`, `baringo`), creating spurious county sections that overwrote correct data
**Solution**: Implemented exact county name matching:
- Added `exact_match_county()` function for strict name matching
- Enhanced `fuzzy_match_county()` with `require_exact=True` parameter
- Applied exact matching to all county detection patterns while preserving flexible document structure matching
**Result**: ✅ False positives eliminated ✅ Baringo Q4 shows correct pages 36-38 data ✅ All legitimate county headings preserved ✅ Backwards compatible

# ISSUE 12: MISSING PAGE 38 PROGRAMME TABLE - RESOLVED ✅
**Problem**: Baringo 2021_22 Q1 missing final programme table on page 38 with Grand Total (9.9B Kshs)
**Root Cause**: Table Y-position estimation placed table after "Key Observations and Recommendations" section end marker due to faulty header matching finding wrong text line
**Solution**: Fixed table positioning with 3-tier approach:
- Added `str_squish()` function for proper header whitespace normalization
- Implemented pdfplumber bounding box coordinates (Method 1)
- Enhanced header matching with actual table content + Y > 700 filter (Method 2)
- Improved fallback heuristic to place tables near page top (Method 3)
**Result**: ✅ Page 38 table captured (34 additional rows) ✅ Grand Total included ✅ Correct processing order ✅ No regressions

# ISSUE 13: BARINGO 2019_20 Q2 PAGE 35 TABLE POSITIONING - RESOLVED ✅
**Problem**: Baringo 2019_20 Q2 missing final 8 lines from page 35 programme table (test expecting 174 lines, got 166)
**Root Cause**: pdfplumber bounding box detection placed table at Y=190.6 (wrong position) while actual table content was at Y=787-653 (correct position). Method 1 (bbox) overrode Method 2 (header matching) which had low confidence score due to single-line scoring
**Solution**: Enhanced table position detection with multi-line header scoring:
- Centralized header keywords to module-level constants to eliminate duplication
- Implemented multi-line scoring that accumulates scores across nearby header lines (within 20 pixels)
- Method 2 override when high confidence (score ≥ 4) and large position difference (> 200 pixels)
- Uses existing proven keyword lists from `is_programme_table()` function
**Result**: ✅ Page 35 table captured (8 additional rows) ✅ Grand Total with 8.7B Kshs included ✅ Test passes ✅ Maintains centralized keyword management

---

# SYSTEM STATUS ✅ PRODUCTION READY

**Current Status**: All major implementation phases completed (2025-09-02)  
**Latest Enhancement**: Exact county name matching - eliminates false positive county detections
**Key Feature**: Strict county name matching while preserving flexible document structure patterns
**Baringo 2019_20 Results**: Q4 extraction fixed - now shows correct pages 36-38 data (not page 128)
**Implementation**: `exact_match_county()` + `require_exact=True` parameter prevents fuzzy fragment matches
**Major Issues Resolved**: ✅ Cross-county data contamination ✅ Q2 2023_24 extraction ✅ Isiolo health data recovery (1.2B Kshs) ✅ Universal header compatibility ✅ Space-separated header formats ✅ Headerless continuation tables ✅ False county detection positives
**Ready for**: Complete multi-county health spending analysis with robust programme table detection and accurate county boundary detection

---