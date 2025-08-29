# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is a data extraction pipeline for processing Kenyan government spending data from Office of the Controller of Budget (OCOB) quarterly reports. The system extracts financial data from PDF reports, converts them to structured CSV files, and performs semantic analysis to identify health-related spending.

## Architecture

The codebase follows a two-stage pipeline:

### Stage 1: PDF to CSV Extraction (`program.py`)
- Extracts programme budget tables from quarterly PDF reports using pdfplumber
- Handles complex multi-page tables with merged cells and hierarchical structures
- Uses fuzzy matching to normalize county names against the official 47 counties list
- Processes TOC to identify expected programme tables per county
- Outputs structured CSV files: `program/{year}/{quarter}/county/{county}_programme_table.csv`

### Stage 2: Health Data Analysis (`baringo-scraping.py`)
- Main driver for health spending analysis using hybrid classification approach
- Combines keyword screening, semantic similarity (sentence-transformers), and contextual analysis
- Validates subtotals against calculated values to ensure data integrity
- Uses HealthBudgetAnalyzer class for comprehensive health spending extraction

## Key Data Flow

```
PDF Reports → program.py → CSV Files → baringo-scraping.py → Health Budget Excel Report
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

# HYBRID SUBTOTAL HANDLING IMPLEMENTATION PLAN

**Status**: Partially implemented (2025-01-29)  
**Current Progress**: Phase 3 completed, Position detection debugging needed  

## IMPLEMENTATION STATUS

### ✅ COMPLETED PHASES

#### Phase 1: New Helper Functions
- `_normalize_programme_subtotals()` - Converts Q4 blank sub-programmes to "Sub Total"
- `_detect_subtotal_position()` - Determines START/END position  
- `_get_programme_context()` - Gets programme context and detail rows
- Helper functions for detail row detection

#### Phase 2: Data Normalization  
- Updated `load_and_clean_data()` with normalization call
- Simplified `_is_subtotal_row()` to check for "Sub Total"  
- Added metadata columns: `is_normalized_subtotal`, `subtotal_position`

#### Phase 3: Programme Structure Update
- Replaced `parse_programme_structure()` with position-aware logic
- Groups now include `subtotal_position` metadata
- Health classification priority restored (13 health records maintained)

### 🔄 CURRENT ISSUE: Position Detection
**Problem**: All subtotals showing as "ISOLATED" position, groups only have 1 record each  
**Root Cause**: Detail row detection logic not working after normalization  
**Impact**: Validation works (4 perfect matches!) but grouping suboptimal  

### ⏳ PENDING PHASES

#### Phase 4: Fix Position Detection Logic
```python
# Issue in _has_programme_details_after/_before functions
# Need to handle both patterns:
# Pattern 1: "Programme", "Sub Total" → "", "detail"  
# Pattern 2: "Programme", "Sub Total" → "Programme", "detail"
```

#### Phase 5: Update Validation Logic  
```python
def validate_subtotals(self):
    # Add position-aware validation
    subtotal_position = group.get('subtotal_position', 'END')
    # Enhanced validation results with position context
```

#### Phase 6: Update Output Functions
- Add `subtotal_position`, `is_normalized_subtotal` to health data output
- Update programme totals with position-aware source tracking
- Enhanced methodology documentation

## PROGRESS METRICS

**Before Implementation**:
- 34 programme groups, 0 perfect validation matches
- 13 health records, manual subtotal handling

**Current Status** (After Phase 3):  
- ✅ Normalization: 33 programme subtotals converted to "Sub Total"
- ✅ Validation: 4 perfect matches (significant improvement!)
- ✅ Health records: 13 maintained (health classification priority preserved)
- ❌ Position detection: All showing "ISOLATED" (needs debugging)
- ❌ Groups: 1 record each (detail rows not properly associated)

**Target Status** (After completion):
- Proper START/END position detection for cross-quarter compatibility  
- Position-aware validation with enhanced reporting
- Unified subtotal handling for Q1-Q4 data

## DEBUGGING NEXT STEPS

1. **Examine normalized data structure** - Check what the data looks like after normalization
2. **Fix detail row detection** - Update `_has_programme_details_after/before` logic  
3. **Test position detection** - Verify START/END detection works correctly
4. **Update validation** - Implement position-aware validation logic
5. **Test cross-quarter** - Verify works on both Q1-Q3 (END) and Q4 (START) data

## KEY INSIGHTS FROM IMPLEMENTATION

✅ **Normalization approach works**: Converting blank sub-programmes to "Sub Total" creates consistent structure  
✅ **Validation improvements**: 4 perfect matches vs 0 before shows validation logic improvements  
✅ **Health classification preserved**: 13 health records maintained through changes  
❌ **Position detection complex**: Q4 data pattern requires more sophisticated detail row detection  

## METHODOLOGY UPDATES NEEDED

Once implementation complete, update methodology with:
- Data Structure Normalization section
- Hybrid Subtotal Processing explanation  
- Position-Aware Validation details
- Cross-quarter compatibility approach

---

# ISSUE 1: HEALTH CLASSIFICATION FIXES - COMPLETED ✅

**Status**: Implementation completed 2025-08-29  
**Problem**: HEALTH_DEPARTMENT_CONTEXT method causing false positives (e.g., Agricultural Development admin classified as health)

## Root Cause
- 20-row window search created spurious associations between unrelated programmes  
- Line break formatting (`"General administra-\ntion"`) broke keyword detection

## Solution Implemented
- **Removed** 6 contextual analysis functions causing false positives
- **Added** simple 3-row adjacency check for large health admin entries  
- **Applied** text normalization to handle formatting issues
- **Preserved** LARGE_HEALTH_ADMIN classification method for reporting

## Results
- ✅ Eliminates false positives (Agricultural Development, Tourism, etc.)
- ✅ Preserves legitimate large health admin detection  
- ✅ Handles line break formatting: `"administra-\ntion"` → `"administration"`
- ✅ Faster, more predictable classification logic

---

# ISSUE 2: SUBTOTAL VALIDATION - PENDING ⏳

**Status**: Not yet implemented  
**Problem**: After normalization converts 33 programme subtotals to "Sub Total", validation shows `no_explicit_subtotal` and `calculated_approved` is blank

## Root Cause
- All subtotals show as "ISOLATED" position because detail row detection logic fails after normalization
- Each programme group only contains 1 record (the subtotal) instead of subtotal + detail rows
- No detail rows → no calculated totals → blank validation results

## Impact
- Validation reports "no_explicit_subtotal" despite 33 normalized subtotals existing
- Cannot calculate programme totals for validation comparison  
- All programme totals default to "CALCULATED" source instead of "REPORTED_SUBTOTAL"
- 4 perfect matches achieved but groups artificially small (1 record each)

## Next Steps
1. Debug position detection with Q4 data structure
2. Update `_has_programme_details_after/before` logic for Q4 compatibility  
3. Fix detail row association with subtotals
4. Test position-aware validation logic

---