# OCOB Reports Data Extraction Project

This project extracts and processes data from Office of the Controller of Budget (OCOB) quarterly reports available at https://cob.go.ke/reports/.

## Project Overview

The goal is to systematically scrape financial data tables from OCOB PDF reports, organizing them by report type, financial year, quarter, and geographic scope (national vs county-level).

## Project Structure

```
# Source PDF documents
├── 2019_20/
│   ├── 01/
│   │   ├── county/
│   │   └── national/
│   └── ... (02, 03, 04)
└── ... (other financial years)
├── program/            # Extracted programme data
│   ├── 2019_20/
│   │   ├── 01/
│   │   │   └── county/
│   │   │       ├── baringo_programme_table.csv
│   │   │       ├── bomet_programme_table.csv
│   │   │       └── ... (all 47 counties)
│   │   └── ... (02, 03, 04)
│   └── ... (other financial years)
├── scraped_data/       # Other extracted data
├── program.py  # Main extraction script (working)
├── department_extract.py # Departmental data extraction (WIP)
├── county.R           # Failed R implementation
├── national.R         # Failed R implementation
├── scraping.py        # Failed Python implementation
└── open-africa.R      # Planned upload script
```

## Data Source

- **Source**: Office of the Controller of Budget (OCOB)
- **URL**: https://cob.go.ke/reports/
- **Report Type**: Quarterly financial reports
- **Coverage**: Both National and County-level data
- **Time Range**: Financial years 2011-12 to 2024-25

## File Organization Structure

The PDFs follow a systematic naming convention:
```
{financial_year}_{quarter}_{scope}.pdf
```

**Examples:**
- `2019_20_01_county.pdf` (County report for Q1 of FY 2019-20)
- `2019_20_01_national.pdf` (National report for Q1 of FY 2019-20)

**Directory Structure:**
```
├── 2019_20/
│   ├── 01/
│   │   ├── county/
│   │   └── national/
│   ├── 02/
│   ├── 03/
│   └── 04/
├── 2020_21/
│   └── ... (same structure)
└── ... (through 2024_25)
```

## Current Implementation Status

### ✅ **Production-Ready Scripts**

#### `program.py` (Sequential Processing Architecture)
- **Status**: Production-ready with sequential processing (2025-09-02)
- **Purpose**: Extracts programme budget tables from county reports using document-order processing
- **Architecture**: Title-bounded county sections with multi-page table continuity
- **Key Features**:
  - **Sequential Processing**: Processes PDF elements in document order for accurate county boundaries
  - **Cross-County Integrity**: Prevents table misattribution between counties
  - **Multi-page Support**: Handles programme tables spanning multiple pages within county sections
  - **Fuzzy County Matching**: Normalizes county names against official 47 counties list
  - **Cross-Quarter Compatibility**: Handles different PDF formats (Q2 2023_24 patterns, etc.)
- **Output**: Individual CSV files for each of the 47 counties
- **Output Structure**: `program/{financial_year}/{quarter}/county/{county_name}_programme_table.csv`
- **Example**: `program/2019_20/01/county/baringo_programme_table.csv`
- **Coverage**: FY 2019-20 onwards, all quarters (42/47 counties typically successful)

#### `baringo-scraping.py` (Health Budget Analysis)
- **Status**: Production-ready multi-year health spending analysis
- **Purpose**: Semantic classification and validation of health-related government spending
- **Architecture**: Hybrid classification with subtotal validation
- **Key Features**:
  - **Multi-year Processing**: Handles 2019_20 through 2024_25 (6 years)
  - **Semantic Classification**: Uses sentence-transformers for health spending identification
  - **Subtotal Validation**: Cross-references calculated vs reported totals for data integrity
  - **Health Context Analysis**: Contextual understanding (e.g., "LAN installation at hospital")
  - **Excel Reporting**: Multi-sheet output with health programmes, totals, and validation
- **Output**: Excel files with health spending analysis and validation reports
- **Test Results**: Successfully processed Baringo 2019_20 with 4-25 perfect subtotal matches per quarter

### 🚧 **In Progress**

#### `department_extract.py`
- **Status**: Work in progress
- **Purpose**: Extract departmental budget data (similar structure to program_extract.py)

#### `health_program_extract.py` *(Planned)*
- **Status**: Planned development
- **Purpose**: Specialized extraction for health programme data

### ❌ **Attempted/Failed Scripts**

The following scripts were attempted but did not work as expected:
- `county.R` - R-based county data extraction
- `national.R` - R-based national data extraction  
- `scraping.py` - Alternative Python scraping approach

## Data Categories

The project aims to extract the following table categories separately:
- **Programme budgets** (Currently implemented)
- **Departmental budgets** (In progress)
- **Health programmes** (Planned)
- Additional categories as identified

## Output Format

- **File Type**: CSV
- **Geographic Coverage**: All 47 Kenyan counties
- **Temporal Coverage**: Multiple financial years and quarters
- **Organization**: Hierarchical by year/quarter/scope/county

## Usage

### Extract Programme Data from PDFs
```bash
# Process specific year and quarter
python program.py --year 2022 --quarter 4

# Process all available PDFs (recommended)
python program.py --all

# Process a range of years/quarters
python program.py --year 2020 --quarter 2 --end-year 2021 --end-quarter 3
```

### Analyze Health Spending Data
```bash
# Process all available health data (primary command)
python baringo-scraping.py

# Process specific years/quarters/counties
python baringo-scraping.py --years 2019_20 2020_21 --quarters 01 02 --counties baringo nakuru

# Specify custom output file
python baringo-scraping.py --output "custom_health_analysis.xlsx"
```

### Test Sequential Processing
```bash
# Test the new sequential processing on 2019_20 Q1
python test_sequential_processing.py
```

## Architecture Highlights

### Sequential PDF Processing
- **Document Order Processing**: Elements processed as encountered in PDF (top to bottom)
- **Title-Bounded Sections**: County headings and "Recommendations" provide clear boundaries
- **No Spatial Validation**: Eliminates Y-coordinate geometry dependencies
- **Multi-page Continuity**: Preserves table spans within county sections using header matching

### Health Classification Pipeline
- **Keyword Screening**: Quick identification of obvious health/non-health terms
- **Semantic Similarity**: sentence-transformers model for contextual health classification
- **Subtotal Validation**: Cross-references calculated vs reported totals for data integrity
- **Excel Reporting**: Multi-sheet output with programmes, totals, and validation

## Future Plans

### Data Publishing
- **Platform**: Open Africa (open-africa.org)
- **Upload Script**: `open-africa.R` (planned)
- **Goal**: Make processed data publicly available for research and analysis


