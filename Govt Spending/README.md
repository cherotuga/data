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
├── program_extract.py  # Main extraction script (working)
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

### ✅ **Working Scripts**

#### `program_extract.py`
- **Status**: Fully functional
- **Purpose**: Extracts programme budget tables from county reports
- **Output**: Individual CSV files for each of the 47 counties
- **Output Structure**: `program/{financial_year}/{quarter}/county/{county_name}_programme_table.csv`
- **Example**: `program/2019_20/01/county/baringo_programme_table.csv`
- **Coverage**: FY 2019-20 onwards, all quarters

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

## Future Plans

### Data Publishing
- **Platform**: Open Africa (open-africa.org)
- **Upload Script**: `open-africa.R` (planned)
- **Goal**: Make processed data publicly available for research and analysis


