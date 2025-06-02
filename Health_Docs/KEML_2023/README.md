## KEML Data Processing

### Step 1: Table Extraction
- **Script**: `extract_keml_tables.py`
- **Purpose**: Extract tables from KEML source documents
- **Output**: Raw tables in `extracted_tables/` directory

### Step 2: Initial Data Cleaning
- **Script**: `keml_clean_tables.py`
- **Purpose**: Clean extracted table data
- **Outputs**: 
  - `outputs/cleaned_medicines.xlsx`
  - `outputs/cleaned_medicines.csv`

### Step 3: Footnote Removal
- **Script**: `keml_remove_footnotes.py`
- **Purpose**: Remove footnotes from cleaned data
- **Outputs**:
  - `no_footnotes_medicines.xlsx`
  - `no_footnotes_medicines.csv`

### Step 4: Manual Data Refinement
- **Process**: Manual cleaning in Excel
- **Output**: `outputs/keml_clean.csv`
- **Key Tasks**:
  - Shift rows where strength/size contains fractions (e.g., 1/2)
  - Correct medicine names by pulling from rows above
  - Verify dose-form accuracy
  - Handle NA values appropriately
