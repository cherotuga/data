"""
Health Budget Analysis Workflow with Subtotal Validation
Extracts health-related spending from government budget data using hybrid classification approach
and validates subtotals for data integrity
"""

# Suppress common warnings
import os
os.environ['TOKENIZERS_PARALLELISM'] = 'false'  # Disable tokenizers parallelism warnings
os.environ['MKL_NUM_THREADS'] = '1'             # Avoid numpy/MKL threading issues

import pandas as pd
import numpy as np
import re
from pathlib import Path
from sentence_transformers import SentenceTransformer
from sklearn.metrics.pairwise import cosine_similarity
import warnings
warnings.filterwarnings('ignore')

class HealthBudgetAnalyzer:
    def __init__(self, csv_path=None, model=None, health_embeddings=None):
        """Initialize the analyzer with the CSV file path and optional shared model"""
        self.csv_path = csv_path
        self.data = None
        self.health_data = None
        self.classification_log = []
        self.subtotal_validations = []
        self.programme_structure = []
        
        # Use provided model or load new one
        if model is not None:
            self.model = model
        else:
            print("Loading semantic similarity model...")
            self.model = SentenceTransformer('all-MiniLM-L6-v2')
        
        # Health reference texts for semantic similarity
        self.health_references = [
            "healthcare services medical treatment",
            "hospital administration clinical services", 
            "preventive health public health programs",
            "curative medical care rehabilitation",
            "health infrastructure medical facilities",
            "primary healthcare community health",
            "health administration medical staff"
        ]
        
        # Use provided embeddings or create new ones
        if health_embeddings is not None:
            self.health_embeddings = health_embeddings
        else:
            print("Creating health reference embeddings...")
            self.health_embeddings = self.model.encode(self.health_references)
        
        # Keywords for quick screening
        self.obvious_health_keywords = [
            'health', 'medical', 'hospital', 'curative', 'rehabilitative', 
            'preventive', 'promotive', 'clinical', 'clinic'
        ]
        
        self.obvious_non_health_keywords = [
            'agriculture', 'livestock', 'roads', 'trade', 'industrial', 
            'land', 'urban development', 'crop', 'fisheries'
        ]

        # Subtotal keywords
        self.subtotal_keywords = [
            'sub total', 'sub-total', 'subtotal', 'total', 'grand total'
        ]
        
        # Validation tolerance
        self.validation_tolerance_pct = 0.01  # 0.01%
        self.validation_tolerance_abs = 1.0   # 1 unit

        # Debugging
        self.debug_semantic = False
        self.debug_boundary = False
        self.debug_programme_section = False
        self.debug_contextual = False
        self.debug_is_large_administration_entry = False
        self.debug_detect_health_section_boundaries = False
        self.debug_rec_dev_setup = False

    def load_and_clean_data(self):
        """Load CSV and perform initial cleaning"""
        print("Loading and cleaning data...")
        
        # Check if file is empty or invalid
        try:
            # Check file size first
            import os
            if os.path.getsize(self.csv_path) == 0:
                print(f"Warning: Empty file {self.csv_path}, skipping...")
                self.data = pd.DataFrame()
                return None
                
            # Load the CSV
            self.data = pd.read_csv(self.csv_path)
            
            # Check if DataFrame is empty after loading
            if self.data.empty or len(self.data.columns) == 0:
                print(f"Warning: No data found in {self.csv_path}, skipping...")
                self.data = pd.DataFrame()
                return None
                
        except (pd.errors.EmptyDataError, FileNotFoundError) as e:
            print(f"Warning: Could not read {self.csv_path}: {e}, skipping...")
            self.data = pd.DataFrame()
            return None
        
        print(f"Original columns: {list(self.data.columns)}")
        
        # Clean column names
        self.data.columns = self.data.columns.str.strip().str.replace('\n', ' ')
        
        print(f"Cleaned columns: {list(self.data.columns)}")

        # Detect if this is the new Rec/Dev format or old simple format
        is_rec_dev_format = self._detect_rec_dev_format()

        if is_rec_dev_format:
            print("Detected Rec/Dev format - processing recurrent and development columns")
            self._setup_rec_dev_columns()
        else:
            print("Detected simple format - using single budget columns")
            self._setup_simple_columns()

        print(f"Detected columns:")
        print(f"  Programme: {self.programme_col}")
        print(f"  Sub-programme: {self.subprogramme_col}")
        print(f"  Approved Budget: {self.approved_budget_col if hasattr(self, 'approved_budget_col') else 'COMBINED'}")
        print(f"  Actual Payments: {self.actual_payments_col if hasattr(self, 'actual_payments_col') else 'COMBINED'}")

        # Handle missing Programme names by forward filling
        self.data[self.programme_col] = self.data[self.programme_col].replace('', np.nan)
        self.data[self.programme_col] = self.data[self.programme_col].fillna(method='ffill')

        # Create clean versions of budget columns (now handled in setup methods)
        # Budget columns and data flags are created in _setup_rec_dev_columns() or _setup_simple_columns()
        
        # NEW: Normalize programme subtotals for cross-quarter compatibility
        self._normalize_programme_subtotals()
        
        # NEW: Detect subtotal positions
        self.data['subtotal_position'] = self.data.index.to_series().apply(self._detect_subtotal_position)
        
        print(f"Loaded {len(self.data)} records")
        return self.data
    
    def _find_column_name(self, possible_names):
        """Find the correct column name from a list of possibilities using exact word matching first, then substring fallback"""

        # First pass: Try exact word matching after normalization
        for possible_name in possible_names:
            normalized_possible = self._normalize_programme_text(possible_name).replace('-', '').replace(' ', '')
            for col in self.data.columns:
                normalized_col = self._normalize_programme_text(col).replace('-', '').replace(' ', '')
                if normalized_possible == normalized_col:  # Exact match
                    return col

        # Second pass: Fallback to substring matching
        for possible_name in possible_names:
            normalized_possible = self._normalize_programme_text(possible_name).replace('-', '').replace(' ', '')
            for col in self.data.columns:
                normalized_col = self._normalize_programme_text(col).replace('-', '').replace(' ', '')
                if normalized_possible in normalized_col:  # Substring match
                    return col

        # If no match found, show available columns for debugging
        raise ValueError(f"Could not find column matching any of {possible_names}. Available columns: {list(self.data.columns)}")

    def _find_all_matching_columns(self, possible_names):
        """Find ALL column names that match any of the possible names"""
        matching_columns = []

        # First pass: Try exact word matching after normalization
        for possible_name in possible_names:
            normalized_possible = self._normalize_programme_text(possible_name).replace('-', '').replace(' ', '')
            for col in self.data.columns:
                normalized_col = self._normalize_programme_text(col).replace('-', '').replace(' ', '')
                if normalized_possible == normalized_col:  # Exact match
                    if col not in matching_columns:
                        matching_columns.append(col)

        # Second pass: Fallback to substring matching for any not found in exact match
        if not matching_columns:
            for possible_name in possible_names:
                normalized_possible = self._normalize_programme_text(possible_name).replace('-', '').replace(' ', '')
                for col in self.data.columns:
                    normalized_col = self._normalize_programme_text(col).replace('-', '').replace(' ', '')
                    if normalized_possible in normalized_col:  # Substring match
                        if col not in matching_columns:
                            matching_columns.append(col)

        return matching_columns

    def _is_column_mostly_numeric(self, column_name, sample_size=50):
        """
        Check if a column contains mostly numeric values vs text values.
        Returns True if column is mostly numeric, False if mostly text.

        This checks the first `sample_size` non-empty values to determine content type.
        Numeric patterns include: pure numbers, numbers with commas, dashes for missing values.
        """
        if column_name not in self.data.columns:
            return False

        # Get non-empty values from the column (sample for efficiency)
        sample_data = self.data[column_name].dropna().head(sample_size)
        if len(sample_data) == 0:
            return False  # Empty column treated as text

        numeric_count = 0
        text_count = 0

        for value in sample_data:
            value_str = str(value).strip()

            # Skip empty or dash values (missing data indicators)
            if not value_str or value_str in ['-', 'nan', 'None']:
                continue

            # Check if it looks like a number (with or without commas)
            # Pattern: optional quotes + digits + optional commas + optional decimals
            import re
            number_pattern = r'^["\']?-?[\d,]+\.?\d*["\']?$'
            if re.match(number_pattern, value_str):
                numeric_count += 1
            else:
                text_count += 1

        # Return True if more numeric than text content
        total_meaningful_values = numeric_count + text_count
        if total_meaningful_values == 0:
            return False  # If no meaningful values, treat as text

        numeric_ratio = numeric_count / total_meaningful_values
        return numeric_ratio > 0.6  # Consider mostly numeric if >60% are numbers

    def _select_text_column(self, candidate_columns):
        """
        Given multiple candidate columns, select the one that contains more text content.
        This is used to prefer descriptive text columns over numeric code columns.
        """
        if len(candidate_columns) <= 1:
            return candidate_columns[0] if candidate_columns else None

        # Score each column: lower score = more text-like (preferred)
        column_scores = []
        for col in candidate_columns:
            is_mostly_numeric = self._is_column_mostly_numeric(col)
            # Score: 1 for mostly numeric, 0 for mostly text
            score = 1 if is_mostly_numeric else 0
            column_scores.append((col, score))

        # Sort by score (ascending) - prefer columns with more text (score=0)
        column_scores.sort(key=lambda x: x[1])

        # Debug info
        print(f"DEBUG: Multiple sub-programme columns found: {[f'{col}(score={score})' for col, score in column_scores]}")
        selected_col = column_scores[0][0]
        print(f"DEBUG: Selected text-based column: {selected_col}")

        return selected_col

    def _find_subprogramme_column_name(self, possible_names):
        """
        Specialized method for finding sub-programme column that prefers text content over numeric.
        When multiple columns match sub-programme patterns, this selects the one with more text content.
        """
        # Find all matching columns
        matching_columns = self._find_all_matching_columns(possible_names)

        if not matching_columns:
            raise ValueError(f"Could not find column matching any of {possible_names}. Available columns: {list(self.data.columns)}")

        # If only one match, return it
        if len(matching_columns) == 1:
            return matching_columns[0]

        # Multiple matches - select the text-based one
        return self._select_text_column(matching_columns)

    def _clean_currency(self, value):
        """Clean currency values, preserving NaN for missing data"""
        if pd.isna(value) or value == '-' or value == '':
            return np.nan

        # Remove quotes, commas, and convert to float
        try:
            cleaned = str(value).replace('"', '').replace(',', '').strip()
            return float(cleaned) if cleaned else np.nan
        except:
            return np.nan

    def _convert_units_if_needed(self):
        """
        Automatically detect and convert units from millions to thousands for consistency.
        2024_25 format uses millions while earlier years use thousands.
        """
        # Check if any column headers indicate millions
        all_columns = ' '.join(self.data.columns).lower()
        has_millions = 'million' in all_columns

        if has_millions:
            print("DEBUG: Detected 'Million' units in column headers - converting to actual Kshs for consistency")

            # Convert the clean budget columns from millions to actual Kshs
            # Multiply by 1,000,000 (millions -> actual Kshs)
            multiplier = 1_000_000

            self.data['Approved_Budget_Clean'] = self.data['Approved_Budget_Clean'] * multiplier
            self.data['Actual_Payments_Clean'] = self.data['Actual_Payments_Clean'] * multiplier

            print(f"DEBUG: Applied {multiplier:,}x multiplier to budget values for unit consistency")
        else:
            print("DEBUG: Standard 'Kshs' units detected - no conversion needed")

    def _smart_sum(self, series1, series2):
        """
        Sum two series with smart None handling:
        - If both are None/NaN, result is None
        - If only one is None/NaN, treat as 0 and sum with the other
        - Otherwise, sum normally
        """
        def sum_values(val1, val2):
            # Both are NaN/None - keep as NaN
            if pd.isna(val1) and pd.isna(val2):
                return np.nan
            # One is NaN/None - treat as 0
            elif pd.isna(val1):
                return val2
            elif pd.isna(val2):
                return val1
            # Both have values - sum normally
            else:
                return val1 + val2

        return pd.Series([sum_values(v1, v2) for v1, v2 in zip(series1, series2)], index=series1.index)

    def _detect_rec_dev_format(self):
        """Detect if this CSV uses the new Rec/Dev column structure"""
        print("DEBUG: Starting Rec/Dev format detection...")
        print(f"DEBUG: CSV has {len(self.data.columns)} columns: {list(self.data.columns)}")

        # Check if we have enough columns (at least 6: Programme, Sub-Programme, Rec, Dev, Rec, Dev)
        if len(self.data.columns) < 6:
            print("DEBUG: Not enough columns for Rec/Dev format (< 6), returning False")
            return False

        # Method 1: Look for the "Rec" and "Dev" pattern in header rows (Q1 style)
        print("DEBUG: Method 1 - Looking for 'Rec'/'Dev' patterns in first 5 rows...")
        for idx in range(min(5, len(self.data))):  # Check first 5 rows
            row_values = self.data.iloc[idx].astype(str).str.lower().str.strip()
            rec_count = (row_values == 'rec').sum()
            dev_count = (row_values == 'dev').sum()

            print(f"DEBUG: Row {idx} values: {list(row_values)}")
            print(f"DEBUG: Row {idx}: 'rec' count = {rec_count}, 'dev' count = {dev_count}")

            # Should have at least 2 pairs of Rec/Dev (Approved Estimates, Actual Expenditure)
            if rec_count >= 2 and dev_count >= 2:
                print(f"Found Rec/Dev pattern in row {idx}: {rec_count} 'Rec', {dev_count} 'Dev'")
                return True

        # Method 2: Look for "recurrent" and "development" keywords in column headers (Q2 style)
        print("DEBUG: Method 2 - Checking column headers for recurrent/development...")
        column_text = ' '.join(self.data.columns).lower()
        print(f"DEBUG: Column text: '{column_text}'")

        # Check for recurrent/development indicators in headers
        has_recurrent = 'recurrent' in column_text or 'expenditure' in column_text
        has_development = 'development' in column_text
        print(f"DEBUG: has_recurrent = {has_recurrent}, has_development = {has_development}")

        # Method 3: Check first few data rows for recurrent/development indicators
        print("DEBUG: Method 3 - Checking data rows for recurrent/development patterns...")
        for idx in range(min(5, len(self.data))):
            row_text = ' '.join(self.data.iloc[idx].astype(str)).lower()
            # Normalize newlines to spaces for pattern matching
            row_text_normalized = row_text.replace('\n', ' ')
            print(f"DEBUG: Row {idx} text: '{row_text_normalized}'")

            if 'recurrent expenditure' in row_text_normalized and 'development expenditure' in row_text_normalized:
                print(f"Found Recurrent/Development expenditure pattern in row {idx}")
                return True

        # Method 4: Improved detection - check for both 'recurrent' and 'development' anywhere in first 5 rows
        print("DEBUG: Method 4 - Improved detection for 2024_25 format...")
        for idx in range(min(5, len(self.data))):
            row_text = ' '.join(self.data.iloc[idx].astype(str)).lower()
            row_text_normalized = row_text.replace('\n', ' ')
            has_rec_in_row = 'recurrent' in row_text_normalized
            has_dev_in_row = 'development' in row_text_normalized or 'develop-' in row_text_normalized
            print(f"DEBUG: Row {idx} - has_recurrent: {has_rec_in_row}, has_development: {has_dev_in_row}")

            if has_rec_in_row and has_dev_in_row:
                print(f"DEBUG: Found both 'recurrent' and 'development' patterns in row {idx} - detecting as Rec/Dev format")
                return True

        if has_recurrent and has_development:
            print(f"Found Recurrent/Development keywords in column headers")
            return True

        print("DEBUG: No Rec/Dev format patterns detected, defaulting to simple format")
        return False

    def _setup_rec_dev_columns(self):
        """Setup column mappings for Rec/Dev format - sum recurrent and development amounts"""
        if self.debug_rec_dev_setup:
            print("DEBUG _setup_rec_dev_columns: Starting Rec/Dev column setup...")

        # Find basic columns first
        self.programme_col = self._find_column_name(['programme', 'programmes', 'programs', 'program'])

        # Try to find Sub-Programme column - make it optional for Programme-only formats
        try:
            self.subprogramme_col = self._find_subprogramme_column_name(['sub-programs', 'sub programs', 'sub program', 'sub- programme', 'sub- programmes', 'sub programme', 'sub programmes', 'subprogramme', 'subprogrammes', 'description', 'descriptions'])
        except ValueError:
            # No Sub-Programme column found - use Programme column for both (valid for 2023_24 Q1 format)
            print("  No Sub-Programme column found - using Programme-only format")
            self.subprogramme_col = self.programme_col

        # For Rec/Dev format, identify column positions
        # Expected: Programme, Sub-Programme, Approved_Rec, Approved_Dev, Actual_Rec, Actual_Dev, ...

        # Approved Estimates columns (positions 2-3)
        self.approved_rec_col = self.data.columns[2] if len(self.data.columns) > 2 else None
        self.approved_dev_col = self.data.columns[3] if len(self.data.columns) > 3 else None

        # Actual expenditure columns (positions 4-5)
        self.actual_rec_col = self.data.columns[4] if len(self.data.columns) > 4 else None
        self.actual_dev_col = self.data.columns[5] if len(self.data.columns) > 5 else None

        print(f"Rec/Dev columns detected:")
        print(f"  Approved Rec: {self.approved_rec_col}")
        print(f"  Approved Dev: {self.approved_dev_col}")
        print(f"  Actual Rec: {self.actual_rec_col}")
        print(f"  Actual Dev: {self.actual_dev_col}")

        # Create combined budget columns by summing Rec + Dev with smart None handling
        approved_rec = self.data[self.approved_rec_col].apply(self._clean_currency)
        approved_dev = self.data[self.approved_dev_col].apply(self._clean_currency)
        self.data['Approved_Budget_Clean'] = self._smart_sum(approved_rec, approved_dev)

        actual_rec = self.data[self.actual_rec_col].apply(self._clean_currency)
        actual_dev = self.data[self.actual_dev_col].apply(self._clean_currency)
        self.data['Actual_Payments_Clean'] = self._smart_sum(actual_rec, actual_dev)

        if self.debug_rec_dev_setup:
            print("DEBUG _setup_rec_dev_columns: Sample currency cleaning results...")
            # Show first few health records
            health_indices = []
            for idx, row in self.data.iterrows():
                programme = str(row[self.programme_col]) if pd.notna(row[self.programme_col]) else ''
                if 'health' in programme.lower() and len(health_indices) < 5:
                    health_indices.append(idx)

            for idx in health_indices:
                row = self.data.iloc[idx]
                prog = row[self.programme_col]
                sub = row[self.subprogramme_col]

                # Raw values
                app_rec_raw = row[self.approved_rec_col]
                app_dev_raw = row[self.approved_dev_col]
                act_rec_raw = row[self.actual_rec_col]
                act_dev_raw = row[self.actual_dev_col]

                # Cleaned values
                app_rec_clean = approved_rec.iloc[idx]
                app_dev_clean = approved_dev.iloc[idx]
                act_rec_clean = actual_rec.iloc[idx]
                act_dev_clean = actual_dev.iloc[idx]

                # Final totals
                app_total = row['Approved_Budget_Clean']
                act_total = row['Actual_Payments_Clean']

                print(f"  Row {idx}: {prog} / {sub}")
                print(f"    Raw: App_Rec={repr(app_rec_raw)}, App_Dev={repr(app_dev_raw)}")
                print(f"    Raw: Act_Rec={repr(act_rec_raw)}, Act_Dev={repr(act_dev_raw)}")
                print(f"    Clean: App_Rec={app_rec_clean}, App_Dev={app_dev_clean}")
                print(f"    Clean: Act_Rec={act_rec_clean}, Act_Dev={act_dev_clean}")
                print(f"    Totals: Approved={app_total}, Actual={act_total}")

        # Set flags for data availability - has data if either Rec or Dev has non-dash values
        self.data['Has_Approved_Budget'] = (
            (self.data[self.approved_rec_col] != '-') | (self.data[self.approved_dev_col] != '-')
        )
        self.data['Has_Actual_Payments'] = (
            (self.data[self.actual_rec_col] != '-') | (self.data[self.actual_dev_col] != '-')
        )

        # For backward compatibility, create combined original columns
        self.approved_budget_col = 'Approved_Budget_Combined'
        self.actual_payments_col = 'Actual_Payments_Combined'
        self.data[self.approved_budget_col] = (
            self.data[self.approved_rec_col].astype(str) + " + " + self.data[self.approved_dev_col].astype(str)
        )
        self.data[self.actual_payments_col] = (
            self.data[self.actual_rec_col].astype(str) + " + " + self.data[self.actual_dev_col].astype(str)
        )

        # Apply automatic units conversion (millions -> actual Kshs for consistency)
        self._convert_units_if_needed()

        if self.debug_rec_dev_setup:
            print("DEBUG _setup_rec_dev_columns: Rec/Dev setup complete!")

    def _setup_simple_columns(self):
        """Setup column mappings for simple format (backward compatibility)"""
        # Find the correct column names using partial matching
        self.programme_col = self._find_column_name(['programme', 'programmes', 'programs', 'program'])

        # Try to find Sub-Programme column - make it optional for Programme-only formats
        try:
            self.subprogramme_col = self._find_subprogramme_column_name(['sub-programs', 'sub programs', 'sub program', 'sub- programme', 'sub- programmes', 'sub programme', 'sub programmes', 'subprogramme', 'subprogrammes', 'description', 'descriptions'])
        except ValueError:
            # No Sub-Programme column found - use Programme column for both (valid for 2023_24 Q1 format)
            print("  No Sub-Programme column found - using Programme-only format")
            self.subprogramme_col = self.programme_col

        self.approved_budget_col = self._find_column_name(['approved budget', 'approved', 'submitted estimates', 'submitted', 'budget'])
        self.actual_payments_col = self._find_column_name(['actual payments', 'actual', 'expenditure'])

        # Create clean versions of budget columns
        self.data['Approved_Budget_Clean'] = self.data[self.approved_budget_col].apply(self._clean_currency)
        self.data['Actual_Payments_Clean'] = self.data[self.actual_payments_col].apply(self._clean_currency)

        # Apply automatic units conversion (millions -> thousands for consistency)
        self._convert_units_if_needed()

        # Add flags for missing data
        self.data['Has_Approved_Budget'] = self.data[self.approved_budget_col] != '-'
        self.data['Has_Actual_Payments'] = self.data[self.actual_payments_col] != '-'

    def _is_subtotal_row(self, subprogramme_text):
        """Check if row is a subtotal (case-insensitive)"""
        return str(subprogramme_text).strip().lower() == "sub total"
    
    def _is_programme_level_subtotal(self, idx):
        """Check if a row is a programme-level subtotal (programme name + empty sub-programme)"""
        if idx >= len(self.data):
            return False
            
        row = self.data.loc[idx]
        programme = row[self.programme_col]
        subprogramme = row[self.subprogramme_col]
        
        # Must have programme name but empty/missing sub-programme
        has_programme = pd.notna(programme) and str(programme).strip()
        empty_subprogramme = pd.isna(subprogramme) or str(subprogramme).strip() == ""
        
        return has_programme and empty_subprogramme
    
    def _normalize_programme_subtotals(self):
        """Convert Q4-style blank sub-programmes to explicit 'Sub Total' markers"""
        # Identify programme + blank sub-programme pattern
        programme_subtotal_mask = (
            (pd.notna(self.data[self.programme_col]) & (self.data[self.programme_col].astype(str).str.strip() != '')) &
            (pd.isna(self.data[self.subprogramme_col]) | (self.data[self.subprogramme_col].astype(str).str.strip().isin(['', 'nan'])))
        )
        
        # Replace blank with "Sub Total" and add metadata
        self.data.loc[programme_subtotal_mask, self.subprogramme_col] = "Sub Total"
        self.data['is_normalized_subtotal'] = programme_subtotal_mask
        
        normalized_count = programme_subtotal_mask.sum()
        print(f"Normalized {normalized_count} programme-level subtotals")
        return normalized_count
    
    def _has_programme_details_after(self, idx, programme):
        """Check if there are detail rows for the programme after this index"""
        search_end = min(idx + 20, len(self.data))  # Look ahead max 20 rows
        found_detail_rows = False
        
        for check_idx in range(idx + 1, search_end):
            check_row = self.data.iloc[check_idx]
            check_programme = check_row[self.programme_col]
            check_subprogramme = check_row[self.subprogramme_col]
            
            # Same programme name (case insensitive comparison)
            if (pd.notna(check_programme) and 
                str(check_programme).strip().lower() == str(programme).strip().lower()):
                # Found detail row: same programme but not a subtotal
                if (pd.notna(check_subprogramme) and 
                    str(check_subprogramme).strip().lower() != 'sub total'):
                    found_detail_rows = True
                    # Continue looking to see if we have multiple detail rows
                    
            # Hit a different programme, stop searching
            elif (pd.notna(check_programme) and str(check_programme).strip() != '' and
                  str(check_programme).strip().lower() != str(programme).strip().lower()):
                break
                
        return found_detail_rows
    
    def _has_programme_details_before(self, idx, programme):
        """Check if there are detail rows for the programme before this index"""
        search_start = max(0, idx - 20)  # Look back max 20 rows
        found_detail_rows = False
        
        for check_idx in range(idx - 1, search_start - 1, -1):
            check_row = self.data.iloc[check_idx]
            check_programme = check_row[self.programme_col]
            check_subprogramme = check_row[self.subprogramme_col]
            
            # Same programme name (case insensitive comparison)
            if (pd.notna(check_programme) and 
                str(check_programme).strip().lower() == str(programme).strip().lower()):
                # Found detail row: same programme but not a subtotal
                if (pd.notna(check_subprogramme) and 
                    str(check_subprogramme).strip().lower() != 'sub total'):
                    found_detail_rows = True
                    # Continue looking to see if we have multiple detail rows
                    
            # Hit a different programme, stop searching
            elif (pd.notna(check_programme) and str(check_programme).strip() != '' and
                  str(check_programme).strip().lower() != str(programme).strip().lower()):
                break
                
        return found_detail_rows
    
    def _detect_subtotal_position(self, idx):
        """Determine if subtotal is at programme START or END"""
        if idx >= len(self.data):
            return "NOT_SUBTOTAL"
            
        row = self.data.iloc[idx]
        subprogramme = str(row[self.subprogramme_col]).strip()
        
        if subprogramme.lower() != "sub total":  # Case insensitive
            return "NOT_SUBTOTAL"
        
        programme = str(row[self.programme_col]).strip() if pd.notna(row[self.programme_col]) else ""
        
        # Look ahead for detail rows (START pattern)
        has_following_details = self._has_programme_details_after(idx, programme)
        
        # Look behind for detail rows (END pattern)  
        has_preceding_details = self._has_programme_details_before(idx, programme)
        
        if has_following_details and not has_preceding_details:
            return "START"
        elif has_preceding_details and not has_following_details:
            return "END"
        elif has_following_details and has_preceding_details:
            return "MIDDLE"  # Edge case
        else:
            return "ISOLATED"
    
    def _get_effective_programme_name(self, idx):
        """Get the effective programme name for a row, looking backwards if needed"""
        if idx >= len(self.data):
            return "Unknown"
            
        row = self.data.iloc[idx]
        programme = str(row[self.programme_col]).strip() if pd.notna(row[self.programme_col]) else ""
        
        if programme:
            return programme
            
        # Look backwards for programme name
        search_start = max(0, idx - 10)
        for check_idx in range(idx - 1, search_start - 1, -1):
            check_programme = self.data.iloc[check_idx][self.programme_col]
            if pd.notna(check_programme) and str(check_programme).strip():
                return str(check_programme).strip()
                
        return f"Unknown_Programme_{idx}"
    
    def _get_following_detail_rows(self, idx, programme):
        """Get indices of detail rows that follow this subtotal"""
        detail_indices = []
        search_end = min(idx + 20, len(self.data))
        
        for check_idx in range(idx + 1, search_end):
            check_row = self.data.iloc[check_idx]
            check_programme = check_row[self.programme_col]
            check_subprogramme = check_row[self.subprogramme_col]
            
            # Stop if we hit a different programme (case insensitive)
            if (pd.notna(check_programme) and str(check_programme).strip() != '' and
                str(check_programme).strip().lower() != str(programme).strip().lower()):
                break
                
            # Stop if we hit another subtotal (case insensitive)
            if str(check_subprogramme).strip().lower() == 'sub total':
                break
                
            # Collect detail rows - same programme name, not a subtotal
            if (pd.notna(check_programme) and 
                str(check_programme).strip().lower() == str(programme).strip().lower() and
                pd.notna(check_subprogramme) and 
                str(check_subprogramme).strip().lower() != 'sub total'):
                detail_indices.append(check_idx)
                
        return detail_indices
    
    def _get_preceding_detail_rows(self, idx, programme):
        """Get indices of detail rows that precede this subtotal"""
        detail_indices = []
        search_start = max(0, idx - 20)
        
        for check_idx in range(idx - 1, search_start - 1, -1):
            check_row = self.data.iloc[check_idx]
            check_programme = check_row[self.programme_col]
            check_subprogramme = check_row[self.subprogramme_col]
            
            # Stop if we hit a different programme (case insensitive)
            if (pd.notna(check_programme) and str(check_programme).strip() != '' and
                str(check_programme).strip().lower() != str(programme).strip().lower()):
                break
                
            # Stop if we hit another subtotal (case insensitive)
            if str(check_subprogramme).strip().lower() == 'sub total':
                break
                
            # Collect detail rows - same programme name, not a subtotal
            if (pd.notna(check_programme) and 
                str(check_programme).strip().lower() == str(programme).strip().lower() and
                pd.notna(check_subprogramme) and 
                str(check_subprogramme).strip().lower() != 'sub total'):
                detail_indices.append(check_idx)
                
        return list(reversed(detail_indices))  # Return in chronological order
    
    def _get_programme_context(self, idx):
        """Get programme context and associated detail rows"""
        subtotal_position = self._detect_subtotal_position(idx)
        programme = self._get_effective_programme_name(idx)
        
        if subtotal_position == "START":
            detail_indices = self._get_following_detail_rows(idx, programme)
        elif subtotal_position == "END":
            detail_indices = self._get_preceding_detail_rows(idx, programme)
        else:
            detail_indices = []
        
        return {
            'programme': programme,
            'detail_indices': detail_indices,
            'subtotal_position': subtotal_position
        }
    
    def parse_programme_structure(self):
        """Parse programme structure handling both START and END subtotal patterns"""
        print("Parsing programme structure...")
        
        # Simplified subtotal detection - now just checks for "Sub Total"
        self.data['is_subtotal_row'] = self.data[self.subprogramme_col].apply(self._is_subtotal_row)
        self.data['programme_group_id'] = -1
        
        programme_groups = []
        group_id = 0
        
        # Group by programme boundaries defined by subtotal positions
        i = 0
        while i < len(self.data):
            if self.data.iloc[i]['is_subtotal_row']:
                # Found a subtotal - get its context
                context = self._get_programme_context(i)
                group_indices = [i] + context['detail_indices']
                
                if group_indices:  # Only create group if we have indices
                    programme_groups.append({
                        'group_id': group_id,
                        'programme': context['programme'],
                        'start_idx': min(group_indices),
                        'end_idx': max(group_indices),
                        'record_indices': sorted(group_indices),
                        'has_explicit_subtotal': True,
                        'subtotal_position': context['subtotal_position']
                    })
                    
                    # Mark group membership
                    for idx in group_indices:
                        if idx < len(self.data):
                            self.data.at[idx, 'programme_group_id'] = group_id
                    
                    group_id += 1
                    
                    # Skip to after processed rows
                    i = max(group_indices) + 1
                else:
                    i += 1
            else:
                i += 1
        
        # Handle orphaned non-subtotal rows (shouldn't happen with good data)
        unassigned_mask = self.data['programme_group_id'] == -1
        if unassigned_mask.any():
            unassigned_indices = self.data[unassigned_mask].index.tolist()
            if unassigned_indices:
                programme_groups.append({
                    'group_id': group_id,
                    'programme': f"Unassigned_Records",
                    'start_idx': min(unassigned_indices),
                    'end_idx': max(unassigned_indices),
                    'record_indices': unassigned_indices,
                    'has_explicit_subtotal': False,
                    'subtotal_position': 'NONE'
                })
                
                for idx in unassigned_indices:
                    self.data.at[idx, 'programme_group_id'] = group_id
        
        self.programme_structure = programme_groups
        print(f"Identified {len(programme_groups)} programme groups")
        
        # Debug: Print structure summary
        for group in programme_groups[:5]:  # Show first 5 groups
            subtotal_pos = group.get('subtotal_position', 'UNKNOWN')
            print(f"  Group {group['group_id']}: {group['programme']} ({len(group['record_indices'])} records, subtotal: {group['has_explicit_subtotal']}, position: {subtotal_pos})")
        
        return programme_groups
    
    
    def _detect_health_section_boundaries(self):
        """Enhanced health section detection with multiple methods and fallback"""
        if self.debug_detect_health_section_boundaries:
            print("DEBUG: Starting health section boundary detection...")
    
        health_section_start = None
        health_section_end = None
    
        # Method 1: Try explicit health programme detection
        for idx, row in self.data.iterrows():
            programme = row[self.programme_col]
            
            if self.debug_detect_health_section_boundaries:
                print(f"DEBUG: Row {idx}, Programme: '{programme}'")
        
            if self._is_explicit_health_programme(programme):
                if self.debug_detect_health_section_boundaries:
                    print(f"DEBUG: Found explicit health programme at index {idx}")

                # Look backwards for potential health admin entries (up to 3 rows)
                for back_idx in range(max(0, idx-3), idx):
                    back_programme = self._normalize_programme_text(self.data.loc[back_idx, self.programme_col])
                    if 'general administration' in back_programme:
                        health_section_start = back_idx
                        if self.debug_detect_health_section_boundaries:
                            print(f"DEBUG: Found health admin start at index {back_idx}")
                        break
            
                if health_section_start is None:
                    health_section_start = idx
                    if self.debug_detect_health_section_boundaries:
                        print(f"DEBUG: Health section starts at health programme index {idx}")
                break

        # Method 2: If no explicit health programme found, try other detection methods
        if health_section_start is None:
            if self.debug_detect_health_section_boundaries:
                print("DEBUG: No health section detected, using fallback (full dataset)")
            return None, None  # This triggers fallback logic in calling functions
    
        # Method 3: Find health section end using subtotals
        if self.debug_detect_health_section_boundaries:
            print(f"DEBUG: Looking for health section end starting from index {health_section_start}")

        last_health_subtotal_idx = None

        # Scan forward from health start to find all health programme subtotals
        for idx in range(health_section_start, len(self.data)):
            if self._is_health_programme_subtotal(idx):
                last_health_subtotal_idx = idx
                if self.debug_detect_health_section_boundaries:
                    print(f"DEBUG: Found health programme subtotal at index {idx}")
        
        if last_health_subtotal_idx is not None:
            health_section_end = last_health_subtotal_idx
            if self.debug_detect_health_section_boundaries:
                print(f"DEBUG: Health section ends at last health subtotal (index {health_section_end})")
        else:
            # Fallback: look for first non-health programme after health start
            if self.debug_detect_health_section_boundaries:
                print("DEBUG: No health subtotals found, looking for first non-health programme")

            for idx in range(health_section_start + 1, len(self.data)):
                programme = self._normalize_programme_text(self.data.loc[idx, self.programme_col])
            
                # Skip empty programmes and subtotals
                if not programme.strip() or self.data.loc[idx, 'is_subtotal_row']:
                    continue
            
                # Check for clear non-health programmes
                non_health_indicators = ['education', 'agriculture', 'livestock', 'trade', 'industrial', 'urban development']
                if any(indicator in programme for indicator in non_health_indicators):
                    health_section_end = idx - 1
                    if self.debug_detect_health_section_boundaries:
                        print(f"DEBUG: Health section ends before non-health programme at index {idx}: '{programme}'")
                    break
        
            # If still no end found, extend to end of data
            if health_section_end is None:
                health_section_end = len(self.data) - 1
                if self.debug_detect_health_section_boundaries:
                    print(f"DEBUG: Health section extends to end of data (index {health_section_end})")
    
        if self.debug_detect_health_section_boundaries:
            print(f"DEBUG: Final health section boundaries: {health_section_start} to {health_section_end}")
        return health_section_start, health_section_end
    
    def validate_subtotals(self):
        """Validate all subtotals in the dataset"""
        print("Validating subtotals...")
        
        validations = []
        
        for group in self.programme_structure:
            group_records = self.data.loc[group['record_indices']].copy()
            
            # Separate subtotal rows from detail rows
            detail_rows = group_records[~group_records['is_subtotal_row']]
            subtotal_rows = group_records[group_records['is_subtotal_row']]
            
            # Calculate sums from detail rows
            calc_approved = detail_rows['Approved_Budget_Clean'].sum() if len(detail_rows) > 0 else np.nan
            calc_actual = detail_rows['Actual_Payments_Clean'].sum() if len(detail_rows) > 0 else np.nan
            
            # Get reported subtotals (if they exist)
            if len(subtotal_rows) > 0:
                # Take the last subtotal row if multiple exist
                subtotal_row = subtotal_rows.iloc[-1]
                reported_approved = subtotal_row['Approved_Budget_Clean']
                reported_actual = subtotal_row['Actual_Payments_Clean']
                has_explicit_subtotal = True
                subtotal_idx = subtotal_row.name
            else:
                reported_approved = np.nan
                reported_actual = np.nan
                has_explicit_subtotal = False
                subtotal_idx = None
            
            # Validate approved budget
            approved_validation = self._validate_values(reported_approved, calc_approved)
            
            # Validate actual payments
            actual_validation = self._validate_values(reported_actual, calc_actual)
            
            validation_result = {
                'group_id': group['group_id'],
                'programme': group['programme'],
                'subtotal_row_idx': subtotal_idx,
                'detail_rows_count': len(detail_rows),
                'has_explicit_subtotal': has_explicit_subtotal,
                
                # Approved Budget
                'reported_approved': reported_approved,
                'calculated_approved': calc_approved,
                'approved_difference': approved_validation['difference'],
                'approved_difference_pct': approved_validation['difference_pct'],
                'approved_validation_status': approved_validation['status'],
                
                # Actual Payments
                'reported_actual': reported_actual,
                'calculated_actual': calc_actual,
                'actual_difference': actual_validation['difference'],
                'actual_difference_pct': actual_validation['difference_pct'],
                'actual_validation_status': actual_validation['status'],
                
                # Overall assessment
                'overall_validation_status': self._get_overall_status(approved_validation['status'], actual_validation['status'])
            }
            
            validations.append(validation_result)
        
        # Add validation entries for health admin programmes that don't have subtotals
        health_admin_programmes = set()
        for classification in self.classification_log:
            if classification['method'] == 'LARGE_HEALTH_ADMIN':
                idx = classification['index']
                programme = self.data.loc[idx, self.programme_col]
                if pd.notna(programme):
                    health_admin_programmes.add(programme)
        
        # Check which health admin programmes are missing from validations
        existing_programmes = {v['programme'] for v in validations}
        missing_health_admin = health_admin_programmes - existing_programmes
        
        for programme in missing_health_admin:
            # Create a validation entry for programmes without subtotals
            validation_result = {
                'group_id': f'health_admin_{programme}',
                'programme': programme,
                'detail_rows_count': 1,  # Assume single line item
                'has_explicit_subtotal': False,
                'subtotal_position': 'NONE',
                
                # No subtotal validation possible
                'calculated_approved': np.nan,
                'calculated_actual': np.nan,
                'reported_approved_subtotal': np.nan,
                'reported_actual_subtotal': np.nan,
                
                # Validation results
                'approved_difference': np.nan,
                'approved_difference_pct': np.nan,
                'approved_validation_status': 'NO_SUBTOTAL',
                'actual_difference': np.nan,
                'actual_difference_pct': np.nan,
                'actual_validation_status': 'NO_SUBTOTAL',
                
                'overall_validation_status': 'NO_SUBTOTAL_AVAILABLE'
            }
            validations.append(validation_result)
        
        self.subtotal_validations = validations
        print(f"Completed validation for {len(validations)} programme groups")
        
        # Summary stats
        perfect_matches = sum(1 for v in validations if v['overall_validation_status'] == 'PERFECT_MATCH')
        within_tolerance = sum(1 for v in validations if v['overall_validation_status'] == 'WITHIN_TOLERANCE')
        has_discrepancies = sum(1 for v in validations if 'DISCREPANCY' in v['overall_validation_status'])
        
        print(f"  Perfect matches: {perfect_matches}")
        print(f"  Within tolerance: {within_tolerance}")
        print(f"  Has discrepancies: {has_discrepancies}")
        
        return validations
    
    def _validate_values(self, reported, calculated):
        """Validate a pair of reported vs calculated values"""
        if pd.isna(reported) and pd.isna(calculated):
            return {'difference': 0, 'difference_pct': 0, 'status': 'BOTH_MISSING'}
        
        if pd.isna(reported):
            return {'difference': np.nan, 'difference_pct': np.nan, 'status': 'NO_REPORTED_TOTAL'}
        
        if pd.isna(calculated):
            return {'difference': np.nan, 'difference_pct': np.nan, 'status': 'CANNOT_CALCULATE'}
        
        # Calculate differences
        difference = abs(calculated - reported)
        difference_pct = (difference / abs(reported) * 100) if reported != 0 else (100 if calculated != 0 else 0)
        
        # Determine status
        if difference == 0:
            status = 'EXACT_MATCH'
        elif difference <= self.validation_tolerance_abs and difference_pct <= self.validation_tolerance_pct:
            status = 'WITHIN_TOLERANCE'
        elif difference_pct <= 5:
            status = 'MINOR_DISCREPANCY'
        else:
            status = 'MAJOR_DISCREPANCY'
        
        return {
            'difference': difference,
            'difference_pct': difference_pct,
            'status': status
        }
    
    def _get_overall_status(self, approved_status, actual_status):
        """Determine overall validation status for a programme group"""
        statuses = [approved_status, actual_status]
        
        # Priority order for determining overall status
        if any('MAJOR_DISCREPANCY' in s for s in statuses):
            return 'MAJOR_DISCREPANCY'
        elif any('MINOR_DISCREPANCY' in s for s in statuses):
            return 'MINOR_DISCREPANCY'
        elif all(s in ['EXACT_MATCH', 'BOTH_MISSING'] for s in statuses):
            return 'PERFECT_MATCH'
        elif all(s in ['EXACT_MATCH', 'WITHIN_TOLERANCE', 'BOTH_MISSING'] for s in statuses):
            return 'WITHIN_TOLERANCE'
        elif any('NO_REPORTED_TOTAL' in s for s in statuses):
            return 'NO_EXPLICIT_SUBTOTAL'
        else:
            return 'MIXED_STATUS'

    def classify_health_records(self):
        """Enhanced classification with non-health keyword veto and comprehensive fallback logic"""
        print("Classifying health-related records...")
    
        classifications = []
    
        # Try to get health section boundaries, but don't fail if it doesn't work
        health_start, health_end = None, None
        try:
            health_start, health_end = self._detect_health_section_boundaries()
            if health_start is not None:
                print(f"Health section detected from index {health_start} to {health_end}")
            else:
                print("No health section boundaries detected - using fallback classification")
        except Exception as e:
            print(f"Health section detection failed: {e} - using fallback classification")
    
        # Create validation lookup for context
        validation_lookup = {v['group_id']: v for v in self.subtotal_validations}
    
        for idx, row in self.data.iterrows():
            programme = str(row[self.programme_col]) if pd.notna(row[self.programme_col]) else ''
            subprogramme = str(row[self.subprogramme_col]) if pd.notna(row[self.subprogramme_col]) else ''
            combined_text = f"{programme} {subprogramme}".strip()
        
            # Skip empty records
            if not combined_text or combined_text == 'nan nan':
                classifications.append({
                    'index': idx,
                    'classification': 'NOT_HEALTH',
                    'method': 'EMPTY_TEXT',
                    'confidence': 1.0,
                    'text': combined_text
                })
                continue
        
            # Note: Subtotal rows are handled by normal health classification logic
            # The is_subtotal_row flag is preserved as structural metadata
        
            # Step 1: Check for explicit health programmes (highest confidence)
            if self._is_explicit_health_programme(programme):
                classifications.append({
                    'index': idx,
                    'classification': 'HEALTH',
                    'method': 'EXPLICIT_HEALTH_PROGRAMME',
                    'confidence': 1.0,
                    'text': combined_text
                })
                continue
        
            # Step 2: Check for obvious NON-HEALTH keywords first (veto power)
            combined_text_normalized = self._normalize_programme_text(combined_text)
            non_health_found = False
        
            for keyword in self.obvious_non_health_keywords:
                if keyword in combined_text_normalized:
                    classifications.append({
                        'index': idx,
                        'classification': 'NOT_HEALTH',
                        'method': 'KEYWORD_NON_HEALTH_VETO',
                        'confidence': 0.9,
                        'text': combined_text
                    })
                    non_health_found = True
                    break
        
            if non_health_found:
                continue
        
            # Step 3: Contextual analysis removed - large health admin handled in keyword screening
        
            # Step 4: Fallback to keyword + semantic approach
            # Original keyword screening
            classification_result = self._quick_keyword_screen(combined_text, idx)
        
            # If uncertain, try semantic similarity
            if classification_result['classification'] in ['UNCERTAIN', 'UNCERTAIN_ADMIN']:
                try:
                    semantic_result = self._semantic_classification(combined_text, idx)
                
                    # For admin entries, be more generous with semantic results
                    if 'ADMIN' in classification_result['classification'] and semantic_result['confidence'] > 0.4:
                        classification_result = semantic_result
                    elif semantic_result['classification'] == 'HEALTH':
                        classification_result = semantic_result
                except Exception as e:
                    if self.debug_semantic:
                        print(f"DEBUG: Semantic classification failed for idx {idx}: {e}")
        
            classifications.append(classification_result)
    
        # Post-process: Convert HEALTH to HEALTH_SUBTOTAL for subtotal rows
        for classification in classifications:
            idx = classification['index']
            if (classification['classification'] == 'HEALTH' and 
                idx < len(self.data) and 
                self.data.loc[idx, 'is_subtotal_row']):
                classification['classification'] = 'HEALTH_SUBTOTAL'
        
        # Store classification results
        self.classification_log = classifications
    
        # Add classification to main dataframe
        classification_df = pd.DataFrame(classifications)
        self.data = self.data.merge(
            classification_df[['index', 'classification', 'method', 'confidence']], 
            left_index=True, right_on='index', how='left'
        )
    
        health_count = len([c for c in classifications if c['classification'] in ['HEALTH', 'HEALTH_SUBTOTAL']])
        print(f"Classification complete. Found {health_count} health records")
    
        # Debug: Show classification method breakdown
        method_counts = pd.Series([c['method'] for c in classifications]).value_counts()
        print("Classification method breakdown:")
        for method, count in method_counts.items():
            print(f"  {method}: {count}")
    
        return classifications
    
    def _is_health_programme_subtotal(self, idx):
        """Determine if a subtotal row belongs to a health programme"""
        if not self.data.loc[idx, 'is_subtotal_row']:
            return False
    
        # Look backwards to find the programme this subtotal belongs to
        for back_idx in range(idx - 1, max(0, idx - 20), -1):  # Look back up to 20 rows
            back_programme = self.data.loc[back_idx, self.programme_col]
        
            # Skip empty programmes and other subtotals
            if pd.notna(back_programme) and back_programme.strip() and not self.data.loc[back_idx, 'is_subtotal_row']:
                # Found the programme this subtotal belongs to
                return self._is_explicit_health_programme(back_programme)
    
        return False
    
    
    def _normalize_programme_text(self, text):
        """Centralized text cleaning function for consistent processing"""
        if pd.isna(text):
            return ''
        # Convert to string and lowercase
        text = str(text).lower()
        # Fix hyphenated line breaks: "administra-\ntion" -> "administration"
        text = re.sub(r"-\s*\n\s*", "", text)
        # Replace any remaining line breaks/tabs with a space
        text = re.sub(r"[\r\n\t]+", " ", text)
        # Remove common special characters but keep important ones
        text = text.replace('"', '').replace("'", '').replace(',', ' ')
        # Collapse multiple spaces
        text = ' '.join(text.split())
        return text.strip()

    
    def _is_explicit_health_programme(self, programme):
        """Check if programme is explicitly health-related with flexible matching"""
        programme_normalized = self._normalize_programme_text(programme)
    
        # Debug: Print what we're checking
        #print(f"DEBUG: Checking programme: '{programme_normalized}'")
    
        # Primary: Exact phrase matching (normalized)
        explicit_health_programmes = [
            'curative and rehabilitative services',
            'preventive and promotive health services', 
            'health services',
            'medical services'
        ]
    
        for health_prog in explicit_health_programmes:
            if health_prog in programme_normalized:
                #print(f"DEBUG: Found explicit health programme: '{health_prog}' in '{programme_normalized}'")
                return True
    
        # Secondary: Key term combinations
        health_combinations = [
            ['curative', 'rehabilitative'],  # Both terms present
            ['preventive', 'promotive'],     # Both terms present
            ['health', 'services'],          # Both terms present
            ['medical', 'services']          # Both terms present
        ]
    
        for combo in health_combinations:
            if all(term in programme_normalized for term in combo):
                #print(f"DEBUG: Found health combination {combo} in '{programme_normalized}'")
                return True
    
        # Tertiary: Single strong health indicators
        strong_health_terms = ['curative', 'rehabilitative', 'preventive', 'promotive']
        for term in strong_health_terms:
            if term in programme_normalized and 'services' in programme_normalized:
                #print(f"DEBUG: Found strong health term '{term}' with services in '{programme_normalized}'")
                return True
    
        return False
    
    def _is_adjacent_to_health_programmes(self, idx, window=3):
        """Check if explicit health programmes appear within specified window (before or after)"""
        start_idx = max(0, idx - window)
        end_idx = min(len(self.data), idx + window + 1)
        
        for check_idx in range(start_idx, end_idx):
            if check_idx == idx:
                continue
            programme = str(self.data.loc[check_idx, self.programme_col]) if pd.notna(self.data.loc[check_idx, self.programme_col]) else ''
            if self._is_explicit_health_programme(programme):
                return True
        return False
    
    def _is_health_programme_context(self, programme, subprogramme, combined_text):
        """Check if this is a health programme for subtotal classification"""
        return self._is_explicit_health_programme(programme) or any(
            keyword in combined_text for keyword in self.obvious_health_keywords
        )
    
    
    def _is_large_administration_entry(self, row):
        """Identify high-value administration entries that need special handling"""
        programme = str(row[self.programme_col]) if pd.notna(row[self.programme_col]) else ''
        programme_normalized = self._normalize_programme_text(programme)
        budget_value = row['Approved_Budget_Clean']

        # Must be administration-related
        admin_keywords = ['general administration', 'administration services', 'administrative services']
        is_admin = any(keyword in programme_normalized for keyword in admin_keywords)

        if self.debug_is_large_administration_entry:
            print(f"DEBUG _is_large_administration_entry: programme='{programme}', normalized='{programme_normalized}', budget={budget_value}, is_admin={is_admin}")
    
        if not is_admin or pd.isna(budget_value):
            return False
    
        # Define large as > 500M or top 10% of all budget entries
        large_threshold = 500_000_000  # 500M
    
        # Calculate percentile threshold
        all_budgets = self.data['Approved_Budget_Clean'].dropna()
        if len(all_budgets) > 0:
            percentile_90_threshold = all_budgets.quantile(0.9)
            threshold = max(large_threshold, percentile_90_threshold)
        else:
            threshold = large_threshold
    
        return budget_value >= threshold
    
    
    

    def _quick_keyword_screen(self, text, idx):
        """Enhanced keyword screening with administration-aware logic"""
        row = self.data.loc[idx]
    
        # Normalize text to handle line breaks and formatting issues
        text_normalized = self._normalize_programme_text(text)
    
        # Check for obvious health keywords (unchanged)
        for keyword in self.obvious_health_keywords:
            if keyword in text_normalized:
                return {
                    'index': idx,
                    'classification': 'HEALTH',
                    'method': 'KEYWORD_HEALTH',
                    'confidence': 0.9,
                    'text': text
                }
    
        # Enhanced administration keyword handling
        admin_keywords = ['administration', 'administrative', 'planning', 'support services']
        has_admin_keywords = any(keyword in text_normalized for keyword in admin_keywords)
    
        if has_admin_keywords:
            # For large admin entries, check adjacency to health programmes
            if self._is_large_administration_entry(row):
                if self._is_adjacent_to_health_programmes(idx, 3):
                    return {
                        'index': idx,
                        'classification': 'HEALTH',  # Direct health classification
                        'method': 'LARGE_HEALTH_ADMIN',  # Preserve this method name
                        'confidence': 0.8,
                        'text': text
                    }
                else:
                    # Large admin but not adjacent to health
                    return {
                        'index': idx,
                        'classification': 'UNCERTAIN_ADMIN',
                        'method': 'LARGE_ADMIN_NON_HEALTH',
                        'confidence': 0.6,
                        'text': text
                    }
        
            # For regular admin entries, require additional context
            return {
                'index': idx,
                'classification': 'UNCERTAIN_ADMIN',
                'method': 'ADMIN_KEYWORDS',
                'confidence': 0.6,
                'text': text
            }
    
        # Check for obvious non-health keywords (unchanged)
        for keyword in self.obvious_non_health_keywords:
            if keyword in text_normalized:
                return {
                    'index': idx,
                    'classification': 'NOT_HEALTH',
                    'method': 'KEYWORD_NON_HEALTH',
                    'confidence': 0.8,
                    'text': text
                }
    
        return {
            'index': idx,
            'classification': 'UNCERTAIN',
            'method': 'KEYWORD_UNCERTAIN',
            'confidence': 0.5,
            'text': text
        }

    def _semantic_classification(self, text, idx):
        """Classify using semantic similarity with stricter thresholds for admin entries"""
        # Create embedding for the text
        text_embedding = self.model.encode([text])
        
        # Calculate similarity with health references
        similarities = cosine_similarity(text_embedding, self.health_embeddings)[0]
        max_similarity = np.max(similarities)
        
        # Check if this is an admin entry to apply stricter threshold
        text_normalized = self._normalize_programme_text(text)
        is_admin_entry = any(keyword in text_normalized for keyword in ['administration', 'administrative', 'planning', 'support services'])
        
        # Classification thresholds - stricter for admin entries
        health_threshold = 0.8 if is_admin_entry else 0.6
        
        if max_similarity > health_threshold:
            classification = 'HEALTH'
            method = 'SEMANTIC_HEALTH'
        elif max_similarity < 0.3:
            classification = 'NOT_HEALTH'
            method = 'SEMANTIC_NON_HEALTH'
        else:
            classification = 'UNCERTAIN'
            method = 'SEMANTIC_UNCERTAIN'
        
        return {
            'index': idx,
            'classification': classification,
            'method': method,
            'confidence': float(max_similarity),
            'text': text
        }
    
    def extract_health_data(self):
        """Extract all health-related records with validation results"""
        print("Extracting health data...")
        
        # Filter for health records (including both detail records and subtotals)
        health_mask = self.data['classification'].isin(['HEALTH', 'HEALTH_SUBTOTAL'])
        self.health_data = self.data[health_mask].copy()
        
        # Add validation information
        validation_lookup = {v['group_id']: v for v in self.subtotal_validations}
        
        self.health_data['programme_validation_status'] = self.health_data['programme_group_id'].map(
            lambda x: validation_lookup.get(x, {}).get('overall_validation_status', 'UNKNOWN')
        )
        
        self.health_data['has_validated_subtotal'] = self.health_data['programme_group_id'].map(
            lambda x: validation_lookup.get(x, {}).get('has_explicit_subtotal', False)
        )
        
        # Clean up the health data
        self.health_data = self.health_data[[
            self.programme_col, self.subprogramme_col, self.approved_budget_col, 
            self.actual_payments_col, 'Approved_Budget_Clean', 'Actual_Payments_Clean',
            'Has_Approved_Budget', 'Has_Actual_Payments', 'method', 'confidence',
            'programme_group_id', 'programme_validation_status', 'has_validated_subtotal',
            'is_subtotal_row', 'classification'
        ]].copy()
        
        # Rename columns for clarity
        self.health_data.columns = [
            'Programme', 'Sub_Programme', 'Approved_Budget_Original', 
            'Actual_Payments_Original', 'Approved_Budget', 'Actual_Payments',
            'Has_Approved_Budget', 'Has_Actual_Payments', 'Classification_Method', 'Confidence',
            'Programme_Group_ID', 'Programme_Validation_Status', 'Has_Validated_Subtotal',
            'Is_Subtotal_Row', 'Classification'
        ]
        
        print(f"Extracted {len(self.health_data)} health records")
        return self.health_data
    
    def create_programme_totals(self):
        """Create programme-level aggregations prioritizing validated subtotals"""
        print("Creating programme totals...")
        
        programme_groups = self.health_data[~self.health_data['Is_Subtotal_Row']].groupby('Programme')
        programme_totals = []
        validation_lookup = {v['group_id']: v for v in self.subtotal_validations}
        
        for programme_name, group in programme_groups:
            # Get validation info for this programme
            group_id = group['Programme_Group_ID'].iloc[0] if len(group) > 0 else -1
            validation_info = validation_lookup.get(group_id, {})
            
            # Prioritize reported totals when validation is reliable
            use_reported = (
                validation_info.get('has_explicit_subtotal', False) and 
                validation_info.get('overall_validation_status') in [
                    'PERFECT_MATCH', 'WITHIN_TOLERANCE', 'MINOR_DISCREPANCY'
                ]
            )
            
            if use_reported:
                # Use the validated subtotal
                approved_total = validation_info.get('reported_approved', np.nan)
                payments_total = validation_info.get('reported_actual', np.nan)
                total_source = 'REPORTED_SUBTOTAL'
            else:
                # Calculate from detail records
                approved_total = group['Approved_Budget'].sum() if group['Has_Approved_Budget'].any() else np.nan
                payments_total = group['Actual_Payments'].sum() if group['Has_Actual_Payments'].any() else np.nan
                total_source = 'CALCULATED'
            
            # Count missing data
            missing_approved = (~group['Has_Approved_Budget']).sum()
            missing_payments = (~group['Has_Actual_Payments']).sum()
            total_subprogrammes = len(group)
            
            programme_totals.append({
                'Programme': programme_name,
                'Total_Subprogrammes': total_subprogrammes,
                'Approved_Budget_Total': approved_total,
                'Actual_Payments_Total': payments_total,
                'Total_Source': total_source,
                'Missing_Approved_Budget': missing_approved,
                'Missing_Actual_Payments': missing_payments,
                'Data_Completeness_Budget': f"{total_subprogrammes - missing_approved}/{total_subprogrammes}",
                'Data_Completeness_Payments': f"{total_subprogrammes - missing_payments}/{total_subprogrammes}",
                'Validation_Status': validation_info.get('overall_validation_status', 'NO_VALIDATION'),
                'Has_Explicit_Subtotal': validation_info.get('has_explicit_subtotal', False)
            })
        
        programme_df = pd.DataFrame(programme_totals)
        print(f"Created totals for {len(programme_df)} programmes")
        return programme_df
    
    def create_subtotal_validation_report(self):
        """Create detailed subtotal validation report"""
        print("Creating subtotal validation report...")
        
        validation_df = pd.DataFrame(self.subtotal_validations)
        
        # Add summary columns
        if len(validation_df) > 0:
            validation_df['Perfect_Match'] = validation_df['overall_validation_status'] == 'PERFECT_MATCH'
            validation_df['Has_Discrepancy'] = validation_df['overall_validation_status'].str.contains('DISCREPANCY')
            validation_df['Needs_Review'] = validation_df['overall_validation_status'].isin(['MAJOR_DISCREPANCY', 'MIXED_STATUS'])
            
            # Get health programmes (including health admin)
            health_programmes = set()
            for classification in self.classification_log:
                if classification['classification'] in ['HEALTH', 'HEALTH_SUBTOTAL']:
                    # Standard health programmes
                    idx = classification['index']
                    if idx < len(self.data):
                        programme = self.data.loc[idx, self.programme_col]
                        if pd.notna(programme):
                            health_programmes.add(programme)
                            
                # Include programmes with health admin
                if classification['method'] == 'LARGE_HEALTH_ADMIN':
                    idx = classification['index'] 
                    if idx < len(self.data):
                        programme = self.data.loc[idx, self.programme_col]
                        if pd.notna(programme):
                            health_programmes.add(programme)
            
            validation_df['Is_Health_Programme'] = validation_df['programme'].isin(health_programmes)
            
            # FILTER TO HEALTH PROGRAMMES ONLY
            validation_df = validation_df[validation_df['Is_Health_Programme'] == True]
            
            # ENSURE ALL PROGRAMMES FROM PROGRAMME_TOTALS ARE INCLUDED
            # Get programmes that appear in programme totals but missing from validation
            programme_totals = self.create_programme_totals()
            all_health_programmes = set(programme_totals['Programme'].tolist())
            existing_validation_programmes = set(validation_df['programme'].tolist())
            missing_programmes = all_health_programmes - existing_validation_programmes
            
            # Add missing programmes with NO_SUBTOTAL_AVAILABLE status
            for programme in missing_programmes:
                missing_validation = {
                    'group_id': f'no_subtotal_{programme}',
                    'programme': programme,
                    'detail_rows_count': 1,
                    'has_explicit_subtotal': False,
                    'subtotal_position': 'NONE',
                    'calculated_approved': np.nan,
                    'calculated_actual': np.nan,
                    'reported_approved_subtotal': np.nan,
                    'reported_actual_subtotal': np.nan,
                    'approved_difference': np.nan,
                    'approved_difference_pct': np.nan,
                    'approved_validation_status': 'NO_SUBTOTAL',
                    'actual_difference': np.nan,
                    'actual_difference_pct': np.nan,
                    'actual_validation_status': 'NO_SUBTOTAL',
                    'overall_validation_status': 'NO_SUBTOTAL_AVAILABLE',
                    'Perfect_Match': False,
                    'Has_Discrepancy': False,
                    'Needs_Review': False,
                    'Is_Health_Programme': True
                }
                validation_df = pd.concat([validation_df, pd.DataFrame([missing_validation])], ignore_index=True)
        
        return validation_df
    
    def create_data_quality_report(self):
        """Create enhanced data quality and methodology report with validation metrics"""
        print("Creating data quality report...")
        
        quality_data = []
        
        # Classification method summary
        method_counts = pd.Series([c['method'] for c in self.classification_log]).value_counts()
        for method, count in method_counts.items():
            quality_data.append({
                'Metric': f'Classification_Method_{method}',
                'Value': count,
                'Description': f'Records classified using {method}'
            })
        
        # Confidence statistics for health records
        health_confidences = [c['confidence'] for c in self.classification_log if c['classification'] in ['HEALTH', 'HEALTH_SUBTOTAL']]
        if health_confidences:
            quality_data.extend([
                {'Metric': 'Health_Records_Avg_Confidence', 'Value': np.mean(health_confidences), 
                 'Description': 'Average confidence score for health classifications'},
                {'Metric': 'Health_Records_Min_Confidence', 'Value': np.min(health_confidences),
                 'Description': 'Minimum confidence score for health classifications'},
                {'Metric': 'Health_Records_Max_Confidence', 'Value': np.max(health_confidences),
                 'Description': 'Maximum confidence score for health classifications'}
            ])
        
        # Validation statistics
        validation_stats = pd.Series([v['overall_validation_status'] for v in self.subtotal_validations]).value_counts()
        for status, count in validation_stats.items():
            quality_data.append({
                'Metric': f'Validation_Status_{status}',
                'Value': count,
                'Description': f'Programme groups with {status} validation status'
            })
        
        # Health programme validation statistics
        health_validations = [v for v in self.subtotal_validations if self._is_health_programme_validation(v)]
        if health_validations:
            health_validation_stats = pd.Series([v['overall_validation_status'] for v in health_validations]).value_counts()
            quality_data.append({
                'Metric': 'Total_Health_Programmes_Validated',
                'Value': len(health_validations),
                'Description': 'Number of health programmes with validation data'
            })
            
            for status, count in health_validation_stats.items():
                quality_data.append({
                    'Metric': f'Health_Validation_Status_{status}',
                    'Value': count,
                    'Description': f'Health programmes with {status} validation status'
                })
        
        # Reported vs calculated usage statistics
        if hasattr(self, 'health_data') and self.health_data is not None:
            programme_totals = self.create_programme_totals()
            source_counts = programme_totals['Total_Source'].value_counts()
            
            for source, count in source_counts.items():
                quality_data.append({
                    'Metric': f'Totals_Source_{source}',
                    'Value': count,
                    'Description': f'Programmes using {source} for totals'
                })
        
        # Data completeness statistics
        if hasattr(self, 'health_data') and self.health_data is not None:
            health_detail_records = self.health_data[~self.health_data['Is_Subtotal_Row']]
            quality_data.extend([
                {'Metric': 'Total_Health_Records', 'Value': len(self.health_data),
                 'Description': 'Total number of health-related records identified'},
                {'Metric': 'Health_Detail_Records', 'Value': len(health_detail_records),
                 'Description': 'Health detail records (excluding subtotals)'},
                {'Metric': 'Records_With_Budget_Data', 'Value': health_detail_records['Has_Approved_Budget'].sum(),
                 'Description': 'Detail records with approved budget information'},
                {'Metric': 'Records_With_Payment_Data', 'Value': health_detail_records['Has_Actual_Payments'].sum(),
                 'Description': 'Detail records with actual payment information'},
                {'Metric': 'Budget_Data_Completeness_Pct', 
                 'Value': (health_detail_records['Has_Approved_Budget'].sum() / len(health_detail_records) * 100) if len(health_detail_records) > 0 else 0,
                 'Description': 'Percentage of detail records with budget data'},
                {'Metric': 'Payment_Data_Completeness_Pct', 
                 'Value': (health_detail_records['Has_Actual_Payments'].sum() / len(health_detail_records) * 100) if len(health_detail_records) > 0 else 0,
                 'Description': 'Percentage of detail records with payment data'}
            ])
        
        return pd.DataFrame(quality_data)
    
    def _is_health_programme_validation(self, validation_record):
        """Check if a validation record corresponds to a health programme"""
        programme_name = validation_record['programme']
        return any(
            c['classification'] in ['HEALTH', 'HEALTH_SUBTOTAL'] and 
            programme_name.lower() in c['text'].lower()
            for c in self.classification_log
        )
    
    def create_department_summary(self):
        """Create department-level summary"""
        print("Creating department summary...")
        
        # Filter to detail records only for aggregation
        health_detail_records = self.health_data[~self.health_data['Is_Subtotal_Row']]
        
        # Overall totals using the same logic as programme totals
        programme_totals_df = self.create_programme_totals()
        
        total_approved = programme_totals_df['Approved_Budget_Total'].sum() if len(programme_totals_df) > 0 else np.nan
        total_payments = programme_totals_df['Actual_Payments_Total'].sum() if len(programme_totals_df) > 0 else np.nan
        
        # Count statistics
        total_records = len(health_detail_records)
        missing_approved = (~health_detail_records['Has_Approved_Budget']).sum()
        missing_payments = (~health_detail_records['Has_Actual_Payments']).sum()
        unique_programmes = health_detail_records['Programme'].nunique()
        
        # Source breakdown
        reported_count = len(programme_totals_df[programme_totals_df['Total_Source'] == 'REPORTED_SUBTOTAL'])
        calculated_count = len(programme_totals_df[programme_totals_df['Total_Source'] == 'CALCULATED'])
        
        summary = {
            'Department': 'Health',
            'Total_Detail_Records': total_records,
            'Total_Records_Including_Subtotals': len(self.health_data),
            'Unique_Programmes': unique_programmes,
            'Total_Approved_Budget': total_approved,
            'Total_Actual_Payments': total_payments,
            'Missing_Approved_Budget': missing_approved,
            'Missing_Actual_Payments': missing_payments,
            'Budget_Data_Completeness': f"{total_records - missing_approved}/{total_records}",
            'Payments_Data_Completeness': f"{total_records - missing_payments}/{total_records}",
            'Programmes_Using_Reported_Totals': reported_count,
            'Programmes_Using_Calculated_Totals': calculated_count,
            'Total_Programmes_With_Validation': reported_count + calculated_count
        }
        
        return pd.DataFrame([summary])

    def generate_methodology_notes(self):
        """Generate methodology documentation"""
        notes = {
            'Classification_Approach': [
                'Hybrid classification using keyword screening and semantic similarity',
                'Explicit health programmes identified first (curative, preventive, health services)',
                'Health department context analysis for support services and administration',
                'Keywords checked: health, medical, hospital, curative, rehabilitative, etc.',
                'Semantic similarity using sentence-transformers model: all-MiniLM-L6-v2',
                'Health administration blocks identified using context clues'
            ],
            'Subtotal_Validation': [
                f'Validation tolerance: {self.validation_tolerance_pct}% or {self.validation_tolerance_abs} units',
                'Programme boundaries identified by non-empty programme names and subtotal rows',
                'Explicit subtotals validated against sum of detail records',
                'Missing data excluded from calculations (not treated as zero)',
                'Health programme subtotals validated separately after classification'
            ],
            'Data_Processing': [
                'Missing values preserved as NaN, not converted to zero',
                'Currency values cleaned by removing quotes and commas',
                'Sub-total rows included in health extraction but excluded from aggregations',
                'Programme names forward-filled for missing entries',
                'Validation context used to improve classification accuracy'
            ],
            'Aggregation_Rules': [
                'PRIORITY: Reported subtotals used when validation status is acceptable',
                'Acceptable validation: PERFECT_MATCH, WITHIN_TOLERANCE, MINOR_DISCREPANCY',
                'Calculated totals used when subtotals missing or have MAJOR_DISCREPANCY',
                'Department total: sum of programme totals using prioritized values',
                'Missing data handling: excluded from sums, completeness ratios provided',
                'Source tracking: each total tagged as REPORTED_SUBTOTAL or CALCULATED'
            ],
            'Validation_Categories': [
                'EXACT_MATCH: Calculated and reported values are identical',
                'WITHIN_TOLERANCE: Difference ≤0.01% or ≤1 unit',
                'MINOR_DISCREPANCY: Difference 0.01%-5%',
                'MAJOR_DISCREPANCY: Difference >5%',
                'NO_REPORTED_TOTAL: Programme has no explicit subtotal',
                'CANNOT_CALCULATE: Insufficient data to calculate expected total'
            ],
            'Large_Health_Administration': [
                'Large administration entries (>500M) classified as health when adjacent to health programmes',
                'Adjacency defined as within 3 rows before or after the admin entry',
                'Must contain administration keywords: administration, administrative, planning, support services',
                'Adjacent health programmes identified using explicit health programme detection',
                'Method tagged as LARGE_HEALTH_ADMIN for reporting transparency'
            ],
            'Assumptions': [
                'Dataset represents health department budget allocation',
                'Records classified with confidence > 0.6 considered reliable',
                'Semantic similarity threshold: >0.6 health, <0.3 non-health, 0.3-0.6 uncertain',
                'Programme boundaries determined by non-empty programme names and subtotals',
                'Reported subtotals preferred over calculated when validation is acceptable'
            ]
        }
        
        # Convert to DataFrame format
        methodology_data = []
        for category, items in notes.items():
            for i, item in enumerate(items, 1):
                methodology_data.append({
                    'Category': category,
                    'Point': i,
                    'Description': item
                })
        
        return pd.DataFrame(methodology_data)
    
    def parse_metadata_from_path(self, file_path: str):
        """
        Extract Year, Quarter, County from a standardized path:
        program/{year}/{quarter}/county/{county}_programme_table.csv
        """
        p = Path(file_path)
        parts = p.parts  # e.g., ('program','2019_20','04','county','baringo_programme_table.csv')
        # Basic sanity check
        if len(parts) < 5 or parts[0] != "program" or parts[3] != "county":
            raise ValueError(f"Unexpected path structure: {file_path}")
        year = parts[1]
        quarter = parts[2]
        county = p.stem.replace("_programme_table", "")
        return year, quarter, county

    
    def generate_file_paths(self, years=None, quarters=None, counties=None):
        """
        Build a list of existing CSV paths in the expected layout:
        program/{year}/{quarter}/county/{county}_programme_table.csv

        Accepts years as either:
        - "2019_20" style strings
        - or "2019" (will auto-convert to "2019_20").

        - If `counties` is None, we auto-discover counties present under each year/quarter folder.
        - Returns paths in deterministic order: year ↑, quarter ↑, county ↑.
        """
        # Defaults
        all_years = [f"{y}_{str(y+1)[-2:]}" for y in range(2019, 2025)]  # 2019_20 → 2024_25
        all_quarters = [f"{q:02d}" for q in range(1, 5)]                  # 01 → 04

        years = years or all_years
        quarters = quarters or all_quarters

        # Normalize: if user passed 2019 → make it "2019_20"
        normalized_years = []
        for y in years:
            if re.fullmatch(r"\d{4}$", str(y)):  # plain 2019
                y = int(y)
                normalized_years.append(f"{y}_{str(y+1)[-2:]}")
            else:
                normalized_years.append(str(y))

        file_paths = []

        for year in normalized_years:
            for quarter in quarters:
                county_dir = Path(f"program/{year}/{quarter}/county")
                if counties:
                    # Use exactly the counties provided (alphabetical order)
                    for county in sorted(counties, key=str.lower):
                        fp = county_dir / f"{county}_programme_table.csv"
                        if fp.exists():
                            file_paths.append(str(fp))
                else:
                    # Auto-discover counties present for this year/quarter
                    if county_dir.exists():
                        # List *_programme_table.csv and sort by county name
                        candidates = sorted(
                            [f for f in county_dir.iterdir() if f.is_file() and f.name.endswith("_programme_table.csv")],
                            key=lambda x: x.stem.replace("_programme_table", "").lower()
                        )
                        for f in candidates:
                            file_paths.append(str(f))

        return file_paths


    def run_multi_file_analysis(
            self, years=None, quarters=None, counties=None, 
            output_path="program/health_budget_analysis.xlsx"):
        """
        Process multiple files, stack everything, and export one consolidated Excel.
        Relies on your existing HealthBudgetAnalyzer for per-file logic.
        """
        file_paths = self.generate_file_paths(years, quarters, counties)

        # Load model and create embeddings once for all files
        print("Loading semantic similarity model...")
        shared_model = SentenceTransformer('all-MiniLM-L6-v2')
        
        # Create health reference embeddings once
        health_references = [
            "healthcare services medical treatment",
            "hospital administration clinical services", 
            "preventive health public health programs",
            "curative medical care rehabilitation",
            "health infrastructure medical facilities",
            "primary healthcare community health",
            "health administration medical staff"
        ]
        print("Creating health reference embeddings...")
        shared_embeddings = shared_model.encode(health_references)

        all_health_data = []
        all_programme_totals = []
        all_dept_summaries = []
        all_subtotal_validations = []
        all_quality_reports = []
        methodology_notes = None

        for fp in file_paths:
            # Instantiate analyzer for this file with shared model
            analyzer = HealthBudgetAnalyzer(fp, model=shared_model, health_embeddings=shared_embeddings)

            # Run per-file pipeline (NO export inside)
            df = analyzer.run_full_analysis()
            if df is None or df.empty:
                continue

            # Attach metadata
            year, quarter, county = self.parse_metadata_from_path(fp)
            df = df.copy()
            df["Year"], df["Quarter"], df["County"] = year, quarter, county
            all_health_data.append(df)

            # Per-file summaries + metadata
            prog_totals = analyzer.create_programme_totals().copy()
            prog_totals["Year"], prog_totals["Quarter"], prog_totals["County"] = year, quarter, county
            all_programme_totals.append(prog_totals)

            dept_summary = analyzer.create_department_summary().copy()
            dept_summary["Year"], dept_summary["Quarter"], dept_summary["County"] = year, quarter, county
            all_dept_summaries.append(dept_summary)

            subtotal_report = analyzer.create_subtotal_validation_report().copy()
            subtotal_report["Year"], subtotal_report["Quarter"], subtotal_report["County"] = year, quarter, county
            all_subtotal_validations.append(subtotal_report)

            quality_report = analyzer.create_data_quality_report().copy()
            quality_report["Year"], quality_report["Quarter"], quality_report["County"] = year, quarter, county
            all_quality_reports.append(quality_report)

            # Methodology (grab once; it's generic)
            if methodology_notes is None:
                methodology_notes = analyzer.generate_methodology_notes()

        if not all_health_data:
            print("No data files processed.")
            return None

        # Stack all results
        final_health_data = pd.concat(all_health_data, ignore_index=True)
        final_programme_totals = pd.concat(all_programme_totals, ignore_index=True)
        final_dept_summary = pd.concat(all_dept_summaries, ignore_index=True)
        final_subtotal_validation = pd.concat(all_subtotal_validations, ignore_index=True)
        final_quality_report = pd.concat(all_quality_reports, ignore_index=True)

        # Export to main location
        self.export_multi_to_excel(
            final_health_data,
            final_programme_totals,
            final_dept_summary,
            final_subtotal_validation,
            final_quality_report,
            methodology_notes,
            output_path
        )

        # Export to secondary location
        alt_output_path = "../../health_finance/01_rawdata/health_budget_analysis.xlsx"
        self.export_multi_to_excel(
            final_health_data,
            final_programme_totals,
            final_dept_summary,
            final_subtotal_validation,
            final_quality_report,
            methodology_notes,
            alt_output_path
        )

        return final_health_data
    
    def export_multi_to_excel(
            self,
            final_health_data: pd.DataFrame,
            final_programme_totals: pd.DataFrame,
            final_dept_summary: pd.DataFrame,
            final_subtotal_validation: pd.DataFrame,
            final_quality_report: pd.DataFrame,
            methodology_notes: pd.DataFrame,
            output_path: str
            ):
        """Export consolidated multi-file results to a multi-sheet Excel workbook."""
        print(f"Exporting consolidated results to {output_path}...")

        with pd.ExcelWriter(output_path, engine="openpyxl") as writer:
            # Sheet 1: Raw health data
            final_health_data.to_excel(writer, sheet_name="Raw_Health_Data", index=False)

            # Sheet 2: Programme totals
            final_programme_totals.to_excel(writer, sheet_name="Programme_Totals", index=False)

            # Sheet 3: Department summary
            final_dept_summary.to_excel(writer, sheet_name="Department_Summary", index=False)

            # Sheet 4: Subtotal validation report
            final_subtotal_validation.to_excel(writer, sheet_name="Subtotal_Validation", index=False)

            # Sheet 5: Data quality report
            final_quality_report.to_excel(writer, sheet_name="Data_Quality", index=False)

            # Sheet 6: Methodology notes
            if methodology_notes is not None:
                methodology_notes.to_excel(writer, sheet_name="Methodology_Notes", index=False)

        print(f"Export complete! File saved as {output_path}")


    def export_to_excel(self, output_path):
        """Export all results to multi-sheet Excel file"""
        print(f"Exporting results to {output_path}...")
        
        with pd.ExcelWriter(output_path, engine='openpyxl') as writer:
            # Sheet 1: Raw health data
            if self.health_data is not None:
                self.health_data.to_excel(writer, sheet_name='Raw_Health_Data', index=False)
            
            # Sheet 2: Programme totals
            programme_totals = self.create_programme_totals()
            programme_totals.to_excel(writer, sheet_name='Programme_Totals', index=False)
            
            # Sheet 3: Department summary
            dept_summary = self.create_department_summary()
            dept_summary.to_excel(writer, sheet_name='Department_Summary', index=False)
            
            # Sheet 4: Subtotal validation report
            validation_report = self.create_subtotal_validation_report()
            validation_report.to_excel(writer, sheet_name='Subtotal_Validation', index=False)
            
            # Sheet 5: Data quality report
            quality_report = self.create_data_quality_report()
            quality_report.to_excel(writer, sheet_name='Data_Quality', index=False)
            
            # Sheet 6: Methodology notes
            methodology = self.generate_methodology_notes()
            methodology.to_excel(writer, sheet_name='Methodology_Notes', index=False)
        
        print(f"Export complete! File saved as {output_path}")

    def run_full_analysis(self):
        """Run the complete analysis workflow for a single file (NO export here)"""
        print("=== Health Budget Analysis Workflow ===")
        print()
        
        # Step 1: Load and clean data
        if self.load_and_clean_data() is None:
            print("Skipping analysis for empty/invalid file")
            return pd.DataFrame()  # Return empty DataFrame for empty files
        
        # Step 2: Parse programme structure and validate subtotals BEFORE classification
        self.parse_programme_structure()
        self.validate_subtotals()
        
        # Step 3: Classify records (now with validation context)
        self.classify_health_records()
        
        # Step 4: Extract health data
        self.extract_health_data()
        
        print()
        print("=== Analysis Complete ===")
        print(f"Health records found: {len(self.health_data) if self.health_data is not None else 0}")
        
        # Summary of validation and totals approach
        if hasattr(self, 'health_data') and self.health_data is not None and len(self.health_data) > 0:
            programme_totals = self.create_programme_totals()
            if len(programme_totals) > 0 and 'Total_Source' in programme_totals.columns:
                reported_count = len(programme_totals[programme_totals['Total_Source'] == 'REPORTED_SUBTOTAL'])
                calculated_count = len(programme_totals[programme_totals['Total_Source'] == 'CALCULATED'])
                print(f"Programme totals: {reported_count} using reported subtotals, {calculated_count} using calculated totals")
        
        return self.health_data


if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser(description="Health Budget Analysis: single or multi-file")
    parser.add_argument("--years", nargs="*", help="Years to include (e.g. 2019_20 2020_21). Default = all")
    parser.add_argument("--quarters", nargs="*", help="Quarters to include (01 02 03 04). Default = all")
    parser.add_argument("--counties", nargs="*", help="Counties to include (e.g. baringo bungoma). Default = all")
    parser.add_argument("--output", default="program/health_budget_analysis.xlsx", help="Output Excel file")

    args = parser.parse_args()

    analyzer = HealthBudgetAnalyzer()
    health_data = analyzer.run_multi_file_analysis(
        years=args.years,
        quarters=args.quarters,
        counties=args.counties,
        output_path=args.output
    )

    if health_data is not None:
        print("\n=== Quick Summary Across Files ===")
        print(f"Total records: {len(health_data)}")
        print(f"Counties covered: {health_data['County'].nunique()}")
        print(f"Years covered: {health_data['Year'].nunique()}")
        print(f"Quarters covered: {health_data['Quarter'].nunique()}")



