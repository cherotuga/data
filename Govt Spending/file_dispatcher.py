"""
File Dispatcher and Parser

This module handles the initial processing of raw CSV files. It is responsible for:
- Identifying and cleaning column headers, even if they span multiple rows.
- Validating the file structure against a set of ideal columns.
- Loading the data into a standardized pandas DataFrame.

This helps to decouple the file parsing logic from the data analysis logic,
adhering to the Single Responsibility Principle.
"""

import pandas as pd
import os
import re

def _identify_header(csv_path):
    """
    Identifies and cleans the header of a CSV file.
    Assumes the header is on the first row but cleans it effectively.

    Args:
        csv_path (str): The path to the CSV file.

    Returns:
        tuple: A tuple containing:
            - list: The cleaned list of column names.
            - int: The number of rows identified as the header (returns 1 if successful).
                   Returns 0 on failure.
    """
    if not os.path.exists(csv_path):
        return None, 0

    try:
        # We only need the column names, so reading 0 rows is efficient.
        df = pd.read_csv(csv_path, nrows=0)

        # Clean column names: replace newlines, carriage returns, and collapse whitespace.
        cleaned_columns = []
        for col in df.columns:
            # Replace any newline or carriage return characters with a space
            clean_col = re.sub(r'[\n\r]+', ' ', str(col))
            # Replace multiple whitespace characters with a single space and strip
            clean_col = re.sub(r'\s+', ' ', clean_col).strip()
            cleaned_columns.append(clean_col)

        return cleaned_columns, 1
    except Exception:
        # If pandas can't even read the header, the file is likely problematic.
        return None, 0

# Ideal column structure for program budget reports
IDEAL_COLUMNS = [
    'Programmes', 'Sub- Programmes', 'Approved Budget (Kshs)',
    'Actual Payments (Kshs)', 'Variance', 'Absorption (%)',
    'page_number', 'table_index'
]

def dispatch_file(csv_path):
    """
    Reads a CSV file, validates its columns against a standard, and returns a DataFrame.

    This function serves as a gatekeeper. It uses _identify_header to parse the
    column names and then compares them against a predefined ideal structure.
    If the file matches, it's loaded into a DataFrame. Otherwise, it's rejected.

    Args:
        csv_path (str): The path to the CSV file.

    Returns:
        pd.DataFrame or None: A DataFrame with standardized columns if validation
                              succeeds, otherwise None.
    """
    actual_columns, header_rows = _identify_header(csv_path)

    if actual_columns is None:
        # _identify_header failed, message would have been printed there.
        return None

    # Normalize column names for a more robust comparison (case and space insensitive)
    norm_actual = [col.lower().replace(' ', '') for col in actual_columns]
    norm_ideal = [col.lower().replace(' ', '') for col in IDEAL_COLUMNS]

    if norm_actual != norm_ideal:
        # This is a critical validation step. If columns don't match, we can't process the file reliably.
        # print(f"  [!] Column mismatch in {os.path.basename(csv_path)}. Skipping file.")
        # print(f"    - Expected: {IDEAL_COLUMNS}")
        # print(f"    - Found:    {actual_columns}")
        return None

    try:
        # Read the CSV data, skipping the header rows we've already processed.
        # Apply the standardized column names.
        df = pd.read_csv(
            csv_path,
            skiprows=header_rows,
            names=IDEAL_COLUMNS,
            na_values=['-'],      # Treat standalone '-' as Not a Number
            thousands=','         # Correctly parse numbers like "1,234,567"
        )
        return df
    except Exception as e:
        # Catch potential errors during the full read, even if header was fine.
        # print(f"  [!] Error reading data from {os.path.basename(csv_path)}: {e}")
        return None
