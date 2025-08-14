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

def is_likely_header(cell_content):
    """
    Determines if a cell's content is likely part of a header.
    - Returns False if it's a number (int or float).
    - Returns False if it's empty or NA.
    - Returns True otherwise (likely text).
    """
    # Check for NaN, None, or empty strings
    if pd.isna(cell_content) or cell_content is None or str(cell_content).strip() == "":
        return False
    # Try to convert to a number
    try:
        float(str(cell_content).replace(",", ""))
        return False # It's a number
    except (ValueError, TypeError):
        return True # It's not a number

def _identify_header(csv_path, max_rows_to_check=5):
    """
    Identifies and cleans the header of a CSV file, which may span multiple rows.
    """
    if not os.path.exists(csv_path):
        return None, 0

    try:
        # Read the first few rows without assuming any header
        df_peek = pd.read_csv(csv_path, header=None, nrows=max_rows_to_check, na_filter=False)

        header_rows_count = 0
        for index, row in df_peek.iterrows():
            non_empty_cells = [cell for cell in row if str(cell).strip() != ""]
            if not non_empty_cells:
                break  # Stop at an empty row

            if all(is_likely_header(cell) for cell in non_empty_cells):
                header_rows_count += 1
            else:
                break  # Stop at the first data row

        if header_rows_count == 0:
            # Fallback to single header if no header rows were identified
            header_rows_count = 1

        # Read just the header rows
        header_df = pd.read_csv(csv_path, header=None, nrows=header_rows_count, na_filter=False)

        # Combine header rows
        if header_rows_count > 1:
            # Transpose, fill forward, and then join
            temp_header_df = header_df.T
            temp_header_df.ffill(inplace=True)
            combined_header = temp_header_df.apply(lambda x: ' '.join(x.dropna().astype(str)), axis=1)
        else:
            combined_header = header_df.iloc[0]

        # Clean column names
        cleaned_columns = []
        for col in combined_header:
            clean_col = re.sub(r'[\n\r]+', ' ', str(col))
            clean_col = re.sub(r'\s+', ' ', clean_col).strip()
            cleaned_columns.append(clean_col)

        return cleaned_columns, header_rows_count

    except Exception as e:
        print(f"  [!] Error identifying header in {os.path.basename(csv_path)}: {e}")
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

    # Normalize column names for a more robust comparison (case, space, and underscore insensitive)
    norm_actual = [col.lower().replace(' ', '').replace('_', '') for col in actual_columns]
    norm_ideal = [col.lower().replace(' ', '').replace('_', '') for col in IDEAL_COLUMNS]

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
