import os
import pandas as pd
import re

def find_all_csv_files(root_dir):
    """
    Recursively finds all CSV files in a given directory.
    """
    csv_files = []
    print(f"Searching for CSV files in: {root_dir}")
    if not os.path.isdir(root_dir):
        print(f"  [!] Directory not found: {root_dir}")
        return []

    for subdir, _, files in os.walk(root_dir):
        for file in files:
            if file.endswith('_programme_table.csv'):
                csv_files.append(os.path.join(subdir, file))
    print(f"Found {len(csv_files)} potential program CSV files.")
    return csv_files

def clean_numeric_value(value):
    """
    Cleans a string value to be a float.
    - Removes commas
    - Treats '-' as 0
    """
    if isinstance(value, str):
        value = value.replace(',', '').strip()
        if value == '-':
            return 0.0
    try:
        return float(value)
    except (ValueError, TypeError):
        return 0.0

def parse_health_data_from_csv(csv_path):
    """
    Parses a single CSV file to find and extract health-related program data.
    """
    try:
        df = pd.read_csv(csv_path)
        # Standardize column names: remove leading/trailing spaces, newlines.
        df.columns = [str(col).strip().replace('\n', ' ') for col in df.columns]
    except Exception as e:
        #print(f"  [!] Could not read or process {os.path.basename(csv_path)}: {e}")
        return None

    health_data = []
    current_programme = ''
    health_keywords = ['health', 'curative', 'preventive', 'medical', 'hospital']

    # Heuristic: Check if any health keywords exist in the entire CSV first.
    # This can help us skip clearly irrelevant files faster.
    if not any(df.apply(lambda row: row.astype(str).str.contains('|'.join(health_keywords), case=False).any(), axis=1)):
        return None

    for _, row in df.iterrows():
        # Handle hierarchical program structure: carry forward the last valid program name.
        if 'Programmes' in row and pd.notna(row['Programmes']) and row['Programmes'].strip():
            current_programme = row['Programmes'].strip()

        # Identify health-related rows based on keywords in program or sub-program
        programme_text = current_programme.lower()
        sub_programme_text = str(row.get('Sub- Programmes', '')).lower()

        is_health_related = False
        if any(keyword in programme_text for keyword in health_keywords) or \
           any(keyword in sub_programme_text for keyword in health_keywords):
            is_health_related = True

        # Skip rows that are not health-related or are summary rows
        if not is_health_related or 'total' in sub_programme_text or 'total' in programme_text:
            continue

        # Extract data from the row
        budget = clean_numeric_value(row.get('Approved Budget (Kshs)'))
        expenditure = clean_numeric_value(row.get('Actual Payments (Kshs)'))

        # Extract metadata from file path, handling potential errors
        try:
            path_parts = csv_path.replace('\\', '/').split('/')
            year = path_parts[-4]
            quarter = path_parts[-3]
            county = os.path.basename(path_parts[-1]).split('_')[0]
        except IndexError:
            year, quarter, county = 'unknown', 'unknown', 'unknown'

        health_data.append({
            'county': county,
            'year': year,
            'quarter': quarter,
            'program': current_programme,
            'sub_program': row.get('Sub- Programmes'),
            'approved_budget_kshs': budget,
            'actual_expenditure_kshs': expenditure,
        })

    if not health_data:
        return None

    return pd.DataFrame(health_data)


def main():
    """
    Main pipeline to process pre-extracted CSVs and aggregate health data.
    """
    program_data_dir = 'program'
    print(f"--- Starting Health Data Aggregation from: '{program_data_dir}' ---")

    # Step 1: Find all relevant CSV files
    all_csvs = find_all_csv_files(program_data_dir)

    # Step 2 & 3: Parse health data and aggregate
    aggregated_data = []
    for csv_file in all_csvs:
        health_df = parse_health_data_from_csv(csv_file)
        if health_df is not None:
            print(f"  [+] Extracted {len(health_df)} health record(s) from {os.path.basename(csv_file)}")
            aggregated_data.append(health_df)

    # Step 4: Save the final aggregated data
    if aggregated_data:
        final_df = pd.concat(aggregated_data, ignore_index=True)
        output_csv = "health_spending_summary.csv"
        final_df.to_csv(output_csv, index=False)
        print(f"\n--- Pipeline Finished: Successfully saved aggregated data to '{output_csv}' ---")
        print(f"Final DataFrame shape: {final_df.shape}")
        print("Sample of final data:")
        print(final_df.head())
    else:
        print("\n--- Pipeline Finished: No health data was found in any of the scanned files. ---")


if __name__ == "__main__":
    # Ensure the script runs from its own directory context
    script_dir = os.path.dirname(os.path.abspath(__file__))
    os.chdir(script_dir)
    main()
