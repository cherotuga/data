import os
import pandas as pd
import re
import argparse

def find_all_csv_files(root_dir, year=None, quarter=None):
    """
    Recursively finds all CSV files in a given directory, optionally filtering by year and quarter.
    """
    csv_files = []
    print(f"Searching for CSV files in: {root_dir}")
    if not os.path.isdir(root_dir):
        print(f"  [!] Directory not found: {root_dir}")
        return []

    year_str = None
    if year:
        # Format year to match '2019_20'
        year_str = f"{year}_{str(year + 1)[-2:]}"

    quarter_str = None
    if quarter:
        # Format quarter to match '01'
        quarter_str = f"{quarter:02d}"

    for subdir, _, files in os.walk(root_dir):
        for file in files:
            if file.endswith('_programme_table.csv'):
                path_parts = subdir.replace('\\', '/').split('/')
                # path_parts for program/2019_20/01/county is ['program', '2019_20', '01', 'county']
                if len(path_parts) >= 3:
                    file_year = path_parts[1]
                    file_quarter = path_parts[2]

                    if (year is None or file_year == year_str) and \
                       (quarter is None or file_quarter == quarter_str):
                        csv_files.append(os.path.join(subdir, file))
                elif year is None and quarter is None:
                    # Include files in root of 'program' only when no filters are applied
                    csv_files.append(os.path.join(subdir, file))

    print(f"Found {len(csv_files)} potential program CSV files.")
    return sorted(csv_files)

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


def main(year=None, quarter=None, all_available=False):
    """
    Main pipeline to process pre-extracted CSVs and aggregate health data.
    """
    program_data_dir = 'program'
    print(f"--- Starting Health Data Aggregation from: '{program_data_dir}' ---")

    # Determine which files to process based on arguments
    if all_available:
        year, quarter = None, None
        print("Processing all available years and quarters.")
    elif year and quarter:
        print(f"Processing data for year {year}, quarter {quarter}.")
    elif year:
        print(f"Processing all quarters for year {year}.")
        quarter = None
    elif quarter:
        print(f"Processing all years for quarter {quarter}.")
        year = None
    else:
        # Default behavior if no arguments are provided
        print("No specific year or quarter specified. Processing all available data.")
        year, quarter = None, None

    # Step 1: Find all relevant CSV files
    all_csvs = find_all_csv_files(program_data_dir, year, quarter)

    # Step 2 & 3: Parse health data and aggregate
    aggregated_data = []
    for csv_file in all_csvs:
        health_df = parse_health_data_from_csv(csv_file)
        if health_df is not None and not health_df.empty:
            # Extract year and quarter from the dataframe for the printout
            file_year = health_df['year'].iloc[0]
            file_quarter = health_df['quarter'].iloc[0]
            print(f"  [+] Extracted {len(health_df)} health record(s) from {os.path.basename(csv_file)} (Year: {file_year}, Quarter: {file_quarter})")
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
        print("\n--- Pipeline Finished: No health data was found in any of the scanned files for the specified criteria. ---")


if __name__ == "__main__":
    # Ensure the script runs from its own directory context
    script_dir = os.path.dirname(os.path.abspath(__file__))
    os.chdir(script_dir)

    parser = argparse.ArgumentParser(description="Aggregate health spending data from pre-extracted CSV files.")
    parser.add_argument("--year", "-y", type=int, help="Specify the financial year to process (e.g., 2022 for 2022_23).")
    parser.add_argument("--quarter", "-q", type=int, choices=[1, 2, 3, 4], help="Specify the quarter to process (1-4).")
    parser.add_argument("--all", "-a", action="store_true", help="Process all available CSV files, ignoring year and quarter filters.")

    args = parser.parse_args()

    # If --all is used, or if no arguments are given at all, process everything.
    process_all = args.all or (args.year is None and args.quarter is None)

    main(year=args.year, quarter=args.quarter, all_available=process_all)
