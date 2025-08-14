import os
import sys
import pandas as pd
from collections import defaultdict

# Add the script's directory to the Python path to allow importing sibling modules
script_dir = os.path.dirname(os.path.abspath(__file__))
sys.path.append(script_dir)

# Now we can import the function from the sibling module
from file_dispatcher import _identify_header

def find_all_program_csv_files(root_dir):
    """
    Recursively finds all '*_programme_table.csv' files in the given directory.
    """
    csv_files = []
    print(f"Searching for program CSV files in: {root_dir}")
    if not os.path.isdir(root_dir):
        print(f"  [!] Directory not found: {root_dir}")
        return []

    for subdir, _, files in os.walk(root_dir):
        for file in files:
            if file.endswith('_programme_table.csv'):
                csv_files.append(os.path.join(subdir, file))

    print(f"Found {len(csv_files)} program CSV files.")
    return sorted(csv_files)

def discover_and_report_variations(root_dir):
    """
    Discovers all unique column header variations in the CSV files and reports them.
    """
    csv_files = find_all_program_csv_files(root_dir)
    if not csv_files:
        print("No CSV files found to analyze.")
        return

    header_variations = defaultdict(list)
    file_count = 0

    for csv_path in csv_files:
        file_count += 1
        # The _identify_header function returns a list of cleaned column names and the number of header rows
        columns, _ = _identify_header(csv_path)

        if columns:
            # Create a tuple of the column list so it can be used as a dictionary key
            header_tuple = tuple(columns)
            header_variations[header_tuple].append(os.path.basename(csv_path))
        else:
            # Log files where header extraction failed
            header_variations[("HEADER_EXTRACTION_FAILED",)].append(os.path.basename(csv_path))

    # --- Reporting ---
    print("\n--- Column Header Variation Report ---")
    print(f"Scanned {file_count} files.")
    print(f"Found {len(header_variations)} unique header variations.\n")

    variation_count = 0
    for headers, files in header_variations.items():
        variation_count += 1
        print(f"--- Variation #{variation_count} (Found in {len(files)} files) ---")
        print("Columns:")
        for i, col in enumerate(headers):
            print(f"  {i+1}: '{col}'")

        # To keep the report clean, show up to 3 example files for each variation
        if len(files) > 3:
            print(f"Example files: {files[:3]} and {len(files) - 3} more.")
        else:
            print(f"Example files: {files}")
        print("-" * 40)

if __name__ == "__main__":
    # Set the working directory to the location of the script
    os.chdir(script_dir)
    # The 'program' directory is a sibling to the script
    program_data_dir = 'program'
    discover_and_report_variations(program_data_dir)
