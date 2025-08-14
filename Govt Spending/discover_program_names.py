import os
import sys
import pandas as pd
from collections import defaultdict

# Add the script's directory to the Python path to allow importing sibling modules
script_dir = os.path.dirname(os.path.abspath(__file__))
sys.path.append(script_dir)

# Now we can import the function from the sibling module
from file_dispatcher import dispatch_file

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

def discover_and_report_program_names(root_dir):
    """
    Discovers all unique program and sub-program names in the CSV files and reports them.
    """
    csv_files = find_all_program_csv_files(root_dir)
    if not csv_files:
        print("No CSV files found to analyze.")
        return

    all_program_names = set()
    all_sub_program_names = set()
    file_count = 0

    for csv_path in csv_files:
        file_count += 1
        print(f"Processing file {file_count}/{len(csv_files)}: {os.path.basename(csv_path)}")
        df = dispatch_file(csv_path)

        if df is not None:
            if 'program' in df.columns:
                # Ensure all values are treated as strings before finding unique ones
                all_program_names.update(df['program'].dropna().astype(str).unique())
            if 'sub_program' in df.columns:
                # Ensure all values are treated as strings
                all_sub_program_names.update(df['sub_program'].dropna().astype(str).unique())

    # --- Reporting ---
    print("\n--- Unique Program and Sub-Program Names Report ---")
    print(f"Scanned {file_count} files.\n")

    print(f"--- Found {len(all_program_names)} Unique Program Names ---")
    for name in sorted(list(all_program_names)):
        print(f"- {name}")

    print(f"\n--- Found {len(all_sub_program_names)} Unique Sub-Program Names ---")
    for name in sorted(list(all_sub_program_names)):
        print(f"- {name}")


if __name__ == "__main__":
    # Set the working directory to the location of the script
    os.chdir(script_dir)
    # The 'program' directory is a sibling to the script
    program_data_dir = 'program'
    discover_and_report_program_names(program_data_dir)
