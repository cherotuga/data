import os
import pandas as pd
import re
import argparse
from file_dispatcher import dispatch_file
from hierarchical_health_data_processor import identify_hierarchy
from sentence_transformers import SentenceTransformer, util
import torch


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

# Global model and concepts for semantic search to avoid reloading
MODEL = SentenceTransformer('all-MiniLM-L6-v2')
HEALTH_CONCEPTS = [
    'health', 'medical', 'hospital', 'curative', 'preventive',
    'sanitation', 'disease control', 'pharmaceuticals', 'maternal care', 'child health'
]
HEALTH_CONCEPTS_EMBEDDINGS = MODEL.encode(HEALTH_CONCEPTS, convert_to_tensor=True)

def is_health_related_semantic(text, health_concepts_embeddings, threshold=0.5):
    """
    Checks if a given text is semantically related to health concepts using sentence embeddings.
    """
    if not text or pd.isna(text):
        return False

    text_embedding = MODEL.encode(text, convert_to_tensor=True)
    cosine_scores = util.cos_sim(text_embedding, health_concepts_embeddings)

    if torch.max(cosine_scores) > threshold:
        return True
    return False

def parse_health_data_from_csv(csv_path):
    """
    Parses a single CSV file to find and extract health-related program data.
    Uses semantic search and robust hierarchical processing.
    """
    df = dispatch_file(csv_path)
    if df is None:
        return None, None, None

    # --- Data Cleaning and Standardization ---
    for col in ['budget', 'expenditure']:
        if col in df.columns:
            df[col] = pd.to_numeric(df[col], errors='coerce').fillna(0)

    # --- Semantic Filtering for Health Data ---
    df['program_str'] = df['program'].astype(str)

    # Identify health-related departments
    df['is_health_dept'] = df['program_str'].apply(
        lambda x: is_health_related_semantic(x, HEALTH_CONCEPTS_EMBEDDINGS)
    )

    health_dept_indices = df[df['is_health_dept']].index

    if health_dept_indices.empty:
        return None, None, None

    # --- Extract and Process Health Department Blocks ---
    all_depts, all_progs, all_sub_progs = [], [], []

    for start_index in health_dept_indices:
        # Find the end of the current department block
        end_index = len(df)
        for i in range(start_index + 1, len(df)):
            # The block ends when a new department starts
            if pd.notna(df['program'].iloc[i]) and df['program'].iloc[i].strip():
                end_index = i
                break

        health_block_df = df.iloc[start_index:end_index].copy()

        # --- Hierarchical Processing ---
        df_dept, df_prog, df_sub_prog = identify_hierarchy(health_block_df)

        all_depts.append(df_dept)
        all_progs.append(df_prog)
        all_sub_progs.append(df_sub_prog)

    if not all_depts:
        return None, None, None

    # Concatenate results from all found health departments in the file
    final_df_dept = pd.concat(all_depts, ignore_index=True) if all_depts else pd.DataFrame()
    final_df_prog = pd.concat(all_progs, ignore_index=True) if all_progs else pd.DataFrame()
    final_df_sub_prog = pd.concat(all_sub_progs, ignore_index=True) if all_sub_progs else pd.DataFrame()

    # --- Metadata Extraction ---
    try:
        path_parts = csv_path.replace('\\', '/').split('/')
        year = path_parts[-4]
        quarter = path_parts[-3]
        county = os.path.basename(path_parts[-1]).split('_')[0]
    except IndexError:
        year, quarter, county = 'unknown', 'unknown', 'unknown'

    # Add metadata to each dataframe
    for temp_df in [final_df_dept, final_df_prog, final_df_sub_prog]:
        if not temp_df.empty:
            temp_df['county'] = county
            temp_df['year'] = year
            temp_df['quarter'] = quarter

    return final_df_dept, final_df_prog, final_df_sub_prog


def main(year=None, quarter=None, all_available=False):
    """
    Main pipeline to process pre-extracted CSVs and aggregate health data.
    """
    program_data_dir = 'program'
    print(f"--- Starting Health Data Aggregation from: '{program_data_dir}' ---")

    if all_available:
        year, quarter = None, None
        print("Processing all available years and quarters.")
    elif year and quarter:
        print(f"Processing data for year {year}, quarter {quarter}.")
    # ... (rest of the argument handling is fine)

    all_csvs = find_all_csv_files(program_data_dir, year, quarter)

    # --- Aggregation for each level ---
    agg_depts = []
    agg_progs = []
    agg_sub_progs = []

    for csv_file in all_csvs:
        df_dept, df_prog, df_sub_prog = parse_health_data_from_csv(csv_file)

        if df_dept is not None and not df_dept.empty:
            agg_depts.append(df_dept)
        if df_prog is not None and not df_prog.empty:
            agg_progs.append(df_prog)
        if df_sub_prog is not None and not df_sub_prog.empty:
            agg_sub_progs.append(df_sub_prog)

        if (df_dept is not None and not df_dept.empty):
             print(f"  [+] Extracted hierarchical health data from {os.path.basename(csv_file)}")
        else:
             print(f"  [-] No health data extracted from {os.path.basename(csv_file)}")


    # --- Save final aggregated data ---
    if agg_depts:
        final_depts = pd.concat(agg_depts, ignore_index=True)
        final_depts.to_csv("health_department_total.csv", index=False)
        print("\n--- Saved department totals to 'health_department_total.csv' ---")
        print(final_depts.head())

    if agg_progs:
        final_progs = pd.concat(agg_progs, ignore_index=True)
        final_progs.to_csv("health_program_total.csv", index=False)
        print("\n--- Saved program totals to 'health_program_total.csv' ---")
        print(final_progs.head())

    if agg_sub_progs:
        final_sub_progs = pd.concat(agg_sub_progs, ignore_index=True)
        final_sub_progs.to_csv("health_sub_program_total.csv", index=False)
        print("\n--- Saved sub-program totals to 'health_sub_program_total.csv' ---")
        print(final_sub_progs.head())

    if not agg_depts and not agg_progs and not agg_sub_progs:
        print("\n--- Pipeline Finished: No health data was found in any of the scanned files. ---")


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
