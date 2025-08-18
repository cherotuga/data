import os
import pandas as pd
import re
import argparse
from file_dispatcher import dispatch_file

# --- New Imports for Semantic Search ---
# Note: You will need to install the sentence-transformers library.
# Run: pip install sentence-transformers
from sentence_transformers import SentenceTransformer, util

# --- Global Model Initialization ---
# Load the pre-trained model once to be reused across all function calls.
# This is more efficient than loading it inside the function every time.
# The model will be downloaded from the internet on its first run.
print("Loading semantic search model...")
try:
    model = SentenceTransformer('all-MiniLM-L6-v2')
    print("Model loaded successfully.")
except Exception as e:
    print(f"Error loading model: {e}")
    print("Please ensure you have an internet connection and the required libraries are installed.")
    model = None

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
        year_str = f"{year}_{str(year + 1)[-2:]}"

    quarter_str = None
    if quarter:
        quarter_str = f"{quarter:02d}"

    for subdir, _, files in os.walk(root_dir):
        for file in files:
            if file.endswith('_programme_table.csv'):
                path_parts = subdir.replace('\\', '/').split('/')
                if len(path_parts) >= 3:
                    file_year = path_parts[1]
                    file_quarter = path_parts[2]

                    if (year is None or file_year == year_str) and \
                       (quarter is None or file_quarter == quarter_str):
                        csv_files.append(os.path.join(subdir, file))
                elif year is None and quarter is None:
                    csv_files.append(os.path.join(subdir, file))

    print(f"Found {len(csv_files)} potential program CSV files.")
    return sorted(csv_files)

def parse_health_data_from_csv(csv_path):
    """
    Parses a single CSV file to find and extract health-related program data.
    This version uses a semantic search model to identify health-related spending.
    """
    if model is None:
        print("  [!] Semantic search model not loaded. Skipping file.")
        return None

    df = dispatch_file(csv_path)
    if df is None:
        return None

    # --- Data Cleaning and Standardization ---
    for col in ['budget', 'expenditure']:
        if col in df.columns:
            df[col] = pd.to_numeric(df[col], errors='coerce')

    df['program'] = df['program'].ffill().astype(str)
    df['sub_program'] = df['sub_program'].fillna('').astype(str)

    # --- Semantic Search Filtering ---
    # Define a set of reference sentences that capture the essence of "health spending".
    health_references = [
        "expenditure on public health services",
        "curative and rehabilitative healthcare",
        "medical supplies and hospital equipment",
        "preventive health programs and initiatives",
        "maternal and child health services",
        "ambulance and emergency medical services",
        "funding for county hospitals and clinics"
    ]

    # Create embeddings for the reference sentences. This is done once per run of the function.
    health_embeddings = model.encode(health_references, convert_to_tensor=True)

    # Combine program and sub_program columns to create a single descriptive text for each row.
    df['full_description'] = df['program'] + ' ' + df['sub_program']

    # Generate embeddings for all program/sub-program descriptions in the DataFrame.
    description_embeddings = model.encode(df['full_description'].tolist(), convert_to_tensor=True)

    # Calculate cosine similarity between each description and all health references.
    cosine_scores = util.cos_sim(description_embeddings, health_embeddings)

    # Find the maximum similarity score for each description against the set of health references.
    max_scores = cosine_scores.max(axis=1).values

    # Define a confidence threshold.
    similarity_threshold = 0.5

    # Create a boolean mask for rows that meet the similarity threshold.
    is_health_related = (max_scores > similarity_threshold).cpu().numpy()

    health_df = df[is_health_related].copy()

    if health_df.empty:
        return None

    # --- Metadata Extraction ---
    try:
        path_parts = csv_path.replace('\\', '/').split('/')
        year = path_parts[-4]
        quarter = path_parts[-3]
        county = os.path.basename(path_parts[-1]).split('_')[0]
    except IndexError:
        year, quarter, county = 'unknown', 'unknown', 'unknown'

    health_df['county'] = county
    health_df['year'] = year
    health_df['quarter'] = quarter

    # --- Final Column Selection and Renaming ---
    health_df = health_df.rename(columns={
        'budget': 'approved_budget_kshs',
        'expenditure': 'actual_expenditure_kshs'
    })

    final_columns = [
        'county', 'year', 'quarter', 'program', 'sub_program',
        'approved_budget_kshs', 'actual_expenditure_kshs'
    ]
    for col in final_columns:
        if col not in health_df.columns:
            health_df[col] = None

    return health_df[final_columns]


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
    elif year:
        print(f"Processing all quarters for year {year}.")
        quarter = None
    elif quarter:
        print(f"Processing all years for quarter {quarter}.")
        year = None
    else:
        print("No specific year or quarter specified. Processing all available data.")
        year, quarter = None, None

    all_csvs = find_all_csv_files(program_data_dir, year, quarter)

    aggregated_data = []
    processed_files = 0
    for csv_file in all_csvs:
        processed_files += 1
        print(f"Processing file {processed_files}/{len(all_csvs)}: {os.path.basename(csv_file)}")
        health_df = parse_health_data_from_csv(csv_file)
        if health_df is not None and not health_df.empty:
            file_year = health_df['year'].iloc[0]
            file_quarter = health_df['quarter'].iloc[0]
            print(f"  [+] Extracted {len(health_df)} health record(s) from {os.path.basename(csv_file)} (Year: {file_year}, Quarter: {file_quarter})")
            aggregated_data.append(health_df)
        else:
            # This is very common, so we can reduce the verbosity.
            # print(f"  [-] No health data extracted from {os.path.basename(csv_file)}")
            pass

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
    script_dir = os.path.dirname(os.path.abspath(__file__))
    os.chdir(script_dir)

    parser = argparse.ArgumentParser(description="Aggregate health spending data from pre-extracted CSV files.")
    parser.add_argument("--year", "-y", type=int, help="Specify the financial year to process (e.g., 2022 for 2022_23).")
    parser.add_argument("--quarter", "-q", type=int, choices=[1, 2, 3, 4], help="Specify the quarter to process (1-4).")
    parser.add_argument("--all", "-a", action="store_true", help="Process all available CSV files, ignoring year and quarter filters.")

    args = parser.parse_args()

    process_all = args.all or (args.year is None and args.quarter is None)

    main(year=args.year, quarter=args.quarter, all_available=process_all)
