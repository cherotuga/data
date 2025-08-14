import os
import pandas as pd
import re
import argparse
from file_dispatcher import dispatch_file
from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.metrics.pairwise import cosine_similarity
import numpy as np

# Debugging


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

def parse_health_data_from_csv(csv_path):
    """
    Parses a single CSV file to find and extract health-related program data.
    Uses the dispatcher, which now returns a DataFrame with standardized column names.
    """
    df = dispatch_file(csv_path)
    if df is None:
        return None

    # --- Data Cleaning and Standardization ---
    # Convert budget and expenditure columns to numeric, coercing errors to NaN
    for col in ['budget', 'expenditure']:
        if col in df.columns:
            df[col] = pd.to_numeric(df[col], errors='coerce')

    # Forward-fill program names to handle hierarchical structures
    df['program'] = df['program'].ffill()

    # Ensure program and sub_program are strings for searching
    df['program'] = df['program'].astype(str)
    df['sub_program'] = df['sub_program'].astype(str)

    # --- Semantic Filtering with Scikit-learn ---
    # Final refined list of health-related terms to be more specific
    health_terms = [
        'human healthcare', 'medical services', 'hospital services', 'curative care',
        'preventive healthcare', 'pharmaceuticals', 'clinic operations', 'maternal healthcare',
        'child healthcare', 'infant healthcare', 'nutrition services', 'vaccination program',
        'primary health', 'mental health services', 'hospital construction',
        'clinic renovation', 'healthcare financing', 'medical supplies',
        'laboratory services', 'ambulance services', 'community health workers'
    ]

    # Combine program and sub_program into a single text column for analysis
    df['full_description'] = df['program'].fillna('') + ' ' + df['sub_program'].fillna('')

    # Handle cases where the description might be empty
    if df['full_description'].str.strip().eq('').all():
        return None

    # Vectorize the descriptions and health terms
    vectorizer = TfidfVectorizer(stop_words='english')
    all_text = list(df['full_description']) + health_terms
    tfidf_matrix = vectorizer.fit_transform(all_text)

    # Split the matrix back into descriptions and terms
    description_vectors = tfidf_matrix[:len(df)]
    term_vectors = tfidf_matrix[len(df):]

    # Calculate cosine similarity between each description and all health terms
    # We take the max similarity for each description against all health terms
    cosine_similarities = cosine_similarity(description_vectors, term_vectors)
    max_similarities = np.max(cosine_similarities, axis=1)

    # Set a similarity threshold
    similarity_threshold = 0.4

    # Filter rows that meet the similarity threshold
    is_health_related = max_similarities >= similarity_threshold
    health_df = df[is_health_related].copy()

    if health_df.empty:
        return None

    # Add the similarity score to the dataframe for analysis (optional)
    health_df['health_similarity_score'] = max_similarities[is_health_related]

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
    # Select and rename columns to the final desired output format
    health_df = health_df.rename(columns={
        'budget': 'approved_budget_kshs',
        'expenditure': 'actual_expenditure_kshs'
    })

    # Ensure all required columns are present
    final_columns = [
        'county', 'year', 'quarter', 'program', 'sub_program',
        'approved_budget_kshs', 'actual_expenditure_kshs', 'health_similarity_score'
    ]
    for col in final_columns:
        if col not in health_df.columns:
            health_df[col] = None # Add missing columns and fill with None

    return health_df[final_columns]


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
        else:
            print(f"  [-] No health data extracted from {os.path.basename(csv_file)} (Year: {file_year}, Quarter: {file_quarter})")

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
