"""
File Dispatcher and Parser

This module handles the initial processing of raw CSV files. It is responsible for:
- Identifying and cleaning column headers, even if they span multiple rows.
- Intellegently mapping real-world column headers to a standard schema using semantic similarity.
- Loading the data into a standardized pandas DataFrame.

This helps to decouple the file parsing logic from the data analysis logic,
adhering to the Single Responsibility Principle.
"""

import pandas as pd
import os
import re
from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.metrics.pairwise import cosine_similarity

# --- Target Schema Definition ---
# Defines the ideal columns and keywords to search for.
# Using a list of keywords for each target is more effective for TF-IDF.
TARGET_SCHEMA = {
    "program": ["program", "programme", "programmes", "vote head", "department"],
    "sub_program": ["sub-program", "sub programme", "sub-programmes", "item", "description"],
    "budget": ["budget", "approved", "estimates", "submitted"],
    "expenditure": ["expenditure", "actual", "payments", "total payments"]
}

# --- Core Functions ---

def is_likely_header(cell_content):
    """
    Determines if a cell's content is likely part of a header.
    - Returns False if it's a number (int or float).
    - Returns False if it's empty or NA.
    - Returns True otherwise (likely text).
    """
    if pd.isna(cell_content) or cell_content is None or str(cell_content).strip() == "":
        return False
    try:
        float(str(cell_content).replace(",", ""))
        return False
    except (ValueError, TypeError):
        return True

def _identify_header(csv_path, max_rows_to_check=5):
    """
    Identifies and cleans the header of a CSV file, which may span multiple rows.
    """
    if not os.path.exists(csv_path):
        return None, 0

    try:
        # Explicitly set the separator to a comma for robustness.
        df_peek = pd.read_csv(csv_path, header=None, nrows=max_rows_to_check, na_filter=False, encoding='utf-8', sep=',', engine='python')

        header_rows_count = 0
        for index, row in df_peek.iterrows():
            non_empty_cells = [cell for cell in row if str(cell).strip() != ""]
            if not non_empty_cells:
                break

            if all(is_likely_header(cell) for cell in non_empty_cells):
                header_rows_count += 1
            else:
                break

        if header_rows_count == 0:
            header_rows_count = 1

        # Explicitly set the separator here as well.
        header_df = pd.read_csv(csv_path, header=None, nrows=header_rows_count, na_filter=False, encoding='utf-8', sep=',', engine='python')

        if header_rows_count > 1:
            temp_header_df = header_df.T
            temp_header_df.ffill(inplace=True)
            combined_header = temp_header_df.apply(lambda x: ' '.join(x.dropna().astype(str)), axis=1)
        else:
            combined_header = header_df.iloc[0]

        cleaned_columns = []
        for col in combined_header:
            clean_col = re.sub(r'[\n\r]+', ' ', str(col))
            clean_col = re.sub(r'\s+', ' ', clean_col).strip().lower()
            cleaned_columns.append(clean_col)

        return cleaned_columns, header_rows_count

    except Exception as e:
        # print(f"  [!] Error identifying header in {os.path.basename(csv_path)}: {e}")
        return None, 0


def map_columns_semantic(column_list, confidence_threshold=0.2):
    """
    Maps a list of messy, real-world column names to the TARGET_SCHEMA using TF-IDF and cosine similarity.
    This version uses a more effective corpus building strategy.
    """
    if not column_list:
        return None, "No columns provided"

    # Create a flat list of all target keywords and a corresponding list of their labels
    target_labels = []
    target_keywords = []
    for label, keywords in TARGET_SCHEMA.items():
        for keyword in keywords:
            target_labels.append(label)
            target_keywords.append(keyword)

    corpus = target_keywords + column_list

    try:
        vectorizer = TfidfVectorizer(min_df=1, ngram_range=(1, 3))
        tfidf_matrix = vectorizer.fit_transform(corpus)

        target_matrix = tfidf_matrix[:len(target_keywords)]
        actual_matrix = tfidf_matrix[len(target_keywords):]

        similarity_matrix = cosine_similarity(target_matrix, actual_matrix)

        mapping = {}
        used_indices = set()

        # Iterate through each CANONICAL label we want to find
        for canonical_label in TARGET_SCHEMA.keys():
            best_overall_score = -1
            best_match_index = -1

            # Find all the rows in the similarity matrix that correspond to the current canonical label
            for i, label in enumerate(target_labels):
                if label == canonical_label:
                    # For each keyword, find the best matching real column
                    scores = similarity_matrix[i]
                    for j, score in enumerate(scores):
                        if j not in used_indices and score > best_overall_score:
                            best_overall_score = score
                            best_match_index = j

            if best_match_index != -1 and best_overall_score >= confidence_threshold:
                mapping[canonical_label] = column_list[best_match_index]
                used_indices.add(best_match_index)

        required_columns = ["program", "budget", "expenditure"]
        missing_columns = [col for col in required_columns if col not in mapping]

        if missing_columns:
            return None, f"Failed to find required columns: {', '.join(missing_columns)}"

        return mapping, None

    except Exception as e:
        return None, f"Error during semantic mapping: {e}"


def dispatch_file(csv_path):
    """
    Reads a CSV file, semantically maps its columns to a standard, and returns a DataFrame.
    This function now intelligently handles variations in column naming.
    """
    actual_columns, header_rows = _identify_header(csv_path)

    if not actual_columns:
        return None # Header identification failed

    # Get the mapping from actual columns to our canonical schema
    column_mapping, error = map_columns_semantic(actual_columns)

    if error:
        # Silently skip files that fail mapping
        return None

    # We need the original names of the columns we are interested in for the read_csv `usecols` parameter
    cols_to_use = list(column_mapping.values())

    try:
        df = pd.read_csv(
            csv_path,
            skiprows=header_rows,
            header=None, # We've already processed headers
            names=actual_columns, # Use the actual (cleaned) column names
            usecols=cols_to_use,
            na_values=['-'],
            thousands=',',
            encoding='utf-8',
            sep=',',
            engine='python'
        )

        # Create the reverse mapping to rename columns to the canonical names
        rename_dict = {v: k for k, v in column_mapping.items()}
        df.rename(columns=rename_dict, inplace=True)

        # Ensure all target columns exist, filling missing optional ones with None
        for col in TARGET_SCHEMA.keys():
            if col not in df.columns:
                df[col] = None

        return df

    except Exception as e:
        # Silently skip files that fail during the read process
        return None
