import pandas as pd
from sentence_transformers import SentenceTransformer, util

def classify_row_semantic(row_text, model, confidence_threshold=0.3):
    """
    Classifies a row as 'department', 'program', or 'sub_program' using semantic similarity.
    """
    if not row_text or pd.isna(row_text):
        return 'unknown'

    # Reference embeddings for each category
    reference_embeddings = {
        'department': model.encode(['department total', 'ministry total', 'grand total']),
        'program': model.encode(['program total', 'sub-total for program']),
        'sub_program': model.encode(['sub-program total', 'item total'])
    }

    # Encode the row text
    row_embedding = model.encode(row_text)

    # Calculate cosine similarity with each reference category
    max_similarity = 0
    best_category = 'unknown'

    for category, refs in reference_embeddings.items():
        similarities = util.pytorch_cos_sim(row_embedding, refs)
        max_cat_similarity = similarities.max()
        if max_cat_similarity > max_similarity:
            max_similarity = max_cat_similarity
            best_category = category

    if max_similarity > confidence_threshold:
        return best_category
    else:
        return 'unknown'

def extract_hierarchical_data(df, model):
    """
    Extracts department, program, and sub-program totals from the DataFrame using a hybrid approach.
    """
    department_totals = []
    program_totals = []
    sub_program_totals = []

    for _, row in df.iterrows():
        # Use a combination of program and sub_program for classification
        text_to_classify = f"{row.get('program', '')} {row.get('sub_program', '')}"

        # Get the semantic classification
        classification = classify_row_semantic(text_to_classify, model)

        if classification == 'department':
            department_totals.append(row)
        elif classification == 'program':
            program_totals.append(row)
        elif classification == 'sub_program':
            sub_program_totals.append(row)

    department_df = pd.DataFrame(department_totals) if department_totals else pd.DataFrame()
    program_df = pd.DataFrame(program_totals) if program_totals else pd.DataFrame()
    sub_program_df = pd.DataFrame(sub_program_totals) if sub_program_totals else pd.DataFrame()

    return department_df, program_df, sub_program_df

def process_hierarchical_data(df, model):
def process_hierarchical_data(df, model):
    """
    Main function to process a DataFrame and extract hierarchical totals.
    """
    # No need to pre-identify total rows, the extractor will do it
    department_df, program_df, sub_program_df = extract_hierarchical_data(df, model)

    # Create a mask for all total rows
    total_indices = set()
    if not department_df.empty:
        total_indices.update(department_df.index)
    if not program_df.empty:
        total_indices.update(program_df.index)
    if not sub_program_df.empty:
        total_indices.update(sub_program_df.index)

    details_df = df[~df.index.isin(total_indices)]

    return {
        "department": department_df,
        "program": program_df,
        "sub_program": sub_program_df,
        "details": details_df
    }
