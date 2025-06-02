# Created using manus ai.
# Prompt was: I would like to scrape the tables of medicine in this pdf 
# (not the ones in the appendix because that is deletions/additions.) 
# Then I want them to be put together in one table. 
# I would like this in python and to be able to run this on my machine so also provide me working code.
import os
import pandas as pd
from tabula.io import read_pdf
import PyPDF2
import re
from tqdm import tqdm

def extract_tables_from_pdf(pdf_path, output_dir, max_page=104):
    """
    Extract tables from a PDF file up to a specified page (excluding appendix)
    
    Args:
        pdf_path: Path to the PDF file
        output_dir: Directory to save extracted tables
        max_page: Maximum page number to extract (exclusive of appendix)
    
    Returns:
        List of dataframes containing the extracted tables
    """
    print(f"Extracting tables from {pdf_path} (pages 1-{max_page})...")
    
    # Create output directory if it doesn't exist
    os.makedirs(output_dir, exist_ok=True)
    
    # Get total number of pages in the PDF
    with open(pdf_path, 'rb') as f:
        pdf_reader = PyPDF2.PdfReader(f)
        total_pages = len(pdf_reader.pages)
    
    print(f"Total pages in PDF: {total_pages}")
    print(f"Extracting tables from pages 1 to {max_page} (excluding appendix)")
    
    # Extract all tables from the PDF (up to max_page)
    all_tables = []
    
    # Use tqdm for progress tracking
    for page in tqdm(range(1, min(max_page + 1, total_pages + 1))):
        try:
            # Extract tables from the current page
            tables = read_pdf(
                pdf_path,
                pages=page,
                multiple_tables=True,
                guess=True,
                lattice=True,
                stream=True
            )
            
            # Filter out empty tables
            tables = [table for table in tables if not table.empty]
            
            # Add page number information to each table
            for i, table in enumerate(tables):
                # Add metadata as attributes
                table.attrs = {'page': page, 'table_index': i}
                all_tables.append(table)
                
                # Save individual table to CSV for inspection
                table_filename = f"page_{page}_table_{i}.csv"
                table_path = os.path.join(output_dir, table_filename)
                table.to_csv(table_path, index=False)
                
        except Exception as e:
            print(f"Error extracting tables from page {page}: {str(e)}")
    
    print(f"Extracted {len(all_tables)} tables from {pdf_path}")
    return all_tables

def clean_table(df):
    """
    Clean and preprocess a table dataframe
    
    Args:
        df: Pandas dataframe to clean
    
    Returns:
        Cleaned dataframe
    """
    # Drop completely empty rows and columns
    df = df.dropna(how='all').dropna(axis=1, how='all')
    
    # Reset index
    df = df.reset_index(drop=True)
    
    # Clean column names - remove newlines and extra spaces
    if df.columns is not None:
        df.columns = [str(col).strip().replace('\r', ' ').replace('\n', ' ') for col in df.columns]
        df.columns = [re.sub(r'\s+', ' ', col) for col in df.columns]
    
    # Clean cell values - remove extra whitespace
    for col in df.columns:
        if df[col].dtype == 'object':
            df[col] = df[col].apply(lambda x: re.sub(r'\s+', ' ', str(x).strip()) if pd.notna(x) else x)
    
    return df

def combine_tables(tables):
    """
    Combine multiple tables into a single dataframe with source information
    
    Args:
        tables: List of dataframes to combine
    
    Returns:
        Combined dataframe with source information
    """
    if not tables:
        return pd.DataFrame()
    
    # Clean each table
    cleaned_tables = []
    for table in tables:
        # Get metadata
        page = table.attrs.get('page', 'unknown')
        table_index = table.attrs.get('table_index', 'unknown')
        
        # Clean the table
        cleaned_table = clean_table(table)
        
        # Add source information columns
        cleaned_table['Source_Page'] = page
        cleaned_table['Source_Table'] = table_index
            
        cleaned_tables.append(cleaned_table)
    
    # Combine all tables
    # We'll use a list of dataframes and concat at the end for better performance
    if cleaned_tables:
        combined_df = pd.concat(cleaned_tables, ignore_index=True)
        return combined_df
    else:
        return pd.DataFrame()

def validate_combined_table(df):
    """
    Validate the combined table to ensure it contains only main section data
    
    Args:
        df: Combined dataframe to validate
    
    Returns:
        Validated dataframe
    """
    if df.empty:
        return df
    
    # Check for appendix-related keywords in column names or content
    appendix_keywords = ['appendix', 'addition', 'deletion', 'reference']
    
    # Filter out rows that might be from appendix based on content
    for col in df.columns:
        if df[col].dtype == 'object':
            mask = df[col].str.contains('|'.join(appendix_keywords), case=False, na=False)
            if mask.any():
                print(f"Found {mask.sum()} potential appendix-related rows based on content in column '{col}'")
                # Instead of dropping, mark these rows for review
                df.loc[mask, 'Potential_Appendix'] = True
    
    # If 'Potential_Appendix' column was added, ensure it's filled with False for other rows
    if 'Potential_Appendix' in df.columns:
        df['Potential_Appendix'] = df['Potential_Appendix'].fillna(False)
    
    return df

def main():
    # Define paths
    script_dir = os.path.dirname(os.path.abspath(__file__))
    pdf_path = input("Enter the path to the Kenya Essential Medicines List PDF file: ")
    
    # Use default path if input is empty
    if not pdf_path.strip():
        pdf_path = os.path.join(script_dir, "Kenya Essential Medicines List 2023.pdf")
    
    # Validate PDF path
    if not os.path.exists(pdf_path):
        print(f"Error: File not found at {pdf_path}")
        return
    
    # Create output directory
    output_dir = os.path.join(script_dir, "extracted_tables")
    os.makedirs(output_dir, exist_ok=True)
    
    # Extract tables from the PDF (excluding appendix)
    tables = extract_tables_from_pdf(pdf_path, output_dir, max_page=104)
    
    # Combine all tables into a single dataframe
    combined_df = combine_tables(tables)
    
    # Validate the combined table
    validated_df = validate_combined_table(combined_df)
    
    # Save the combined and validated table
    if not validated_df.empty:
        combined_path = os.path.join(output_dir, "combined_medicine_tables.csv")
        validated_df.to_csv(combined_path, index=False)
        print(f"Combined table saved to {combined_path}")
        
        # Also save as Excel for better formatting
        excel_path = os.path.join(output_dir, "combined_medicine_tables.xlsx")
        validated_df.to_excel(excel_path, index=False)
        print(f"Combined table also saved to {excel_path}")
        
        # Print summary statistics
        print("\nSummary Statistics:")
        print(f"Total tables extracted: {len(tables)}")
        print(f"Total rows in combined table: {len(validated_df)}")
        print(f"Total columns in combined table: {len(validated_df.columns)}")
        
        # If potential appendix content was flagged, save a separate file for review
        if 'Potential_Appendix' in validated_df.columns and validated_df['Potential_Appendix'].any():
            review_path = os.path.join(output_dir, "potential_appendix_content.xlsx")
            validated_df[validated_df['Potential_Appendix']].to_excel(review_path, index=False)
            print(f"\nWARNING: Some content may be from appendix. Review file saved to {review_path}")
    else:
        print("No tables were extracted or combined.")

if __name__ == "__main__":
    main()
