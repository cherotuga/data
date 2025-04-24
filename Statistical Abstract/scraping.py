import tabula
import pandas as pd
import os

import os

# Get the directory of the current script
script_dir = os.path.dirname(os.path.abspath(__file__))
os.chdir(script_dir)

def extract_tables_from_pdf(pdf_path, pages='all', output_format='csv'):
    """
    Extract tables from a PDF file.
    
    Args:
        pdf_path (str): Path to the PDF file
        pages (str or list): Page numbers to extract tables from. Default 'all'
        output_format (str): Format to save tables in ('csv' or 'excel')
    
    Returns:
        list: List of pandas DataFrames containing the extracted tables
    """
    try:
        # Verify if file exists
        if not os.path.exists(pdf_path):
            raise FileNotFoundError(f"PDF file not found at: {pdf_path}")
            
        # Extract all tables from the PDF
        tables = tabula.read_pdf(
            pdf_path,
            pages=pages,
            multiple_tables=True,
            guess=True,
            pandas_options={'header': None}  # Don't assume first row is header
        )
        
        if not tables:
            print("No tables found in the PDF.")
            return []
            
        # Clean the extracted tables
        cleaned_tables = []
        for i, table in enumerate(tables):
            # Remove empty rows and columns
            table.dropna(how='all', axis=0, inplace=True)
            table.dropna(how='all', axis=1, inplace=True)
            
            # Reset index after dropping rows
            table.reset_index(drop=True, inplace=True)
            
            cleaned_tables.append(table)
            
            # Save individual tables based on specified format
            base_name = os.path.splitext(pdf_path)[0]
            if output_format.lower() == 'csv':
                table.to_csv(f"{base_name}_table_{i+1}.csv", index=False)
            elif output_format.lower() == 'excel':
                table.to_excel(f"{base_name}_table_{i+1}.xlsx", index=False)
        
        return cleaned_tables
    
    except Exception as e:
        print(f"Error extracting tables: {str(e)}")
        return []

def main():
    pdf_path = "pdf/2024-Statistical-Abstract.pdf"
    
    # Extract tables from all pages and save as CSV
    tables = extract_tables_from_pdf(pdf_path, output_format='csv')
    
    # Print information about extracted tables
    print(f"\nFound {len(tables)} tables in the PDF")
    
    for i, table in enumerate(tables, 1):
        print(f"\nTable {i} Shape: {table.shape}")
        print("\nFirst few rows of Table {i}:")
        print(table.head())

if __name__ == "__main__":
    main()
