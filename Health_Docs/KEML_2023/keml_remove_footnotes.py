import pandas as pd
import re
import argparse
import os

def remove_footnotes(text):
    """
    Remove footnote numbers from text
    
    Args:
        text: Text string that may contain footnote numbers
        
    Returns:
        Cleaned text with footnote numbers removed
    """
    if pd.isna(text) or text is None:
        return text
    
    # Convert to string if not already
    text = str(text)
    
    # Remove footnote numbers (digits at the end of words)
    # This pattern matches digits at the end of the string
    cleaned_text = re.sub(r'(\D+)(\d+)$', r'\1', text)
    
    # Also handle superscript numbers in the middle (e.g., "Name123 text")
    cleaned_text = re.sub(r'(\D+)(\d+)(\s+)', r'\1\3', cleaned_text)
    
    # Remove any trailing whitespace
    cleaned_text = cleaned_text.strip()
    
    return cleaned_text

def clean_csv_footnotes(input_file, output_file=None, columns=None):
    """
    Clean footnote numbers from specified columns in a CSV file
    
    Args:
        input_file: Path to input CSV file
        output_file: Path to output CSV file (if None, will use input_file_cleaned.csv)
        columns: List of column names to clean (if None, will clean all columns)
        
    Returns:
        DataFrame with cleaned data
    """
    print(f"Reading input file: {input_file}")
    
    try:
        # Read the CSV file
        df = pd.read_csv(input_file)
        print(f"Successfully read file with {len(df)} rows and {len(df.columns)} columns")
        
        # If no columns specified, use all columns
        if columns is None:
            columns = df.columns
            print("No columns specified, will clean all columns")
        else:
            # Verify specified columns exist
            for col in columns:
                if col not in df.columns:
                    print(f"Warning: Column '{col}' not found in input file")
            
            # Filter to only existing columns
            columns = [col for col in columns if col in df.columns]
            
            if not columns:
                print("Error: None of the specified columns exist in the input file")
                return None
                
            print(f"Will clean the following columns: {', '.join(columns)}")
        
        # Apply footnote removal to each specified column
        for col in columns:
            print(f"Cleaning column: {col}")
            df[col] = df[col].apply(remove_footnotes)
        
        # Determine output file path
        if output_file is None:
            base, ext = os.path.splitext(input_file)
            output_file = f"{base}_no_footnotes{ext}"
        
        # Save the cleaned data
        df.to_csv(output_file, index=False)
        print(f"Cleaned data saved to: {output_file}")
        
        # Also save as Excel for better formatting
        excel_output = os.path.splitext(output_file)[0] + ".xlsx"
        df.to_excel(excel_output, index=False)
        print(f"Cleaned data also saved to Excel: {excel_output}")
        
        return df
        
    except Exception as e:
        print(f"Error processing file: {str(e)}")
        return None

def main():
    print("=== Remove Footnotes from Medicine CSV ===")

    # Default paths and example column guesses
    default_input = "outputs/cleaned_medicines.csv"
    default_output = "outputs/no_footnotes_medicines.csv"

    # Prompt user
    input_file = input(f"Input CSV path [{default_input}]: ").strip() or default_input
    output_file = input(f"Output CSV path [{default_output}]: ").strip() or default_output

    # Ask user whether to specify columns
    col_input = input("Specify columns to clean (comma-separated, leave blank to clean all): ").strip()
    if col_input:
        columns = [col.strip() for col in col_input.split(",")]
    else:
        columns = None

    clean_csv_footnotes(input_file=input_file, output_file=output_file, columns=columns)

if __name__ == "__main__":
    main()

