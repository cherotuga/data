import pandas as pd
import os
import argparse
import numpy as np


def clean_medicine_table(input_file, name_col, dose_form_col, strength_col, output_file=None):
    """
    Post-process the extracted medicine table to:
    1. Keep only the specified columns: 'Name of Medicine', 'Dose-form', 'Strength / Size'
    2. Fill down the 'Name of Medicine' values to ensure each row has a medicine name
    3. Fill down the 'Dose-form' values within groups of the same medicine and dose form
    
    Args:
        input_file: Path to the input CSV file (combined medicine tables)
        name_col: Column name or index for 'Name of Medicine'
        dose_form_col: Column name or index for 'Dose-form'
        strength_col: Column name or index for 'Strength / Size'
        output_file: Path to save the cleaned CSV file (if None, will use input_file_cleaned.csv)
    
    Returns:
        DataFrame with cleaned data
    """
    print(f"Reading input file: {input_file}")
    
    # Read the CSV file
    try:
        df = pd.read_csv(input_file)
        print(f"Successfully read file with {len(df)} rows and {len(df.columns)} columns")
    except Exception as e:
        print(f"Error reading file: {str(e)}")
        return None
    
    # Display column names to help with debugging
    print("Available columns:")
    for i, col in enumerate(df.columns):
        print(f"  {i}: {col}")
    
    # Convert column indices to column names if needed
    if isinstance(name_col, int):
        try:
            name_col = df.columns[name_col]
            print(f"Using column index {name_col} for Name of Medicine")
        except IndexError:
            print(f"Error: Column index {name_col} is out of bounds")
            return None
    
    if isinstance(dose_form_col, int):
        try:
            dose_form_col = df.columns[dose_form_col]
            print(f"Using column index {dose_form_col} for Dose-form")
        except IndexError:
            print(f"Error: Column index {dose_form_col} is out of bounds")
            return None
    
    if isinstance(strength_col, int):
        try:
            strength_col = df.columns[strength_col]
            print(f"Using column index {strength_col} for Strength / Size")
        except IndexError:
            print(f"Error: Column index {strength_col} is out of bounds")
            return None
    
    print(f"\nUsing columns:")
    print(f"  Name of Medicine: '{name_col}'")
    print(f"  Dose-form: '{dose_form_col}'")
    print(f"  Strength / Size: '{strength_col}'")
    
    
    # Create a new DataFrame with only the selected columns
    try:
        # Check if columns exist
        for col in [name_col, dose_form_col, strength_col]:
            if col not in df.columns:
                print(f"Error: Column '{col}' not found in the input file")
                return None
        
        cleaned_df = df[[name_col, dose_form_col, strength_col]].copy()
        
        # Rename columns to standardized names
        cleaned_df.columns = ['Name of Medicine', 'Dose-form', 'Strength / Size']
        
        # First, fill down the 'Name of Medicine' column
        cleaned_df['Name of Medicine'] = cleaned_df['Name of Medicine'].ffill()
        
        # Create a helper column to identify medicine groups
        cleaned_df['medicine_group'] = (cleaned_df['Name of Medicine'] != cleaned_df['Name of Medicine'].shift()).cumsum()
        
        # Create a helper column to identify dose form groups within medicine groups
        # A new dose form group starts when:
        # 1. The dose form is not empty AND
        # 2. Either the previous dose form was empty OR it was different
        cleaned_df['dose_form_change'] = (
            cleaned_df['Dose-form'].notna() & 
            ((cleaned_df['Dose-form'].shift().isna()) | 
             (cleaned_df['Dose-form'] != cleaned_df['Dose-form'].shift()))
        )
        
        # Create a dose form group identifier
        cleaned_df['dose_form_group'] = cleaned_df.groupby('medicine_group')['dose_form_change'].cumsum()
        
        # Now fill the dose form within each medicine+dose_form group
        cleaned_df['Dose-form'] = cleaned_df.groupby(['medicine_group', 'dose_form_group'])['Dose-form'].ffill()
        
        # Drop the helper columns
        cleaned_df = cleaned_df.drop(['medicine_group', 'dose_form_change', 'dose_form_group'], axis=1)
        
        # Remove rows where all values are NaN
        cleaned_df = cleaned_df.dropna(how='all')
        
        print(f"\nCleaned data: {len(cleaned_df)} rows")
        
        # Save the cleaned data
        if output_file is None:
            base, ext = os.path.splitext(input_file)
            output_file = f"{base}_cleaned{ext}"
        
        cleaned_df.to_csv(output_file, index=False)
        print(f"Cleaned data saved to: {output_file}")
        
        # Also save as Excel for better formatting
        excel_output = os.path.splitext(output_file)[0] + ".xlsx"
        cleaned_df.to_excel(excel_output, index=False)
        print(f"Cleaned data also saved to Excel: {excel_output}")
        
        return cleaned_df
    
    except Exception as e:
        print(f"Error during cleaning: {str(e)}")
        return None

def main():
    print("=== Kenya Medicine Table Cleaner ===")

    # Default values
    default_input = "extracted_tables/combined_medicine_tables.csv"
    default_output = "outputs/cleaned_medicines.csv"
    default_name_col = 5
    default_dose_col = 6
    default_strength_col = 7

    # Prompt user
    input_file = input(f"Input CSV file path [{default_input}]: ").strip() or default_input
    output_file = input(f"Output CSV file path [{default_output}]: ").strip() or default_output
    name_col = input(f"Column name/index for 'Name of Medicine' [{default_name_col}]: ").strip() or default_name_col
    dose_col = input(f"Column name/index for 'Dose-form' [{default_dose_col}]: ").strip() or default_dose_col
    strength_col = input(f"Column name/index for 'Strength / Size' [{default_strength_col}]: ").strip() or default_strength_col

    # Try to convert to int if it's a number
    for var in ['name_col', 'dose_col', 'strength_col']:
        val = locals()[var]
        try:
            locals()[var] = int(val)
        except ValueError:
            pass

    clean_medicine_table(
        input_file=input_file,
        name_col=name_col,
        dose_form_col=dose_col,
        strength_col=strength_col,
        output_file=output_file
    )

if __name__ == "__main__":
    main()


