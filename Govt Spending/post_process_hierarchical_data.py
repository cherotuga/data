import pandas as pd
import os

def process_data(input_file, details_file, dept_file, prog_file, sub_prog_file):
    df = pd.read_csv(input_file)

    # Clean up column names
    df.columns = [col.strip() for col in df.columns]

    # Add new columns
    df['department_new'] = ''
    df['program_new'] = ''
    df['sub_program_new'] = ''

    # Forward fill the program and department columns
    if 'department' in df.columns:
        df['department'].ffill(inplace=True)
    if 'program' in df.columns:
        df['program'].ffill(inplace=True)

    # Iterate through the DataFrame and populate the new columns
    for i, row in df.iterrows():
        if 'department' in df.columns:
            df.at[i, 'department_new'] = row['department']
        if 'program' in df.columns:
            df.at[i, 'program_new'] = row['program']
        if 'sub_program' in df.columns:
            df.at[i, 'sub_program_new'] = row['sub_program']

    # Extract totals
    dept_totals = df[df['program'].str.contains('total', case=False, na=False) & df['sub_program'].isna()]
    prog_totals = df[df['program'].str.contains('total', case=False, na=False) & df['sub_program'].notna()]
    sub_prog_totals = df[df['sub_program'].str.contains('total', case=False, na=False)]

    # Get details
    details_mask = ~df['program'].str.contains('total', case=False, na=False) & ~df['sub_program'].str.contains('total', case=False, na=False)
    details = df[details_mask]

    # Save files
    details.to_csv(details_file, index=False)
    dept_totals.to_csv(dept_file, index=False)
    prog_totals.to_csv(prog_file, index=False)
    sub_prog_totals.to_csv(sub_prog_file, index=False)

if __name__ == '__main__':
    # Get the directory of the script
    script_dir = os.path.dirname(os.path.abspath(__file__))

    process_data(
        os.path.join(script_dir, 'health_spending_summary.csv'),
        os.path.join(script_dir, 'health_spending_summary_details.csv'),
        os.path.join(script_dir, 'health_department_total.csv'),
        os.path.join(script_dir, 'health_program_total.csv'),
        os.path.join(script_dir, 'health_sub_program_total.csv')
    )
