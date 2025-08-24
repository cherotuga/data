import pandas as pd
import numpy as np

def identify_hierarchy(df):
    """
    Identifies the hierarchical structure of the data for a single department block.
    """
    if df.empty:
        return pd.DataFrame(), pd.DataFrame(), pd.DataFrame()

    # --- Department Identification ---
    department_name = str(df.iloc[0]['program']).strip()
    department_row = df.iloc[0]

    # --- Pre-processing ---
    df = df.copy()
    df['program_str'] = df['program'].astype(str).str.strip().str.lower()
    df['sub_program_str'] = df['sub_program'].astype(str).str.strip().str.lower()

    subtotal_keywords = ['total', 'sub-total', 'sub total']
    df['is_subtotal'] = df['program_str'].str.contains('|'.join(subtotal_keywords), na=False) | \
                        df['sub_program_str'].str.contains('|'.join(subtotal_keywords), na=False)

    # --- Hierarchical Parsing ---
    programs = []
    sub_programs = []
    current_program_name = None

    # Iterate from the second row onwards
    for index, row in df.iloc[1:].iterrows():
        if row['is_subtotal']:
            continue

        program_val = str(row.get('program', '')).strip()
        sub_program_val = str(row.get('sub_program', '')).strip()

        # Heuristic: A non-empty 'program' field signifies a new program
        if program_val and program_val.lower() != 'nan' and program_val != department_name:
            current_program_name = program_val
            programs.append({
                'department': department_name,
                'program': current_program_name,
                'budget': row.get('budget', 0),
                'expenditure': row.get('expenditure', 0)
            })
        # A non-empty 'sub_program' field signifies a program or sub-program
        elif sub_program_val and sub_program_val.lower() != 'nan':
            # If there's no program context, it's a program
            if not current_program_name:
                 current_program_name = sub_program_val
                 programs.append({
                    'department': department_name,
                    'program': current_program_name,
                    'budget': row.get('budget', 0),
                    'expenditure': row.get('expenditure', 0)
                })
            # Otherwise, it is a sub-program of the current program
            else:
                sub_programs.append({
                    'department': department_name,
                    'program': current_program_name,
                    'sub_program': sub_program_val,
                    'budget': row.get('budget', 0),
                    'expenditure': row.get('expenditure', 0)
                })

    df_programs = pd.DataFrame(programs)
    df_sub_programs = pd.DataFrame(sub_programs)

    # --- Subtotal Calculation ---
    # Calculate department total from the sum of its programs
    if not df_programs.empty:
        dept_total_budget = df_programs['budget'].sum()
        dept_total_expenditure = df_programs['expenditure'].sum()
    else:
        # If no programs, use the department row's values (minus its own contribution if it was also counted as a program)
        dept_total_budget = department_row.get('budget', 0)
        dept_total_expenditure = department_row.get('expenditure', 0)

    df_departments = pd.DataFrame([{
        'department': department_name,
        'budget': dept_total_budget,
        'expenditure': dept_total_expenditure
    }])

    # Calculate program totals from their sub-programs if any
    if not df_sub_programs.empty:
        prog_calculated_totals = df_sub_programs.groupby(['department', 'program'])[['budget', 'expenditure']].sum().reset_index()

        # Update program totals with calculated sums
        if not df_programs.empty:
            # Keep original program entries, but update their totals from sub-programs
            df_programs = df_programs.set_index(['department', 'program'])
            prog_calculated_totals = prog_calculated_totals.set_index(['department', 'program'])
            df_programs.update(prog_calculated_totals)
            df_programs.reset_index(inplace=True)
        else:
            df_programs = prog_calculated_totals


    return df_departments, df_programs, df_sub_programs
