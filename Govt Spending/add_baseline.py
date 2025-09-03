#!/usr/bin/env python3
"""
Helper script to add verified line counts to test_program.py.

Usage:
    # Add a single county verification
    python add_baseline.py --year 2019_20 --quarter 01 --county baringo --lines 134

    # Auto-discover and show current line counts (for manual verification)
    python add_baseline.py --discover

    # Add multiple entries from a file
    python add_baseline.py --batch baseline_data.txt
"""

import argparse
import os
import re
from pathlib import Path

def get_current_line_count(year, quarter, county):
    """Get current line count for a CSV file."""
    csv_path = Path('program') / year / quarter / 'county' / f'{county}_programme_table.csv'
    if not csv_path.exists():
        return 0
    
    with open(csv_path, 'r', encoding='utf-8') as f:
        return sum(1 for line in f if line.strip())

def add_baseline_entry(year, quarter, county, line_count):
    """Add a baseline entry to test_program.py."""
    key = f"{year}_{quarter}_{county}"
    entry = f"    '{key}': {line_count},"
    
    # Read current test file
    with open('test_program.py', 'r') as f:
        content = f.read()
    
    # Find the EXPECTED_LINE_COUNTS dict
    pattern = r'(EXPECTED_LINE_COUNTS = {[^}]*)(})'
    match = re.search(pattern, content, re.DOTALL)
    
    if match:
        before_dict = match.group(1)
        after_dict = match.group(2)
        
        # Add new entry if not already present
        if key not in before_dict:
            new_content = content.replace(
                before_dict + after_dict,
                before_dict + '\n' + entry + '\n' + after_dict
            )
            
            with open('test_program.py', 'w') as f:
                f.write(new_content)
            print(f"Added baseline: {key} = {line_count}")
        else:
            print(f"Baseline already exists: {key}")

def discover_existing_csvs():
    """Discover all existing CSV files and their line counts."""
    program_dir = Path('program')
    if not program_dir.exists():
        print("Program directory not found. Run 'python program.py --all' first.")
        return
    
    print("Existing CSV files and line counts:")
    print("Format: year_quarter_county = line_count")
    print("-" * 50)
    
    for year_dir in sorted(program_dir.iterdir()):
        if year_dir.is_dir():
            for quarter_dir in sorted(year_dir.iterdir()):
                if quarter_dir.is_dir():
                    county_dir = quarter_dir / 'county'
                    if county_dir.exists():
                        for csv_file in sorted(county_dir.glob('*_programme_table.csv')):
                            county = csv_file.stem.replace('_programme_table', '')
                            line_count = get_current_line_count(year_dir.name, quarter_dir.name, county)
                            key = f"{year_dir.name}_{quarter_dir.name}_{county}"
                            print(f"{key} = {line_count}")

def main():
    parser = argparse.ArgumentParser(description='Add baseline data to test_program.py')
    parser.add_argument('--year', help='Year (e.g., 2019_20)')
    parser.add_argument('--quarter', help='Quarter (e.g., 01)')
    parser.add_argument('--county', help='County name (e.g., baringo)')
    parser.add_argument('--lines', type=int, help='Expected line count')
    parser.add_argument('--discover', action='store_true', help='Show all existing CSV line counts')
    
    args = parser.parse_args()
    
    if args.discover:
        discover_existing_csvs()
    elif args.year and args.quarter and args.county and args.lines:
        add_baseline_entry(args.year, args.quarter, args.county, args.lines)
    else:
        parser.print_help()

if __name__ == '__main__':
    main()