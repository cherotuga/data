#!/usr/bin/env python3
"""
Helper script to add verified baselines to test files.

Usage:
    # Add CSV line count baseline (program.py)
    python add_baseline.py --year 2019_20 --quarter 01 --county baringo --lines 134

    # Add health records baseline (baringo-scraping.py)
    python add_baseline.py --year 2019_20 --quarter 01 --county baringo --health-records 9

    # Auto-discover current line counts and health records
    python add_baseline.py --discover
    python add_baseline.py --discover --health-records

    # Add multiple entries from a file
    python add_baseline.py --batch baseline_data.txt
"""

# Suppress common warnings
import os
os.environ['TOKENIZERS_PARALLELISM'] = 'false'  # Disable tokenizers parallelism warnings
os.environ['MKL_NUM_THREADS'] = '1'             # Avoid numpy/MKL threading issues

import argparse
import re
from pathlib import Path

def get_current_line_count(year, quarter, county):
    """Get current line count for a CSV file."""
    normalized_year = normalize_year_format(year)
    csv_path = Path('program') / normalized_year / quarter / 'county' / f'{county}_programme_table.csv'
    if not csv_path.exists():
        return 0
    
    with open(csv_path, 'r', encoding='utf-8') as f:
        return sum(1 for line in f if line.strip())

def get_current_health_records_count(year, quarter, county):
    """Get current health records count using HealthBudgetAnalyzer."""
    normalized_year = normalize_year_format(year)
    csv_path = Path('program') / normalized_year / quarter / 'county' / f'{county}_programme_table.csv'
    if not csv_path.exists():
        return 0
    
    try:
        # Import here to avoid issues if baringo-scraping.py is not available
        from baringo_scraping import HealthBudgetAnalyzer
        
        analyzer = HealthBudgetAnalyzer(str(csv_path))
        health_data = analyzer.run_full_analysis()
        
        if health_data is None or health_data.empty:
            return 0
            
        return len(health_data)
        
    except Exception as e:
        print(f"Error analyzing {csv_path}: {e}")
        return 0

def normalize_year_format(year):
    """Convert year to proper YYYY_YY format (e.g., '2019' -> '2019_20', '2019_20' -> '2019_20')."""
    if re.match(r'^\d{4}_\d{2}$', year):
        # Already in correct format
        return year
    elif re.match(r'^\d{4}$', year):
        # Convert single year to financial year format
        year_int = int(year)
        next_year_suffix = str(year_int + 1)[-2:]
        return f"{year}_{next_year_suffix}"
    else:
        raise ValueError(f"Invalid year format: {year}. Expected YYYY or YYYY_YY format.")

def add_baseline_entry(year, quarter, county, line_count):
    """Add a baseline entry to test_program.py."""
    normalized_year = normalize_year_format(year)
    key = f"{normalized_year}_{quarter}_{county}"
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
            print(f"Added CSV line baseline: {key} = {line_count}")
        else:
            print(f"CSV line baseline already exists: {key}")

def add_health_records_baseline_entry(year, quarter, county, health_records_count):
    """Add a health records baseline entry to test_baringo_scraping.py."""
    normalized_year = normalize_year_format(year)
    key = f"{normalized_year}_{quarter}_{county}"
    entry = f"    '{key}': {health_records_count},"
    
    # Read current test file
    with open('test_baringo_scraping.py', 'r') as f:
        content = f.read()
    
    # Find the EXPECTED_HEALTH_RECORDS dict
    pattern = r'(EXPECTED_HEALTH_RECORDS = {[^}]*)(})'
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
            
            with open('test_baringo_scraping.py', 'w') as f:
                f.write(new_content)
            print(f"Added health records baseline: {key} = {health_records_count}")
        else:
            print(f"Health records baseline already exists: {key}")

def discover_existing_csvs(health_records=False, counties=None):
    """Discover all existing CSV files and their line counts or health records counts."""
    program_dir = Path('program')
    if not program_dir.exists():
        print("Program directory not found. Run 'python program.py --all' first.")
        return

    if health_records:
        print("Existing CSV files and health records counts:")
        print("Format: year_quarter_county = health_records_count")
        print("(This may take a while as it runs health analysis on each file)")
    else:
        print("Existing CSV files and line counts:")
        print("Format: year_quarter_county = line_count")

    if counties:
        print(f"Filtering for counties: {', '.join(counties)}")

    print("-" * 60)

    added_count = 0

    for year_dir in sorted(program_dir.iterdir()):
        if year_dir.is_dir():
            for quarter_dir in sorted(year_dir.iterdir()):
                if quarter_dir.is_dir():
                    county_dir = quarter_dir / 'county'
                    if county_dir.exists():
                        for csv_file in sorted(county_dir.glob('*_programme_table.csv')):
                            county = csv_file.stem.replace('_programme_table', '')

                            # Filter by county if specified
                            if counties and county not in counties:
                                continue

                            if health_records:
                                count = get_current_health_records_count(year_dir.name, quarter_dir.name, county)
                                metric = "health_records"
                                add_health_records_baseline_entry(year_dir.name, quarter_dir.name, county, count)
                            else:
                                count = get_current_line_count(year_dir.name, quarter_dir.name, county)
                                metric = "lines"
                                add_baseline_entry(year_dir.name, quarter_dir.name, county, count)

                            key = f"{year_dir.name}_{quarter_dir.name}_{county}"
                            print(f"'{key}': {count},  # {metric}")
                            added_count += 1

    print("-" * 60)
    print(f"Total entries processed: {added_count}")

def main():
    parser = argparse.ArgumentParser(description='Add baseline data to test files')
    parser.add_argument('--year', help='Year (e.g., 2019_20)')
    parser.add_argument('--quarter', help='Quarter (e.g., 01)')
    parser.add_argument('--county', help='County name (e.g., baringo)')
    parser.add_argument('--counties', nargs='+', help='County names to filter when using --discover (e.g., baringo nakuru)')
    parser.add_argument('--lines', type=int, help='Expected line count (for test_program.py)')
    parser.add_argument('--health-records', nargs='?', const=True, type=lambda x: int(x) if x.isdigit() else True, help='Expected health records count (for test_baringo_scraping.py), or use with --discover to analyze health records')
    parser.add_argument('--discover', action='store_true', help='Show and add all existing counts')

    args = parser.parse_args()

    if args.discover:
        # Check if --health-records flag was passed (True) or has a value
        if args.health_records is not None and (args.health_records is True or isinstance(args.health_records, bool)):
            discover_existing_csvs(health_records=True, counties=args.counties)
        else:
            discover_existing_csvs(health_records=False, counties=args.counties)
    elif args.year and args.quarter and args.county:
        if args.lines is not None:
            add_baseline_entry(args.year, args.quarter, args.county, args.lines)
        elif args.health_records is not None:
            add_health_records_baseline_entry(args.year, args.quarter, args.county, args.health_records)
        else:
            print("Error: Must specify either --lines or --health-records")
            parser.print_help()
    else:
        parser.print_help()

if __name__ == '__main__':
    main()