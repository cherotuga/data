"""
Test framework for program.py CSV extraction validation.

Usage:
    python test_program.py
    pytest test_program.py -v
"""

import os
import pytest
import subprocess
from pathlib import Path

# Expected line counts for CSV files (to be populated manually)
EXPECTED_LINE_COUNTS = {
    # Format: 'year_quarter_county': expected_lines
    # Example entries - replace with actual verified counts
    '2019_20_01_baringo': 166,
    # '2019_20_01_bomet': 89,
    # '2019_20_01_busia': 156,

    '2019_20_02_baringo': 174,

    '2019_20_03_Baringo': 165,

    '2019_20_04_baringo': 155,

    '2020_21_01_baringo': 151,

    '2020_21_02_baringo': 180,

    '2020_21_03_baringo': 248,

    '2020_21_04_baringo': 255,

    '2021_22_01_baringo': 291,

    '2021_22_02_baringo': 228,

    '2021_22_03_baringo': 192,

    '2021_22_04_baringo': 222,

    '2022_23_01_baringo': 238,

    '2022_23_02_baringo': 206,

    '2022_23_03_baringo': 136,

    '2022_23_04_baringo': 169,

    '2023_24_01_baringo': 363,

    '2023_24_02_baringo': 383,

    '2023_24_03_baringo': 464,

    '2023_24_04_baringo': 469,

    '2024_25_01_baringo': 408,

    '2024_25_02_baringo': 487,

    '2024_25_03_baringo': 336,

    '2019_20_01_nakuru': 159,

    '2019_20_02_nakuru': 208,

    '2019_20_03_nakuru': 169,

    '2019_20_04_nakuru': 185,

    '2020_21_01_nakuru': 167,

    '2020_21_02_nakuru': 180,

    '2020_21_03_nakuru': 201,

    '2020_21_04_nakuru': 234,

    '2021_22_01_nakuru': 315,

    '2021_22_02_nakuru': 602,

    '2021_22_03_nakuru': 425,

    '2021_22_04_nakuru': 442,

    '2022_23_01_nakuru': 438,

    '2022_23_02_nakuru': 442,

    '2022_23_03_nakuru': 337,

    '2022_23_04_nakuru': 575,

    '2023_24_01_nakuru': 459,

    '2023_24_02_nakuru': 496,

    '2023_24_03_nakuru': 367,

    '2023_24_04_nakuru': 402,

    '2024_25_01_nakuru': 500,

    '2024_25_02_nakuru': 560,

    '2024_25_03_nakuru': 392,
}

def get_csv_line_count(file_path):
    """Get line count of a CSV file."""
    if not os.path.exists(file_path):
        return 0
    
    with open(file_path, 'r', encoding='utf-8') as f:
        return sum(1 for line in f if line.strip())  # Count non-empty lines

def test_program_csv_extraction():
    """Test that program.py generates CSVs with expected line counts."""
    program_dir = Path('program')
    
    if not program_dir.exists():
        pytest.skip("Program directory does not exist. Run 'python program.py --all' first.")
    
    missing_files = []
    incorrect_counts = []
    
    for key, expected_count in EXPECTED_LINE_COUNTS.items():
        # Parse key format: year_quarter_county
        parts = key.split('_')
        if len(parts) >= 3:
            year = f"{parts[0]}_{parts[1]}"  # e.g., "2019_20"
            quarter = parts[2]               # e.g., "01"
            county = '_'.join(parts[3:])     # e.g., "baringo" or "elgeyo_marakwet"
            
            csv_path = program_dir / year / quarter / 'county' / f'{county}_programme_table.csv'
            
            if not csv_path.exists():
                missing_files.append(str(csv_path))
                continue
                
            actual_count = get_csv_line_count(csv_path)
            if actual_count != expected_count:
                incorrect_counts.append({
                    'file': str(csv_path),
                    'expected': expected_count,
                    'actual': actual_count
                })
    
    # Report results
    if missing_files:
        pytest.fail(f"Missing CSV files:\n" + '\n'.join(missing_files))
    
    if incorrect_counts:
        error_msg = "Incorrect line counts:\n"
        for item in incorrect_counts:
            error_msg += f"  {item['file']}: expected {item['expected']}, got {item['actual']}\n"
        pytest.fail(error_msg)

def test_program_extraction_runs():
    """Test that program.py can run without errors on a sample."""
    # This test ensures the basic functionality works
    result = subprocess.run(['python', 'program.py', '--help'], 
                          capture_output=True, text=True)
    assert result.returncode == 0, f"program.py --help failed: {result.stderr}"

if __name__ == '__main__':
    # Run tests directly
    test_program_extraction_runs()
    test_program_csv_extraction()
    print("All tests passed!")