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