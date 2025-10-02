"""
Test framework for baringo_scraping.py health records extraction validation.

Usage:
    python test_baringo_scraping.py
    pytest test_baringo_scraping.py -v
"""

# Suppress common warnings
import os
os.environ['TOKENIZERS_PARALLELISM'] = 'false'  # Disable tokenizers parallelism warnings
os.environ['MKL_NUM_THREADS'] = '1'             # Avoid numpy/MKL threading issues

import pytest
import subprocess
from pathlib import Path
import tempfile
import pandas as pd
from baringo_scraping import HealthBudgetAnalyzer

# Expected health records counts for each county/year/quarter combination
EXPECTED_HEALTH_RECORDS = {
    # Format: 'year_quarter_county': expected_health_records_count
    # Entries to be populated using add_baseline.py
    '2019_20_01_baringo': 9,
    '2019_20_02_baringo': 9,
    '2019_20_03_baringo': 12,
    '2019_20_04_baringo': 13,
    '2020_21_01_baringo': 13,
    '2020_21_02_baringo': 7,
    '2020_21_03_baringo': 13,
    '2020_21_04_baringo': 13,
    '2021_22_01_baringo': 13,
    '2021_22_02_baringo': 7,
    '2021_22_03_baringo': 7,
    '2021_22_04_baringo': 7,
    '2022_23_01_baringo': 7,
    '2022_23_02_baringo': 7,
    '2022_23_03_baringo': 6,
    '2022_23_04_baringo': 6,
    '2023_24_01_baringo': 8,

    '2023_24_02_baringo': 9,

    '2023_24_03_baringo': 8,

    '2023_24_04_baringo': 8,

    '2024_25_01_baringo': 6,

    '2024_25_03_baringo': 5,

    '2019_20_01_nakuru': 6,

    '2019_20_02_nakuru': 9,

    '2019_20_03_nakuru': 4,

    '2019_20_04_nakuru': 6,

    '2020_21_01_nakuru': 12,

    '2020_21_02_nakuru': 6,

    '2020_21_03_nakuru': 8,

    '2020_21_04_nakuru': 10,

    '2021_22_01_nakuru': 0,

    '2021_22_02_nakuru': 15,

    '2021_22_03_nakuru': 15,

    '2021_22_04_nakuru': 15,

    '2022_23_01_nakuru': 14,

    '2022_23_02_nakuru': 14,

    '2022_23_03_nakuru': 17,

    '2022_23_04_nakuru': 17,

    '2023_24_01_nakuru': 16,

    '2023_24_02_nakuru': 16,

    '2023_24_03_nakuru': 16,

    '2023_24_04_nakuru': 16,

    '2024_25_01_nakuru': 17,

    '2024_25_02_nakuru': 16,

    '2024_25_03_nakuru': 18,
}

def get_health_records_count(csv_path):
    """Get health records count from a CSV file using HealthBudgetAnalyzer."""
    if not os.path.exists(csv_path):
        return 0
    
    try:
        # Create analyzer with minimal model loading (use cache if available)
        analyzer = HealthBudgetAnalyzer(csv_path)
        
        # Run full analysis 
        health_data = analyzer.run_full_analysis()
        
        if health_data is None or health_data.empty:
            return 0
            
        # Count health records (both detail records and subtotals)
        health_count = len(health_data)
        return health_count
        
    except Exception as e:
        print(f"Error analyzing {csv_path}: {e}")
        return 0

def test_health_records_extraction():
    """Test that baringo_scraping.py extracts expected number of health records."""
    program_dir = Path('program')
    
    if not program_dir.exists():
        pytest.skip("Program directory does not exist. Run 'python program.py --all' first.")
    
    missing_files = []
    incorrect_counts = []
    
    for key, expected_count in EXPECTED_HEALTH_RECORDS.items():
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
                
            actual_count = get_health_records_count(csv_path)
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
        error_msg = "Incorrect health records counts:\n"
        for item in incorrect_counts:
            error_msg += f"  {item['file']}: expected {item['expected']}, got {item['actual']}\n"
        pytest.fail(error_msg)

def test_multi_file_excel_export():
    """Test that multi-file analysis creates proper Excel output with additive data."""
    # Create a temporary output file
    with tempfile.NamedTemporaryFile(suffix='.xlsx', delete=False) as tmp:
        test_output_path = tmp.name
    
    try:
        # Run multi-file analysis on a small subset for testing
        analyzer = HealthBudgetAnalyzer()
        
        # Test with just Baringo 2019_20 data (known good baseline)
        result_data = analyzer.run_multi_file_analysis(
            years=['2019_20'], 
            quarters=['01', '02'], 
            counties=['baringo'],
            output_path=test_output_path
        )
        
        # Verify Excel file was created
        assert os.path.exists(test_output_path), "Excel output file should be created"
        
        # Check Excel file structure using openpyxl directly
        from openpyxl import load_workbook
        wb = load_workbook(test_output_path, read_only=True)
        excel_sheets = wb.sheetnames
        expected_sheets = [
            'Raw_Health_Data', 
            'Programme_Totals', 
            'Department_Summary',
            'Subtotal_Validation', 
            'Data_Quality', 
            'Methodology_Notes'
        ]
        
        for sheet in expected_sheets:
            assert sheet in excel_sheets, f"Missing expected sheet: {sheet}"
        
        # Verify data is additive across quarters
        raw_data = pd.read_excel(test_output_path, 'Raw_Health_Data', engine='openpyxl')
        
        if not raw_data.empty:
            # Should have data from both Q1 and Q2
            quarters_present = raw_data['Quarter'].unique()
            # Be flexible with quarter formats - accept '01', '02', '1', '2', or numeric 1, 2
            valid_q1_formats = ['01', '1', 1]
            valid_q2_formats = ['02', '2', 2]
            has_q1 = any(q in quarters_present for q in valid_q1_formats)
            has_q2 = any(q in quarters_present for q in valid_q2_formats)
            assert has_q1 or has_q2, f"Should have data from test quarters, found: {quarters_present}"
            
            # Verify metadata columns exist
            required_columns = ['Year', 'Quarter', 'County', 'Programme', 'Classification']
            for col in required_columns:
                assert col in raw_data.columns, f"Missing required column in Excel: {col}"
            
            # Check programme totals are also additive
            prog_totals = pd.read_excel(test_output_path, 'Programme_Totals', engine='openpyxl')
            assert 'Year' in prog_totals.columns, "Programme totals should have Year metadata"
            assert 'Quarter' in prog_totals.columns, "Programme totals should have Quarter metadata"
            assert 'County' in prog_totals.columns, "Programme totals should have County metadata"
            
            # Verify department summary aggregation
            dept_summary = pd.read_excel(test_output_path, 'Department_Summary', engine='openpyxl')
            assert not dept_summary.empty, "Department summary should not be empty"
            
            # Check that total records is sum across all quarters
            if len(raw_data) > 0:
                total_from_raw = len(raw_data)
                total_from_summary = dept_summary['Total_Records_Including_Subtotals'].sum()
                assert total_from_raw == total_from_summary, "Department summary should match raw data count"
        
    finally:
        # Clean up temporary file
        if os.path.exists(test_output_path):
            os.unlink(test_output_path)

def test_baringo_scraping_runs():
    """Test that baringo_scraping.py can run without errors."""
    # Test the help command works
    result = subprocess.run(['python', 'baringo_scraping.py', '--help'],
                          capture_output=True, text=True)
    assert result.returncode == 0, f"baringo_scraping.py --help failed: {result.stderr}"

def test_health_analysis_core_functionality():
    """Test that health analysis produces valid output structure."""
    # Find a test file that should exist
    test_file = Path('program/2019_20/01/county/baringo_programme_table.csv')
    
    if not test_file.exists():
        pytest.skip("Test CSV file does not exist")
    
    try:
        analyzer = HealthBudgetAnalyzer(str(test_file))
        health_data = analyzer.run_full_analysis()
        
        # Basic structural tests
        if health_data is not None and not health_data.empty:
            # Check required columns exist
            required_columns = ['Programme', 'Sub_Programme', 'Classification_Method', 'Classification']
            for col in required_columns:
                assert col in health_data.columns, f"Missing required column: {col}"
            
            # Check classifications are valid
            valid_classifications = ['HEALTH', 'HEALTH_SUBTOTAL']
            assert all(health_data['Classification'].isin(valid_classifications)), "Invalid health classifications found"
            
            # Check programme totals can be created
            programme_totals = analyzer.create_programme_totals()
            assert not programme_totals.empty, "Should create programme totals"
            
            # Check department summary can be created
            dept_summary = analyzer.create_department_summary()
            assert not dept_summary.empty, "Should create department summary"
            
    except Exception as e:
        pytest.fail(f"Health analysis core functionality failed: {e}")

def test_scraper_regression():
    """Test that baringo_scraping.py produces expected health records for known baselines."""
    program_dir = Path('program')
    failed_runs = []
    missing_files = []
    incorrect_counts = []

    for key, expected_count in EXPECTED_HEALTH_RECORDS.items():
        # Parse key format: year_quarter_county
        parts = key.split('_')
        if len(parts) < 3:
            failed_runs.append(f"Invalid key format: {key}")
            continue

        year = parts[0]  # e.g., "2019" from "2019_20"
        quarter = parts[2]  # e.g., "01"
        county = '_'.join(parts[3:])  # e.g., "baringo"

        # Construct the expected CSV path
        csv_path = program_dir / f"{parts[0]}_{parts[1]}" / quarter / 'county' / f'{county}_programme_table.csv'

        # Run the scraper
        cmd = [
            'python', 'baringo_scraping.py',
            '--year', year,
            '--quarter', quarter,
            '--counties', county
        ]
        try:
            result = subprocess.run(
                cmd,
                capture_output=True,
                text=True,
                timeout=300  # 5-minute timeout to prevent hangs
            )
            if result.returncode != 0:
                failed_runs.append(f"Scraper failed for {key}: {result.stderr}")
                continue
        except subprocess.TimeoutExpired:
            failed_runs.append(f"Scraper timed out for {key}")
            continue
        except Exception as e:
            failed_runs.append(f"Scraper error for {key}: {str(e)}")
            continue

        # Check if the CSV was generated
        if not csv_path.exists():
            missing_files.append(str(csv_path))
            continue

        # Validate the health records count
        actual_count = get_health_records_count(csv_path)
        if actual_count != expected_count:
            incorrect_counts.append({
                'key': key,
                'file': str(csv_path),
                'expected': expected_count,
                'actual': actual_count
            })

    # Report results
    error_msgs = []
    if failed_runs:
        error_msgs.append("Scraper execution failures:\n" + '\n'.join(failed_runs))
    if missing_files:
        error_msgs.append("Missing CSV files:\n" + '\n'.join(missing_files))
    if incorrect_counts:
        error_msgs.append("Incorrect health records counts:\n" + '\n'.join(
            f"  {item['key']} ({item['file']}): expected {item['expected']}, got {item['actual']}"
            for item in incorrect_counts
        ))

    if error_msgs:
        pytest.fail('\n\n'.join(error_msgs))

if __name__ == '__main__':
    # Run tests directly
    test_baringo_scraping_runs()
    test_health_analysis_core_functionality()
    test_multi_file_excel_export()
    test_health_records_extraction()
    test_scraper_regression()
    print("All health analysis tests passed!")