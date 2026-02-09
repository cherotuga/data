#!/usr/bin/env python3
"""
Own Source Revenue Extraction Pipeline

Extracts "Table 2.1: Own Source Revenue Collection" from Kenyan government
county budget quarterly reports and creates a consolidated CSV dataset.

This script targets the summary table that appears on pages 26-27 of the
quarterly reports and contains revenue targets vs actual collections for
all 47 counties.

Usage:
    python extract_revenue.py --year 2023_24 --quarter 03
    python extract_revenue.py --all
    python extract_revenue.py --output custom_revenue_data.csv
"""

import pdfplumber
import pandas as pd
from pathlib import Path
import re
import logging
import argparse
import warnings
from typing import List, Dict, Tuple, Optional

# Suppress warnings for clean output
warnings.filterwarnings("ignore", module="pdfplumber.*")
logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')

# Revenue table column keywords (specific to Table 2.1 structure)
REVENUE_COLUMN_KEYWORDS = {
    'county': ['county'],
    'ordinary_osr': ['ordinary osr'],  # Specific to newer format, no generic 'osr target'
    'fif_aia_target': ['fif/aia target', 'fif aia target', 'fif/ aia', 'fif', 'aia target', 'appropriations in aid', 'a-i-a'],
    'total_target': ['total revenue target', 'total target', 'total osr revenue target', 'annual own source revenue', 'annual osr target', 'annual revenue target', 'annual own source revenue target', 'annual local revenue target', 'local revenue target', 'total annual target'],  # Includes 2015_16 Q1 format
    'osr_actual': ['ordinary osr actual', 'actual realised', 'q1 actual osr', 'actual osr'],  # Specific to newer format
    'fif_aia_actual': ['fif/aia actual', 'fif aia actual', 'aia actual', 'q1 actual aia'],
    'actual_revenue': ['actual revenue', 'total osr revenue', 'osr collection', 'quarter of fy', 'osr actual', 'total own source revenue', 'total own-source revenue', 'total local revenue', 'local revenue collection', 'total (q1)', 'total', 'q1 actual total'],  # Includes 2015_16 Q1 quarterly total format
    'performance': ['performance', 'performance (%)', 'performance %', 'collection of osr against', '% of collection', '% of local revenue against', '% of own source revenue against', '% of total local revenue', '% of the total local revenue', '% of total revenue', '% of first quarter revenue against annual targets', '% of quarter revenue against', 'percentage of']
}

# Standard column names for the output CSV
REVENUE_COLUMNS = [
    'county',
    'year',
    'quarter',
    'ordinary_osr_target',
    'fif_aia_target',
    'total_revenue_target',
    'osr_actual_realised',
    'fif_aia_actual',
    'actual_revenue',
    'performance_percent'
]

# Debug flags
DEBUG_TABLE_DETECTION = False
DEBUG_HEADER_MATCHING = False

class RevenueExtractor:
    """Extracts Own Source Revenue tables from county budget PDFs"""

    def __init__(self, pdf_path: str, year: str, quarter: str):
        """
        Initialize the revenue extractor

        Args:
            pdf_path: Path to the PDF file
            year: Financial year (e.g., "2023_24")
            quarter: Quarter number (e.g., "03")
        """
        self.pdf_path = Path(pdf_path)
        self.year = year
        self.quarter = quarter
        self.revenue_data = []
        self.data_in_millions = False  # Flag to track if current table data is in millions

        if not self.pdf_path.exists():
            raise FileNotFoundError(f"PDF not found: {pdf_path}")

    def is_two_row_revenue_header(self, main_headers: List[str], sub_headers: List[str]) -> bool:
        """
        Check if two consecutive rows form valid revenue table headers

        Args:
            main_headers: First row headers
            sub_headers: Second row headers

        Returns:
            True if the combination has sufficient revenue column matches
        """
        if not main_headers or not sub_headers:
            return False

        # Check if sub_headers contains county names - if so, it's data, not headers
        # Create a minimal table with just the sub_headers row to check for county data
        test_table = [sub_headers]
        if DEBUG_TABLE_DETECTION:
            logging.info(f"Two-row header detection: Testing sub_headers for county data: {sub_headers}")
        if self.has_county_data_pattern(test_table, threshold=1):  # Use threshold=1 for single row test
            if DEBUG_TABLE_DETECTION:
                logging.info(f"Two-row header detection: Found county data in sub-headers - treating as data row, not header")
                logging.info(f"Sub-headers that triggered county detection: {sub_headers}")
            return False

        # Use _analyze_headers to get column mapping
        column_map = self._analyze_headers(main_headers, sub_headers)

        # Need at least 3 mapped columns for valid revenue table
        return len(column_map) >= 3

    def is_revenue_table_header(self, headers: List[str]) -> bool:
        """
        Check if headers match Table 2.1 revenue structure (like is_program_table in program.py)

        Args:
            headers: List of table headers

        Returns:
            True if this looks like the revenue table header
        """
        if not headers or len(headers) < 4:
            return False

        # Normalize headers for comparison
        normalized_headers = []
        for header in headers:
            if header:
                # Clean up header text - handle line breaks, extra spaces, punctuation
                clean_header = str(header).strip().lower()
                clean_header = re.sub(r'-\s*\n\s*', '', clean_header)  # Remove hyphenated line breaks
                clean_header = re.sub(r'\s*\n\s*', ' ', clean_header)  # Replace line breaks with spaces
                clean_header = re.sub(r'\s+', ' ', clean_header)       # Normalize multiple spaces
                clean_header = re.sub(r'[^\w\s/()%-]', '', clean_header)  # Remove punctuation except key chars
                clean_header = re.sub(r'\s*million\s*', '', clean_header)  # Remove "million" modifier
                normalized_headers.append(clean_header)
            else:
                normalized_headers.append("")

        # Count matches for each required column type (following program.py pattern)
        county_match = False
        ordinary_osr_match = False
        fif_aia_target_match = False
        total_target_match = False
        osr_actual_match = False
        performance_match = False

        for header in normalized_headers:
            if not county_match and any(keyword in header for keyword in REVENUE_COLUMN_KEYWORDS['county']):
                county_match = True
            elif not ordinary_osr_match and any(keyword in header for keyword in REVENUE_COLUMN_KEYWORDS['ordinary_osr']):
                ordinary_osr_match = True
            elif not fif_aia_target_match and any(keyword in header for keyword in REVENUE_COLUMN_KEYWORDS['fif_aia_target']):
                fif_aia_target_match = True
            elif not total_target_match and any(keyword in header for keyword in REVENUE_COLUMN_KEYWORDS['total_target']):
                total_target_match = True
            elif not osr_actual_match and any(keyword in header for keyword in REVENUE_COLUMN_KEYWORDS['osr_actual']):
                osr_actual_match = True
            elif not performance_match and any(keyword in header for keyword in REVENUE_COLUMN_KEYWORDS['performance']):
                performance_match = True
            # Also check for general actual revenue (covers both formats)
            elif not osr_actual_match and any(keyword in header for keyword in REVENUE_COLUMN_KEYWORDS['actual_revenue']):
                osr_actual_match = True

        # Must match at least 3 out of 6 core column types for positive detection
        # (older formats have fewer columns, so lower threshold)
        matches = [county_match, ordinary_osr_match, fif_aia_target_match, total_target_match, osr_actual_match, performance_match]
        matched_count = sum(matches)
        is_revenue_header = matched_count >= 3

        if DEBUG_TABLE_DETECTION:
            logging.info(f"Revenue header analysis: {matched_count}/6 columns matched")
            logging.info(f"Matches: county={county_match}, osr={ordinary_osr_match}, fif={fif_aia_target_match}, total={total_target_match}, actual={osr_actual_match}, perf={performance_match}")

        return is_revenue_header

    def has_county_data_pattern(self, table_data: List[List[str]], threshold: int = 2) -> bool:
        """
        Check if table contains county names (not programme data)

        Args:
            table_data: List of table rows to check
            threshold: Minimum number of county matches required (default: 2)
        """
        if not table_data:
            return False

        # All 47 counties
        all_counties = [
            'baringo', 'bomet', 'bungoma', 'busia', 'elgeyo marakwet', 'embu', 'garissa',
            'homa bay', 'isiolo', 'kajiado', 'kakamega', 'kericho', 'kiambu', 'kilifi',
            'kirinyaga', 'kisii', 'kisumu', 'kitui', 'kwale', 'laikipia', 'lamu',
            'machakos', 'makueni', 'mandera', 'marsabit', 'meru', 'migori', 'mombasa',
            'muranga', 'nairobi city', 'nakuru', 'nandi', 'narok', 'nyamira', 'nyandarua',
            'nyeri', 'samburu', 'siaya', 'taita taveta', 'tana river', 'tharaka nithi',
            'trans nzoia', 'turkana', 'uasin gishu', 'vihiga', 'wajir', 'west pokot'
        ]

        county_matches = 0
        found_counties = []

        for row_idx, row in enumerate(table_data):  # Check all rows
            if row and len(row) > 1:  # Need at least 2 columns to check column 1
                # Check both column 0 and column 1 for county names (different table formats)
                col0 = str(row[0] or "").strip().lower()
                col1 = str(row[1] or "").strip().lower()

                # Normalize apostrophes before matching (same as _normalize_county_name)
                col0 = col0.replace(''', "'").replace(''', "'")
                col1 = col1.replace(''', "'").replace(''', "'")

                # Apply key standardization for county detection (especially apostrophe cases)
                if "murang'a" in col0:
                    col0 = col0.replace("murang'a", "muranga")
                if "murang'a" in col1:
                    col1 = col1.replace("murang'a", "muranga")

                county_found = False
                matched_col = ""

                if col0 and any(county in col0 for county in all_counties):
                    county_found = True
                    matched_col = col0
                elif col1 and any(county in col1 for county in all_counties):
                    county_found = True
                    matched_col = col1

                if county_found:
                    county_matches += 1
                    found_counties.append(matched_col)
                    if DEBUG_TABLE_DETECTION:
                        print(f"County match row {row_idx+1}: '{matched_col}'")

        if DEBUG_TABLE_DETECTION:
            print(f"=== RAW TABLE STRUCTURE (first 10 rows) ===")
            for i, row in enumerate(table_data[:10]):
                print(f"Row {i}: {row}")
            print(f"=== COUNTY VALIDATION RESULTS ===")
            print(f"County validation: {county_matches} matches found: {found_counties}")

        return county_matches >= threshold

    def headers_match(self, headers1: List[str], headers2: List[str], threshold: float = 0.8) -> bool:
        """
        Check if two header sets are similar (from program.py pattern)
        """
        if not headers1 or not headers2:
            return False

        # Normalize both header sets
        norm1 = [re.sub(r'\s+', ' ', str(h or "").strip().lower()) for h in headers1]
        norm2 = [re.sub(r'\s+', ' ', str(h or "").strip().lower()) for h in headers2]

        # Count exact matches
        matches = sum(1 for h1, h2 in zip(norm1, norm2) if h1 == h2)
        similarity = matches / max(len(norm1), len(norm2))

        if DEBUG_HEADER_MATCHING:
            logging.info(f"Header similarity: {matches}/{max(len(norm1), len(norm2))} = {similarity:.2f}")

        return similarity >= threshold

    def _analyze_headers(self, headers: List[str], sub_headers: List[str] = None) -> Dict[str, int]:
        """
        Analyze table headers to create column mapping using existing keywords
        Supports both single-row headers (newer format) and two-row headers (2018_19 format)

        Args:
            headers: List of main table headers (row 0)
            sub_headers: Optional list of sub-headers (row 1) for 2018_19 format

        Returns:
            Dictionary mapping data types to column indices
        """
        column_map = {}

        # Normalize main headers
        normalized_headers = []
        for header in headers:
            if header:
                clean_header = str(header).strip().lower()
                clean_header = re.sub(r'-\s*\n\s*', '', clean_header)  # Remove hyphenated line breaks
                clean_header = re.sub(r'\s*\n\s*', ' ', clean_header)  # Replace line breaks with spaces
                clean_header = re.sub(r'\s+', ' ', clean_header)       # Normalize multiple spaces
                clean_header = re.sub(r'[^\w\s/()%-]', '', clean_header)  # Remove punctuation except key chars
                clean_header = re.sub(r'\s*million\s*', '', clean_header)  # Remove "million" modifier
                normalized_headers.append(clean_header)
            else:
                normalized_headers.append("")

        # Normalize sub-headers if provided (2018_19 format)
        normalized_sub_headers = []
        if sub_headers:
            for header in sub_headers:
                if header:
                    clean_header = str(header).strip().lower()
                    clean_header = re.sub(r'-\s*\n\s*', '', clean_header)
                    clean_header = re.sub(r'\s*\n\s*', ' ', clean_header)
                    clean_header = re.sub(r'\s+', ' ', clean_header)
                    clean_header = re.sub(r'[^\w\s/()%-]', '', clean_header)
                    clean_header = re.sub(r'\s*million\s*', '', clean_header)
                    normalized_sub_headers.append(clean_header)
                else:
                    normalized_sub_headers.append("")

        # Map each header to data type using existing keywords
        # Check both main headers and sub-headers for 2018_19 format
        for i, (main_header, sub_header) in enumerate(zip(normalized_headers, normalized_sub_headers or [''] * len(normalized_headers))):
            # Combine main and sub-header text for keyword matching
            combined_header = f"{main_header} {sub_header}".strip()

            if any(keyword in combined_header for keyword in REVENUE_COLUMN_KEYWORDS['county']):
                column_map['county'] = i
            elif any(keyword in combined_header for keyword in REVENUE_COLUMN_KEYWORDS['performance']):
                # Check performance first (more specific patterns like "% of...")
                column_map['performance_percent'] = i
            elif any(keyword in combined_header for keyword in REVENUE_COLUMN_KEYWORDS['ordinary_osr']):
                column_map['ordinary_osr_target'] = i
            elif any(keyword in combined_header for keyword in REVENUE_COLUMN_KEYWORDS['fif_aia_target']):
                column_map['fif_aia_target'] = i
            elif any(keyword in combined_header for keyword in REVENUE_COLUMN_KEYWORDS['total_target']):
                column_map['total_revenue_target'] = i
            elif any(keyword in combined_header for keyword in REVENUE_COLUMN_KEYWORDS['actual_revenue']):
                # Check actual_revenue FIRST to catch comprehensive collection terms like "actual osr collection"
                column_map['actual_revenue'] = i
            elif any(keyword in combined_header for keyword in REVENUE_COLUMN_KEYWORDS['osr_actual']):
                column_map['osr_actual_realised'] = i
            elif any(keyword in combined_header for keyword in REVENUE_COLUMN_KEYWORDS['fif_aia_actual']):
                column_map['fif_aia_actual'] = i

        if DEBUG_TABLE_DETECTION:
            logging.info(f"Headers: {normalized_headers}")
            if sub_headers:
                logging.info(f"Sub-headers: {normalized_sub_headers}")
            logging.info(f"Column mapping: {column_map}")

            # Debug each column to see why mapping fails
            for i, (main_header, sub_header) in enumerate(zip(normalized_headers, normalized_sub_headers or [''] * len(normalized_headers))):
                combined_header = f"{main_header} {sub_header}".strip()
                logging.info(f"Column {i}: '{combined_header}'")

        return column_map

    def _detect_millions_in_headers(self, headers: List[str], sub_headers: List[str] = None) -> bool:
        """
        Detect if table headers indicate data is presented in millions

        Args:
            headers: List of main table headers
            sub_headers: Optional list of sub-headers for two-row format

        Returns:
            True if headers indicate data is in millions
        """
        all_headers = headers[:]
        if sub_headers:
            all_headers.extend(sub_headers)

        # Check for "million" indicators in any header
        for header in all_headers:
            if header and isinstance(header, str):
                header_lower = header.lower()
                # Look for million indicators (before they get stripped by normalization)
                if any(indicator in header_lower for indicator in ['million', 'millions', 'kshs million', 'kshs millions', 'kshs (millions)', 'kshs(millions)', 'ksh million', 'ksh millions']):
                    logging.info(f"Detected millions format from header: '{header}'")
                    return True

        return False

    def _validate_millions_conversion(self, sample_values: List[float]) -> bool:
        """
        Validate that millions conversion makes sense based on data ranges

        Args:
            sample_values: List of numerical values from the table

        Returns:
            True if the values appear to be in millions (i.e., unusually small for revenue data)
        """
        if not sample_values:
            return False

        # Filter out None and zero values
        valid_values = [v for v in sample_values if v is not None and v > 0]

        if not valid_values:
            return False

        # Calculate statistics
        max_val = max(valid_values)
        median_val = sorted(valid_values)[len(valid_values) // 2]

        # County revenue data is typically in hundreds of millions to billions of Kshs
        # If the maximum value is less than 100 million (100,000,000), it's likely in millions format
        # If the median is less than 10 million (10,000,000), it's very likely in millions format

        if max_val < 100_000_000:  # Less than 100M suggests millions format
            logging.info(f"Millions format detected: max value {max_val:,.0f} is unusually low for county revenue")
            return True

        if median_val < 10_000_000:  # Less than 10M median suggests millions format
            logging.info(f"Millions format detected: median value {median_val:,.0f} is unusually low for county revenue")
            return True

        return False

    def _safe_get_column_value(self, row: List[str], column_map: Dict[str, int], column_name: str) -> Optional[str]:
        """
        Safely extract value from row using column mapping

        Args:
            row: Table row data
            column_map: Column name to index mapping
            column_name: Name of column to extract

        Returns:
            Cell value or None if not available
        """
        if column_name not in column_map:
            return None

        col_idx = column_map[column_name]
        if col_idx < 0 or col_idx >= len(row):
            return None

        return row[col_idx]

    def _reverse_string(self, text: str) -> str:
        """
        Reverse a string (for 2014_15 transposed tables)
        """
        if not text or not isinstance(text, str):
            return text
        return text[::-1]

    def _transpose_table(self, table: List[List[str]]) -> List[List[str]]:
        """
        Transpose a table (swap rows and columns) and reverse strings for 2014_15 format

        Args:
            table: Original table data

        Returns:
            Transposed table with reversed strings
        """
        if not table or not table[0]:
            return table

        # Get max row length to handle jagged arrays
        max_cols = max(len(row) for row in table) if table else 0

        # Create transposed table with reversed strings
        transposed = []
        for col_idx in range(max_cols):
            new_row = []
            for row in table:
                if col_idx < len(row):
                    cell_value = row[col_idx]
                    # Reverse the string content
                    if isinstance(cell_value, str):
                        new_row.append(self._reverse_string(cell_value))
                    else:
                        new_row.append(cell_value)
                else:
                    new_row.append("")
            transposed.append(new_row)
        return transposed

    def _process_2014_15_transposed_table(self, table: List[List[str]], page_num: int) -> List[Dict]:
        """
        Process 2014_15 transposed revenue table to extract Q1-Q4 data for all counties

        The 2014_15 table structure after transposition and text reversal:
        - Row 0: Headers (performance %, Q1+Q2+Q3 total, Q4, Q3, Q2, Q1, Annual target, empty)
        - Rows 1-N: Each row represents ONE COUNTY with data across columns:
          - Column 0: Performance %
          - Column 1: Q1+Q2+Q3 Total revenue
          - Column 2: Q4 revenue
          - Column 3: Q3 revenue
          - Column 4: Q2 revenue
          - Column 5: Q1 revenue
          - Column 6: Annual target
          - Column 7: County name

        Args:
            table: Transposed table with reversed text already corrected
            page_num: Page number for debugging

        Returns:
            List of revenue data dictionaries for all quarters
        """
        if DEBUG_TABLE_DETECTION:
            logging.info(f"Page {page_num}: Processing 2014_15 transposed table with {len(table)} rows")

        if not table or len(table) < 2:  # Need at least headers + 1 data row
            if DEBUG_TABLE_DETECTION:
                logging.info(f"Page {page_num}: Table too short ({len(table)} rows)")
            return []

        processed_records = []

        # Process each county row (skip row 0 which contains headers)
        for row_idx in range(1, len(table)):
            row = table[row_idx]

            if not row or len(row) < 8:  # Need at least 8 columns (performance, totals, Q4-Q1, target, county)
                if DEBUG_TABLE_DETECTION:
                    logging.info(f"Page {page_num}: Row {row_idx} too short ({len(row)} columns)")
                continue

            # Extract county name from column 7 (last column in our structure)
            county_name = str(row[7] or "").strip() if len(row) > 7 else ""

            if not county_name or len(county_name) < 3:
                if DEBUG_TABLE_DETECTION:
                    logging.info(f"Page {page_num}: Row {row_idx} has no valid county name: '{county_name}'")
                continue

            county_clean = self._normalize_county_name(county_name)

            if DEBUG_TABLE_DETECTION:
                logging.info(f"Page {page_num}: Processing row {row_idx} for county {county_clean}")

            # Skip "total" summary rows
            if county_clean.lower() in ['total']:
                if DEBUG_TABLE_DETECTION:
                    logging.info(f"Page {page_num}: Skipping total/summary row: {county_clean}")
                continue

            # Extract data from specific columns (based on corrected mapping)
            performance = row[0] if len(row) > 0 else None  # Column 0: Performance %
            q1_q2_q3_total = row[1] if len(row) > 1 else None  # Column 1: Q1+Q2+Q3 cumulative
            q4_actual = self._clean_currency(row[2]) if len(row) > 2 else None  # Column 2: Q4
            q3_actual = self._clean_currency(row[3]) if len(row) > 3 else None  # Column 3: Q3
            q2_actual = self._clean_currency(row[4]) if len(row) > 4 else None  # Column 4: Q2
            q1_actual = self._clean_currency(row[5]) if len(row) > 5 else None  # Column 5: Q1
            annual_target = self._clean_currency(row[6]) if len(row) > 6 else None  # Column 6: Annual target

            # Extract raw performance percentage from column 0 for Q4 validation
            performance_raw = self._clean_percentage(row[0]) if len(row) > 0 else None

            if DEBUG_TABLE_DETECTION:
                logging.info(f"Page {page_num}: {county_clean} - Q1: {q1_actual}, Q2: {q2_actual}, Q3: {q3_actual}, Q4: {q4_actual}, Target: {annual_target}, PDF Performance: {performance_raw}%")

            # Create cumulative quarterly data to match standard reporting format
            # Q1: Q1_actual (as-is)
            # Q2: Q1_actual + Q2_actual
            # Q3: Q1_actual + Q2_actual + Q3_actual
            # Q4: Q1_actual + Q2_actual + Q3_actual + Q4_actual

            cumulative_q1 = q1_actual if q1_actual is not None else 0
            cumulative_q2 = (q1_actual or 0) + (q2_actual or 0) if q1_actual is not None or q2_actual is not None else None
            cumulative_q3 = (q1_actual or 0) + (q2_actual or 0) + (q3_actual or 0) if any(x is not None for x in [q1_actual, q2_actual, q3_actual]) else None
            cumulative_q4 = (q1_actual or 0) + (q2_actual or 0) + (q3_actual or 0) + (q4_actual or 0) if any(x is not None for x in [q1_actual, q2_actual, q3_actual, q4_actual]) else None

            quarters = [
                ('01', cumulative_q1),
                ('02', cumulative_q2),
                ('03', cumulative_q3),
                ('04', cumulative_q4)
            ]

            if DEBUG_TABLE_DETECTION:
                logging.info(f"Page {page_num}: {county_clean} cumulative - Q1: {cumulative_q1}, Q2: {cumulative_q2}, Q3: {cumulative_q3}, Q4: {cumulative_q4}")

            for quarter_num, cumulative_revenue in quarters:
                if cumulative_revenue is not None and cumulative_revenue > 0:  # Only create record if we have cumulative revenue data
                    # Calculate performance: cumulative_revenue / annual_target * 100
                    calculated_performance = None
                    if annual_target and annual_target > 0:
                        calculated_performance = (cumulative_revenue / annual_target) * 100

                    # Validate Q4 performance against PDF value
                    if quarter_num == '04' and performance_raw is not None and calculated_performance is not None:
                        performance_diff = abs(calculated_performance - performance_raw)
                        if performance_diff > 1.0:  # Tolerance of 1%
                            logging.warning(f"Page {page_num}: {county_clean} Q4 performance mismatch - PDF: {performance_raw:.1f}%, Calculated: {calculated_performance:.1f}%, Diff: {performance_diff:.1f}%")
                        elif DEBUG_TABLE_DETECTION:
                            logging.info(f"Page {page_num}: {county_clean} Q4 performance validated - PDF: {performance_raw:.1f}%, Calculated: {calculated_performance:.1f}% ✓")

                    record = {
                        'county': county_clean,
                        'year': self.year,
                        'quarter': quarter_num,
                        'ordinary_osr_target': None,  # Not available in 2014_15 format
                        'fif_aia_target': None,       # Not available in 2014_15 format
                        'total_revenue_target': annual_target,  # Full annual target
                        'osr_actual_realised': None,  # Not available in 2014_15 format
                        'fif_aia_actual': None,       # Not available in 2014_15 format
                        'actual_revenue': cumulative_revenue,
                        'performance_percent': calculated_performance  # Calculated cumulative performance
                    }

                    processed_records.append(record)

        if DEBUG_TABLE_DETECTION:
            logging.info(f"Page {page_num}: Extracted {len(processed_records)} records from 2014_15 transposed table")

        return processed_records

    def _analyze_2014_15_headers(self, headers: List[str]) -> Dict[str, int]:
        """
        Analyze 2014_15 transposed table headers to map quarterly columns

        Args:
            headers: List of header strings (after text reversal)

        Returns:
            Dictionary mapping column types to indices
        """
        column_map = {'county': 0}  # County is typically first column

        for i, header in enumerate(headers):
            if not header:
                continue

            header_lower = str(header).lower()

            # Look for quarterly indicators
            if 'q1' in header_lower or 'first quarter' in header_lower:
                column_map['q1_actual'] = i
            elif 'q2' in header_lower or 'second quarter' in header_lower:
                column_map['q2_actual'] = i
            elif 'q3' in header_lower or 'third quarter' in header_lower:
                column_map['q3_actual'] = i
            elif 'q4' in header_lower or 'fourth quarter' in header_lower:
                column_map['q4_actual'] = i
            elif 'target' in header_lower or 'estimate' in header_lower:
                column_map['annual_target'] = i
            elif 'revenue' in header_lower and 'total' in header_lower:
                column_map['total_actual'] = i

        return column_map

    def _extract_2014_15_quarterly_data(self, row: List[str], column_map: Dict[str, int], county_name: str) -> List[Dict]:
        """
        Extract quarterly revenue data from a 2014_15 transposed table row

        Args:
            row: Table row data
            column_map: Column mapping from _analyze_2014_15_headers
            county_name: Cleaned county name

        Returns:
            List of revenue records for each quarter (Q1-Q4)
        """
        records = []

        # Extract annual target if available
        annual_target = None
        if 'annual_target' in column_map:
            target_val = self._safe_get_column_value(row, column_map, 'annual_target')
            annual_target = self._clean_currency(target_val)

        # Extract quarterly actual values
        q1_actual = None
        q2_actual = None
        q3_actual = None
        q4_actual = None

        if 'q1_actual' in column_map:
            q1_val = self._safe_get_column_value(row, column_map, 'q1_actual')
            q1_actual = self._clean_currency(q1_val)

        if 'q2_actual' in column_map:
            q2_val = self._safe_get_column_value(row, column_map, 'q2_actual')
            q2_actual = self._clean_currency(q2_val)

        if 'q3_actual' in column_map:
            q3_val = self._safe_get_column_value(row, column_map, 'q3_actual')
            q3_actual = self._clean_currency(q3_val)

        if 'q4_actual' in column_map:
            q4_val = self._safe_get_column_value(row, column_map, 'q4_actual')
            q4_actual = self._clean_currency(q4_val)

        # Create records for each quarter that has data
        quarters = [
            ('01', q1_actual),
            ('02', q2_actual),
            ('03', q3_actual),
            ('04', q4_actual)
        ]

        for quarter_num, actual_revenue in quarters:
            if actual_revenue is not None:  # Only create record if we have actual revenue data
                record = {
                    'county': county_name,
                    'year': self.year,
                    'quarter': quarter_num,
                    'ordinary_osr_target': None,  # Not available in 2014_15 format
                    'fif_aia_target': None,       # Not available in 2014_15 format
                    'total_revenue_target': annual_target / 4 if annual_target else None,  # Approximate quarterly target
                    'osr_actual_realised': None,  # Not available in 2014_15 format
                    'fif_aia_actual': None,       # Not available in 2014_15 format
                    'actual_revenue': actual_revenue,
                    'performance_percent': None   # Not available in 2014_15 format
                }

                records.append(record)

        return records

    def _is_2014_15_transposed_table(self, table: List[List[str]]) -> bool:
        """
        Check if this is a 2014_15 transposed revenue table by looking for reversed text patterns
        """
        if not table or len(table) < 3:
            return False

        # Look for reversed revenue indicators in the first few rows
        for row in table[:5]:
            for cell in row[:3]:  # Check first 3 cells of each row
                if isinstance(cell, str):
                    cell_lower = cell.lower()
                    # Look for reversed patterns like 'eunever' (revenue), 'ht4' (4th), '3q2q1q' (q1+q2+q3)
                    if any(pattern in cell_lower for pattern in ['eunever', 'ht4', 'dr3', '3q2q1q', 'lacol']):
                        if DEBUG_TABLE_DETECTION:
                            logging.info(f"Found 2014_15 transposed pattern: '{cell}'")
                        return True

        # Also check for county names in the last few rows (they appear as reversed text in the last row)
        if len(table) >= 7 and table[-1]:  # County names appear in last row
            for cell in table[-1][-10:]:  # Check last 10 columns for county names
                if isinstance(cell, str) and len(cell) > 3:
                    cell_lower = cell.lower()
                    # Look for reversed county patterns (aipikiaL = Laikipia, umaL = Lamu, etc.)
                    if any(pattern in cell_lower for pattern in ['aipiki', 'umal', 'sokahc', 'ineuká', 'aredna']):
                        if DEBUG_TABLE_DETECTION:
                            logging.info(f"Found 2014_15 county pattern: '{cell}'")
                        return True
        return False

    def find_revenue_table_start(self) -> Optional[int]:
        """
        Find the starting page of Table 2.1 using strict header validation
        """
        with pdfplumber.open(self.pdf_path) as pdf:
            for i, page in enumerate(pdf.pages):
                tables = page.extract_tables()

                if not tables:
                    continue

                # Check each table on this page
                for table in tables:
                    if not table or len(table) < 3:
                        continue

                    # Check for 2014_15 transposed table first
                    if self.year == "2014_15" and self._is_2014_15_transposed_table(table):
                        # For 2014_15, we don't need to validate county data pattern
                        # because the pattern changes after transposition
                        logging.info(f"Found 2014_15 transposed revenue table starting on page {i+1}")
                        return i

                    # Try each row and row pairs as potential headers until we find county data
                    # Check two-row headers FIRST to avoid performance keyword interference with formats that require two-row detection
                    for row_idx in range(min(3, len(table))):  # Check first 3 rows max
                        headers = table[row_idx]

                        # Check two-row headers FIRST (current row + next row)
                        if row_idx + 1 < len(table):
                            next_headers = table[row_idx + 1]
                            if self.is_two_row_revenue_header(headers, next_headers):
                                # Additional validation: check if it has county data
                                if self.has_county_data_pattern(table):
                                    logging.info(f"Found Table 2.1 revenue table starting on page {i+1} (headers in rows {row_idx}-{row_idx+1}) [two-row priority]")
                                    return i
                                break  # If headers match but no county data, stop checking this table

                    # Only try single-row detection if two-row detection failed
                    for row_idx in range(min(3, len(table))):  # Check first 3 rows max
                        headers = table[row_idx]

                        # Check single row headers only after two-row detection failed
                        if self.is_revenue_table_header(headers):
                            # Additional validation: check if it has county data
                            if self.has_county_data_pattern(table):
                                logging.info(f"Found Table 2.1 revenue table starting on page {i+1} (headers in row {row_idx}) [single-row fallback]")
                                return i
                            break  # If headers match but no county data, stop checking this table

        return None

    def extract_multi_page_revenue_table(self, start_page: int) -> List[Dict]:
        """
        Extract revenue table data following multi-page continuation (like program.py)
        """
        all_revenue_data = []
        prev_headers = None

        with pdfplumber.open(self.pdf_path) as pdf:
            # Process pages starting from the detected start page
            for page_idx in range(start_page, len(pdf.pages)):
                page = pdf.pages[page_idx]
                tables = page.extract_tables()

                if not tables:
                    # No tables on this page - end of revenue table
                    break

                found_continuation = False

                for table in tables:
                    if not table or len(table) < 2:
                        continue

                    # Check if this is a 2014_15 transposed table and handle it specially
                    if self.year == "2014_15" and self._is_2014_15_transposed_table(table):
                        # Process transposed table
                        transposed_table = self._transpose_table(table)
                        if DEBUG_TABLE_DETECTION:
                            logging.info(f"Page {page_idx+1}: Processing 2014_15 transposed table")
                        page_data = self._process_2014_15_transposed_table(transposed_table, page_idx + 1)
                        all_revenue_data.extend(page_data)
                        found_continuation = True
                        break

                    # Find the correct header row(s) using same logic as detection
                    headers = None
                    sub_headers = None

                    # Check two-row headers FIRST (same priority as find_revenue_table_start)
                    for row_idx in range(min(3, len(table))):
                        potential_headers = table[row_idx]

                        # Check two-row headers FIRST (current row + next row)
                        if row_idx + 1 < len(table):
                            potential_sub_headers = table[row_idx + 1]
                            if self.is_two_row_revenue_header(potential_headers, potential_sub_headers):
                                headers = potential_headers
                                sub_headers = potential_sub_headers
                                break

                    # Only try single-row if two-row detection failed
                    if not headers:
                        for row_idx in range(min(3, len(table))):
                            potential_headers = table[row_idx]

                            # Check single row headers only after two-row detection failed
                            if self.is_revenue_table_header(potential_headers):
                                headers = potential_headers
                                break

                    if not headers:
                        headers = table[0]  # Fallback to row 0

                    # For first page, validate it's a revenue table
                    if prev_headers is None:
                        # For two-row headers, we need to validate using both rows
                        if sub_headers is not None:
                            # Two-row header validation
                            if self.is_two_row_revenue_header(headers, sub_headers):
                                prev_headers = headers
                                found_continuation = True
                                if DEBUG_TABLE_DETECTION:
                                    logging.info(f"Page {page_idx+1}: Validated two-row revenue table header")
                            else:
                                if DEBUG_TABLE_DETECTION:
                                    logging.info(f"Page {page_idx+1}: Two-row header validation failed")
                                continue
                        else:
                            # Single-row header validation
                            if self.is_revenue_table_header(headers):
                                prev_headers = headers
                                found_continuation = True
                                if DEBUG_TABLE_DETECTION:
                                    logging.info(f"Page {page_idx+1}: Validated single-row revenue table header")
                            else:
                                if DEBUG_TABLE_DETECTION:
                                    logging.info(f"Page {page_idx+1}: Single-row header validation failed")
                                continue
                    else:
                        # For subsequent pages, check header similarity
                        if self.headers_match(prev_headers, headers):
                            found_continuation = True
                        else:
                            if DEBUG_HEADER_MATCHING:
                                logging.info(f"Page {page_idx+1}: Headers don't match - end of revenue table")
                            break

                    # Process this table's data
                    if DEBUG_TABLE_DETECTION:
                        logging.info(f"Page {page_idx + 1}: Calling _process_revenue_table_data with table length {len(table)}")
                    page_data = self._process_revenue_table_data(table, page_idx + 1)
                    if DEBUG_TABLE_DETECTION:
                        logging.info(f"Page {page_idx + 1}: _process_revenue_table_data returned {len(page_data)} records")
                    all_revenue_data.extend(page_data)

                    # Update previous headers for next iteration
                    prev_headers = headers
                    break  # Only process first matching table per page

                if not found_continuation:
                    # No matching table found on this page - end of revenue table
                    break

        logging.info(f"Extracted revenue data from pages {start_page+1} to {page_idx+1}")
        return all_revenue_data

    def _process_revenue_table_data(self, table: List[List[str]], page_num: int) -> List[Dict]:
        """
        Process raw table data into structured revenue records

        Args:
            table: Raw table data from pdfplumber
            page_num: Page number for debugging

        Returns:
            List of revenue data dictionaries
        """
        if DEBUG_TABLE_DETECTION:
            logging.info(f"Page {page_num}: _process_revenue_table_data called with table length {len(table) if table else 0}")

        if not table or len(table) < 3:
            if DEBUG_TABLE_DETECTION:
                logging.info(f"Page {page_num}: Returning early - table too short (length: {len(table) if table else 0})")
            return []

        # Find the correct header row(s) (same logic as in find_revenue_table_start)
        headers = []
        sub_headers = None
        header_row_count = 1  # Track how many header rows we found

        # Check two-row headers FIRST (same priority as detection logic)
        for row_idx in range(min(3, len(table))):
            potential_headers = table[row_idx]

            # Check two-row headers FIRST (current row + next row)
            if row_idx + 1 < len(table):
                potential_sub_headers = table[row_idx + 1]
                if self.is_two_row_revenue_header(potential_headers, potential_sub_headers):
                    headers = potential_headers
                    sub_headers = potential_sub_headers
                    header_row_count = 2
                    if DEBUG_TABLE_DETECTION:
                        logging.info(f"Page {page_num}: Using rows {row_idx}-{row_idx+1} as headers (two-row priority)")
                    break

        # Only try single-row if two-row detection failed
        if not headers:
            for row_idx in range(min(3, len(table))):
                potential_headers = table[row_idx]

                # Check single row headers only after two-row detection failed
                if self.is_revenue_table_header(potential_headers):
                    headers = potential_headers
                    header_row_count = 1
                    if DEBUG_TABLE_DETECTION:
                        logging.info(f"Page {page_num}: Using row {row_idx} as headers (single row fallback)")
                    break

        if not headers:
            headers = table[0] if table else []  # Fallback to row 0
            header_row_count = 1

        column_map = self._analyze_headers(headers, sub_headers)

        # Detect if data is in millions format
        header_suggests_millions = self._detect_millions_in_headers(headers, sub_headers)

        # If headers suggest millions, we'll sample some data to validate
        if header_suggests_millions:
            self.data_in_millions = True
            logging.info(f"Page {page_num}: Headers indicate millions format - will validate with data samples")
        else:
            # Reset flag for this table (in case previous table had millions)
            self.data_in_millions = False

        processed_rows = []

        # Find the actual start of data rows by checking table content
        # Start with the row after the identified header(s)
        start_row = 1 if sub_headers is None else 2  # 1 for single-row headers, 2 for two-row headers

        if DEBUG_TABLE_DETECTION:
            logging.info(f"Page {page_num}: Header type: {'two-row' if sub_headers else 'single-row'}, Initial start_row: {start_row}")

        # But if we have county data immediately after headers, start there
        if len(table) > 1:
            # Check if row 1 contains county data (regardless of header type detected)
            test_table = [table[1]] if len(table) > 1 else []
            if self.has_county_data_pattern(test_table):
                start_row = 1
                if DEBUG_TABLE_DETECTION:
                    logging.info(f"Page {page_num}: County data detected in row 1 - setting start_row to 1")
            elif len(table) > 2:
                # Check if row 2 contains county data
                test_table = [table[2]] if len(table) > 2 else []
                if self.has_county_data_pattern(test_table):
                    start_row = 2
                    if DEBUG_TABLE_DETECTION:
                        logging.info(f"Page {page_num}: County data detected in row 2 - setting start_row to 2")

        # Additional check: skip formula rows (A, B, C, etc.)
        while start_row < len(table) and table[start_row]:
            first_cell = str(table[start_row][0] or "").strip()
            if first_cell in ['A', 'B', 'C', 'D', 'E', 'F', 'G'] or 'Target' in first_cell:
                if DEBUG_TABLE_DETECTION:
                    logging.info(f"Page {page_num}: Skipping formula row {start_row}: {first_cell}")
                start_row += 1
            else:
                break

        if DEBUG_TABLE_DETECTION:
            logging.info(f"Page {page_num}: Final start_row: {start_row}, table length: {len(table)}")
            if start_row < len(table):
                logging.info(f"Page {page_num}: First data row will be: {table[start_row][:3] if table[start_row] else 'None'}")

        # If headers don't suggest millions but we have no explicit flag set,
        # collect sample data to validate range
        sample_values = []
        validation_row_count = 0
        need_data_validation = not header_suggests_millions

        for row_idx, row in enumerate(table[start_row:], start=start_row):
            if DEBUG_TABLE_DETECTION:
                logging.info(f"Page {page_num}: Processing row {row_idx}: {row[:5] if row else 'None'}...")

            if not row or len(row) < 3:  # Minimum 3 columns (county + at least 2 data columns)
                if DEBUG_TABLE_DETECTION and row:
                    logging.info(f"Page {page_num}: Skipped row {row_idx} - insufficient columns ({len(row)}): {row[:3]}")
                continue

            county_name = self._safe_get_column_value(row, column_map, 'county') or ""
            county_name = county_name.strip()

            if DEBUG_TABLE_DETECTION:
                logging.info(f"Page {page_num}: Row {row_idx} county_name: '{county_name}'")

            # Add special debug for Baringo and Kisii
            if county_name.lower() in ['baringo', 'kisii']:
                if DEBUG_TABLE_DETECTION:
                    logging.info(f"Page {page_num}: *** TRACKING {county_name.upper()} *** - Row {row_idx}: {row}")

            # Skip empty rows, header repetitions, or summary rows
            if (not county_name or
                county_name.lower() in ['county', 'county title', 'total', ''] or
                'target' in county_name.lower() or
                county_name.strip() in ['A', 'B', 'C', 'D', 'E', 'F', 'G']):
                if DEBUG_TABLE_DETECTION:
                    logging.info(f"Page {page_num}: Skipped row {row_idx} - filtered county name: '{county_name}'")
                    # Extra debug for Baringo and Kisii
                    if county_name.lower() in ['baringo', 'kisii']:
                        logging.info(f"Page {page_num}: *** {county_name.upper()} SKIPPED *** - Reason: filtered county name")
                continue

            # Clean county name
            county_clean = self._normalize_county_name(county_name)

            # Add special debug for Baringo and Kisii after normalization
            if county_clean.lower() in ['baringo', 'kisii']:
                if DEBUG_TABLE_DETECTION:
                    logging.info(f"Page {page_num}: *** {county_clean.upper()} PROCESSING *** - About to create revenue record")

            try:
                # Collect sample data for validation if needed
                if need_data_validation and validation_row_count < 10:  # Sample first 10 rows
                    # Temporarily disable millions conversion to get raw values
                    original_flag = self.data_in_millions
                    self.data_in_millions = False

                    # Extract raw values for validation
                    raw_actual = self._clean_currency(self._safe_get_column_value(row, column_map, 'actual_revenue'))
                    raw_target = self._clean_currency(self._safe_get_column_value(row, column_map, 'total_revenue_target'))

                    if raw_actual and raw_actual > 0:
                        sample_values.append(raw_actual)
                    if raw_target and raw_target > 0:
                        sample_values.append(raw_target)

                    validation_row_count += 1

                    # Restore original flag
                    self.data_in_millions = original_flag

                    # After collecting 10 rows or 20 values, validate
                    if validation_row_count >= 10 or len(sample_values) >= 20:
                        data_suggests_millions = self._validate_millions_conversion(sample_values)
                        if data_suggests_millions:
                            self.data_in_millions = True
                            logging.info(f"Page {page_num}: Data validation confirms millions format - applying conversion")
                        else:
                            self.data_in_millions = False
                            logging.info(f"Page {page_num}: Data validation suggests standard format - no conversion needed")
                        need_data_validation = False  # Done with validation

                # Use dynamic column mapping instead of hardcoded positions
                revenue_data = {
                    'county': county_clean,
                    'year': self.year,
                    'quarter': self.quarter,
                    'ordinary_osr_target': self._clean_currency(self._safe_get_column_value(row, column_map, 'ordinary_osr_target')),
                    'fif_aia_target': self._clean_currency(self._safe_get_column_value(row, column_map, 'fif_aia_target')),
                    'total_revenue_target': self._clean_currency(self._safe_get_column_value(row, column_map, 'total_revenue_target')),
                    'osr_actual_realised': self._clean_currency(self._safe_get_column_value(row, column_map, 'osr_actual_realised')),
                    'fif_aia_actual': self._clean_currency(self._safe_get_column_value(row, column_map, 'fif_aia_actual')),
                    'actual_revenue': self._clean_currency(self._safe_get_column_value(row, column_map, 'actual_revenue')),
                    'performance_percent': self._clean_percentage(self._safe_get_column_value(row, column_map, 'performance_percent'))
                }

                # Validate performance calculation (actual_revenue / total_revenue_target * 100)
                self._validate_performance_calculation(revenue_data, page_num, county_clean)

                processed_rows.append(revenue_data)

                if DEBUG_TABLE_DETECTION:
                    logging.info(f"Page {page_num}: Processed {county_clean}")

                # Log millions conversion if applied
                if self.data_in_millions and DEBUG_TABLE_DETECTION:
                    logging.info(f"Page {page_num}: {county_clean} - Applied millions conversion (x1M)")
                    if revenue_data.get('actual_revenue'):
                        logging.info(f"  Actual revenue: {revenue_data['actual_revenue']:,.0f} Kshs")
                    if revenue_data.get('total_revenue_target'):
                        logging.info(f"  Revenue target: {revenue_data['total_revenue_target']:,.0f} Kshs")

            except Exception as e:
                logging.warning(f"Page {page_num}: Error processing row {row_idx} for {county_name}: {e}")
                continue

        logging.info(f"Page {page_num}: Processed {len(processed_rows)} revenue records")
        return processed_rows

    def _validate_performance_calculation(self, revenue_data: Dict, page_num: int, county_name: str, tolerance: float = 2.0) -> None:
        """
        Validate that reported performance matches calculated performance (actual_revenue / total_revenue_target * 100)

        Args:
            revenue_data: Dictionary containing revenue record
            page_num: Page number for logging context
            county_name: County name for logging context
            tolerance: Acceptable difference percentage (default 2.0%)
        """
        actual_revenue = revenue_data.get('actual_revenue')
        total_target = revenue_data.get('total_revenue_target')
        reported_performance = revenue_data.get('performance_percent')

        # Skip validation if we don't have the required data
        if not all([actual_revenue, total_target, reported_performance]):
            if DEBUG_TABLE_DETECTION and (actual_revenue or total_target or reported_performance):
                logging.info(f"Page {page_num}: {county_name} {self.year} Q{self.quarter} - Skipping validation (missing data): "
                           f"Actual: {actual_revenue}, Target: {total_target}, Performance: {reported_performance}")
            return

        if total_target <= 0:
            logging.warning(f"Page {page_num}: {county_name} {self.year} Q{self.quarter} - Invalid target: {total_target}")
            return

        # Calculate expected performance
        calculated_performance = (actual_revenue / total_target) * 100

        # Check if they match within tolerance
        performance_diff = abs(calculated_performance - reported_performance)

        if performance_diff > tolerance:
            logging.warning(f"Page {page_num}: {county_name} {self.year} Q{self.quarter} - Performance mismatch: "
                          f"PDF: {reported_performance:.2f}%, Calculated: {calculated_performance:.2f}%, "
                          f"Diff: {performance_diff:.2f}% (Revenue: {actual_revenue:,.0f}, Target: {total_target:,.0f})")
        elif DEBUG_TABLE_DETECTION:
            logging.info(f"Page {page_num}: {county_name} {self.year} Q{self.quarter} - Performance validated: "
                        f"PDF: {reported_performance:.2f}%, Calculated: {calculated_performance:.2f}% ✓")

    def _normalize_county_name(self, name: str) -> str:
        """
        Normalize county names to match standard format

        Args:
            name: Raw county name from PDF

        Returns:
            Cleaned county name
        """
        if not name:
            return ""

        # Basic cleaning
        clean_name = name.strip().lower()

        # Normalize apostrophes (smart quotes to straight quotes)
        clean_name = clean_name.replace(''', "'").replace(''', "'")

        # Remove line breaks that may appear in county names
        clean_name = re.sub(r'\s*\n\s*', '', clean_name)
        clean_name = re.sub(r'\s+', ' ', clean_name)

        # County name standardization dictionary
        # Maps variations found in PDFs to standard format
        standardization_map = {
            # Handle wrong counties
            'kiliif' : 'kilifi',

            # Handle hyphenated variations and two words
            'elgeyo-marakwet': 'elgeyo marakwet',
            'elgeyo-marak-wet': 'elgeyo marakwet',  
            'elgeyo/marakwet' : 'elgeyo marakwet',
            'e l g e y o /marakwet' : 'elgeyo marakwet',
            'elgeyo/marak-wet' : 'elgeyo marakwet',

            'homabay' : 'homa bay',

            'taita-taveta': 'taita taveta',
            'taita/tav-eta' : 'taita taveta',
            'taita/taveta' : 'taita taveta',

            'tanariver' : 'tana river',

            'tharaka-nithi': 'tharaka nithi',
            'tharaka -nithi': 'tharaka nithi',
            'tharaka –nithi': 'tharaka nithi',

            'trans-nzoia': 'trans nzoia',
            'transnzoia' : 'trans nzoia',

            'uasingishu' : 'uasin gishu',
            'uasin-gishu' : 'uasin gishu',

            'westpokot' : 'west pokot',

            # Handle apostrophe variations - keep consistent 
            "murang'a": "muranga",   
            "murang’a": "muranga",
            "murang’’a": "muranga",

            # Handle city vs regular name
            'nairobi': 'nairobi city',
            'nairobicity': 'nairobi city',
        }

        # Apply standardization
        for variant, standard in standardization_map.items():
            if variant == clean_name:
                clean_name = standard
                break

        return clean_name

    def _clean_currency(self, value: str) -> Optional[float]:
        """
        Clean currency values and convert to float
        Automatically converts from millions to actual values if data_in_millions flag is set

        Args:
            value: Raw currency string (e.g., "1,234,567")

        Returns:
            Float value or None if parsing fails
        """
        if not value or not isinstance(value, str):
            return None

        try:
            # Remove commas, spaces, and currency symbols
            cleaned = re.sub(r'[,\s]', '', value.strip())
            cleaned = re.sub(r'[^\d.-]', '', cleaned)

            if cleaned:
                result = float(cleaned)

                # Apply millions conversion if detected
                if self.data_in_millions and result > 0:
                    result = result * 1_000_000

                return result
            return None
        except (ValueError, TypeError):
            return None

    def _clean_percentage(self, value: str) -> Optional[float]:
        """
        Clean percentage values and convert to float

        Args:
            value: Raw percentage string (e.g., "85.6")

        Returns:
            Float value or None if parsing fails
        """
        if not value or not isinstance(value, str):
            return None

        try:
            # Remove % symbol and spaces
            cleaned = value.strip().replace('%', '')

            if cleaned:
                return float(cleaned)
            return None
        except (ValueError, TypeError):
            return None

    def extract(self) -> List[Dict]:
        """
        Main extraction method

        Returns:
            List of revenue data dictionaries
        """
        logging.info(f"Extracting revenue data from {self.pdf_path}")

        # Find the starting page of Table 2.1 using strict header validation
        start_page = self.find_revenue_table_start()

        if start_page is None:
            logging.error("Table 2.1 (Own Source Revenue) not found in PDF")
            return []

        # Extract multi-page revenue table following header continuity
        revenue_data = self.extract_multi_page_revenue_table(start_page)

        self.revenue_data = revenue_data
        logging.info(f"Total revenue records extracted: {len(revenue_data)}")

        return revenue_data


def validate_all_revenue_data(all_revenue_data: List[Dict], tolerance: float = 2.0) -> None:
    """
    Validate performance calculations for all extracted revenue data and provide summary statistics

    Args:
        all_revenue_data: List of all revenue records across all files
        tolerance: Acceptable difference percentage (default 2.0%)
    """
    if not all_revenue_data:
        logging.info("No revenue data to validate")
        return

    total_records = len(all_revenue_data)
    validated_records = 0
    mismatched_records = 0
    missing_data_records = 0
    validation_details = []

    for record in all_revenue_data:
        actual_revenue = record.get('actual_revenue')
        total_target = record.get('total_revenue_target')
        reported_performance = record.get('performance_percent')
        county = record.get('county', 'Unknown')
        year = record.get('year', 'Unknown')
        quarter = record.get('quarter', 'Unknown')

        # Skip if missing critical data
        if not all([actual_revenue, total_target, reported_performance]) or total_target <= 0:
            missing_data_records += 1
            continue

        # Calculate expected performance
        calculated_performance = (actual_revenue / total_target) * 100
        performance_diff = abs(calculated_performance - reported_performance)

        validated_records += 1

        if performance_diff > tolerance:
            mismatched_records += 1
            validation_details.append({
                'county': county,
                'year': year,
                'quarter': quarter,
                'pdf_performance': reported_performance,
                'calculated_performance': calculated_performance,
                'difference': performance_diff,
                'actual_revenue': actual_revenue,
                'total_target': total_target
            })

    # Summary statistics
    validation_rate = (validated_records / total_records) * 100 if total_records > 0 else 0
    accuracy_rate = ((validated_records - mismatched_records) / validated_records * 100) if validated_records > 0 else 0

    logging.info("=" * 80)
    logging.info("REVENUE DATA PERFORMANCE VALIDATION SUMMARY")
    logging.info("=" * 80)
    logging.info(f"Total records processed: {total_records:,}")
    logging.info(f"Records with complete data: {validated_records:,} ({validation_rate:.1f}%)")
    logging.info(f"Records missing data: {missing_data_records:,}")
    logging.info(f"Performance calculations validated: {validated_records - mismatched_records:,} ({accuracy_rate:.1f}%)")
    logging.info(f"Performance mismatches (>{tolerance}%): {mismatched_records:,}")

    if mismatched_records > 0:
        logging.warning("=" * 80)
        logging.warning("PERFORMANCE VALIDATION MISMATCHES")
        logging.warning("=" * 80)

        # Sort by difference (largest first) and show top 10 mismatches
        validation_details.sort(key=lambda x: x['difference'], reverse=True)

        for i, detail in enumerate(validation_details[:10]):
            logging.warning(f"  {i+1}. {detail['county']} {detail['year']} Q{detail['quarter']}: "
                          f"PDF: {detail['pdf_performance']:.2f}%, "
                          f"Calculated: {detail['calculated_performance']:.2f}%, "
                          f"Diff: {detail['difference']:.2f}% "
                          f"(Revenue: {detail['actual_revenue']:,.0f}, Target: {detail['total_target']:,.0f})")

        if len(validation_details) > 10:
            logging.warning(f"  ... and {len(validation_details) - 10} more mismatches")

    logging.info("=" * 80)


def discover_pdfs(base_path: Path = None) -> List[Tuple[str, str, str]]:
    """
    Discover available county PDF files

    Args:
        base_path: Base directory to search (defaults to current directory)

    Returns:
        List of tuples: (pdf_path, year, quarter)
    """
    if base_path is None:
        base_path = Path(".")

    pdf_files = []

    # Look for PDFs in year/quarter/county pattern
    for pdf_path in base_path.rglob("*county*.pdf"):
        # Try to extract year and quarter from path
        path_parts = pdf_path.parts

        year_part = None
        quarter_part = None

        # Look for year pattern (e.g., "2023_24")
        for part in path_parts:
            if re.match(r'20\d{2}_\d{2}', part) and not year_part:
                year_part = part
            elif re.match(r'0[1-4]', part) and not quarter_part:
                quarter_part = part

        # Also try to extract from filename
        if not year_part or not quarter_part:
            filename = pdf_path.stem
            year_match = re.search(r'(20\d{2}_\d{2})', filename)
            quarter_match = re.search(r'_0([1-4])_', filename)

            if year_match:
                year_part = year_match.group(1)
            if quarter_match:
                quarter_part = f"0{quarter_match.group(1)}"

        if year_part and quarter_part:
            pdf_files.append((str(pdf_path), year_part, quarter_part))
        else:
            logging.warning(f"Could not extract year/quarter from {pdf_path}")

    pdf_files.sort()  # Sort by path for consistent ordering
    return pdf_files


def main():
    """Main command line interface"""
    parser = argparse.ArgumentParser(
        description="Extract Own Source Revenue data from county budget PDFs"
    )

    parser.add_argument(
        '--year',
        help='Financial year (e.g., 2023_24)'
    )
    parser.add_argument(
        '--quarter',
        help='Quarter (01, 02, 03, 04)'
    )
    parser.add_argument(
        '--all',
        action='store_true',
        help='Process all available PDFs'
    )
    parser.add_argument(
        '--output',
        default='revenue-extraction/revenue_data.csv',
        help='Output CSV file (default: revenue-extraction/revenue_data.csv)'
    )
    parser.add_argument(
        '--debug',
        action='store_true',
        help='Enable debug logging'
    )

    args = parser.parse_args()

    if args.debug:
        logging.getLogger().setLevel(logging.INFO)  # Keep INFO level, not DEBUG
        global DEBUG_TABLE_DETECTION, DEBUG_HEADER_MATCHING
        DEBUG_TABLE_DETECTION = True
        DEBUG_HEADER_MATCHING = True

    all_revenue_data = []

    if args.all:
        # Process all available PDFs
        pdf_files = discover_pdfs()

        if not pdf_files:
            logging.error("No county PDF files found")
            return

        logging.info(f"Found {len(pdf_files)} PDF files to process")

        for pdf_path, year, quarter in pdf_files:
            # Skip 2013_14 completely - insufficient data quality
            if year == "2013_14":
                logging.info(f"Skipping {year} Q{quarter} - insufficient data quality")
                continue

            # Skip 2014_15 Q1, Q2, Q3 - only process Q4 which contains all quarterly data
            if year == "2014_15" and quarter in ["01", "02", "03"]:
                logging.info(f"Skipping {year} Q{quarter} - data available in Q4 transposed table")
                continue

            try:
                extractor = RevenueExtractor(pdf_path, year, quarter)
                revenue_data = extractor.extract()
                all_revenue_data.extend(revenue_data)

                logging.info(f"Extracted {len(revenue_data)} records from {year} Q{quarter}")

            except Exception as e:
                logging.error(f"Error processing {pdf_path}: {e}")
                continue

    elif args.year and args.quarter:
        # Process specific year/quarter
        pdf_pattern = f"{args.year}/{args.quarter}/county/{args.year}_{args.quarter}_county.pdf"
        pdf_path = Path(pdf_pattern)

        if not pdf_path.exists():
            logging.error(f"PDF not found: {pdf_path}")
            return

        extractor = RevenueExtractor(str(pdf_path), args.year, args.quarter)
        revenue_data = extractor.extract()
        all_revenue_data.extend(revenue_data)

    elif args.year and not args.quarter:
        # Process all quarters for a specific year
        quarters = ['01', '02', '03', '04']
        found_pdfs = []

        for quarter in quarters:
            pdf_pattern = f"{args.year}/{quarter}/county/{args.year}_{quarter}_county.pdf"
            pdf_path = Path(pdf_pattern)
            if pdf_path.exists():
                found_pdfs.append((str(pdf_path), args.year, quarter))

        if not found_pdfs:
            logging.error(f"No county PDF files found for year {args.year}")
            return

        logging.info(f"Found {len(found_pdfs)} PDF files for year {args.year}")

        for pdf_path, year, quarter in found_pdfs:
            # Skip 2013_14 completely - insufficient data quality
            if year == "2013_14":
                logging.info(f"Skipping {year} Q{quarter} - insufficient data quality")
                continue

            # Skip 2014_15 Q1, Q2, Q3 - only process Q4 which contains all quarterly data
            if year == "2014_15" and quarter in ["01", "02", "03"]:
                logging.info(f"Skipping {year} Q{quarter} - data available in Q4 transposed table")
                continue

            try:
                extractor = RevenueExtractor(pdf_path, year, quarter)
                revenue_data = extractor.extract()
                all_revenue_data.extend(revenue_data)

                logging.info(f"Extracted {len(revenue_data)} records from {year} Q{quarter}")

            except Exception as e:
                logging.error(f"Error processing {pdf_path}: {e}")
                continue

    else:
        parser.print_help()
        return

    if not all_revenue_data:
        logging.error("No revenue data extracted")
        return

    # Validate performance calculations across all extracted data
    validate_all_revenue_data(all_revenue_data)

    # Create DataFrame and save to CSV
    df = pd.DataFrame(all_revenue_data)

    # Ensure columns are in the right order
    df = df[REVENUE_COLUMNS]

    # Sort by year, quarter, county for consistency
    df = df.sort_values(['year', 'quarter', 'county'])

    output_path = Path(args.output)
    df.to_csv(output_path, index=False)
    df.to_csv("../../absorption-rates-health-outcomes/tasks/data_cleaning_finance/input/own_source_revenue.csv", index = False)

    logging.info(f"Revenue data saved to {output_path}")
    logging.info(f"Total records: {len(df)}")
    logging.info(f"Years: {sorted(df['year'].unique())}")
    logging.info(f"Quarters: {sorted(df['quarter'].unique())}")
    logging.info(f"Counties: {len(df['county'].unique())}")


if __name__ == '__main__':
    main()