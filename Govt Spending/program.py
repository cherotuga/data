# Import required libraries
import asyncio
import platform
import pdfplumber  # For PDF text and table extraction
import pandas as pd  # For handling data in DataFrames
from pathlib import Path  # For file path manipulation
import re  # For regular expressions
import warnings  # For suppressing warnings
from fuzzywuzzy import process  # For fuzzy matching county names
import logging  # For detailed logging
import datetime
import argparse
from statistics import mode

# Set up logging to track progress and debug issues
logging.basicConfig(level=logging.CRITICAL, format='%(asctime)s - %(levelname)s - %(message)s')

# Suppress pdfplumber warnings to keep output clean
warnings.filterwarnings("ignore", module="pdfplumber.*")

# List of 47 counties for validation
COUNTIES = [
    "baringo", "bomet", "bungoma", "busia", "elgeyo marakwet", "embu", "garissa", "homa bay",
    "isiolo", "kajiado", "kakamega", "kericho", "kiambu", "kilifi", "kirinyaga", "kisii",
    "kisumu", "kitui", "kwale", "laikipia", "lamu", "machakos", "makueni", "mandera",
    "marsabit", "meru", "migori", "mombasa", "muranga", "nairobi city", "nakuru", "nandi",
    "narok", "nyamira", "nyandarua", "nyeri", "samburu", "siaya", "taita taveta", "tana river",
    "tharaka nithi", "trans nzoia", "turkana", "uasin gishu", "vihiga", "wajir", "west pokot"
]

# Debugging
TARGET_COUNTY = []  # Case-sensitive
toc_debug = False
normalize_debug = False
match_line_debug = False
header_debug = False
fallback_debug = False 

def normalize_county_name(name):
    """
    Normalize county names to handle special characters, spaces, and case variations.
    
    Args:
        name (str): Raw county name from PDF.
    
    Returns:
        str: Normalized county name.
    """
    if not name:
        return ""
    # Replace curly quotes, en-dashes, and other special characters
    no_special = re.sub(r"[’‘'‑-]", '', name)  # Remove apostrophes and dashes
    # Remove characters except letters, spaces
    letters_only = re.sub(r"[^a-zA-Z\s\-']", '', no_special)
    # Collapse spaces and lowercase
    normalized = re.sub(r'\s+', ' ', letters_only).strip().lower()

    if normalized in TARGET_COUNTY and normalize_debug == True:
        logging.info("Normalizing county name: raw=%s, normalized=%s", name, normalized)
        
    return normalized

NORMALIZED_COUNTY_MAP = {normalize_county_name(c): c for c in COUNTIES}

def fuzzy_match_county(name, counties, threshold=70):
    """
    Match a county name against the NORMALIZED_COUNTY_MAP list using fuzzy matching.
    
    Args:
        name (str): Raw county name.
        counties (list): List of valid county names.
        threshold (int): Minimum similarity score for a match.
    
    Returns:
        str or None: Matched county name or None if no match.
    """
    normalized = normalize_county_name(name)
    match = process.extractOne(normalized, counties, score_cutoff=threshold)

    if not match:
        logging.warning("No match for county name: %s (normalized: %s)", name, normalized)
        return None
    return match[0]

def parse_toc(pdf):
    """
    Extract the table of contents from the PDF, starting at "TABLE OF CONTENT" and stopping at "ACRONYMS".
    It captures county names and table types, ignoring page numbers.
    
    Args:
        pdf: pdfplumber PDF object.
    
    Returns:
        dict: Mapping of county to [(table_number, description), ...].
    """

    # Updated TOC pattern to handle en-dashes, varied spacing, and simpler county formats
    toc_pattern = r"Table\s+(\d+\.\d+(?:\.\d+)?)[\s]*(?:[:-‑])\s*([A-Za-z\s’'’‑-]+?)(?:\s*County)?(?:\s*,\s*(.*?))?(?:\s*\.*\s*\d+)?$"
    toc_map = {}
    in_toc = False
    seen_names = set()
    
    # Iterate through pages to find TOC
    for page in pdf.pages:
        text = page.extract_text() or ""
        # Detect start of TOC
        if "table of content" in text.lower():
            in_toc = True
            if toc_debug:
                logging.info("Found TOC start on page %d", page.page_number)
            continue
        # Detect end of TOC
        if in_toc and "acronyms" in text.lower():
            in_toc = False
            if toc_debug:
                logging.info("Found TOC end on page %d", page.page_number)
            break
        
        # Process TOC lines if in TOC section
        if in_toc:
            for line in text.splitlines():
                line = line.strip()
                
                if toc_debug:
                    print(f"[TOC Line] {line}")  # Diagnostic line
                match = re.search(toc_pattern, line, re.IGNORECASE)
                if match:
                    table_number = match.group(1)
                    county_raw = match.group(2).strip()
                    if toc_debug:
                        print(f"table_number: {table_number}")
                        print(f"county_raw: {county_raw}")
                        seen_names.add(county_raw)  # Track seen county name (raw)
                    description = match.group(3).strip() if match.group(3) else ""
                    county = fuzzy_match_county(county_raw, NORMALIZED_COUNTY_MAP)
                    if county:
                        if county not in toc_map:
                            toc_map[county] = []
                        toc_map[county].append((table_number, description))
                        if county in TARGET_COUNTY and toc_debug == True:
                            logging.info("TOC entry: %s, Table %s, %s", county, table_number, description)
                    else:
                        logging.warning("Failed to match county in TOC: %s", county_raw)
                elif toc_debug:
                    print(f"[NO MATCH] {line}")  # Highlight unmatched lines

    
    # Print all seen names at the end
    if toc_debug:
        print("\n=== County names seen in TOC ===")
        for name in sorted(seen_names):
            print(f"- {name}")

    return toc_map

def is_program_table(table, headers, county_name=None):
    """
    Check if a table is a program table based on its headers or content.
    Uses improved pattern matching to avoid false positives from donor program names.
    
    Args:
        table (list): Table data from pdfplumber.
        headers (list): Table headers.
        county_name (str): County name for debugging.
    
    Returns:
        bool: True if the table is a program table.
    """
    # Strong positive indicators for programme budget tables - need all 4 categories
    programme_keywords = ["programme", "programs", "sector"]
    sub_programme_keywords = ["sub-programme", "sub-program", "sub programme", "description"]
    budget_keywords = ["approved budget", "budget allocation", "revised budget", "estimates", "submitted estimates"]
    payment_keywords = ["actual payments", "payments", "expenditure"]
    
    # Exclude tables with these indicators (revenue, grants, etc.)
    exclude_keywords = ["revenue", "allocation (in kshs)", "receipts", "financing", "grant", 
                       "cara", "conditional grants", "equitable share"]
        
    # Debug logging
    if county_name in TARGET_COUNTY and header_debug == True:
        logging.info("=== DEBUGGING %s TABLE ===", county_name)
        logging.info("Headers: %s", headers)
        logging.info("First 3 rows: %s", table[:3])
    
    # Check for exclusion indicators first
    header_text = " ".join(str(h).lower() for h in headers if h)
    if any(exclude_word in header_text for exclude_word in exclude_keywords):
        if county_name in TARGET_COUNTY and header_debug == True:
            logging.info("✗ Excluded due to header indicators: revenue/grant table")
        return False
    
    # Check headers for all 4 required categories
    programme_header_match = False
    sub_programme_header_match = False
    budget_header_match = False
    payment_header_match = False
    
    for header in headers:
        if header:
            # Normalize header: lowercase, remove spaces around hyphens, remove extra whitespace
            header_normalized = str(header).lower().strip()
            header_normalized = ' '.join(header_normalized.split())  # Remove extra whitespace
            header_normalized = header_normalized.replace(' - ', '-').replace('- ', '-').replace(' -', '-')  # Normalize hyphens
            
            if any(keyword.lower() in header_normalized for keyword in programme_keywords):
                programme_header_match = True
                if county_name in TARGET_COUNTY and header_debug == True:
                    logging.info("✓ Programme header match found: %s (normalized: %s)", header, header_normalized)
                    
            if any(keyword.lower() in header_normalized for keyword in sub_programme_keywords):
                sub_programme_header_match = True
                if county_name in TARGET_COUNTY and header_debug == True:
                    logging.info("✓ Sub-programme header match found: %s (normalized: %s)", header, header_normalized)
                    
            if any(keyword.lower() in header_normalized for keyword in budget_keywords):
                budget_header_match = True
                if county_name in TARGET_COUNTY and header_debug == True:
                    logging.info("✓ Budget header match found: %s (normalized: %s)", header, header_normalized)
                    
            if any(keyword.lower() in header_normalized for keyword in payment_keywords):
                payment_header_match = True
                if county_name in TARGET_COUNTY and header_debug == True:
                    logging.info("✓ Payment header match found: %s (normalized: %s)", header, header_normalized)
    
    # No longer doing content matching - header-only approach
    
    # A table is a program table if it has ALL 4 required categories
    result = programme_header_match and sub_programme_header_match and budget_header_match and payment_header_match
    
    if county_name in TARGET_COUNTY and header_debug == True:
        logging.info("Header analysis:")
        logging.info("  All headers: %s", headers)
        logging.info("  Programme match (%s): %s", programme_header_match, programme_keywords)
        logging.info("  Sub-programme match (%s): %s", sub_programme_header_match, sub_programme_keywords) 
        logging.info("  Budget match (%s): %s", budget_header_match, budget_keywords)
        logging.info("  Payment match (%s): %s", payment_header_match, payment_keywords)
        logging.info("Final result: %s (programme=%s, sub_programme=%s, budget=%s, payment=%s)", 
                    result, programme_header_match, sub_programme_header_match, budget_header_match, payment_header_match)
        logging.info("=== END DEBUG ===")
    
    return result

def extract_programme_table_fallback(page, county_name, page_num):
    """
    Advanced fallback extraction for programme tables using text coordinates and pattern matching.
    Used when pdfplumber.extract_tables() returns empty results.
    
    Args:
        page: pdfplumber page object
        county_name: County name for debugging
        page_num: Page number for debugging
    
    Returns:
        list: Table data as list of lists, or empty list if extraction fails
    """
    if county_name in TARGET_COUNTY and fallback_debug:
        logging.info("=== FALLBACK EXTRACTION START: Page %d, County %s ===", page_num, county_name)
    
    try:
        # Extract text with coordinates
        text_lines = page.extract_text_lines()
        if not text_lines:
            if county_name in TARGET_COUNTY and fallback_debug:
                logging.info("No text lines found on page %d", page_num)
            return []
        
        # Look for programme table patterns
        programme_rows = []
        table_started = False
        
        # Patterns to identify programme table content
        programme_patterns = [
            r'^\d+\.\d+\s+[A-Z][a-zA-Z\s&,-]+',  # Numbered programmes (e.g., "1.1 ADMINISTRATION & COORDINATION")
            r'^[A-Z][A-Z\s&,-]{3,}',             # All-caps programme names
            r'^\s*Sub\s*Total',                  # Subtotal rows
            r'^\s*[A-Z][a-zA-Z\s&,-]+.*\d{1,3}(?:,\d{3})*(?:\.\d{2})?$'  # Programme with numbers at end
        ]
        
        budget_column_patterns = [
            r'\d{1,3}(?:,\d{3})*(?:\.\d{2})?',   # Currency format (e.g., 1,234,567.89)
            r'\d+\.\d{2}',                        # Simple decimal
            r'-'                                  # Dash for zero/empty values
        ]
        
        for line_info in text_lines:
            line_text = line_info.get('text', '').strip()
            
            if not line_text:
                continue
                
            # Check if this looks like a programme table row
            is_programme_row = any(re.match(pattern, line_text) for pattern in programme_patterns)
            
            if is_programme_row:
                table_started = True
                
                # Extract potential columns by position analysis
                # Split on multiple spaces but preserve structure
                cells = re.split(r'\s{3,}', line_text)
                
                # Filter out empty cells
                cells = [cell.strip() for cell in cells if cell.strip()]
                
                if len(cells) >= 2:  # Must have at least programme name + one budget column
                    programme_rows.append(cells)
                    
                    if county_name in TARGET_COUNTY and fallback_debug:
                        logging.info("Found programme row: %s", cells)
            
            elif table_started and not line_text:
                # Empty line might indicate end of table
                break
        
        if not programme_rows:
            if county_name in TARGET_COUNTY and fallback_debug:
                logging.info("No programme rows found using pattern matching")
            return []
        
        # Analyze column structure and create consistent table
        if len(programme_rows) < 2:
            if county_name in TARGET_COUNTY and fallback_debug:
                logging.info("Too few rows (%d) for table structure", len(programme_rows))
            return []
        
        # Determine column count from most common row length
        col_counts = [len(row) for row in programme_rows]
        most_common_cols = max(set(col_counts), key=col_counts.count)
        
        # Filter and normalize rows to consistent column count
        normalized_rows = []
        for row in programme_rows:
            if len(row) == most_common_cols:
                normalized_rows.append(row)
            elif len(row) > most_common_cols:
                # Truncate extra columns
                normalized_rows.append(row[:most_common_cols])
            else:
                # Pad with empty strings
                padded_row = row + [''] * (most_common_cols - len(row))
                normalized_rows.append(padded_row)
        
        if normalized_rows:
            # Create standard headers based on column count
            if most_common_cols == 2:
                headers = ['Programme', 'Approved Budget']
            elif most_common_cols == 3:
                headers = ['Programme', 'Approved Budget', 'Actual Payments']
            elif most_common_cols == 4:
                headers = ['Programme', 'Sub-Programme', 'Approved Budget', 'Actual Payments']
            else:
                headers = [f'Column_{i+1}' for i in range(most_common_cols)]
            
            # Combine headers with data
            table_data = [headers] + normalized_rows
            
            if county_name in TARGET_COUNTY and fallback_debug:
                logging.info("Fallback extraction successful: %d rows, %d columns", 
                           len(normalized_rows), most_common_cols)
                logging.info("Headers: %s", headers)
                logging.info("First 3 data rows: %s", normalized_rows[:3])
            
            return table_data
        
    except Exception as e:
        if county_name in TARGET_COUNTY and fallback_debug:
            logging.error("Fallback extraction failed: %s", e)
        return []
    
    finally:
        if county_name in TARGET_COUNTY and fallback_debug:
            logging.info("=== FALLBACK EXTRACTION END ===")
    
    return []

def extract_text_table(page):
    """
    Legacy fallback to extract table-like data from page text if pdfplumber table extraction fails.
    
    Args:
        page: pdfplumber page object.
    
    Returns:
        list: List of rows, where each row is a list of cells, or empty list if no table-like data found.
    """
    text = page.extract_text() or ""
    lines = text.splitlines()
    table_data = []
    current_row = []
    for line in lines:
        # Split line by multiple spaces to approximate table columns
        cells = re.split(r'\s{2,}', line.strip())
        if len(cells) > 1:  # Likely a table row if multiple cells
            current_row = cells
            table_data.append(current_row)
        elif current_row and line.strip():  # Continuation of row data
            current_row[-1] += " " + line.strip()
        elif current_row and not line.strip():  # End of table
            break
    return table_data if table_data else []

def headers_match(prev_headers, current_headers, county_name, threshold=0.5):
    """
    Check if headers of two tables are sufficiently similar to indicate a multi-page table.
    
    Args:
        prev_headers (list): Headers of the previous table.
        current_headers (list): Headers of the current table.
        threshold (float): Fraction of headers that must match.
    
    Returns:
        bool: True if headers are similar enough.
    """
    if not prev_headers or not current_headers:
        return False
    matches = sum(1 for h1, h2 in zip(prev_headers, current_headers) if h1 == h2)
    similarity = matches / max(len(prev_headers), len(current_headers))
    if similarity < threshold and header_debug == True and county_name in TARGET_COUNTY:
        logging.info("Header mismatch: prev=%s, current=%s, similarity=%.2f", 
                      prev_headers, current_headers, similarity)
    return similarity >= threshold

def finalize_buffer(current_county, table_buffer, county_tables):
    """
    Merge table buffer into county_tables and reset buffer.
    
    Args:
        current_county (str): Current county name.
        table_buffer (list): List of DataFrames to merge.
        county_tables (dict): Dictionary of county to DataFrames.
    """
    if current_county and table_buffer:
        county_tables[current_county] = pd.concat(table_buffer, ignore_index=True).dropna(how='all')
        if current_county in TARGET_COUNTY:
            logging.info("Merged %d program tables for %s", len(table_buffer), current_county)
    elif current_county and not table_buffer:
        if current_county in TARGET_COUNTY:
            logging.info("No program tables for %s; keeping empty DataFrame", current_county)

from statistics import mode

def extract_programme_tables(pdf_path):
    county_tables = {county: pd.DataFrame() for county in NORMALIZED_COUNTY_MAP}
    errors = []
    
    # Updated regex patterns
    program_heading_pattern = r"Table\s+(\d+\.\d+(?:\.\d+)?)[\s]*(?:[:-‑])\s*(?:(?:[A-Za-z\s’'’-]+?)\s*County\s*[,;]?\s*)?(Budget\s+Execution\s+by\s+(?:Programmes|Programs)\s+and\s+(?:Sub-Programmes|Sub-Programs)[^0-9]*?)(?:\s*\.*\s*\d+)?(?=\n|$)"
    new_county_pattern = r"^(?:\d+\.\d+\s*)?(?:County\s+Government\s+of\s+)?([A-Za-z\s’'’-]+?)(?:\s*County)?(?:\s*\.*\s*\d+)?$"
    nairobi_pattern = r"(?:\d+\.\d+\.\s*)?Nairobi\s+City\s+County(?:\s+Government)?(?:\s*\.*\s*\d+)?(?=\n|$)"
    overview_pattern = r"Overview\s+of\s+FY\s+2023/24\s+Budget"
    end_section_pattern = r"(Accounts\s+Operated\s+(?:by\s+)?Commercial\s+Banks|Key\s+Observations\s+and\s+Recommendations)"
    
    try:
        with pdfplumber.open(pdf_path) as pdf:
            toc_map = parse_toc(pdf)
            if toc_debug:
                logging.info(
                    "TOC parsed:\n%s",
                    ",\n".join(
                        f"{k}: [\n    " + ",\n    ".join(f"({t}, {d})" for t, d in v) + "\n  ]"
                        for k, v in toc_map.items()
                    )
                )
            
            current_county = None
            table_buffer = []
            prev_headers = None
            last_heading_lines = []
            county_heading_y = None  # Track Y-position of current county heading
            
            for page_num, page in enumerate(pdf.pages, 1):
                text_lines = []
                if hasattr(page, 'chars'):
                    current_line = []
                    font_sizes = []
                    prev_y = None
                    line_y = None
                    for char in page.chars:
                        y = char['y0']
                        font_size = char.get('size', 10)
                        if prev_y is not None and abs(y - prev_y) > 5:
                            if current_line:
                                line_text = ''.join(current_line)
                                line_font_size = mode([fs for fs in font_sizes if fs > 0]) if any(fs > 0 for fs in font_sizes) else max(font_sizes, default=10)
                                text_lines.append((line_text, line_font_size, line_y))
                            current_line = [char['text']]
                            font_sizes = [font_size]
                            line_y = y
                        else:
                            current_line.append(char['text'])
                            font_sizes.append(font_size)
                            if line_y is None:
                                line_y = y
                        prev_y = y
                    if current_line:
                        line_text = ''.join(current_line)
                        line_font_size = mode([fs for fs in font_sizes if fs > 0]) if any(fs > 0 for fs in font_sizes) else max(font_sizes, default=10)
                        text_lines.append((line_text, line_font_size, line_y))
                
                if not current_county and not text_lines and last_heading_lines:
                    for line, font_size in last_heading_lines[-5:]:
                        nairobi_match = re.search(nairobi_pattern, line, re.IGNORECASE)
                        new_county_match = re.search(new_county_pattern, line, re.IGNORECASE)
                        program_match = re.search(program_heading_pattern, line, re.IGNORECASE)
                        is_heading = font_size is not None and (font_size >= 10 or font_size == 0.0)
                        if nairobi_match and is_heading:
                            county = fuzzy_match_county("Nairobi City", NORMALIZED_COUNTY_MAP)
                            if county:
                                finalize_buffer(current_county, table_buffer, county_tables)
                                current_county = county
                                table_buffer = []
                                prev_headers = None
                                if county in TARGET_COUNTY:
                                    logging.info("Page %d: Set county to %s from previous page Nairobi heading (font_size=%.1f): %s", 
                                             page_num, county, font_size or 0, line.strip())
                                break
                        elif new_county_match and is_heading:
                            county_raw = new_county_match.group(1).strip()
                            county = fuzzy_match_county(county_raw, NORMALIZED_COUNTY_MAP)
                            if county:
                                finalize_buffer(current_county, table_buffer, county_tables)
                                current_county = county
                                table_buffer = []
                                prev_headers = None
                                if county in TARGET_COUNTY:
                                    logging.info("Page %d: Set county to %s from previous page county heading (font_size=%.1f): %s", 
                                             page_num, county, font_size or 0, line.strip())
                                break
                        elif program_match and is_heading:
                            county_raw = program_match.group(1).strip()
                            county = fuzzy_match_county(county_raw, NORMALIZED_COUNTY_MAP)
                            if county:
                                finalize_buffer(current_county, table_buffer, county_tables)
                                current_county = county
                                table_buffer = []
                                prev_headers = None
                                if county in TARGET_COUNTY:
                                    logging.info("Page %d: Set county to %s from previous page program heading (font_size=%.1f): %s", 
                                             page_num, county, font_size or 0, line.strip())
                                break
                
                for line, font_size, line_y in text_lines:
                    normalized_line = normalize_county_name(line)
                    log_county = any(c in normalized_line for c in TARGET_COUNTY)
                    
                    nairobi_match = re.search(nairobi_pattern, line, re.IGNORECASE)
                    new_county_match = re.search(new_county_pattern, line, re.IGNORECASE)
                    program_match = re.search(program_heading_pattern, line, re.IGNORECASE)

                    overview_match = re.search(overview_pattern, line, re.IGNORECASE)
                    end_match = re.search(end_section_pattern, line, re.IGNORECASE)
                    
                    is_heading = font_size is not None and (font_size >= 10 or font_size == 0.0)
                    
                    if log_county and match_line_debug:
                        if nairobi_match:
                            logging.debug("✅ Nairobi regex matched: line='%s'", line)
                        if new_county_match:
                            logging.debug("✅ New county regex matched: line='%s', county_raw='%s'", line, new_county_match.group(1).strip())
                        if program_match:
                            logging.debug("✅ Program regex matched: line='%s', county_raw='%s'", line, program_match.group(1).strip())
                        if not (nairobi_match or new_county_match or program_match):
                            logging.warning("🙅‍♀️Unmatched county line: line='%s', font_size=%.1f, reason='No regex match'", 
                                           line, font_size or 0)
                        if not is_heading:
                            logging.warning("🚫Unmatched county line: line='%s', font_size=%.1f, reason='Font size too small'", 
                                           line, font_size or 0)
                    
                    if nairobi_match and is_heading:
                        county = fuzzy_match_county("Nairobi City", NORMALIZED_COUNTY_MAP)
                        if county:
                            finalize_buffer(current_county, table_buffer, county_tables)
                            current_county = county
                            table_buffer = []
                            prev_headers = None
                            county_heading_y = line_y  # Record Y-position of county heading
                            if county in TARGET_COUNTY:
                                logging.info("Ⓜ️ Matched Nairobi heading: line='%s', font_size=%.1f, county=%s, y=%.1f", 
                                         line, font_size or 0, county, line_y or 0)
                            continue
                    
                    if new_county_match and is_heading:
                        county_raw = new_county_match.group(1).strip()
                        county_raw_clean = re.sub(r"County\s+Government\s+of\s+", "", county_raw, flags=re.IGNORECASE)
                        county = fuzzy_match_county(county_raw_clean, NORMALIZED_COUNTY_MAP)
                        if county:
                            finalize_buffer(current_county, table_buffer, county_tables)
                            current_county = county
                            table_buffer = []
                            prev_headers = None
                            county_heading_y = line_y  # Record Y-position of county heading
                            if county in TARGET_COUNTY:
                                logging.info("Ⓜ️ Matched new county heading: line='%s', font_size=%.1f, county=%s, y=%.1f", 
                                         line, font_size or 0, county, line_y or 0)
                            continue
                    
                    # Additional Q2 patterns: Handle both "County Government [County]" and "County Government of [County]"
                    q2_patterns = [
                        r"(\d+\.\d+\.?\s*)?County\s+Government\s+([A-Za-z\s'''-]+?)(?:\s*$)",  # "3.1. County Government Baringo"
                        r"(\d+\.\d+\.?\s*)?County\s+Government\s+of\s+([A-Za-z\s'''-]+?)(?:\s*$)"  # "3.2. County Government of Bomet"
                    ]
                    
                    for q2_pattern in q2_patterns:
                        q2_match = re.search(q2_pattern, line, re.IGNORECASE)
                        if q2_match and is_heading:
                            county_raw = q2_match.groups()[-1].strip()  # Get last group (county name)
                            # Clean up any remaining "of" prefix
                            county_raw = re.sub(r"^of\s+", "", county_raw, flags=re.IGNORECASE)
                            county = fuzzy_match_county(county_raw, NORMALIZED_COUNTY_MAP)
                            if county:
                                finalize_buffer(current_county, table_buffer, county_tables)
                                current_county = county
                                table_buffer = []
                                prev_headers = None
                                county_heading_y = line_y  # Record Y-position of county heading
                                if county in TARGET_COUNTY:
                                    logging.info("Ⓜ️ Matched Q2 county pattern: line='%s', font_size=%.1f, county=%s, y=%.1f", 
                                             line, font_size or 0, county, line_y or 0)
                                continue
                    
                    # Overview is a back up in case the start isn't detected.
                    # if overview_match and is_heading and current_county:
                    #     finalize_buffer(current_county, table_buffer, county_tables)
                    #     table_buffer = []
                    #     prev_headers = None
                    #     if current_county in TARGET_COUNTY:
                    #         logging.info("🔍 Overview matched for %s: line='%s', font_size=%.1f", 
                    #                  current_county, line, font_size or 0)
                    #     continue
                    
                    if program_match and is_heading:
                        county_raw = program_match.group(1).strip()
                        county = fuzzy_match_county(county_raw, NORMALIZED_COUNTY_MAP)
                        if county:
                            if current_county != county:
                                finalize_buffer(current_county, table_buffer, county_tables)
                                current_county = county
                                table_buffer = []
                                prev_headers = None
                                county_heading_y = line_y  # Record Y-position of county heading
                            if county in TARGET_COUNTY:
                                logging.info("👤 Program heading matched: %s | County raw: %s → %s, y=%.1f", 
                                         line.strip(), county_raw, county, line_y or 0)
                            continue
                    
                    if end_match and current_county:
                        finalize_buffer(current_county, table_buffer, county_tables)
                        table_buffer = []
                        prev_headers = None
                        current_county = None
                        if current_county in TARGET_COUNTY:
                            logging.info("Section end detected (font_size=%.1f): %s", font_size or 0, line.strip())
                        continue
                                
                # Store headings for next page
                last_heading_lines = text_lines[-5:] if text_lines else last_heading_lines
                
                # Log county status before table processing
                if current_county in TARGET_COUNTY:
                    logging.info("Processing tables on page %d, current_county=%s", page_num, current_county)
                
                # Extract tables
                try:
                    tables = page.extract_tables(table_settings={
                        "vertical_strategy": "lines",
                        "horizontal_strategy": "lines",
                        "snap_tolerance": 10,
                        "min_words_vertical": 1,
                        "min_words_horizontal": 1
                    })
                    
                    # Try fallback extraction if no tables found and we have a county
                    if not tables and current_county:
                        fallback_table = extract_programme_table_fallback(page, current_county, page_num)
                        if fallback_table:
                            tables = [fallback_table]
                            if current_county in TARGET_COUNTY and fallback_debug:
                                logging.info("Using fallback extraction for %s on page %d", current_county, page_num)
                    
                    for table_idx, table in enumerate(tables):
                        if not table or len(table) < 2 or len(table[0]) < 2:
                            continue
                        
                        # Handle headers
                        headers = table[0]
                        data_start = 1
                        if headers and (len(headers) == 1 or all(h is None or h == "" for h in headers[1:])):
                            headers = table[1] if len(table) > 1 else [f"col_{i}" for i in range(len(table[0]))]
                            data_start = 2
                        headers = [f"col_{i}" if h is None or h == "" else h for i, h in enumerate(headers)]
                        seen = {}
                        new_headers = []
                        for h in headers:
                            if h in seen:
                                seen[h] += 1
                                new_headers.append(f"{h}_{seen[h]}")
                            else:
                                seen[h] = 0
                                new_headers.append(h)
                        headers = new_headers
                        
                        # Check if table is a program table
                        if current_county and is_program_table(table, headers, current_county):
                            # Spatial positioning check: only assign tables that come AFTER the county heading
                            table_assigned = True
                            if county_heading_y is not None:
                                # Get table's approximate Y-position
                                try:
                                    # Simple heuristic: check if table appears above the county heading
                                    # In PDF coordinates, Y increases downward:
                                    # - Higher Y values = towards bottom of page
                                    # - Lower Y values = towards top of page
                                    # Table should be BELOW heading, so table_y should be < county_heading_y
                                    
                                    # For first table on page, assume it's near top (high Y value ~800)
                                    # County heading Y values are typically lower (closer to bottom)
                                    estimated_table_y = None
                                    
                                    if table_idx == 0:
                                        # First table is usually near top of page
                                        # Check if we can find "Programme Description" or similar in text_lines
                                        for line_text, font_size, line_y in text_lines:
                                            if 'programme' in line_text.lower() and 'description' in line_text.lower():
                                                estimated_table_y = line_y
                                                break
                                        
                                        # If we couldn't find table header, use heuristic
                                        if estimated_table_y is None and len(text_lines) > 0:
                                            # Assume first table is in top 20% of page
                                            max_y = max(line_y for _, _, line_y in text_lines if line_y)
                                            estimated_table_y = max_y * 0.9  # Near top
                                    
                                    # Compare table position with county heading
                                    if estimated_table_y is not None:
                                        # Table should be BELOW heading: table_y < county_heading_y (in PDF coords)
                                        if estimated_table_y > county_heading_y:
                                            # Table is ABOVE heading - should not assign
                                            table_assigned = False
                                            if current_county in TARGET_COUNTY:
                                                logging.info("🚫 Skipping table %d on page %d: appears ABOVE county heading for %s (table_y=%.1f, heading_y=%.1f)", 
                                                           table_idx, page_num, current_county, estimated_table_y, county_heading_y)
                                        else:
                                            if current_county in TARGET_COUNTY:
                                                logging.info("✅ Assigning table %d on page %d: appears BELOW county heading for %s (table_y=%.1f, heading_y=%.1f)", 
                                                           table_idx, page_num, current_county, estimated_table_y, county_heading_y)
                                            
                                except Exception as e:
                                    # If we can't determine positioning, default to assigning the table
                                    if current_county in TARGET_COUNTY:
                                        logging.debug("Could not determine table positioning on page %d: %s", page_num, e)
                            
                            if not table_assigned:
                                continue
                            # Validate with TOC
                            if current_county in toc_map and any("Budget Execution by Programmes" in desc for _, desc in toc_map[current_county]):
                                if current_county in TARGET_COUNTY:
                                    logging.info("Table on page %d for %s matches TOC program table", page_num, current_county)
                            else:
                                if current_county in TARGET_COUNTY:
                                    logging.warning("Table on page %d for %s not in TOC program tables", page_num, current_county)
                            
                            # Verify header continuity
                            if prev_headers and not headers_match(prev_headers, headers, current_county):
                                if current_county in TARGET_COUNTY and header_debug == True:
                                    logging.info("Page %d, table %d: Header mismatch for %s; headers: %s vs %s", 
                                               page_num, table_idx, current_county, headers, prev_headers)
                                finalize_buffer(current_county, table_buffer, county_tables)
                                table_buffer = []
                            
                            # Convert to DataFrame and add to buffer
                            try:
                                df = pd.DataFrame(table[data_start:], columns=headers)
                                df['page_number'] = page_num
                                df['table_index'] = table_idx
                                table_buffer.append(df)
                                prev_headers = headers
                                if current_county in TARGET_COUNTY:
                                    logging.info("Page %d: Added program table %d to buffer for %s", 
                                             page_num, table_idx, current_county)
                            except Exception as e:
                                errors.append(f"Page {page_num}, table {table_idx}: Error processing table: {e}")
                                logging.error("Page %d, table %d: Failed to process table for %s: %s", 
                                              page_num, table_idx, current_county, e)
                        elif current_county in TARGET_COUNTY and header_debug:
                            logging.warning("Page %d, table %d for %s DID NOT pass program table check", 
                                            page_num, table_idx, current_county)
                            logging.info("  ↳ Headers: %s", headers)
                            logging.info("  ↳ First 3 rows: %s", table[data_start:data_start+3])
                    
                    # Fallback to text-based table extraction
                    if not tables and current_county:
                        text_table = extract_text_table(page)
                        if text_table and len(text_table) >= 2 and len(text_table[0]) >= 2:
                            headers = text_table[0]
                            seen = {}
                            new_headers = []
                            for h in headers:
                                if h in seen:
                                    seen[h] += 1
                                    new_headers.append(f"{h}_{seen[h]}")
                                else:
                                    seen[h] = 0
                                    new_headers.append(h)
                            headers = new_headers
                            
                            if is_program_table(text_table, headers, current_county):
                                if current_county in toc_map and any("Budget Execution by Programmes" in desc for _, desc in toc_map[current_county]):
                                    if current_county in TARGET_COUNTY:
                                        logging.info("Text table on page %d for %s matches TOC program table", page_num, current_county)
                                else:
                                    logging.warning("Text table on page %d for %s not in TOC program tables", page_num, current_county)
                                
                                if prev_headers and not headers_match(prev_headers, headers, current_county):
                                    if current_county in TARGET_COUNTY and header_debug == True:
                                        logging.info("Page %d, text table: Header mismatch for %s; headers: %s vs %s", 
                                                   page_num, current_county, headers, prev_headers)
                                    finalize_buffer(current_county, table_buffer, county_tables)
                                    table_buffer = []
                                
                                try:
                                    df = pd.DataFrame(text_table[1:], columns=headers)
                                    df['page_number'] = page_num
                                    df['table_index'] = -1
                                    table_buffer.append(df)
                                    prev_headers = headers
                                    if current_county in TARGET_COUNTY:
                                        logging.info("Page %d: Added text-based program table to buffer for %s", 
                                                 page_num, current_county)
                                except Exception as e:
                                    errors.append(f"Page {page_num}: Error processing text-based table: {e}")
                                    logging.error("Page %d: Failed to process text-based table for %s: %s", 
                                                  page_num, current_county, e)
                
                except Exception as e:
                    errors.append(f"Page {page_num}: Table extraction failed: {e}")
                    logging.error("Page %d: Table extraction failed: %s", page_num, e)
            
            # Finalize any remaining tables
            finalize_buffer(current_county, table_buffer, county_tables)
            
            # Validate with TOC
            missing = [c for c in NORMALIZED_COUNTY_MAP if county_tables.get(c, pd.DataFrame()).empty]
            if missing:
                logging.warning("Missing or empty tables for: %s", ", ".join(missing))
            for county in toc_map:
                if any("Budget Execution by Programmes" in desc for _, desc in toc_map[county]):
                    if county not in county_tables or county_tables[county].empty:
                        logging.warning("TOC expects program table for %s, but none found", county)
    
    except Exception as e:
        errors.append(f"Error opening PDF {pdf_path}: {e}")
        logging.error("Error opening PDF %s: %s", pdf_path, e)
    
    return county_tables, errors

class PageElement:
    """Represents an element (text or table) from a PDF page with position info"""
    def __init__(self, element_type, data, y_position, page_num):
        self.element_type = element_type  # 'text' or 'table'
        self.data = data
        self.y_position = y_position
        self.page_num = page_num
    
    def is_text(self):
        return self.element_type == 'text'
    
    def is_table(self):
        return self.element_type == 'table'

def extract_page_elements_by_position(page, page_num):
    """Extract both text lines and tables from a page, sorted by Y-position"""
    elements = []
    
    # Extract text lines with position
    text_lines = []
    if hasattr(page, 'chars'):
        current_line = []
        font_sizes = []
        prev_y = None
        line_y = None
        for char in page.chars:
            y = char['y0']
            font_size = char.get('size', 10)
            if prev_y is not None and abs(y - prev_y) > 5:
                if current_line:
                    line_text = ''.join(current_line)
                    line_font_size = mode([fs for fs in font_sizes if fs > 0]) if any(fs > 0 for fs in font_sizes) else max(font_sizes, default=10)
                    text_lines.append((line_text, line_font_size, line_y))
                current_line = [char['text']]
                font_sizes = [font_size]
                line_y = y
            else:
                current_line.append(char['text'])
                font_sizes.append(font_size)
                if line_y is None:
                    line_y = y
            prev_y = y
        if current_line:
            line_text = ''.join(current_line)
            line_font_size = mode([fs for fs in font_sizes if fs > 0]) if any(fs > 0 for fs in font_sizes) else max(font_sizes, default=10)
            text_lines.append((line_text, line_font_size, line_y))
    
    # Add text elements
    for line_text, font_size, line_y in text_lines:
        elements.append(PageElement('text', (line_text, font_size, line_y), line_y, page_num))
    
    # Extract tables with estimated positions
    try:
        tables = page.extract_tables(table_settings={
            "vertical_strategy": "lines",
            "horizontal_strategy": "lines", 
            "snap_tolerance": 10,
            "min_words_vertical": 1,
            "min_words_horizontal": 1
        })
        
        # Estimate table positions based on text lines
        for table_idx, table in enumerate(tables):
            if table and len(table) >= 2 and len(table[0]) >= 2:
                # Estimate table Y-position by looking for programme table header combinations
                estimated_y = None
                
                def matches_programme_table_header(line_text):
                    """Check if line matches programme table header using flexible combinations"""
                    line_lower = line_text.lower()
                    
                    # Define header pattern combinations based on actual extracted data
                    header_combinations = {
                        'programme_type': ['programme', 'program', 'sector'],
                        'sub_programme': ['sub-programme', 'sub-program', 'description'],  
                        'budget_type': ['approved budget', 'approved estimates', 'estimated budget', 'approved'],
                        'payment_type': ['actual payments', 'actual pay', 'actual']
                    }
                    
                    # Must match at least 2 categories to be a programme table
                    matches = 0
                    
                    # Check programme type (programme/program/sector)
                    if any(pattern in line_lower for pattern in header_combinations['programme_type']):
                        matches += 1
                    
                    # Check budget type (approved budget/estimates/etc)
                    if any(pattern in line_lower for pattern in header_combinations['budget_type']):
                        matches += 1
                        
                    # Check payment type (actual payments/pay/etc)  
                    if any(pattern in line_lower for pattern in header_combinations['payment_type']):
                        matches += 1
                        
                    return matches >= 2
                
                for line_text, font_size, line_y in text_lines:
                    if matches_programme_table_header(line_text):
                        estimated_y = line_y
                        break
                
                # If we can't find programme headers, use a heuristic based on table index
                if estimated_y is None and text_lines:
                    max_y = max(line_y for _, _, line_y in text_lines if line_y)
                    estimated_y = max_y - (table_idx * 100)  # Rough estimate
                
                if estimated_y is not None:
                    elements.append(PageElement('table', table, estimated_y, page_num))
    
    except Exception as e:
        logging.warning(f"Failed to extract tables from page {page_num}: {e}")
    
    # Sort elements by Y-position (descending - top to bottom)
    elements.sort(key=lambda x: x.y_position, reverse=True)
    
    return elements

def extract_programme_tables_sequential(pdf_path):
    """
    Sequential processing version that handles elements in document order
    """
    county_tables = {county: pd.DataFrame() for county in NORMALIZED_COUNTY_MAP}
    errors = []
    
    # Regex patterns (reuse existing ones)
    program_heading_pattern = r"Table\s+(\d+\.\d+(?:\.\d+)?)[\s]*(?:[:-‑])\s*(?:(?:[A-Za-z\s'''-]+?)\s*County\s*[,;]?\s*)?(Budget\s+Execution\s+by\s+(?:Programmes|Programs)\s+and\s+(?:Sub-Programmes|Sub-Programs)[^0-9]*?)(?:\s*\.*\s*\d+)?(?=\n|$)"
    new_county_pattern = r"^(?:\d+\.\d+\s*)?(?:County\s+Government\s+of\s+)?([A-Za-z\s'''-]+?)(?:\s*County)?(?:\s*\.*\s*\d+)?$"
    nairobi_pattern = r"(?:\d+\.\d+\.\s*)?Nairobi\s+City\s+County(?:\s+Government)?(?:\s*\.*\s*\d+)?(?=\n|$)"
    end_section_pattern = r"(Accounts\s+Operated\s+(?:by\s+)?Commercial\s+Banks|Key\s+Observations\s+and\s+Recommendations)"
    
    # Q2-specific patterns
    q2_patterns = [
        r"(\d+\.\d+\.?\s*)?County\s+Government\s+([A-Za-z\s'''-]+?)(?:\s*$)",
        r"(\d+\.\d+\.?\s*)?County\s+Government\s+of\s+([A-Za-z\s'''-]+?)(?:\s*$)"
    ]
    
    try:
        with pdfplumber.open(pdf_path) as pdf:
            toc_map = parse_toc(pdf)
            
            current_county = None
            table_buffer = []
            prev_headers = None
            
            for page_num, page in enumerate(pdf.pages, 1):
                elements = extract_page_elements_by_position(page, page_num)
                
                for element in elements:
                    if element.is_text():
                        line, font_size, line_y = element.data
                        normalized_line = normalize_county_name(line)
                        is_heading = font_size is not None and (font_size >= 10 or font_size == 0.0)
                        
                        # County section START detection
                        county_detected = False
                        new_county = None
                        
                        # Nairobi special case
                        nairobi_match = re.search(nairobi_pattern, line, re.IGNORECASE)
                        if nairobi_match and is_heading:
                            new_county = fuzzy_match_county("Nairobi City", NORMALIZED_COUNTY_MAP)
                            county_detected = True
                        
                        # Standard county patterns
                        if not county_detected:
                            new_county_match = re.search(new_county_pattern, line, re.IGNORECASE)
                            if new_county_match and is_heading:
                                county_raw = new_county_match.group(1).strip()
                                county_raw_clean = re.sub(r"County\s+Government\s+of\s+", "", county_raw, flags=re.IGNORECASE)
                                new_county = fuzzy_match_county(county_raw_clean, NORMALIZED_COUNTY_MAP)
                                county_detected = True
                        
                        # Q2 patterns
                        if not county_detected:
                            for q2_pattern in q2_patterns:
                                q2_match = re.search(q2_pattern, line, re.IGNORECASE)
                                if q2_match and is_heading:
                                    county_raw = q2_match.groups()[-1].strip()
                                    county_raw = re.sub(r"^of\s+", "", county_raw, flags=re.IGNORECASE)
                                    new_county = fuzzy_match_county(county_raw, NORMALIZED_COUNTY_MAP)
                                    county_detected = True
                                    break
                        
                        # Programme heading pattern
                        if not county_detected:
                            program_match = re.search(program_heading_pattern, line, re.IGNORECASE)
                            if program_match and is_heading:
                                county_raw = program_match.group(1).strip()
                                new_county = fuzzy_match_county(county_raw, NORMALIZED_COUNTY_MAP)
                                county_detected = True
                        
                        # Handle county section START
                        if county_detected and new_county:
                            # Finalize previous county
                            finalize_buffer(current_county, table_buffer, county_tables)
                            
                            # Start new county section
                            current_county = new_county
                            table_buffer = []
                            prev_headers = None
                            
                            if current_county in TARGET_COUNTY:
                                logging.info("Page %d: Started county section %s", page_num, current_county)
                            continue
                        
                        # County section END detection
                        end_match = re.search(end_section_pattern, line, re.IGNORECASE)
                        if end_match and current_county:
                            finalize_buffer(current_county, table_buffer, county_tables)
                            if current_county in TARGET_COUNTY:
                                logging.info("Page %d: Ended county section %s", page_num, current_county)
                            current_county = None
                            table_buffer = []
                            prev_headers = None
                            continue
                    
                    elif element.is_table() and current_county:
                        table = element.data
                        
                        # Handle table headers
                        if not table or len(table) < 2 or len(table[0]) < 2:
                            continue
                            
                        headers = table[0]
                        data_start = 1
                        if headers and (len(headers) == 1 or all(h is None or h == "" for h in headers[1:])):
                            headers = table[1] if len(table) > 1 else [f"col_{i}" for i in range(len(table[0]))]
                            data_start = 2
                        headers = [f"col_{i}" if h is None or h == "" else h for i, h in enumerate(headers)]
                        
                        # Clean duplicate headers
                        seen = {}
                        new_headers = []
                        for h in headers:
                            if h in seen:
                                seen[h] += 1
                                new_headers.append(f"{h}_{seen[h]}")
                            else:
                                seen[h] = 0
                                new_headers.append(h)
                        headers = new_headers
                        
                        # Check if this is a programme table
                        if current_county in TARGET_COUNTY:
                            logging.info("Page %d: Found table for %s with headers: %s", page_num, current_county, headers[:3])
                        
                        if is_program_table(table, headers, current_county):
                            # Multi-page table continuity check
                            if prev_headers and not headers_match(prev_headers, headers, current_county):
                                # Different headers - finalize previous table batch
                                if table_buffer:
                                    finalize_buffer(current_county, table_buffer, county_tables) 
                                    table_buffer = []
                                    if current_county in TARGET_COUNTY:
                                        logging.info("Page %d: Header mismatch, finalized table batch for %s", page_num, current_county)
                            
                            # Add table to buffer
                            try:
                                df = pd.DataFrame(table[data_start:], columns=headers)
                                df['page_number'] = page_num
                                df['table_index'] = 0  # Sequential processing doesn't have table index
                                table_buffer.append(df)
                                prev_headers = headers
                                
                                if current_county in TARGET_COUNTY:
                                    logging.info("Page %d: Added program table to buffer for %s (buffer size: %d)", 
                                               page_num, current_county, len(table_buffer))
                            except Exception as e:
                                errors.append(f"Page {page_num}: Error processing table for {current_county}: {e}")
                                logging.error("Page %d: Failed to process table for %s: %s", page_num, current_county, e)
            
            # Finalize any remaining tables
            finalize_buffer(current_county, table_buffer, county_tables)
            
            # Validate with TOC
            missing = [c for c in NORMALIZED_COUNTY_MAP if county_tables.get(c, pd.DataFrame()).empty]
            if missing:
                logging.warning("Missing or empty tables for: %s", ", ".join(missing))
    
    except Exception as e:
        errors.append(f"Error opening PDF {pdf_path}: {e}")
        logging.error("Error opening PDF %s: %s", pdf_path, e)
    
    return county_tables, errors

def process_pdf(pdf_path):
    """
    Process a single PDF file: extract tables, save to CSVs, log errors.
    """
    try:
        pdf_path_obj = Path(pdf_path)
        output_dir = Path("program") / pdf_path_obj.parent
        output_dir.mkdir(parents=True, exist_ok=True)

        logging.info("Processing %s using sequential processing", pdf_path)
        county_tables, errors = extract_programme_tables_sequential(pdf_path)

        for county in NORMALIZED_COUNTY_MAP:
            df = county_tables.get(county, pd.DataFrame())
            output_path = output_dir / f"{county.replace(' ', '_').replace('\'', '')}_programme_table.csv"
            df.to_csv(output_path, index=False)
            if not df.empty:
                if county in TARGET_COUNTY:
                    logging.info("Saved table for %s to %s", county, output_path)
            else:
                if county in TARGET_COUNTY:
                    logging.info("Created empty table for %s at %s", county, output_path)

        if errors:
            logging.error("Errors encountered in %s:", pdf_path)
            for error in errors:
                logging.error(error)

        extracted_count = sum(1 for df in county_tables.values() if not df.empty)
        logging.info("Extracted %d non-empty tables out of expected 47", extracted_count)
        if extracted_count < 47:
            missing = [c for c in NORMALIZED_COUNTY_MAP if county_tables.get(c, pd.DataFrame()).empty]
            logging.warning("Missing or empty tables for: %s", ", ".join(missing))

    except Exception as e:
        logging.exception("Failed to process %s: %s", pdf_path, str(e))


def main(year=None, quarter=None, end_year=None, end_quarter=None, all_available=False):
    """
    Process county PDFs with flexible selection options.
    
    Args:
        year: Starting year (optional)
        quarter: Starting quarter (optional, defaults to 1)
        end_year: Ending year (for range processing)
        end_quarter: Ending quarter (for range processing)
        all_available: If True, processes all available PDFs (overrides other args)
    """
    base_dir = Path(".")
    logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')

    # Validate arguments
    if all_available and (year or end_year): # Changed from 'all' to check if any specific range arg is present
        logging.warning("Both range parameters (year/end_year) and all_available=True specified. Using all_available mode.")
    
    
    if all_available:
        # Case 3: Process all available PDFs from 2019 onwards
        # For all_available, we need a broad range to find all possible files
        # We set a very late end_year/quarter to ensure we catch everything up to current.
        pdf_paths = find_pdfs_in_range(base_dir, 2019, 1, datetime.datetime.now().year + 5, 4) # Added a future end year
        action_desc = "all available PDFs from 2019 onwards"

    elif year and end_year:
        # Case 2: Process a range
        quarter = quarter or 1
        end_quarter = end_quarter or 4
        pdf_paths = find_pdfs_in_range(base_dir, year, quarter, end_year, end_quarter)
        action_desc = f"PDFs from {year} Q{quarter} to {end_year} Q{end_quarter}"
    elif year:
        # Case 1: Process single year/quarter
        quarter = quarter or 1
        pdf_paths = find_pdfs_in_range(base_dir, year, quarter, year, quarter)
        action_desc = f"PDFs for {year} Q{quarter}"
    else:
        # Default case: Process all from 2019
        pdf_paths = find_pdfs_in_range(base_dir, 2019, 1, datetime.datetime.now().year + 5, 4) # Added a future end year
        action_desc = "all available PDFs from 2019 onwards (default)"

    if not pdf_paths:
        logging.warning(f"No county PDFs found for {action_desc}.")
        return

    logging.info("Found %d county PDF files for %s", len(pdf_paths), action_desc)
    process_pdfs(pdf_paths)

def find_pdfs_in_range(base_dir, start_year, start_quarter, end_year=None, end_quarter=None):
    """
    Find PDFs within a specified range, parsing year and quarter from PDF filenames.
    Assumes PDF filenames are in the format: YYYY_YY_QQ_county.pdf
    e.g., 2019_20_01_county.pdf
    """
    end_year = end_year if end_year is not None else datetime.datetime.now().year
    end_quarter = end_quarter if end_quarter is not None else 4 # Default to Q4 of end_year

    
    all_found_pdfs = []
    # Glob for all potential PDF files first, then filter
    for pdf_path in base_dir.glob("**/county/*.pdf"):
        # Extract filename (e.g., "2019_20_01_county.pdf")
        filename = pdf_path.name
        
        # Regex to match the expected filename format
        match = re.match(r"(\d{4})_\d{2}_(\d{2})_county\.pdf", filename)
        
        if match:
            pdf_file_year = int(match.group(1))
            pdf_file_quarter = int(match.group(2))

            # Check if this PDF's year and quarter fall within the requested range
            is_after_start = (pdf_file_year > start_year) or \
                             (pdf_file_year == start_year and pdf_file_quarter >= start_quarter)
            
            is_before_end = (pdf_file_year < end_year) or \
                            (pdf_file_year == end_year and pdf_file_quarter <= end_quarter)
            
            if is_after_start and is_before_end:
                all_found_pdfs.append(pdf_path)
    
    # Sort for consistent processing order (optional but good practice)
    all_found_pdfs.sort()
    return all_found_pdfs


def process_pdfs(pdf_paths):
    """Process a list of PDF paths"""
    for pdf_path in pdf_paths:
        try:
            # Assuming process_pdf is defined elsewhere and handles the actual work
            print(f"Processing {pdf_path}") # Placeholder for actual processing
            process_pdf(pdf_path) 
        except Exception as e:
            logging.error("Failed to process %s: %s", pdf_path, e)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Process government spending PDFs by financial year")
    
    # Main arguments
    parser.add_argument("--year", "-y", type=int, help="Starting year (e.g., 2022)")
    parser.add_argument("--quarter", "-q", type=int, choices=[1,2,3,4], help="Starting quarter (1-4)")
    
    # Range arguments
    parser.add_argument("--end-year", "-ey", type=int, help="End year for range processing")
    parser.add_argument("--end-quarter", "-eq", type=int, choices=[1,2,3,4], help="End quarter for range processing")
    
    # All-available flag
    parser.add_argument("--all", "-a", action="store_true", dest="all_available", 
                       help="Process all available PDFs (overrides other arguments)")
    
    args = parser.parse_args()
    
    main(
        year=args.year,
        quarter=args.quarter,
        end_year=args.end_year,
        end_quarter=args.end_quarter,
        all_available=args.all_available
    )


### END OF SCRIPT ### 

# TO RUN:
# python program.py --year 2022 --quarter 4
# python program.py -y 2022 -q 4 (shorthand)
# python program.py --year 2020 --quarter 2 --end-year 2021 --end-quarter 3 (exact range)
# python program.py --all (all available pdfs)
# python program.py (default; same as all available)
# python program.py --help (help)