import pdfplumber
import re
import pandas as pd

def extract_list_of_tables(pdf_path, pages_to_search=range(3, 50)):
    """
    Searches the specified pages for lines that look like table entries.
    Each entry is expected to have the form:
    
      Table 2.1: Own Source Revenue Collection in the First Quarter of FY 2024/25 ........... 2

    The function adjusts the listed page number by adding 24.
    """
    list_of_tables = {}
    with pdfplumber.open(pdf_path) as pdf:
        for page_number in pages_to_search:
            page = pdf.pages[page_number]
            text = page.extract_text()
            if text:
                lines = text.split("\n")
                # Debug: print all lines from this page
                print(f"\n--- Page index {page_number+1} ---")
                for idx, line in enumerate(lines):
                    print(f"Line {idx}: {line}")
                # Updated regex:
                #   - Starts with "Table" (case-insensitive), then a number, colon, some text,
                #     then either a series of dots or whitespace, then the page number at the end.
                pattern = re.compile(r"^(Table\s+\d+\.\d+):\s*(.+?)(?:\.{2,}|\s+)(\d+)\s*$", re.IGNORECASE)
                for line in lines:
                    match = pattern.search(line)
                    if match:
                        table_id = match.group(1)
                        table_title = match.group(2)
                        listed_page = int(match.group(3))
                        actual_page = listed_page + 24  # Adjusting for offset
                        list_of_tables[table_id] = {"title": table_title, "page": actual_page}
                        print(f"Extracted {table_id}: listed page {listed_page} -> actual page {actual_page}")
    return list_of_tables

def extract_table_from_page(pdf, page_index):
    """
    Extracts the first table found on a given page.
    Returns a DataFrame if a table is found; otherwise returns None.
    """
    page = pdf.pages[page_index]
    tables = page.extract_tables()
    if tables:
        table_data = tables[0]
        if len(table_data) > 1:
            df = pd.DataFrame(table_data[1:], columns=table_data[0])
            return df
    return None

def extract_all_tables(pdf_path, list_tables_info):
    """
    Iterates over the provided table info and extracts each table from its actual PDF page.
    """
    extracted_tables = {}
    with pdfplumber.open(pdf_path) as pdf:
        for table_id, info in list_tables_info.items():
            page_index = info["page"] - 1  # Convert actual page number to 0-based index
            print(f"\nExtracting {table_id} from actual PDF page {info['page']} (index {page_index})")
            df = extract_table_from_page(pdf, page_index)
            extracted_tables[table_id] = df
    return extracted_tables

if __name__ == "__main__":
    pdf_file = "county_jan25.pdf"
    
    # Try searching pages 3 to 8 (adjust this range if needed)
    tables_info = extract_list_of_tables(pdf_file, pages_to_search=range(3, 8))
    
    print("\nList of Tables Extracted:")
    print(tables_info)
    
    extracted_tables = extract_all_tables(pdf_file, tables_info)
    
    for table_id, df in extracted_tables.items():
        if df is not None:
            csv_filename = f"{table_id.replace(' ', '_')}.csv"
            df.to_csv(csv_filename, index=False)
            print(f"Saved {table_id} to {csv_filename}")
        else:
            page_used = tables_info.get(table_id, {}).get('page', 'Unknown')
            print(f"No table found on page {page_used} for {table_id}.")
    
    # OPTIONAL: Manually inspect a known page (for example, actual PDF page 27) to confirm table extraction.
    with pdfplumber.open(pdf_file) as pdf:
        page_index = 26  # For example, actual page 27 (0-indexed 26)
        print(f"\nManually extracting table from PDF page {page_index+1}:")
        table = pdf.pages[page_index].extract_tables()
        if table:
            for row in table[0]:
                print(row)
        else:
            print("No table found on this page.")
