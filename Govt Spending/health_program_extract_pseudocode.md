```markdown
# Pseudo-code for Health Program Data Extraction

This pseudo-code outlines the process for extracting and structuring health program spending data from various CSV files. It is based on the brainstorming notes in `health_program_extract.py`.

## 1. Define Output Schema

```
STRUCTURE HealthProgramRecord
  program: STRING
  sub_program: STRING
  budget: FLOAT
  expenditure: FLOAT
  county: STRING
  year: INTEGER
  quarter: INTEGER
END STRUCTURE

LIST_OF_RECORDS = LIST OF HealthProgramRecord
```

## 2. Main Process

```
FUNCTION main(list_of_files)
  // Initialize a list to hold all the extracted records
  all_records = NEW LIST_OF_RECORDS

  // Phase 1: Simple Prototype
  // Process each file using a combination of rule-based and keyword matching
  FOR each file in list_of_files
    // The dispatcher will decide which parser to use
    extracted_data = process_file(file, "prototype")
    ADD extracted_data TO all_records
  END FOR

  // Phase 2: Refinement (Optional, can be a separate process)
  // Refine the extracted data, possibly using more advanced techniques
  refined_records = refine_data(all_records)

  // Phase 3: Production
  // Save the final, standardized data
  save_to_csv(refined_records, "health_program_spending.csv")

  // Generate validation and audit reports
  generate_audit_report(refined_records)

  RETURN refined_records
END FUNCTION
```

## 3. File Processing and Dispatcher

```
FUNCTION process_file(file_path, processing_mode)
  // Read the raw data from the CSV file
  raw_data = read_csv(file_path)

  // 1. Pre-analysis and format identification
  file_format = identify_format(raw_data)

  // 2. Dispatch to the appropriate parser based on the identified format
  parsed_data = NULL
  SWITCH file_format
    CASE "multi_level_header":
      parsed_data = parse_multi_level_header(raw_data)
    CASE "hierarchical_simple":
      parsed_data = parse_hierarchical_simple(raw_data)
    CASE "standard_table":
      parsed_data = parse_standard_table(raw_data)
    DEFAULT:
      // Fallback for unknown formats
      parsed_data = parse_with_general_rules(raw_data)
  END SWITCH

  // 3. Data Cleaning and Standardization
  cleaned_data = clean_and_standardize(parsed_data)

  // 4. Semantic Analysis (especially for edge cases or in refinement mode)
  IF processing_mode == "refinement" OR file_format == "unknown"
    semantic_data = analyze_with_embeddings(cleaned_data)
    RETURN semantic_data
  END IF

  RETURN cleaned_data
END FUNCTION
```

## 4. Format Identification and Parsing

```
FUNCTION identify_format(raw_data)
  // Logic to detect file characteristics
  IF has_multi_level_headers(raw_data)
    RETURN "multi_level_header"
  ELSE IF is_hierarchical(raw_data)
    RETURN "hierarchical_simple"
  ELSE
    RETURN "standard_table"
  END IF
END FUNCTION

// Specific parser functions for each format
FUNCTION parse_multi_level_header(raw_data)
  // Flatten headers, handle merged cells
  // ... implementation ...
END FUNCTION

FUNCTION parse_hierarchical_simple(raw_data)
  // Track parent programs, fill down missing values
  // ... implementation ...
END FUNCTION

FUNCTION parse_standard_table(raw_data)
  // Standard row-column extraction
  // ... implementation ...
END FUNCTION

FUNCTION parse_with_general_rules(raw_data)
  // A more robust, general-purpose parser
  // ... implementation ...
END FUNCTION
```

## 5. Data Cleaning and Standardization

```
FUNCTION clean_and_standardize(data)
  // Handle various data quality issues
  FOR each row in data
    // Remove noise rows (titles, empty rows, etc.)
    IF is_noise_row(row)
      REMOVE row
      CONTINUE
    END IF

    // Handle subtotal rows (keep if they contain useful info)
    IF is_subtotal_row(row)
      // Special handling for subtotals
    END IF

    // Standardize numeric values (remove commas, convert millions)
    row.budget = standardize_number(row.budget)
    row.expenditure = standardize_number(row.expenditure)

    // Handle missing data
    IF is_data_missing(row)
      // Log a warning or apply imputation
      log_warning("Missing data in row: " + row)
    END IF
  END FOR

  RETURN data
END FUNCTION
```

## 6. Semantic Analysis with Embeddings

```
FUNCTION analyze_with_embeddings(data)
  // Load pre-trained embedding model (e.g., all-MiniLM-L6-v2)
  embedding_model = load_embedding_model()

  // Create reference embeddings for health-related terms
  health_embeddings = create_reference_embeddings(embedding_model, ["health", "medical", "hospital", ...])

  // Initialize a vector database
  vector_db = initialize_vector_db()

  // Generate embeddings for program and sub-program names
  FOR each row in data
    program_embedding = embedding_model.embed(row.program)
    sub_program_embedding = embedding_model.embed(row.sub_program)

    // Store embeddings in the vector database
    vector_db.add(program_embedding, row.id)
    vector_db.add(sub_program_embedding, row.id)
  END FOR

  // Perform semantic search to find relevant rows
  relevant_rows = []
  FOR each row in data
    program_embedding = vector_db.get(row.id)
    similarity = calculate_cosine_similarity(program_embedding, health_embeddings)

    // Apply a confidence threshold
    IF similarity > 0.75
      ADD row TO relevant_rows
    END IF
  END FOR

  RETURN relevant_rows
END FUNCTION
```

## 7. Verification and Saving

```
FUNCTION save_to_csv(data, filename)
  // Convert the list of records to a CSV file
  // ... implementation ...
END FUNCTION

FUNCTION generate_audit_report(data)
  // Create a report summarizing the extraction process
  // - Number of files processed
  // - Number of records extracted
  // - List of files with warnings (e.g., missing data)
  // ... implementation ...
END FUNCTION
```
```
