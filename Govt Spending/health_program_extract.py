# Identify what I want ie output schema
# Columns: program, sub-program, budget, expenditure, county, year, quarter

# First try rule based extraction for columns
# Also rule based extraction for rows

# Some issues:
    # Multi-level headers
    # Noise rows ie
        # Department titles
        # Subsection titles
        # Repeated headers
        # Subtotal rows: can't be filtered out since could have useful info
            # Will need a second layer of analysis
    # Hierarchical data 
        # Department
        # Program only appears once
    # Some data is missing: so I will probably need to return to my program.py
        # I need it to warn me that ... is missing
        # No health at all
        # No data at all - useful for subtotals only
    # Some in millions others need to be multiplied
    # Some numbers have commas

# This is a dispatcher approach
# So I need to identify some things before I start extraction:
# Maybe identify if there are multi-level headers
# Maybe identify if there are hierarchical data
# So I need several if statements then I apply different rules.
# Maybe I begin alphabetically ie start with Baringo y1, q1
# Then build for that 
# Then try other quarters
# Then other years
# Then do the same for the next county

# So perhaps begin with a parser function per format

# Then I can now do the embeddings

# Create reference health embeddings
# Consider glove.6B.100d.txt to do this


# Use Vector DB
#  Embedding models: 
    # OpenAI: text-embedding-3-small
    # Hugging Gace: all-MiniLM-L6-v2
    # Store in a vector database: Chroma, FAISS, Pinecone
        # Chroma: lightweight, good for prototyping
        # FAISS: by Meta: very fast for large datasets
        # Pinecone: cloud-based; handles scaling automatically
# Sematic search:
    # sentence-transformers library
        # pre-trained
        # LangChain or LlamaIndex

# Workflow:
    # Extract everything
    # Generate embeddings for everything
    # Create reference health embedding
    # Compare to predefined embeddings using cosine similarity
    # Apply confidence thresholds
    # Map the closest matches to my output schema


# Verify 
# Manual validation

# Save


# Phases:
## Simple prototype:
    ## Rule-based + keyword matching
    ## Vector similarity for edge cases
    ## Manual validation on sample
## Refinement:
    ## Fine-tune similarity thresholds
    ## Add county-specific terminology
    ## Handle multi-level program hierarchies
## Production:
    ## Batch process all csvs
    ## Generate standardised output
    ## Create validation/ audit reports