# This script is to scrape the county financial data
# Previously, I converted the pdf to word, then scraped the word files after cleaning them within the word document.
# Now, I aim to make my workflow fully reproducible so that one can trace what I did.
# I eventually aim to redo all the scraping I previously did.
# I also want to include the quarterly figures rather than annual
# Finally, I want to collect data by program and not just department
# Additionally, I want to collect national data: national.R

# Last updated: September 23rd 2024.
# Written by Angela

# Load libraries ----
library(pdftools)
library(tabulapdf)

# September 2024 (Annual Expenditure) ----
sep_2024 <- extract_tables("Govt Spending/county_sep24.pdf")
