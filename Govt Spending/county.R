# This script is to scrape the county financial data
# Previously, I converted the pdf to word, then scraped the word files after cleaning them within the word document.
# Now, I aim to make my workflow fully reproducible so that one can trace what I did.
# I eventually aim to redo all the scraping I previously did.
# I also want to include the quarterly figures rather than annual
# Finally, I want to collect data by program and not just department
# Additionally, I want to collect national data: national.R
# I have used Python in scraping.py to first scrape the data. It is more effective.

# Last updated: Dec 6 2024.
# Written by Angela

# Load libraries ----
library(readr)
library(tidyr)
library(dplyr)
library(stringr)
library(purrr)

# Functions ----


# Function to remove NA values from a vector
remove_na <- function(x) {
  str_trim(str_remove_all(x, "\\bNA\\b"))
} # consider how to have this when I don't have to merge rows. 

# Function to merge rows
merge_rows <- function(data, rows, cleanup = TRUE) {
  data |>
    slice(rows) |> # choose the rows
    summarise(across(
      everything(),
      ~ {
        merged <- paste(.x, collapse = " ") # paste the rows together
        if (cleanup) {
          merged <- remove_na(merged)
        }
        merged
      }
    ))
}

## Function to process county data ie separate columns
process_county_data <- function(df) {
  # Read the CSV file
  cdf<- df |>
    # Separate column 2 into budget_rec and budget_dev
    separate(
      col = 2, 
      into = c("budget_rec", "budget_dev"), 
      sep = "\\s+", 
      # Suppress warnings about fill=
      fill = "right"
    ) |>
    # Separate column 4 into exch_rec and exch_dev
    separate(
      col = 4, 
      into = c("exch_rec", "exch_dev"), 
      sep = "\\s+", 
      fill = "right"
    ) |>
    # Separate column 6 into exp_rec and exp_dev
    separate(
      col = 6, 
      into = c("exp_rec", "exp_dev"), 
      sep = "\\s+", 
      fill = "right"
    ) |>
    # Separate column 8 into exp_exch_rec and exp_exch_dev
    separate(
      col = 8, 
      into = c("exp_exch_rec", "exp_exch_dev"), 
      sep = "\\s+", 
      fill = "right"
    ) |>
    # Separate column 10 into abs_rec and abs_dev
    separate(
      col = 10, 
      into = c("abs_rec", "abs_dev"), 
      sep = "\\s+", 
      fill = "right"
    )
  
  return(cdf)
}

# clean text
## Function to normalize spaces and basic text cleanup
normalize_text <- function(txt) {
    txt <- gsub("([a-zA-Z])-\\s+([a-zA-Z])", "\\1\\2", txt)  # Fix split hyphenated words
    txt <- gsub("\r", " ", txt)  # Remove "\r"
    txt <- gsub("\\s*,\\s*", ", ", txt)  # Normalize commas
    txt <- gsub("\\s*;\\s*", "; ", txt)  # Normalize semicolons
    txt <- gsub("\\s*&\\s*", " & ", txt)  # Normalize &
    txt <- gsub("\\s+", " ", txt)  # Clean up extra spaces
    trimws(txt)
}

## Function to check if a word should be kept as-is
is_special_word <- function(word) {
    nchar(word) == 1 || grepl("^[0-9]+$", word)
}

## Function to extract punctuation and word
extract_word_parts <- function(word) {
    if (is_special_word(word)) {
        return(list(
            leading_punct = NA,
            word = word,
            trailing_punct = NA
        ))
    }
    
    list(
        leading_punct = str_extract(word, "^[^a-zA-Z\\-]+(?=[a-zA-Z])"),
        word = str_remove_all(word, "^[^a-zA-Z\\-]+(?=[a-zA-Z])|(?<=[a-zA-Z])[^a-zA-Z\\-]+$"),
        trailing_punct = str_extract(word, "(?<=[a-zA-Z])[^a-zA-Z\\-]+$")
    )
}

## Function to apply capitalization rules
apply_capitalization <- function(word, position, total_words, acronyms, small_words) {
    # If word is empty, return original
    if (word == "") return(word)
    
    # Handle acronyms
    if (toupper(word) %in% acronyms) {
        return(toupper(word))
    }
    
    # Handle other cases
    is_special_position <- position == 1 || position == total_words
    is_small_word <- tolower(word) %in% small_words
    
    if (is_special_position || !is_small_word) {
        return(str_to_title(word))
    }
    
    tolower(word)
}

## Function to reassemble word with punctuation
reassemble_word <- function(parts) {
    result <- parts$word
    if (!is.na(parts$leading_punct)) {
        result <- paste0(parts$leading_punct, result)
    }
    if (!is.na(parts$trailing_punct)) {
        result <- paste0(result, parts$trailing_punct)
    }
    result
}

## Main function that uses all the helper functions
clean_text <- function(text) {
    # Configuration
    small_words <- c("and", "or", "but", "a", "an", "the", "for", "nor",
                    "on", "at", "to", "by", "from", "of", "in", "with", "&")
    acronyms <- c("ICT", "PSM", "ECRA")
    
    # Ensure input is character
    text <- as.character(text)
    
    # Process each text element
    sapply(text, function(txt) {
        # Normalize the text
        txt <- normalize_text(txt)
        
        # Split into words
        words <- unlist(str_split(txt, " "))
        total_words <- length(words)
        
        # Process each word
        processed_words <- sapply(seq_along(words), function(i) {
            # Extract parts
            parts <- extract_word_parts(words[i])
            
            # Apply capitalization
            parts$word <- apply_capitalization(
                parts$word, i, total_words, acronyms, small_words
            )
            
            # Reassemble word
            reassemble_word(parts)
        })
        
        # Recombine into final string
        paste(processed_words, collapse = " ")
    }, USE.NAMES = FALSE)
}

## Function to rename and clean
rename_and_clean <- function(data, department = 0) {
  clean_df <- data %>%
    rename(department = !!sym(as.character(department))) %>%
    mutate(across(
      -department,
      ~ as.numeric(str_remove_all(., "[,\\-]"))
    ))  |> 
    mutate(
      department = clean_text(department)
    )

    return(clean_df)
  
}

# Objects 
county_vector <- c(
  "baringo", "bomet", "bungoma", "busia", "elgeyo_marakwet",
  "embu", "garissa", "homa_bay", "isiolo", "kajiado", 
  "kakamega", "kericho", "kiambu", "kilifi", "kirinyaga",
  "kisii", "kisumu", "kitui", "kwale", "laikipia", "lamu", "machakos",
  "makueni", "mandera", "marsabit", "meru", "migori", "mombasa",
  "muranga", "nairobi", "nakuru", "nandi", "narok", "nyamira", "nyandarua", "nyeri",
  "samburu", "siaya", "taita_taveta", "tana_river", "tharaka_nithi",
  "trans_nzoia", "turkana", "uasin_gishu", "vihiga", "wajir",
  "west_pokot") 

names_department <- c(
  "0", "budget_rec", "budget_dev", "exch_rec", "exch_dev", 
  "exp_rec", "exp_dev", "exp_exch_rec", "exp_exch_dev", "abs_rec", "abs_dev")

# TO DO: Make NAs Total 

# September 2024 (Annual Expenditure) ----

## Total ----
county_sep24_table_21a <- read_csv("Govt Spending/sep2024/county_sep24_table_16.csv")
county_sep24_table_21b <- read_csv("Govt Spending/sep2024/county_sep24_table_17.csv")

county_sep24_table_22a <- read_csv("Govt Spending/sep2024/county_sep24_table_19.csv")
county_sep24_table_22b <- read_csv("Govt Spending/sep2024/county_sep24_table_20.csv")

county_sep24_table_23a <- read_csv("Govt Spending/sep2024/county_sep24_table_22.csv")
county_sep24_table_23b <- read_csv("Govt Spending/sep2024/county_sep24_table_23.csv")

county_sep24_table_24a <- read_csv("Govt Spending/sep2024/county_sep24_table_24.csv")
county_sep24_table_24b <- read_csv("Govt Spending/sep2024/county_sep24_table_25.csv")
county_sep24_table_24c <- read_csv("Govt Spending/sep2024/county_sep24_table_27.csv")

county_sep24_table_25a <- read_csv("Govt Spending/sep2024/county_sep24_table_28.csv")
county_sep24_table_25b <- read_csv("Govt Spending/sep2024/county_sep24_table_29.csv")

## Baringo ----

county_sep24_table_31a <- read_csv("Govt Spending/sep2024/county_sep24_table_31.csv")
county_sep24_table_31b <- read_csv("Govt Spending/sep2024/county_sep24_table_33.csv")

county_sep24_table_32 <- read_csv("Govt Spending/sep2024/county_sep24_table_34.csv")

county_sep24_table_33 <- read_csv("Govt Spending/sep2024/county_sep24_table_37.csv")

county_sep24_table_34a <- read_csv("Govt Spending/sep2024/county_sep24_table_38.csv")
county_sep24_table_34b <- read_csv("Govt Spending/sep2024/county_sep24_table_39.csv")

county_sep24_table_35 <- read_csv("Govt Spending/sep2024/county_sep24_table_40.csv")

### Department
county_sep24_table_36 <- process_county_data(read_csv("Govt Spending/sep2024/county_sep24_table_42.csv"))
    
baringo_department_sep24 <- bind_rows(
    # merge_rows(county_sep24_table_36, 6:7), # Merge rows 1 - 3: Headers
    county_sep24_table_36 |> slice(4:5),  # Keep rows 4-5: Assembly and Governor
    merge_rows(county_sep24_table_36, 6:7),  # Merge rows 6-7: Finance
    merge_rows(county_sep24_table_36, 8:9),  # Merge rows 8-9: Roads
    merge_rows(county_sep24_table_36, 10:11),  # Merge rows 10-11: Trade
    merge_rows(county_sep24_table_36, 12:13),  # Merge rows 12-13: Education
    county_sep24_table_36 |> slice(14),  # Row 14: Health
    merge_rows(county_sep24_table_36, 15:16),  # Merge rows 15-16: Land
    merge_rows(county_sep24_table_36, 17:18),  # Merge rows 17-18: Agriculture
    merge_rows(county_sep24_table_36, 19:21),  # Merge rows 19-21: Youth
    county_sep24_table_36 |> slice(22),  # Row 22: Water
    merge_rows(county_sep24_table_36, 23:25),  # Merge rows 23-25: Environment
    county_sep24_table_36 |> slice(26)  # Row 26: Total
)  |> 
  rename_and_clean() 
# There are issues here beyond me ie why doesn't agriculture have a budget but has an absorption rate?
# Also why does the Governor's office have such huge expenditure?

rm(county_sep24_table_36)


### Program
county_sep24_table_37a <- read_csv("Govt Spending/sep2024/county_sep24_table_43.csv")
county_sep24_table_37b <- read_csv("Govt Spending/sep2024/county_sep24_table_44.csv")
county_sep24_table_37c <- read_csv("Govt Spending/sep2024/county_sep24_table_46.csv")
county_sep24_table_37d <- read_csv("Govt Spending/sep2024/county_sep24_table_47.csv")
county_sep24_table_37e <- read_csv("Govt Spending/sep2024/county_sep24_table_49.csv")
county_sep24_table_37f <- read_csv("Govt Spending/sep2024/county_sep24_table_51.csv")

## Bomet -----
county_sep24_table_38 <- read_csv("Govt Spending/sep2024/county_sep24_table_54.csv")

county_sep24_table_39 <- read_csv("Govt Spending/sep2024/county_sep24_table_55.csv")

county_sep24_table_310 <- read_csv("Govt Spending/sep2024/county_sep24_table_56.csv")

county_sep24_table_311a <- read_csv("Govt Spending/sep2024/county_sep24_table_57.csv")
county_sep24_table_311b <- read_csv("Govt Spending/sep2024/county_sep24_table_58.csv")

county_sep24_table_312 <- read_csv("Govt Spending/sep2024/county_sep24_table_59.csv")

### Department 
county_sep24_table_313 <- read_csv("Govt Spending/sep2024/county_sep24_table_60.csv")  |> 
    separate(2, c("budget_rec", "budget_dev"), sep = "\\s+")  |>  # brings warnings
    separate(4, c("exch_rec", "exch_dev"), sep = "\\s+")  |> 
    separate(6, c("exp_rec", "exp_dev"), sep = "\\s+")  |> 
    separate(8, c("exp_exch_rec", "exp_exch_dev"), sep = "\\s+")  |> 
    select(-10)  |> 
    separate(10, c("abs_rec", "abs_dev"), sep = "\\s+")

bomet_department_sep24 <- bind_rows(
    #merge_rows(county_sep24_table_313, 1:3), # Header
    merge_rows(county_sep24_table_313, 4:5), # Administration
    merge_rows(county_sep24_table_313, 6:7), # Finance
    merge_rows(county_sep24_table_313, 8:9), # Land
    merge_rows(county_sep24_table_313, 10:11), # Gender
    merge_rows(county_sep24_table_313, 12:13), # Medical
    merge_rows(county_sep24_table_313, 14:15), # Agriculture
    merge_rows(county_sep24_table_313, 16:17), # Water
    merge_rows(county_sep24_table_313, 18:19), # Education
    merge_rows(county_sep24_table_313, 20:21), # Roads
    merge_rows(county_sep24_table_313, 22:23), # Trade
    county_sep24_table_313  |> slice(24), # Assembly
    county_sep24_table_313  |> slice(25) # Total
) |> 
  rename_and_clean()

rm(county_sep24_table_313)

# Program
county_sep24_table_314a <- read_csv("Govt Spending/sep2024/county_sep24_table_61.csv")
county_sep24_table_314b <- read_csv("Govt Spending/sep2024/county_sep24_table_62.csv")
county_sep24_table_314c <- read_csv("Govt Spending/sep2024/county_sep24_table_64.csv")
county_sep24_table_314d <- read_csv("Govt Spending/sep2024/county_sep24_table_66.csv")

## Bungoma ----
county_sep24_table_315 <- read_csv("Govt Spending/sep2024/county_sep24_table_68.csv")

county_sep24_table_316 <- read_csv("Govt Spending/sep2024/county_sep24_table_79.csv")

county_sep24_table_317a <- read_csv("Govt Spending/sep2024/county_sep24_table_80.csv")
county_sep24_table_317b <- read_csv("Govt Spending/sep2024/county_sep24_table_81.csv")

county_sep24_table_318 <- read_csv("Govt Spending/sep2024/county_sep24_table_85.csv")

county_sep24_table_319 <- read_csv("Govt Spending/sep2024/county_sep24_table_86.csv")

### Department
county_sep24_table_320a <- read_csv("Govt Spending/sep2024/county_sep24_table_87.csv")  
county_sep24_table_320b <- read_csv("Govt Spending/sep2024/county_sep24_table_88.csv")

county_sep24_table_320 <- bind_rows(county_sep24_table_320a, county_sep24_table_320b)
rm(county_sep24_table_320a)
rm(county_sep24_table_320b)

bungoma_department_sep24 <- bind_rows( 
  #merge_rows(county_sep24_table_320, 1:2), # Header 
  county_sep24_table_320  |> slice(3:6),
  #merge_rows(county_sep24_table_320, 8:9), # Header
  county_sep24_table_320  |> slice(10:26),
)  |> 
rename_and_clean()

names(bungoma_department_sep24) <- c("department", "budget_rec", "budget_dev", "exch_rec", "exch_dev", "exp_rec", "exp_dev", "exp_exch_rec", "exp_exch_dev", "abs_rec", "abs_dev")
rm(county_sep24_table_320)

### Program
county_sep24_table_321a <- read_csv("Govt Spending/sep2024/county_sep24_table_89.csv")
county_sep24_table_321b <- read_csv("Govt Spending/sep2024/county_sep24_table_91.csv")
county_sep24_table_321c <- read_csv("Govt Spending/sep2024/county_sep24_table_93.csv")
county_sep24_table_321d <- read_csv("Govt Spending/sep2024/county_sep24_table_95.csv")
county_sep24_table_321e <- read_csv("Govt Spending/sep2024/county_sep24_table_97.csv")
county_sep24_table_321f <- read_csv("Govt Spending/sep2024/county_sep24_table_99.csv")
county_sep24_table_321g <- read_csv("Govt Spending/sep2024/county_sep24_table_101.csv")
county_sep24_table_321h <- read_csv("Govt Spending/sep2024/county_sep24_table_103.csv")
county_sep24_table_321i <- read_csv("Govt Spending/sep2024/county_sep24_table_105.csv")
county_sep24_table_321j <- read_csv("Govt Spending/sep2024/county_sep24_table_107.csv")
county_sep24_table_321k <- read_csv("Govt Spending/sep2024/county_sep24_table_109.csv")

## Busia -----

county_sep24_table_322a <- read_csv("Govt Spending/sep2024/county_sep24_table_111.csv")
county_sep24_table_322b <- read_csv("Govt Spending/sep2024/county_sep24_table_112.csv")

county_sep24_table_323 <- read_csv("Govt Spending/sep2024/county_sep24_table_113.csv")

county_sep24_table_324 <- read_csv("Govt Spending/sep2024/county_sep24_table_114.csv")

county_sep24_table_325 <- read_csv("Govt Spending/sep2024/county_sep24_table_118.csv")

county_sep24_table_326 <- read_csv("Govt Spending/sep2024/county_sep24_table_119.csv")

### Department
county_sep24_table_327 <- read_csv("Govt Spending/sep2024/county_sep24_table_120.csv")  |>
  mutate(
    `1` = ifelse(!is.na(`0`) & `0` == "Health and sanitation", "2,372.29 407.51", `1`), # correct health which has a space between the number
     `4` = ifelse(!is.na(`4`) & `4` == "72.1 202..0", "72.1 202.0", `4`) # correct 20..0
  )  |>  
  process_county_data()  |> 
  mutate(`0` = ifelse(is.na(`0`), "Total", `0`))

busia_department_sep24 <- bind_rows(
  #merge_rows(county_sep24_table_327, 1:3), # Header
  county_sep24_table_327  |> slice(4), # Agriculture
  merge_rows(county_sep24_table_327, 5:6), # Trade
  county_sep24_table_327  |> slice(7:11), # Education - PSM
  merge_rows(county_sep24_table_327, 12:13), # Land 
  county_sep24_table_327  |> slice(14:21), # Water - Total
)  |> 
  rename_and_clean()

rm(county_sep24_table_327)

### Program
county_sep24_table_328a <- read_csv("Govt Spending/sep2024/county_sep24_table_121.csv")
county_sep24_table_328b <- read_csv("Govt Spending/sep2024/county_sep24_table_122.csv")
county_sep24_table_328c <- read_csv("Govt Spending/sep2024/county_sep24_table_124.csv")
county_sep24_table_328d <- read_csv("Govt Spending/sep2024/county_sep24_table_126.csv")
county_sep24_table_328e <- read_csv("Govt Spending/sep2024/county_sep24_table_128.csv")
county_sep24_table_328f <- read_csv("Govt Spending/sep2024/county_sep24_table_130.csv")
county_sep24_table_328g <- read_csv("Govt Spending/sep2024/county_sep24_table_132.csv")

## Elgeyo Marakwet ----

county_sep24_table_329a <- read_csv("Govt Spending/sep2024/county_sep24_table_133.csv")
county_sep24_table_329b <- read_csv("Govt Spending/sep2024/county_sep24_table_135.csv")

county_sep24_table_330a <- read_csv("Govt Spending/sep2024/county_sep24_table_136.csv")
county_sep24_table_330b <- read_csv("Govt Spending/sep2024/county_sep24_table_137.csv")

county_sep24_table_331a <- read_csv("Govt Spending/sep2024/county_sep24_table_139.csv")
county_sep24_table_331b <- read_csv("Govt Spending/sep2024/county_sep24_table_141.csv")

county_sep24_table_332 <- read_csv("Govt Spending/sep2024/county_sep24_table_142.csv")

county_sep24_table_333 <- read_csv("Govt Spending/sep2024/county_sep24_table_143.csv")

### Department
county_sep24_table_334 <- read_csv("Govt Spending/sep2024/county_sep24_table_144.csv")  |> 
  mutate(
    `6` = ifelse(!is.na(`0`) & `0` == "Education and", "NA", `6`), # correct education
    `6` = ifelse(!is.na(`0`) & `0` == "Technical Training", "100.0 70.5", `6`) # correct training
  )  |>
    process_county_data()  |> 
    mutate(
      `0` = ifelse(is.na(`0`), "Total", `0`)
    )

elgeyo_marakwet_department_sep24 <- bind_rows(
  #merge_rows(county_sep24_table_334, 1:3), # Header
  county_sep24_table_334  |> slice(4), # Assembly
  merge_rows(county_sep24_table_334, 5:6), # Governor
  merge_rows(county_sep24_table_334, 7:8), # Finance
  merge_rows(county_sep24_table_334, 9:11), # Agriculture
  merge_rows(county_sep24_table_334, 12:13), # Water
  merge_rows(county_sep24_table_334, 14:15), # Education
  merge_rows(county_sep24_table_334, 16:17), # Health
  merge_rows(county_sep24_table_334, 18:21), # Lands
  merge_rows(county_sep24_table_334, 22:23), # Roads
  merge_rows(county_sep24_table_334, 24:26), # Tourism
  merge_rows(county_sep24_table_334, 27:28), # Youth
  merge_rows(county_sep24_table_334, 29:30), # PSM
  merge_rows(county_sep24_table_334, 31:32), # PSB
  county_sep24_table_334  |> slice(33) # Total
) |> 
  rename_and_clean()

rm(county_sep24_table_334)

### Program
county_sep24_table_335a <- read_csv("Govt Spending/sep2024/county_sep24_table_145.csv")
county_sep24_table_335b <- read_csv("Govt Spending/sep2024/county_sep24_table_146.csv")
county_sep24_table_335c <- read_csv("Govt Spending/sep2024/county_sep24_table_148.csv")

## Embu ----

county_sep24_table_336 <- read_csv("Govt Spending/sep2024/county_sep24_table_151.csv")

county_sep24_table_337 <- read_csv("Govt Spending/sep2024/county_sep24_table_161.csv")

county_sep24_table_338 <- read_csv("Govt Spending/sep2024/county_sep24_table_162.csv")

county_sep24_table_339a <- read_csv("Govt Spending/sep2024/county_sep24_table_164.csv")
county_sep24_table_339b <- read_csv("Govt Spending/sep2024/county_sep24_table_165.csv")

county_sep24_table_340 <- read_csv("Govt Spending/sep2024/county_sep24_table_166.csv")

### Department
county_sep24_table_341 <- read_csv("Govt Spending/sep2024/county_sep24_table_167.csv") |>
    process_county_data()

embu_department_sep24 <- bind_rows(
  #merge_rows(county_sep24_table_341, 1:2), # Header
  county_sep24_table_341  |> slice(3), # Governor
  county_sep24_table_341  |> slice(4), # PSB
  merge_rows(county_sep24_table_341, 5:6), # Admin
  county_sep24_table_341  |> slice(7), # Assembly
  merge_rows(county_sep24_table_341, 8:9), # Finance
  merge_rows(county_sep24_table_341, 10:12), # Trade
  merge_rows(county_sep24_table_341, 13:15), # Agriculture
  merge_rows(county_sep24_table_341, 16:17), # Water
  county_sep24_table_341  |> slice(18:19), # Health & Hospital
  merge_rows(county_sep24_table_341, 20:21), # Infrastructure
  merge_rows(county_sep24_table_341, 22:23), # Education
  merge_rows(county_sep24_table_341, 24:26), # Youth
  merge_rows(county_sep24_table_341, 27:29), # Land
  merge_rows(county_sep24_table_341, 30:31), # ECRA
  county_sep24_table_341  |> slice(32:34) # Municipal, Climate Change, Total
) |> 
  rename_and_clean()  |> 
  # To make compatible, it is be divided
  mutate(
    across(
      c(budget_rec, budget_dev, exch_rec, exch_dev, exp_rec, exp_dev),
      ~ . / 1000000
    )
  )

rm(county_sep24_table_341)

### Program
county_sep24_table_342a <- read_csv("Govt Spending/sep2024/county_sep24_table_168.csv")
county_sep24_table_342b <- read_csv("Govt Spending/sep2024/county_sep24_table_169.csv")
county_sep24_table_342c <- read_csv("Govt Spending/sep2024/county_sep24_table_171.csv")
county_sep24_table_342d <- read_csv("Govt Spending/sep2024/county_sep24_table_173.csv")
county_sep24_table_342e <- read_csv("Govt Spending/sep2024/county_sep24_table_175.csv")
county_sep24_table_342d <- read_csv("Govt Spending/sep2024/county_sep24_table_177.csv")

## Garissa ----

county_sep24_table_343a <- read_csv("Govt Spending/sep2024/county_sep24_table_179.csv")
county_sep24_table_343b <- read_csv("Govt Spending/sep2024/county_sep24_table_180.csv")

county_sep24_table_344 <- read_csv("Govt Spending/sep2024/county_sep24_table_181.csv")

county_sep24_table_345 <- read_csv("Govt Spending/sep2024/county_sep24_table_182.csv")

county_sep24_table_346 <- read_csv("Govt Spending/sep2024/county_sep24_table_183.csv")

county_sep24_table_347a <- read_csv("Govt Spending/sep2024/county_sep24_table_184.csv")
county_sep24_table_347b <- read_csv("Govt Spending/sep2024/county_sep24_table_185.csv")

### Department
county_sep24_table_348 <- read_csv("Govt Spending/sep2024/county_sep24_table_186.csv") |>
    mutate(
      `1` = ifelse(`0` == "and Intergovernmental" & !is.na(`0`), NA, `1`),
      `1` = ifelse(`0` == "Relations" & !is.na(`0`), "504.77 -", `1`)
    )  |> 
    process_county_data()

garissa_department_sep24 <- bind_rows(
  #merge_rows(county_sep24_table_348, 1:3), # Header
  merge_rows(county_sep24_table_348, 4:5), # Agriculture
  county_sep24_table_348  |> slice(6), # Gender
  merge_rows(county_sep24_table_348, 7:8), # Roads
  county_sep24_table_348  |> slice(9), # Education
  merge_rows(county_sep24_table_348, 10:11), # Lands
  merge_rows(county_sep24_table_348, 12:13), # Finance
  county_sep24_table_348  |> slice(14), # Health
  merge_rows(county_sep24_table_348, 15:16), # Trade
  merge_rows(county_sep24_table_348, 17:18), # Water
  merge_rows(county_sep24_table_348, 19:21), # County Affairs
  county_sep24_table_348  |> slice(22:24), # Municipal, Climate Change, Total
  ) |> 
  rename_and_clean()

rm(county_sep24_table_348)

### Program 
county_sep24_table_349a <- read_csv("Govt Spending/sep2024/county_sep24_table_187.csv")
county_sep24_table_349b <- read_csv("Govt Spending/sep2024/county_sep24_table_188.csv")
county_sep24_table_349c <- read_csv("Govt Spending/sep2024/county_sep24_table_190.csv")

## Homa Bay ----

county_sep24_table_350 <- read_csv("Govt Spending/sep2024/county_sep24_table_192.csv")

county_sep24_table_351 <- read_csv("Govt Spending/sep2024/county_sep24_table_193.csv")

county_sep24_table_352a <- read_csv("Govt Spending/sep2024/county_sep24_table_194.csv")
county_sep24_table_352b <- read_csv("Govt Spending/sep2024/county_sep24_table_195.csv")

county_sep24_table_353a <- read_csv("Govt Spending/sep2024/county_sep24_table_196.csv")
county_sep24_table_353b <- read_csv("Govt Spending/sep2024/county_sep24_table_197.csv")

county_sep24_table_354 <- read_csv("Govt Spending/sep2024/county_sep24_table_198.csv")

### Department
county_sep24_table_355a <- read_csv("Govt Spending/sep2024/county_sep24_table_199.csv")  |> 
  process_county_data()

county_sep24_table_355b <- read_csv("Govt Spending/sep2024/county_sep24_table_200.csv")  |> 
  mutate(`6` = ifelse(`0` == "Total" & !is.na(`0`), "3,025.19", `6`))  
names(county_sep24_table_355b) <- names_department

county_sep24_table_355 <- rbind(county_sep24_table_355a, county_sep24_table_355b)

rm(county_sep24_table_355a)
rm(county_sep24_table_355b)

homa_bay_department_sep24 <- bind_rows(
  #merge_rows(county_sep24_table_355, 1:3), # Header
  merge_rows(county_sep24_table_355, 4:5), # Finance
  merge_rows(county_sep24_table_355, 6:7), # CPSB
  merge_rows(county_sep24_table_355, 8:9), # County Assembly
  merge_rows(county_sep24_table_355, 10:11), # Homa Bay
  merge_rows(county_sep24_table_355, 12:16), # Deputy Governor
  merge_rows(county_sep24_table_355, 17:22), # Gender
  merge_rows(county_sep24_table_355, 23:25), # Roads
  merge_rows(county_sep24_table_355, 26:28), # Blue Economy
  merge_rows(county_sep24_table_355, 29:32), # Education
  merge_rows(county_sep24_table_355, 33:34), # Health
  merge_rows(county_sep24_table_355, 35:37), # Lands
  merge_rows(county_sep24_table_355, 38:41), # Trade
  merge_rows(county_sep24_table_355, 42:45), # Water
  merge_rows(county_sep24_table_355, 46:49), # Governance
  merge_rows(county_sep24_table_355, 50:51), # Executive
  merge_rows(county_sep24_table_355, 52:53), # Kendu Bay
  merge_rows(county_sep24_table_355, 54:55), # Mbita
  #merge_rows(county_sep24_table_355, 56:57), # Header
  county_sep24_table_355  |> slice(58:60) # Ndhiwa, Oyugis, Total
)  |> 
rename_and_clean()

rm(county_sep24_table_355)

### Program 
county_sep24_table_356a <- read_csv("Govt Spending/sep2024/County_sep24_table_201.csv")
county_sep24_table_356b <- read_csv("Govt Spending/sep2024/County_sep24_table_203.csv")
county_sep24_table_356c <- read_csv("Govt Spending/sep2024/County_sep24_table_204.csv")
county_sep24_table_356d <- read_csv("Govt Spending/sep2024/County_sep24_table_205.csv")
county_sep24_table_356e <- read_csv("Govt Spending/sep2024/County_sep24_table_206.csv")
county_sep24_table_356f <- read_csv("Govt Spending/sep2024/County_sep24_table_208.csv")
county_sep24_table_356g <- read_csv("Govt Spending/sep2024/County_sep24_table_209.csv")
county_sep24_table_356h <- read_csv("Govt Spending/sep2024/County_sep24_table_211.csv")

## Isiolo ----

county_sep24_table_357 <- read_csv("Govt Spending/sep2024/County_sep24_table_214.csv")

county_sep24_table_358 <- read_csv("Govt Spending/sep2024/County_sep24_table_216.csv")

county_sep24_table_359 <- read_csv("Govt Spending/sep2024/County_sep24_table_217.csv")

county_sep24_table_360 <- read_csv("Govt Spending/sep2024/County_sep24_table_219.csv")

county_sep24_table_361 <- read_csv("Govt Spending/sep2024/County_sep24_table_221.csv")

### Department 
county_sep24_table_362a <- read_csv("Govt Spending/sep2024/County_sep24_table_222.csv")  |> 
process_county_data()  

county_sep24_table_362b <- read_csv("Govt Spending/sep2024/County_sep24_table_223.csv")
names(county_sep24_table_362b) <- names_department

county_sep24_table_362 <- rbind(county_sep24_table_362a, county_sep24_table_362b)  

rm(county_sep24_table_362a)
rm(county_sep24_table_362b)

isiolo_department_sep24 <- bind_rows(
  #merge_rows(county_sep24_table_362, 1:3), # Header
  merge_rows(county_sep24_table_362, 4:5), # Finance
  merge_rows(county_sep24_table_362, 6:10), # Executive
  merge_rows(county_sep24_table_362, 11:14), # Lands
  #merge_rows(county_sep24_table_362, 15:16), # Header
  county_sep24_table_362  |> slice(17:23) # Total
)  |> 
rename_and_clean()

rm(county_sep24_table_362)

### Program
county_sep24_table_363a <- read_csv("Govt Spending/sep2024/County_sep24_table_224.csv")
county_sep24_table_363b <- read_csv("Govt Spending/sep2024/County_sep24_table_226.csv")
county_sep24_table_363c <- read_csv("Govt Spending/sep2024/County_sep24_table_228.csv")
county_sep24_table_363d <- read_csv("Govt Spending/sep2024/County_sep24_table_230.csv")
county_sep24_table_363e <- read_csv("Govt Spending/sep2024/County_sep24_table_232.csv") 

## Kajiado ----

county_sep24_table_364a <- read_csv("Govt Spending/sep2024/County_sep24_table_235.csv")
county_sep24_table_364b <- read_csv("Govt Spending/sep2024/County_sep24_table_236.csv")

county_sep24_table_365 <- read_csv("Govt Spending/sep2024/County_sep24_table_237.csv")

county_sep24_table_366 <- read_csv("Govt Spending/sep2024/County_sep24_table_238.csv")

county_sep24_table_367 <- read_csv("Govt Spending/sep2024/County_sep24_table_241.csv")

county_sep24_table_368 <- read_csv("Govt Spending/sep2024/County_sep24_table_243.csv")

### Department
county_sep24_table_369a <- read_csv("Govt Spending/sep2024/County_sep24_table_244.csv")  |> 
  # separate column 2 into budget_rec, budget_dev, exch_rec, and exch_dev
  separate(
    col = 2, 
    into = c("budget_rec", "budget_dev", "exch_rec", "exch_dev"),
    sep = "\\s+",
    fill = "right"
  )  |> 
  # separate column 6 into exp_rec and exp dev
  separate(
    col = 6, 
    into = c("exp_rec", "exp_dev"),
    sep = "\\s+",
    fill = "right"
  )  |> 
  # separate column 8 into exp_exch_rec and exp_exch_dev
  separate(
    col = 8, 
    into = c("exp_exch_rec", "exp_exch_dev"),
    sep = "\\s+",
    fill = "right"
  )  |> 
  # separate column 10 into abs_rec and abs_dev
  separate(
    col = 10, 
    into = c("abs_rec", "abs_dev"),
    sep = "\\s+",
    fill = "right"
  )

county_sep24_table_369b <- read_csv("Govt Spending/sep2024/County_sep24_table_245.csv")
names(county_sep24_table_369b) <- names_department

county_sep24_table_369 <- rbind(county_sep24_table_369a, county_sep24_table_369b)

rm(county_sep24_table_369a)
rm(county_sep24_table_369b)

kajiado_department_sep24 <- bind_rows(
  #merge_rows(county_sep24_table_369, 1:3), # Header
  merge_rows(county_sep24_table_369, 4:6), # Governor
  merge_rows(county_sep24_table_369, 7:8), # CPSB
  merge_rows(county_sep24_table_369, 9:10), # Health
  merge_rows(county_sep24_table_369, 11:13), # Education
  merge_rows(county_sep24_table_369, 14:16), # Roads
  merge_rows(county_sep24_table_369, 17:20), # PSA
  county_sep24_table_369  |> slice(21), # Treasury
  #merge_rows(county_sep24_table_369, 24:25), # Header
  county_sep24_table_369  |> slice(26:36) # Table 2
  )  |> 
  rename_and_clean()

rm(county_sep24_table_369)

### Program 
county_sep24_table_370a <- read_csv("Govt Spending/sep2024/County_sep24_table_246.csv")
county_sep24_table_370b <- read_csv("Govt Spending/sep2024/County_sep24_table_248.csv")
county_sep24_table_370c <- read_csv("Govt Spending/sep2024/County_sep24_table_250.csv")
county_sep24_table_370d <- read_csv("Govt Spending/sep2024/County_sep24_table_252.csv")
county_sep24_table_370e <- read_csv("Govt Spending/sep2024/County_sep24_table_253.csv")

## Kakamega ----

county_sep24_table_371a <- read_csv("Govt Spending/sep2024/County_sep24_table_256.csv")
county_sep24_table_371b <- read_csv("Govt Spending/sep2024/County_sep24_table_257.csv")

county_sep24_table_372 <- read_csv("Govt Spending/sep2024/County_sep24_table_258.csv")

county_sep24_table_373 <- read_csv("Govt Spending/sep2024/County_sep24_table_259.csv")

county_sep24_table_374 <- read_csv("Govt Spending/sep2024/County_sep24_table_260.csv")

county_sep24_table_375 <- read_csv("Govt Spending/sep2024/County_sep24_table_261.csv")

### Department 
county_sep24_table_376a <- read_csv("Govt Spending/sep2024/County_sep24_table_262.csv")  |> 
  process_county_data()

county_sep24_table_376b <- read_csv("Govt Spending/sep2024/County_sep24_table_263.csv")
names(county_sep24_table_376b) <- names_department

county_sep24_table_376 <- rbind(county_sep24_table_376a, county_sep24_table_376b)

rm(county_sep24_table_376a)
rm(county_sep24_table_376b)

kakamega_department_sep24 <- bind_rows(
  #merge_rows(county_sep24_376, 1:3), # Header
  county_sep24_table_376  |> slice(4), # Assembly
  merge_rows(county_sep24_table_376, 5:7), # Agriculture
  county_sep24_table_376  |>  slice(8), # Health
  merge_rows(county_sep24_table_376, 9:10), # Education
  merge_rows(county_sep24_table_376, 11:13), # Roads
  merge_rows(county_sep24_table_376, 14:16), # Lands
  merge_rows(county_sep24_table_376, 17:18), # Social Service
  merge_rows(county_sep24_table_376, 19:21), # Trade
  merge_rows(county_sep24_table_376, 22:24), # Water
  merge_rows(county_sep24_table_376, 25:26), # Admin
  #merge_rows(county_sep24_376, 27:28), # Header
  county_sep24_table_376  |> slice(29:33), # Table 2
)  |> 
  rename_and_clean()

rm(county_sep24_table_376)

### No program

## Kericho ----
county_sep24_table_377 <- read_csv("Govt Spending/sep2024/County_sep24_table_266.csv")

county_sep24_table_378 <- read_csv("Govt Spending/sep2024/County_sep24_table_267.csv")

county_sep24_table_379a <- read_csv("Govt Spending/sep2024/County_sep24_table_268.csv")
county_sep24_table_379b <- read_csv("Govt Spending/sep2024/County_sep24_table_269.csv")

county_sep24_table_380 <- read_csv("Govt Spending/sep2024/County_sep24_table_272.csv")

county_sep24_table_381 <- read_csv("Govt Spending/sep2024/County_sep24_table_273.csv")

### Department
county_sep24_table_382a <- read_csv("Govt Spending/sep2024/County_sep24_table_274.csv")  |> 
  process_county_data()

county_sep24_table_382b <- read_csv("Govt Spending/sep2024/County_sep24_table_275.csv")
names(county_sep24_table_382b) <- names_department

county_sep24_table_382 <- rbind(county_sep24_table_382a, county_sep24_table_382b)

rm(county_sep24_table_382a)
rm(county_sep24_table_382b)

kericho_department_sep24 <- bind_rows(
  #merge_rows(county_sep24_382, 1:3), # Header
  county_sep24_table_382  |> slice(4), # Assembly
  merge_rows(county_sep24_table_382, 5:6), # Public Service
  county_sep24_table_382  |>  slice(7), # Governor
  merge_rows(county_sep24_table_382, 8:9), # Public Service Board
  merge_rows(county_sep24_table_382, 10:11), # Finance
  county_sep24_table_382  |>  slice(12), # Health
  merge_rows(county_sep24_table_382, 13:15), # Agriculture
  merge_rows(county_sep24_table_382, 16:18), # Education
  merge_rows(county_sep24_table_382, 19:21), # Roads
  #merge_rows(county_sep24_382, 22:23),
  county_sep24_table_382  |> slice(24:28) # Total
)  |> 
rename_and_clean()

rm(county_sep24_table_382)

### Program
county_sep24_table_383a <- read_csv("Govt Spending/sep2024/County_sep24_table_276.csv")
county_sep24_table_383b <- read_csv("Govt Spending/sep2024/County_sep24_table_278.csv")
county_sep24_table_383c <- read_csv("Govt Spending/sep2024/County_sep24_table_279.csv")
county_sep24_table_383d <- read_csv("Govt Spending/sep2024/County_sep24_table_281.csv")

## Kiambu ----

county_sep24_table_384a <- read_csv("Govt Spending/sep2024/County_sep24_table_283.csv")
county_sep24_table_384b <- read_csv("Govt Spending/sep2024/County_sep24_table_285.csv")

county_sep24_table_385 <- read_csv("Govt Spending/sep2024/County_sep24_table_286.csv")

county_sep24_table_386a <- read_csv("Govt Spending/sep2024/County_sep24_table_287.csv")
county_sep24_table_386b <- read_csv("Govt Spending/sep2024/County_sep24_table_288.csv")

county_sep24_table_387a <- read_csv("Govt Spending/sep2024/County_sep24_table_289.csv")
county_sep24_table_387b <- read_csv("Govt Spending/sep2024/County_sep24_table_290.csv")

county_sep24_table_388a <- read_csv("Govt Spending/sep2024/County_sep24_table_291.csv")
county_sep24_table_388b <- read_csv("Govt Spending/sep2024/County_sep24_table_293.csv")

### Department
county_sep24_table_389 <- read_csv("Govt Spending/sep2024/County_sep24_table_294.csv")  |> 
  process_county_data()

kiambu_department_sep24 <- bind_rows(
  #merge_rows(county_sep24_table_389, 1:3), # Header
  county_sep24_table_389  |>  slice(4:5), # Assembly + Executive
  merge_rows(county_sep24_table_389, 6:7), # CPSB
  merge_rows(county_sep24_table_389, 8:9), # Finance
  merge_rows(county_sep24_table_389, 10:11), # Water
  county_sep24_table_389  |> slice(12), # Health
  merge_rows(county_sep24_table_389, 13:14), # Roads
  merge_rows(county_sep24_table_389, 15:16), # Admin
  merge_rows(county_sep24_table_389, 17:19), # Agriculture
  merge_rows(county_sep24_table_389, 20:22), # Education
  merge_rows(county_sep24_table_389, 23:24), # Youth
  merge_rows(county_sep24_table_389, 25:28), # Lands
  merge_rows(county_sep24_table_389, 29:31), # Trade
  county_sep24_table_389  |>  slice(32) # Totals
)  |> 
rename_and_clean()

rm(county_sep24_table_389)

### Program
county_sep24_table_390a <- read_csv("Govt Spending/sep2024/County_sep24_table_295.csv")
county_sep24_table_390b <- read_csv("Govt Spending/sep2024/County_sep24_table_296.csv")
county_sep24_table_390c <- read_csv("Govt Spending/sep2024/County_sep24_table_298.csv")

## Kilifi ----

county_sep24_table_391 <- read_csv("Govt Spending/sep2024/County_sep24_table_300.csv")

county_sep24_table_392 <- read_csv("Govt Spending/sep2024/County_sep24_table_301.csv")

county_sep24_table_393 <- read_csv("Govt Spending/sep2024/County_sep24_table_302.csv")

county_sep24_table_394a <- read_csv("Govt Spending/sep2024/County_sep24_table_303.csv")
county_sep24_table_394b <- read_csv("Govt Spending/sep2024/County_sep24_table_304.csv")

county_sep24_table_395 <- read_csv("Govt Spending/sep2024/County_sep24_table_305.csv")

### Department 
county_sep24_table_396a <- read_csv("Govt Spending/sep2024/County_sep24_table_306.csv")  |> 
process_county_data()

county_sep24_table_396b <- read_csv("Govt Spending/sep2024/County_sep24_table_307.csv")
names(county_sep24_table_396b) <- names_department

county_sep24_table_396c <- read_csv("Govt Spending/sep2024/County_sep24_table_309.csv")
names(county_sep24_table_396c) <- names_department

county_sep24_table_396 <- rbind(county_sep24_table_396a, county_sep24_table_396b, county_sep24_table_396c)

rm(county_sep24_table_396a)
rm(county_sep24_table_396b)
rm(county_sep24_table_396c)

kilifi_department_sep24 <- bind_rows(
  #merge_rows(county_sep24_table_396, 1:3), # Header
  county_sep24_table_396  |> slice(4), # Assembly
  merge_rows(county_sep24_table_396, 5:6), # Governor
  merge_rows(county_sep24_table_396, 7:8), # Finance
  merge_rows(county_sep24_table_396, 9:10), # Agriculture
  merge_rows(county_sep24_table_396, 11:12), # Land
  #merge_rows(county_sep24_table_396, 13:14), # Header
  county_sep24_table_396  |> slice(15:37), # Table 2
  #merge_rows(county_sep24_table_396, 38:39), # Header
  county_sep24_table_396  |> slice(40:41), # Table 3
)  |> 
rename_and_clean()

rm(county_sep24_table_396)

### Program
county_sep24_table_397a <- read_csv("Govt Spending/sep2024/County_sep24_table_310.csv")
county_sep24_table_397b <- read_csv("Govt Spending/sep2024/County_sep24_table_312.csv")
county_sep24_table_397c <- read_csv("Govt Spending/sep2024/County_sep24_table_314.csv")
county_sep24_table_397d <- read_csv("Govt Spending/sep2024/County_sep24_table_316.csv")

## Kirinyaga ----

county_sep24_table_398a <- read_csv("Govt Spending/sep2024/County_sep24_table_318.csv")
county_sep24_table_398b <- read_csv("Govt Spending/sep2024/County_sep24_table_319.csv")

county_sep24_table_399 <- read_csv("Govt Spending/sep2024/County_sep24_table_320.csv")  

county_sep24_table_3100 <- read_csv("Govt Spending/sep2024/County_sep24_table_323.csv")

county_sep24_table_3101 <- read_csv("Govt Spending/sep2024/County_sep24_table_325.csv")

county_sep24_table_3102 <- read_csv("Govt Spending/sep2024/County_sep24_table_327.csv")

### Department 
county_sep24_table_3103 <- read_csv("Govt Spending/sep2024/County_sep24_table_328.csv")  |> 
process_county_data()

kirinyaga_department_sep24 <- bind_rows(
  #merge_rows(county_sep24_table_3103, 1:4), # Header
  merge_rows(county_sep24_table_3103, 5:7), # Agriculture
  merge_rows(county_sep24_table_3103, 8:11), # Cooperatives
  merge_rows(county_sep24_table_3103, 12:13), # Assembly
  merge_rows(county_sep24_table_3103, 14:15), # Executive
  county_sep24_table_3103  |> slice(16), # Education
  merge_rows(county_sep24_table_3103, 17:19), # Environment
  merge_rows(county_sep24_table_3103, 20:22), # Finance
  merge_rows(county_sep24_table_3103, 23:24), # Gender
  merge_rows(county_sep24_table_3103, 25:27), # Land
  merge_rows(county_sep24_table_3103, 28:30), # Health
  merge_rows(county_sep24_table_3103, 31:33), # Sports
  merge_rows(county_sep24_table_3103, 34:35), # Transport
  county_sep24_table_3103  |> slice(36), # Total
)  |> 
rename_and_clean()  |> 
mutate(
  across(
    c("budget_rec", "budget_dev", "exch_rec", "exch_dev", "exp_rec", "exp_dev"),
    ~ ./1000000
  )
)

rm(county_sep24_table_3103)

### Program
county_sep24_table_3104a <- read_csv("Govt Spending/sep2024/County_sep24_table_329.csv")
county_sep24_table_3104b <- read_csv("Govt Spending/sep2024/County_sep24_table_330.csv")
county_sep24_table_3104c <- read_csv("Govt Spending/sep2024/County_sep24_table_332.csv")
county_sep24_table_3104d <- read_csv("Govt Spending/sep2024/County_sep24_table_334.csv")
county_sep24_table_3104e <- read_csv("Govt Spending/sep2024/County_sep24_table_336.csv")

## Kisii ----

county_sep24_table_3105 <- read_csv("Govt Spending/sep2024/County_sep24_table_339.csv")

county_sep24_table_3106 <- read_csv("Govt Spending/sep2024/County_sep24_table_340.csv")

county_sep24_table_3107a <- read_csv("Govt Spending/sep2024/County_sep24_table_341.csv")
county_sep24_table_3107b <- read_csv("Govt Spending/sep2024/County_sep24_table_342.csv")

county_sep24_table_3108a <- read_csv("Govt Spending/sep2024/County_sep24_table_343.csv")
county_sep24_table_3108b <- read_csv("Govt Spending/sep2024/County_sep24_table_344.csv")
county_sep24_table_3108_a <- read_csv("Govt Spending/sep2024/County_sep24_table_345.csv")

county_sep24_table_3109a <- read_csv("Govt Spending/sep2024/County_sep24_table_346.csv")
county_sep24_table_3109b <- read_csv("Govt Spending/sep2024/County_sep24_table_348.csv")

### Department
county_sep24_table_3110a <- read_csv("Govt Spending/sep2024/County_sep24_table_349.csv")  |> 
  process_county_data()

county_sep24_table_3110b <- read_csv("Govt Spending/sep2024/County_sep24_table_350.csv")
names(county_sep24_table_3110b) <- names_department

county_sep24_table_3110 <- rbind(county_sep24_table_3110a, county_sep24_table_3110b)

rm(county_sep24_table_3110a)
rm(county_sep24_table_3110b)

kisii_department_sep24 <- bind_rows(
  #merge_rows(county_sep24_table_3110, 1:3), # Header
  merge_rows(county_sep24_table_3110, 4:5), # PSB
  merge_rows(county_sep24_table_3110, 6:8), # Admin
  merge_rows(county_sep24_table_3110, 9:10), # Finance
  merge_rows(county_sep24_table_3110, 11:13), # Agriculture
  merge_rows(county_sep24_table_3110, 14:16), # Water
  merge_rows(county_sep24_table_3110, 17:19), # Education
  merge_rows(county_sep24_table_3110, 20:21), # Health
  merge_rows(county_sep24_table_3110, 22:24), # Lands
  merge_rows(county_sep24_table_3110, 25:26), # Roads
  merge_rows(county_sep24_table_3110, 27:28), # Trade
  merge_rows(county_sep24_table_3110, 29:30), # culture
  county_sep24_table_3110  |>  slice(31:32), # Municipalities
  #merge_rows(county_sep24_table_3110, 33:34), # Header
  county_sep24_table_3110  |> slice(35:36), # Table 2
)  |> 
rename_and_clean()

rm(county_sep24_table_3110)

### Program
county_sep24_table_3111a <- read_csv("Govt Spending/sep2024/County_sep24_table_351.csv")
county_sep24_table_3111b <- read_csv("Govt Spending/sep2024/County_sep24_table_353.csv")
county_sep24_table_3111c <- read_csv("Govt Spending/sep2024/County_sep24_table_355.csv")

## Kisumu ----
county_sep24_table_3112a <- read_csv("Govt Spending/sep2024/County_sep24_table_358.csv")
county_sep24_table_3112b <- read_csv("Govt Spending/sep2024/County_sep24_table_359.csv")

county_sep24_table_3113 <- read_csv("Govt Spending/sep2024/County_sep24_table_360.csv")

county_sep24_table_3114 <- read_csv("Govt Spending/sep2024/County_sep24_table_361.csv")

county_sep24_table_3115 <- read_csv("Govt Spending/sep2024/County_sep24_table_365.csv")

county_sep24_table_3116 <- read_csv("Govt Spending/sep2024/County_sep24_table_366.csv")

### Department
county_sep24_table_3117 <- read_csv("Govt Spending/sep2024/County_sep24_table_367.csv")   |> 
  # separate column 2 into budget_rec, budget_dev
  separate(
    col = 2, 
    into = c("budget_rec", "budget_dev"),
    sep = "\\s+",
    fill = "right"
  )  |> 
  # separate column 4 into exch_rec and exch_dev
  separate(
    col = 4,
    into = c("exch_rec", "exch_dev"),
    sep = "\\s+",
    fill = "right"
  )  |> 
  # separate column 6 into exp_rec and exp dev
  separate(
    col = 6,
    into = c("exp_rec", "exp_dev"),
    sep = "\\s+",
    fill = "right"
  )  |> 
  # separate column 8 into exp_exch_rec and exp_exch_dev
  separate(
    col = 8,
    into = c("exp_exch_rec", "exp_exch_dev"),
    sep = "\\s+",
    fill = "right"
  )  |> 
  # rename columns 
  rename(
    "abs_rec" = `5`,
    "abs_dev" = `6`
  )

kisumu_department_sep24 <- bind_rows(
  #merge_rows(county_sep24_table_3117, 1:3), # Header
  merge_rows(county_sep24_table_3117, 4:5), # Agriculture
  county_sep24_table_3117  |>  slice(6:8), # Assembly, city, cpsb
  merge_rows(county_sep24_table_3117, 9:10), # Education
  merge_rows(county_sep24_table_3117, 11:14), # Admin + Executive
  merge_rows(county_sep24_table_3117, 15:16), # Health
  merge_rows(county_sep24_table_3117, 17:18), # Trade
  merge_rows(county_sep24_table_3117, 19:20), # Sports
  merge_rows(county_sep24_table_3117, 21:22), # Roads
  merge_rows(county_sep24_table_3117, 23:25), # Lands
  merge_rows(county_sep24_table_3117, 26:27), # Finance
  merge_rows(county_sep24_table_3117, 28:29), # Water
  county_sep24_table_3117  |>  slice(30), # Totals
)  |> 
rename_and_clean()

rm(county_sep24_table_3117)

### Program 
county_sep24_table_3118a <- read_csv("Govt Spending/sep2024/County_sep24_table_368.csv")
county_sep24_table_3118b <- read_csv("Govt Spending/sep2024/County_sep24_table_369.csv")
county_sep24_table_3118c <- read_csv("Govt Spending/sep2024/County_sep24_table_371.csv")
county_sep24_table_3118d <- read_csv("Govt Spending/sep2024/County_sep24_table_373.csv")
county_sep24_table_3118e <- read_csv("Govt Spending/sep2024/County_sep24_table_375.csv")
county_sep24_table_3118f <- read_csv("Govt Spending/sep2024/County_sep24_table_377.csv")

## Kitui ----

county_sep24_table_3119 <- read_csv("Govt Spending/sep2024/County_sep24_table_379.csv")

county_sep24_table_3120 <- read_csv("Govt Spending/sep2024/County_sep24_table_380.csv")

county_sep24_table_3121 <- read_csv("Govt Spending/sep2024/County_sep24_table_381.csv")

county_sep24_table_3122a <- read_csv("Govt Spending/sep2024/County_sep24_table_382.csv")
county_sep24_table_3122b <- read_csv("Govt Spending/sep2024/County_sep24_table_383.csv")

county_sep24_table_3123a <- read_csv("Govt Spending/sep2024/County_sep24_table_384.csv")
county_sep24_table_3123b <- read_csv("Govt Spending/sep2024/County_sep24_table_386.csv")

### Department
county_sep24_table_3124a <- read_csv("Govt Spending/sep2024/County_sep24_table_387.csv")
county_sep24_table_3124b <- read_csv("Govt Spending/sep2024/County_sep24_table_388.csv")

county_sep24_table_3124 <- rbind(county_sep24_table_3124a, county_sep24_table_3124b)
names(county_sep24_table_3124) <- names_department
# They haven't calculated the exch_to_exp_dev at all

rm(county_sep24_table_3124a)
rm(county_sep24_table_3124b)

kitui_department_sep24 <- bind_rows(
  #merge_rows(county_sep24_table_3124, 1:2), # Header
  county_sep24_table_3124  |>  slice(3:11), # Table 1
  #merge_rows(county_sep24_table_3124, 13:14), # header
  county_sep24_table_3124  |>  slice(15:22), # Table 2
)  |> 
rename_and_clean()

rm(county_sep24_table_3124)

### Program
county_sep24_table_3125a <- read_csv("Govt Spending/sep2024/County_sep24_table_389.csv")
county_sep24_table_3125b <- read_csv("Govt Spending/sep2024/County_sep24_table_391.csv")
county_sep24_table_3125c <- read_csv("Govt Spending/sep2024/County_sep24_table_393.csv")
county_sep24_table_3125d <- read_csv("Govt Spending/sep2024/County_sep24_table_395.csv")
county_sep24_table_3125e <- read_csv("Govt Spending/sep2024/County_sep24_table_397.csv")

## Kwale ----

county_sep24_table_3126a <- read_csv("Govt Spending/sep2024/County_sep24_table_399.csv")
county_sep24_table_3126b <- read_csv("Govt Spending/sep2024/County_sep24_table_401.csv")

county_sep24_table_3127 <- read_csv("Govt Spending/sep2024/County_sep24_table_411.csv")

county_sep24_table_3128 <- read_csv("Govt Spending/sep2024/County_sep24_table_412.csv")

county_sep24_table_3129a <- read_csv("Govt Spending/sep2024/County_sep24_table_415.csv")
county_sep24_table_3129b <- read_csv("Govt Spending/sep2024/County_sep24_table_417.csv")

county_sep24_table_3130 <- read_csv("Govt Spending/sep2024/County_sep24_table_418.csv")

### Department 
county_sep24_table_3131a <- read_csv("Govt Spending/sep2024/County_sep24_table_419.csv")  |> 
process_county_data()

county_sep24_table_3131b <- read_csv("Govt Spending/sep2024/County_sep24_table_420.csv")
names(county_sep24_table_3131b) <- names_department

county_sep24_table_3131 <- rbind(county_sep24_table_3131a, county_sep24_table_3131b)

rm(county_sep24_table_3131a)
rm(county_sep24_table_3131b)

kwale_department_sep24 <- bind_rows(
  # merge_rows(county_sep24_table_3131, 1:3), # Header
  merge_rows(county_sep24_table_3131, 4:5), # Finance
  merge_rows(county_sep24_table_3131, 6:7), # Agriculture
  #merge_rows(county_sep24_table_3131, 8:9), # Header
  county_sep24_table_3131  |>  slice(10:28) # Table 2
)  |> 
rename_and_clean()

rm(county_sep24_table_3131)

### Program 
county_sep24_table_3132a <- read_csv("Govt Spending/sep2024/County_sep24_table_421.csv")
county_sep24_table_3132b <- read_csv("Govt Spending/sep2024/County_sep24_table_423.csv")
county_sep24_table_3132c <- read_csv("Govt Spending/sep2024/County_sep24_table_424.csv")
county_sep24_table_3132d <- read_csv("Govt Spending/sep2024/County_sep24_table_426.csv")
county_sep24_table_3132e <- read_csv("Govt Spending/sep2024/County_sep24_table_428.csv")
county_sep24_table_3132f <- read_csv("Govt Spending/sep2024/County_sep24_table_430.csv")
county_sep24_table_3132g <- read_csv("Govt Spending/sep2024/County_sep24_table_432.csv")
county_sep24_table_3132h <- read_csv("Govt Spending/sep2024/County_sep24_table_434.csv")
county_sep24_table_3132i <- read_csv("Govt Spending/sep2024/County_sep24_table_436.csv")
county_sep24_table_3132j <- read_csv("Govt Spending/sep2024/County_sep24_table_438.csv")
county_sep24_table_3132k <- read_csv("Govt Spending/sep2024/County_sep24_table_440.csv")

## Laikipia ----

county_sep24_table_3133 <- read_csv("Govt Spending/sep2024/County_sep24_table_442.csv")

county_sep24_table_3134 <- read_csv("Govt Spending/sep2024/County_sep24_table_443.csv")

county_sep24_table_3135 <- read_csv("Govt Spending/sep2024/County_sep24_table_444.csv")

county_sep24_table_3136 <- read_csv("Govt Spending/sep2024/County_sep24_table_446.csv")

county_sep24_table_3137a <- read_csv("Govt Spending/sep2024/County_sep24_table_447.csv")
county_sep24_table_3137b <- read_csv("Govt Spending/sep2024/County_sep24_table_449.csv")

### Department
county_sep24_table_3138 <- read_csv("Govt Spending/sep2024/County_sep24_table_450.csv")  |> 
process_county_data()  |> 
mutate(
  budget_rec = ifelse(`0` == "Assembly" & !is.na(`0`), 565.81, budget_rec)
)

laikipia_department_sep24 <- bind_rows(
  #merge_rows(county_sep24_table_3138, 1:3), # header
  merge_rows(county_sep24_table_3138, 4:8), # admin
  merge_rows(county_sep24_table_3138, 9:12), # finance
  merge_rows(county_sep24_table_3138, 13:14), # health
  merge_rows(county_sep24_table_3138, 15:17), # agriculture
  merge_rows(county_sep24_table_3138, 18:19), # infrastructure
  merge_rows(county_sep24_table_3138, 20:21), # education
  merge_rows(county_sep24_table_3138, 22:24), # trade
  merge_rows(county_sep24_table_3138, 25:27), # gender
  county_sep24_table_3138  |>  slice(28), # water
  merge_rows(county_sep24_table_3138, 29:30), # nanyuki
  merge_rows(county_sep24_table_3138, 31:32), # rumuruti
  merge_rows(county_sep24_table_3138, 33:34), # county assembly
  county_sep24_table_3138  |>  slice(35) # Totals
)  |> 
rename_and_clean()

rm(county_sep24_table_3138)

### Program 
county_sep24_table_3139a <- read_csv("Govt Spending/sep2024/County_sep24_table_451.csv")
county_sep24_table_3139b <- read_csv("Govt Spending/sep2024/County_sep24_table_452.csv")
county_sep24_table_3139c <- read_csv("Govt Spending/sep2024/County_sep24_table_453.csv")
county_sep24_table_3139d <- read_csv("Govt Spending/sep2024/County_sep24_table_455.csv")
county_sep24_table_3139e <- read_csv("Govt Spending/sep2024/County_sep24_table_457.csv")
county_sep24_table_3139f <- read_csv("Govt Spending/sep2024/County_sep24_table_459.csv")
county_sep24_table_3139g <- read_csv("Govt Spending/sep2024/County_sep24_table_461.csv")

## Lamu ----

county_sep24_table_3140 <- read_csv("Govt Spending/sep2024/County_sep24_table_463.csv")

county_sep24_table_3141 <- read_csv("Govt Spending/sep2024/County_sep24_table_464.csv")

county_sep24_table_3142 <- read_csv("Govt Spending/sep2024/County_sep24_table_465.csv")

county_sep24_table_3143a <- read_csv("Govt Spending/sep2024/County_sep24_table_466.csv")
county_sep24_table_3143b <- read_csv("Govt Spending/sep2024/County_sep24_table_467.csv")

county_sep24_table_3144 <- read_csv("Govt Spending/sep2024/County_sep24_table_468.csv")  

### Department
county_sep24_table_3145 <- read_csv("Govt Spending/sep2024/County_sep24_table_470.csv")  |> 
  process_county_data()

lamu_department_sep24 <- bind_rows(
  #merge_rows(county_sep24_table_3145, 1:3), # Department
  county_sep24_table_3145  |>  slice(4), # assembly
  merge_rows(county_sep24_table_3145, 5:6), # executive
  merge_rows(county_sep24_table_3145, 7:8), # finance
  merge_rows(county_sep24_table_3145, 9:10), # Agriculture
  merge_rows(county_sep24_table_3145, 11:12), # Land
  merge_rows(county_sep24_table_3145, 13:15), # education
  county_sep24_table_3145  |>  slice(16:17), # health + trade
  merge_rows(county_sep24_table_3145, 18:21), # comms
  merge_rows(county_sep24_table_3145, 22:24), # animals
  county_sep24_table_3145  |>  slice(25:26), # water
  merge_rows(county_sep24_table_3145, 27:28), # health + environment
  merge_rows(county_sep24_table_3145, 29:30), # budget
  merge_rows(county_sep24_table_3145, 31:32), # roads
  county_sep24_table_3145  |>  slice(33), # lamu
  merge_rows(county_sep24_table_3145, 34:35), # devolution
  county_sep24_table_3145  |>  slice(36)
)  |> 
rename_and_clean()

rm(county_sep24_table_3145)

### Program 
county_sep24_table_3146a <- read_csv("Govt Spending/sep2024/County_sep24_table_471.csv")
county_sep24_table_3146b <- read_csv("Govt Spending/sep2024/County_sep24_table_472.csv")
county_sep24_table_3146c <- read_csv("Govt Spending/sep2024/County_sep24_table_474.csv")
county_sep24_table_3146d <- read_csv("Govt Spending/sep2024/County_sep24_table_475.csv")
county_sep24_table_3146e <- read_csv("Govt Spending/sep2024/County_sep24_table_477.csv")
county_sep24_table_3146f <- read_csv("Govt Spending/sep2024/County_sep24_table_479.csv")

## Machakos ----
county_sep24_table_3147a <- read_csv("Govt Spending/sep2024/County_sep24_table_481.csv")
county_sep24_table_3147b <- read_csv("Govt Spending/sep2024/County_sep24_table_483.csv")

county_sep24_table_3148 <- read_csv("Govt Spending/sep2024/County_sep24_table_484.csv")

county_sep24_table_3149 <- read_csv("Govt Spending/sep2024/County_sep24_table_485.csv")

county_sep24_table_3150a <- read_csv("Govt Spending/sep2024/County_sep24_table_486.csv")
county_sep24_table_3150b <- read_csv("Govt Spending/sep2024/County_sep24_table_487.csv")

county_sep24_table_3151 <- read_csv("Govt Spending/sep2024/County_sep24_table_489.csv")

### Department
county_sep24_table_3152 <- read_csv("Govt Spending/sep2024/County_sep24_table_490.csv")
names(county_sep24_table_3152) <- names_department

machakos_department_sep24 <- bind_rows(
  #merge_rows(county_sep24_table_3152, 1:2), # header
  county_sep24_table_3152  |>  slice(3:17)
)  |> 
rename_and_clean()

rm(county_sep24_table_3152)

### Program
county_sep24_table_3153a <- read_csv("Govt Spending/sep2024/County_sep24_table_492.csv")
county_sep24_table_3153b <- read_csv("Govt Spending/sep2024/County_sep24_table_493.csv")
county_sep24_table_3153c <- read_csv("Govt Spending/sep2024/County_sep24_table_495.csv")
county_sep24_table_3153d <- read_csv("Govt Spending/sep2024/County_sep24_table_496.csv")
county_sep24_table_3153e <- read_csv("Govt Spending/sep2024/County_sep24_table_498.csv")

## Makueni ----
county_sep24_table_3154 <- read_csv("Govt Spending/sep2024/County_sep24_table_501.csv")

county_sep24_table_3155 <- read_csv("Govt Spending/sep2024/County_sep24_table_503.csv")

county_sep24_table_3156a <- read_csv("Govt Spending/sep2024/County_sep24_table_504.csv")
county_sep24_table_3156b <- read_csv("Govt Spending/sep2024/County_sep24_table_505.csv")

county_sep24_table_3157 <- read_csv("Govt Spending/sep2024/County_sep24_table_506.csv")

county_sep24_table_3158 <- read_csv("Govt Spending/sep2024/County_sep24_table_507.csv")

### Department
county_sep24_table_3159 <- read_csv("Govt Spending/sep2024/County_sep24_table_508.csv")  |> 
  process_county_data()

makueni_department_sep24 <- bind_rows(
  #merge_rows(county_sep24_table_3159, 1:3), # header
  county_sep24_table_3159  |>  slice(4:6), # executive
  merge_rows(county_sep24_table_3159, 7:9), # devolution
  merge_rows(county_sep24_table_3159, 10:11), # finance
  merge_rows(county_sep24_table_3159, 12:14), # agriculture
  merge_rows(county_sep24_table_3159, 15:17), # fruit
  merge_rows(county_sep24_table_3159, 18:19), # education
  merge_rows(county_sep24_table_3159, 20:21), #gender
  county_sep24_table_3159  |>  slice(22), # health
  merge_rows(county_sep24_table_3159, 23:24), # trade
  merge_rows(county_sep24_table_3159, 25:27), # infrastructure
  merge_rows(county_sep24_table_3159, 28:30), # lands
  county_sep24_table_3159  |>  slice(31), # wote
  merge_rows(county_sep24_table_3159, 32:33), # sultan hamud
  county_sep24_table_3159  |>  slice(34), # water
  merge_rows(county_sep24_table_3159, 35:36), # sand harvesting
  merge_rows(county_sep24_table_3159, 37:38), #cpsb
  county_sep24_table_3159  |>  slice(40:41) # totals
)  |> 
rename_and_clean()

rm(county_sep24_table_3159)

### Program
county_sep24_table_3160a <- read_csv("Govt Spending/sep2024/County_sep24_table_509.csv")
county_sep24_table_3160b <- read_csv("Govt Spending/sep2024/County_sep24_table_510.csv")
county_sep24_table_3160c <- read_csv("Govt Spending/sep2024/County_sep24_table_512.csv")
county_sep24_table_3160d <- read_csv("Govt Spending/sep2024/County_sep24_table_514.csv")

## Mandera ----

county_sep24_table_3161a <- read_csv("Govt Spending/sep2024/County_sep24_table_516.csv")
county_sep24_table_3161b <- read_csv("Govt Spending/sep2024/County_sep24_table_518.csv")

county_sep24_table_3162 <- read_csv("Govt Spending/sep2024/County_sep24_table_519.csv")

county_sep24_table_3163 <- read_csv("Govt Spending/sep2024/County_sep24_table_520.csv")

county_sep24_table_3164 <- read_csv("Govt Spending/sep2024/County_sep24_table_533.csv")

county_sep24_table_3165 <- read_csv("Govt Spending/sep2024/County_sep24_table_534.csv")

### Department 
county_sep24_table_3166 <- read_csv("Govt Spending/sep2024/County_sep24_table_535.csv")  |> 
  process_county_data()

mandera_department_sep24 <- bind_rows(
  #merge_rows(county_sep24_table_3166, 1:3), # header
  county_sep24_table_3166  |>  slice(4), # County assembly
  merge_rows(county_sep24_table_3166, 5:6), # Governor
  merge_rows(county_sep24_table_3166, 7:8), # Finance
  merge_rows(county_sep24_table_3166, 9:10), # agriculture
  merge_rows(county_sep24_table_3166, 11:12), # water
  merge_rows(county_sep24_table_3166, 13:14), # education
  county_sep24_table_3166  |>  slice(15), # health
  merge_rows(county_sep24_table_3166, 16:17), # lands
  merge_rows(county_sep24_table_3166, 18:19), # roads
  county_sep24_table_3166  |>  slice(20), # social dvpt
  merge_rows(county_sep24_table_3166, 21:23), # psm
  merge_rows(county_sep24_table_3166, 24:25), # cpsb
  merge_rows(county_sep24_table_3166, 26:27), # trade
  merge_rows(county_sep24_table_3166, 28:29), # secretary
  merge_rows(county_sep24_table_3166, 30:31), # attorney
  county_sep24_table_3166  |>  slice(32)
)  |> 
rename_and_clean()

rm(county_sep24_table_3166)

### Program
county_sep24_table_3167a <- read_csv("Govt Spending/sep2024/County_sep24_table_536.csv")
county_sep24_table_3167b <- read_csv("Govt Spending/sep2024/County_sep24_table_538.csv")

## Marsabit ----

county_sep24_table_3168 <- read_csv("Govt Spending/sep2024/County_sep24_table_541.csv")

county_sep24_table_3169 <- read_csv("Govt Spending/sep2024/County_sep24_table_543.csv")

county_sep24_table_3170 <- read_csv("Govt Spending/sep2024/County_sep24_table_544.csv")

county_sep24_table_3171a <- read_csv("Govt Spending/sep2024/County_sep24_table_545.csv")
county_sep24_table_3171b <- read_csv("Govt Spending/sep2024/County_sep24_table_546.csv")

county_sep24_table_3172 <- read_csv("Govt Spending/sep2024/County_sep24_table_547.csv")

### Department
county_sep24_table_3173a <- read_csv("Govt Spending/sep2024/County_sep24_table_548.csv")  |> 
  # separate 2 into budget_rec, budget_dev, exch_rec, exch_dev
  separate(
    col = 2,
    into = c("budget_rec", "budget_dev", "exch_rec", "exch_dev"),
    sep = "\\s+",
    fill = "right"
  )  |> 
  select(-`2`)  |> 
  # separate 6 into exp_rec, exp_dev
  separate(
    col = 6,
    into = c("exp_rec", "exp_dev"),
    sep = "\\s+",
    fill = "right"
  )  |> 
  separate(
    col = 8,
    into = c("exp_exch_rec", "exp_exch_dev"),
    sep = "\\s+",
    fill = "right"
  )  |> 
  separate(
    col = 10,
    into = c("abs_rec", "abs_dev"),
    sep = "\\s+",
    fill = "right"
  )

county_sep24_table_3173b <- read_csv("Govt Spending/sep2024/County_sep24_table_549.csv")
names(county_sep24_table_3173b) <- names_department

county_sep24_table_3173 <- rbind(county_sep24_table_3173a, county_sep24_table_3173b)

rm(county_sep24_table_3173a)
rm(county_sep24_table_3173b)

marsabit_department_sep24 <- bind_rows(
  #merge_rows(county_sep24_table_3173, 1:3),
  county_sep24_table_3173  |> slice(4:5),
  #merge_rows(county_sep24_table_3173, 8:9),
  county_sep24_table_3173  |> slice(10:23)
)  |> 
  rename_and_clean()

rm(county_sep24_table_3173)

### Program
county_sep24_table_3174a <- read_csv("Govt Spending/sep2024/County_sep24_table_550.csv")
county_sep24_table_3174b <- read_csv("Govt Spending/sep2024/County_sep24_table_552.csv")
county_sep24_table_3174c <- read_csv("Govt Spending/sep2024/County_sep24_table_554.csv")
county_sep24_table_3174d <- read_csv("Govt Spending/sep2024/County_sep24_table_556.csv")
county_sep24_table_3174e <- read_csv("Govt Spending/sep2024/County_sep24_table_558.csv")
county_sep24_table_3174f <- read_csv("Govt Spending/sep2024/County_sep24_table_560.csv")

## Meru ----

county_sep24_table_3175 <- read_csv("Govt Spending/sep2024/County_sep24_table_562.csv")

county_sep24_table_3176 <- read_csv("Govt Spending/sep2024/County_sep24_table_573.csv")

county_sep24_table_3177a <- read_csv("Govt Spending/sep2024/County_sep24_table_574.csv")
county_sep24_table_3177b <- read_csv("Govt Spending/sep2024/County_sep24_table_575.csv")

county_sep24_table_3178a <- read_csv("Govt Spending/sep2024/County_sep24_table_578.csv")
county_sep24_table_3178b <- read_csv("Govt Spending/sep2024/County_sep24_table_580.csv")

county_sep24_table_3179 <- read_csv("Govt Spending/sep2024/County_sep24_table_581.csv")

### Department ----
county_sep24_table_3180a <- read_csv("Govt Spending/sep2024/County_sep24_table_582.csv")  |> 
  process_county_data()
county_sep24_table_3180b <- read_csv("Govt Spending/sep2024/County_sep24_table_583.csv")  
names(county_sep24_table_3180b) <- names_department
 
county_sep24_table_3180 <- rbind(county_sep24_table_3180a, county_sep24_table_3180b)

rm(county_sep24_table_3180a)
rm(county_sep24_table_3180b)

meru_department_sep24 <- bind_rows(
  #merge_rows(county_sep24_table_3180, 1:3), #header
  county_sep24_table_3180  |> slice(4), #county assembly
  merge_rows(county_sep24_table_3180, 5:6), #governor
  merge_rows(county_sep24_table_3180, 7:9), #finance
  merge_rows(county_sep24_table_3180, 10:12), #agriculture
  merge_rows(county_sep24_table_3180, 13:14), #water
  #merge_rows(county_sep24_table_3180, 15:16), #header
  county_sep24_table_3180  |> slice(17:25), # table2
)  |> 
rename_and_clean()

rm(county_sep24_table_3180)

### Program ----
county_sep24_table_3181a <- read_csv("Govt Spending/sep2024/County_sep24_table_584.csv")
county_sep24_table_3181b <- read_csv("Govt Spending/sep2024/County_sep24_table_585.csv")
county_sep24_table_3181c <- read_csv("Govt Spending/sep2024/County_sep24_table_587.csv")
county_sep24_table_3181d <- read_csv("Govt Spending/sep2024/County_sep24_table_589.csv")

## Migori ----
county_sep24_table_3182 <- read_csv("Govt Spending/sep2024/County_sep24_table_592.csv")

county_sep24_table_3183 <- read_csv("Govt Spending/sep2024/County_sep24_table_594.csv")

county_sep24_table_3184 <- read_csv("Govt Spending/sep2024/County_sep24_table_595.csv")

county_sep24_table_3185 <- read_csv("Govt Spending/sep2024/County_sep24_table_597.csv")

county_sep24_table_3186 <- read_csv("Govt Spending/sep2024/County_sep24_table_599.csv")

### Department ----
county_sep24_table_3187a <- read_csv("Govt Spending/sep2024/County_sep24_table_600.csv")  |> 
process_county_data()
county_sep24_table_3187b <- read_csv("Govt Spending/sep2024/County_sep24_table_601.csv")
names(county_sep24_table_3187b) <- names_department

county_sep24_table_3187 <- rbind(county_sep24_table_3187a, county_sep24_table_3187b)

rm(county_sep24_table_3187a)
rm(county_sep24_table_3187b)

migori_department_sep24 <- bind_rows(
  #merge_rows(county_sep24_table_3187, 1:3), #header
  merge_rows(county_sep24_table_3187, 4:7), #agriculture
  merge_rows(county_sep24_table_3187, 8:11), #trade
  #merge_rows(county_sep24_table_3187, 13:14), #header
  county_sep24_table_3187  |> slice(15:26), # table2
)  |> 
rename_and_clean()

rm(county_sep24_table_3187)

### Program ----
county_sep24_table_3188a <- read_csv("Govt Spending/sep2024/County_sep24_table_602.csv")
county_sep24_table_3188b <- read_csv("Govt Spending/sep2024/County_sep24_table_604.csv")
county_sep24_table_3188c <- read_csv("Govt Spending/sep2024/County_sep24_table_605.csv")
county_sep24_table_3188d <- read_csv("Govt Spending/sep2024/County_sep24_table_606.csv")
county_sep24_table_3188e <- read_csv("Govt Spending/sep2024/County_sep24_table_608.csv")
county_sep24_table_3188f <- read_csv("Govt Spending/sep2024/County_sep24_table_610.csv")
county_sep24_table_3188g <- read_csv("Govt Spending/sep2024/County_sep24_table_612.csv")
county_sep24_table_3188h <- read_csv("Govt Spending/sep2024/County_sep24_table_614.csv")
county_sep24_table_3188i <- read_csv("Govt Spending/sep2024/County_sep24_table_616.csv")
county_sep24_table_3188j <- read_csv("Govt Spending/sep2024/County_sep24_table_618.csv")
county_sep24_table_3188k <- read_csv("Govt Spending/sep2024/County_sep24_table_620.csv")

## Mombasa ----
county_sep24_table_3189 <- read_csv("Govt Spending/sep2024/County_sep24_table_621.csv")
county_sep24_table_3190 <- read_csv("Govt Spending/sep2024/County_sep24_table_623.csv")

county_sep24_table_3191 <- read_csv("Govt Spending/sep2024/County_sep24_table_624.csv")

county_sep24_table_3192a <- read_csv("Govt Spending/sep2024/County_sep24_table_626.csv")
county_sep24_table_3192b <- read_csv("Govt Spending/sep2024/County_sep24_table_628.csv")

county_sep24_table_3193 <- read_csv("Govt Spending/sep2024/County_sep24_table_629.csv")

### Department
county_sep24_table_3194 <- read_csv("Govt Spending/sep2024/County_sep24_table_630.csv")  |> 
process_county_data()

mombasa_department_sep24 <- bind_rows(
  #merge_rows(county_sep24_table_3194, 1:3), #header  
  county_sep24_table_3194  |> slice(4), #county assembly
  merge_rows(county_sep24_table_3194, 5:6), #cpsb
  merge_rows(county_sep24_table_3194, 7:8), #finance
  county_sep24_table_3194  |> slice(9), #health
  merge_rows(county_sep24_table_3194, 10:11), #transport
  merge_rows(county_sep24_table_3194, 12:14), #environment
  merge_rows(county_sep24_table_3194, 15:16), #education
  merge_rows(county_sep24_table_3194, 17:20), #environment
  merge_rows(county_sep24_table_3194, 21:25), #psa
  merge_rows(county_sep24_table_3194, 26:27), #tourism
  merge_rows(county_sep24_table_3194, 28:30), #land
  merge_rows(county_sep24_table_3194, 31:33), #agriculture
  merge_rows(county_sep24_table_3194, 34:35), #attorney
  county_sep24_table_3194  |> slice(36:37) #total
)  |> 
rename_and_clean()

rm(county_sep24_table_3194)

### Program 
county_sep24_table_3195a <- read_csv("Govt Spending/sep2024/County_sep24_table_631.csv")
county_sep24_table_3195b <- read_csv("Govt Spending/sep2024/County_sep24_table_632.csv")
county_sep24_table_3195c <- read_csv("Govt Spending/sep2024/County_sep24_table_634.csv")
county_sep24_table_3195d <- read_csv("Govt Spending/sep2024/County_sep24_table_636.csv")

## Muranga ----
county_sep24_table_3196 <- read_csv("Govt Spending/sep2024/County_sep24_table_638.csv")

county_sep24_table_3197 <- read_csv("Govt Spending/sep2024/County_sep24_table_640.csv")

county_sep24_table_3198a <- read_csv("Govt Spending/sep2024/County_sep24_table_641.csv")
county_sep24_table_3198b <- read_csv("Govt Spending/sep2024/County_sep24_table_642.csv")

county_sep24_table_3199a <- read_csv("Govt Spending/sep2024/County_sep24_table_643.csv")
county_sep24_table_3199b <- read_csv("Govt Spending/sep2024/County_sep24_table_644.csv")

county_sep24_table_3200 <- read_csv("Govt Spending/sep2024/County_sep24_table_645.csv")

### Department
county_sep24_table_3201a <- read_csv("Govt Spending/sep2024/County_sep24_table_646.csv")   |> 
  # Separate column 2 into budget_rec and budget_dev
  separate(
    col = 2,
    into = c("budget_rec", "budget_dev"),
    sep = "\\s+",
    fill = "right"
  )  |> 
  # separate column 4 into exch_rec and exch_dev
  separate(
    col = 4,
    into = c("exch_rec", "exch_dev"),
    sep = "\\s+",
    fill = "right"
  )  |> 
  # separate column 6 into exp_rec, exp_dev
  separate(
    col = 6,
    into = c("exp_rec", "exp_dev"),
    sep = "\\s+",
    fill = "right"
  )  |> 
  # separate column 8 into exp_exch_rec and exp_exch_dev
  separate(
    col = 8,
    into = c("exp_exch_rec", "exp_exch_dev"),
    sep = "\\s+",
    fill = "right"
  )  |> 
  rename(
    "abs_rec" = `7`,
    "abs_dev" = `8`
  )

county_sep24_table_3201b <- read_csv("Govt Spending/sep2024/County_sep24_table_647.csv")
names(county_sep24_table_3201b) <- names_department

county_sep24_table_3201 <- rbind(county_sep24_table_3201a, county_sep24_table_3201b)

rm(county_sep24_table_3201a)
rm(county_sep24_table_3201b)

muranga_department_sep24 <- bind_rows(
  #merge_rows(county_sep24_table_3201, 1:3), #header
  merge_rows(county_sep24_table_3201, 4:6), # governor
  merge_rows(county_sep24_table_3201, 7:8), # finance
  merge_rows(county_sep24_table_3201, 9:10), # agriculture
  #merge_rows(county_sep24_table_3201, 13:14), # header
  county_sep24_table_3201  |> slice(15:26) # table 2
)  |> 
rename_and_clean()

rm(county_sep24_table_3201)

### Program
county_sep24_table_3202a <- read_csv("Govt Spending/sep2024/County_sep24_table_648.csv")
county_sep24_table_3202b <- read_csv("Govt Spending/sep2024/County_sep24_table_650.csv")
county_sep24_table_3202c <- read_csv("Govt Spending/sep2024/County_sep24_table_652.csv")
county_sep24_table_3202d <- read_csv("Govt Spending/sep2024/County_sep24_table_653.csv")
county_sep24_table_3202e <- read_csv("Govt Spending/sep2024/County_sep24_table_655.csv")
county_sep24_table_3202f <- read_csv("Govt Spending/sep2024/County_sep24_table_657.csv")

## Nairobi ----
county_sep24_table_3203 <- read_csv("Govt Spending/sep2024/County_sep24_table_659.csv")

county_sep24_table_3204 <- read_csv("Govt Spending/sep2024/County_sep24_table_661.csv")

county_sep24_table_3205 <- read_csv("Govt Spending/sep2024/County_sep24_table_662.csv")

county_sep24_table_3206a <- read_csv("Govt Spending/sep2024/County_sep24_table_665.csv")
county_sep24_table_3206b <- read_csv("Govt Spending/sep2024/County_sep24_table_667.csv")

county_sep24_table_3207 <- read_csv("Govt Spending/sep2024/County_sep24_table_668.csv")

### Department 
county_sep24_table_3208 <- read_csv("Govt Spending/sep2024/County_sep24_table_670.csv")  |> 
  select(-`8`)  |> 
  process_county_data()

nairobi_department_sep24 <- bind_rows(
  #merge_rows(county_sep24_table_3208, 1:3), # header
  county_sep24_table_3208  |>  slice(4), # cpsb
  merge_rows(county_sep24_table_3208, 5:7), # finance
  county_sep24_table_3208  |>  slice(8), # psm
  merge_rows(county_sep24_table_3208, 9:12), # agriculture
  merge_rows(county_sep24_table_3208, 13:16), # water
  merge_rows(county_sep24_table_3208, 17:19), # ward
  county_sep24_table_3208  |>  slice(20), # emergency fund
  merge_rows(county_sep24_table_3208, 21:22), # boroughs
  county_sep24_table_3208  |> slice(23), # county attorney
  merge_rows(county_sep24_table_3208, 24:25), # innovation
  merge_rows(county_sep24_table_3208, 26:27), # health
  merge_rows(county_sep24_table_3208, 28:30), # built environment
  merge_rows(county_sep24_table_3208, 31:32), # works
  merge_rows(county_sep24_table_3208, 33:35), # talent
  merge_rows(county_sep24_table_3208, 36:37), # business
  merge_rows(county_sep24_table_3208, 38:40), # inclusivity
  merge_rows(county_sep24_table_3208, 41:42), # nra
  merge_rows(county_sep24_table_3208, 43:44), # liquor
  county_sep24_table_3208  |>  slice(45:46) # total
)  |> 
rename_and_clean()

rm(county_sep24_table_3208)

### Program 
county_sep24_table_3209a <- read_csv("Govt Spending/sep2024/County_sep24_table_671.csv")
county_sep24_table_3209b <- read_csv("Govt Spending/sep2024/County_sep24_table_672.csv")
county_sep24_table_3209c <- read_csv("Govt Spending/sep2024/County_sep24_table_674.csv")
county_sep24_table_3209d <- read_csv("Govt Spending/sep2024/County_sep24_table_676.csv")
county_sep24_table_3209e <- read_csv("Govt Spending/sep2024/County_sep24_table_678.csv")
county_sep24_table_3209f <- read_csv("Govt Spending/sep2024/County_sep24_table_680.csv")

## Nakuru ----

county_sep24_table_3210a <- read_csv("Govt Spending/sep2024/County_sep24_table_683.csv")
county_sep24_table_3210b <- read_csv("Govt Spending/sep2024/County_sep24_table_684.csv")

county_sep24_table_3211 <- read_csv("Govt Spending/sep2024/County_sep24_table_685.csv")

county_sep24_table_3212 <- read_csv("Govt Spending/sep2024/County_sep24_table_686.csv")

county_sep24_table_3213a <- read_csv("Govt Spending/sep2024/County_sep24_table_687.csv")
county_sep24_table_3213b <- read_csv("Govt Spending/sep2024/County_sep24_table_688.csv")

county_sep24_table_3214 <- read_csv("Govt Spending/sep2024/County_sep24_table_689.csv")

### Department
county_sep24_table_3215a <- read_csv("Govt Spending/sep2024/County_sep24_table_690.csv")  |> 
  separate(
    col = 2,
    into = c("budget_rec", "budget_dev", "exch_rec", "exch_dev"),
    sep = "\\s+",
    fill = "right"
  )  |>
  mutate(
    `2` = remove_na(paste(`2`, `3`)),
    `3` = NULL
  )   |> 
  separate(
    col = 6,
    into = c("exp_rec", "exp_dev"),
    sep = "\\s+",
    fill = "right"
  )  |> 
  separate(
    col = 8,
    into = c("exp_exch_rec", "exp_exch_dev"),
    sep = "\\s+",
    fill = "right"
  )  |> 
  separate(
    col = 10,
    into = c("abs_rec", "abs_dev"),
    sep = "\\s+",
    fill = "right"
    )

county_sep24_table_3215b <- read_csv("Govt Spending/sep2024/County_sep24_table_691.csv")
names(county_sep24_table_3215b) <- names_department

county_sep24_table_3215 <- rbind(county_sep24_table_3215a, county_sep24_table_3215b)

rm(county_sep24_table_3215a)
rm(county_sep24_table_3215b)

nakuru_department_sep24 <- bind_rows(
  #merge_rows(county_sep24_table_3215, 1:3), # header
  county_sep24_table_3215  |>  slice(4:5), # assembly + treasury
  merge_rows(county_sep24_table_3215, 6:8), # public service
  #merge_rows(county_sep24_table_3215, 11:12), # header
  county_sep24_table_3215  |> slice(13:28) # table2
)  |> 
rename_and_clean()

rm(county_sep24_table_3215)

### Program
county_sep24_table_3216a <- read_csv("Govt Spending/sep2024/County_sep24_table_692.csv")
county_sep24_table_3216b <- read_csv("Govt Spending/sep2024/County_sep24_table_694.csv")
county_sep24_table_3216c <- read_csv("Govt Spending/sep2024/County_sep24_table_696.csv")
county_sep24_table_3216d <- read_csv("Govt Spending/sep2024/County_sep24_table_698.csv")
county_sep24_table_3216e <- read_csv("Govt Spending/sep2024/County_sep24_table_700.csv")
county_sep24_table_3216f <- read_csv("Govt Spending/sep2024/County_sep24_table_702.csv")
county_sep24_table_3216g <- read_csv("Govt Spending/sep2024/County_sep24_table_703.csv")
county_sep24_table_3216h <- read_csv("Govt Spending/sep2024/County_sep24_table_705.csv")

## Nandi ----
county_sep24_table_3217a <- read_csv("Govt Spending/sep2024/County_sep24_table_707.csv")
county_sep24_table_3217b <- read_csv("Govt Spending/sep2024/County_sep24_table_709.csv")

county_sep24_table_3218 <- read_csv("Govt Spending/sep2024/County_sep24_table_710.csv")

county_sep24_table_3219 <- read_csv("Govt Spending/sep2024/County_sep24_table_711.csv")

county_sep24_table_3220 <- read_csv("Govt Spending/sep2024/County_sep24_table_712.csv")

county_sep24_table_3221a <- read_csv("Govt Spending/sep2024/County_sep24_table_713.csv")
county_sep24_table_3221b <- read_csv("Govt Spending/sep2024/County_sep24_table_714.csv")

### Department 
county_sep24_table_3222 <- read_csv("Govt Spending/sep2024/County_sep24_table_715.csv")  |> 
  process_county_data()

nandi_department_sep24 <- bind_rows(
  #merge_rows(county_sep24_table_3222, 1:3), # header
  county_sep24_table_3222  |> slice(4), # executive
  merge_rows(county_sep24_table_3222, 5:6), # finance
  merge_rows(county_sep24_table_3222, 7:8), # public service
  county_sep24_table_3222  |> slice(9), # health
  merge_rows(county_sep24_table_3222, 10:11), # agriculture
  merge_rows(county_sep24_table_3222, 12:14), # gender
  merge_rows(county_sep24_table_3222, 15:16), # education
  merge_rows(county_sep24_table_3222, 17:20), # lands
  merge_rows(county_sep24_table_3222, 21:23), # transport
  merge_rows(county_sep24_table_3222, 24:26), # environment
  county_sep24_table_3222  |> slice(27:31), # the rest
)  |> 
rename_and_clean()

rm(county_sep24_table_3222)

### Program 
county_sep24_table_3223a <- read_csv("Govt Spending/sep2024/County_sep24_table_716.csv")
county_sep24_table_3223b <- read_csv("Govt Spending/sep2024/County_sep24_table_717.csv")

## Narok ----
county_sep24_table_3224 <- read_csv("Govt Spending/sep2024/County_sep24_table_719.csv")

county_sep24_table_3225 <- read_csv("Govt Spending/sep2024/County_sep24_table_720.csv")

county_sep24_table_3226 <- read_csv("Govt Spending/sep2024/County_sep24_table_721.csv")

county_sep24_table_3227 <- read_csv("Govt Spending/sep2024/County_sep24_table_722.csv")

county_sep24_table_3228 <- read_csv("Govt Spending/sep2024/County_sep24_table_723.csv")

### Department 
county_sep24_table_3229 <- read_csv("Govt Spending/sep2024/County_sep24_table_724.csv")  |> 
  separate(
    col = 2,
    into = c("budget_rec", "budget_dev", "exch_rec", "exch_dev"),
    sep = "\\s+",
    fill = "right"
  )  |> 
  separate(
    col = 6,
    into = c("exp_rec", "exp_dev"),
    sep = "\\s+",
    fill = "right"
  )  |> 
  separate(
    col = 8,
    into = c("exp_exch_rec", "exp_exch_dev"),
    sep = "\\s+",
    fill = "right"
  )  |> 
  separate(
    col = 10,
    into = c("abs_rec", "abs_dev"),
    sep = "\\s+",
    fill = "right"
  )

narok_department_sep24 <- bind_rows(
  #merge_rows(county_sep24_table_3229, 1:3), # header
  county_sep24_table_3229  |> slice(4:5), # executive + assembly
  merge_rows(county_sep24_table_3229, 6:7), # finance
  merge_rows(county_sep24_table_3229, 8:9), # transport
  merge_rows(county_sep24_table_3229, 10:12), # education
  merge_rows(county_sep24_table_3229, 13:16), # environment
  county_sep24_table_3229  |> slice(17), # psb
  merge_rows(county_sep24_table_3229, 18:19), # agriculture
  county_sep24_table_3229  |> slice(20), # health
  merge_rows(county_sep24_table_3229, 21:23), # lands
  county_sep24_table_3229  |> slice(24), # ict
  merge_rows(county_sep24_table_3229, 25:27), # admin
  merge_rows(county_sep24_table_3229, 28:30), # trade
  county_sep24_table_3229  |> slice(31:32), # attorney + total
)  |> 
rename_and_clean()

rm(county_sep24_table_3229)

### Program 
county_sep24_table_3230a <- read_csv("Govt Spending/sep2024/County_sep24_table_725.csv")
county_sep24_table_3230b <- read_csv("Govt Spending/sep2024/County_sep24_table_726.csv")
county_sep24_table_3230c <- read_csv("Govt Spending/sep2024/County_sep24_table_728.csv")

## Nyamira ----
county_sep24_table_3231 <- read_csv("Govt Spending/sep2024/County_sep24_table_731.csv")

county_sep24_table_3232 <- read_csv("Govt Spending/sep2024/County_sep24_table_732.csv")

county_sep24_table_3233 <- read_csv("Govt Spending/sep2024/County_sep24_table_733.csv")

county_sep24_table_3234a <- read_csv("Govt Spending/sep2024/County_sep24_table_734.csv")
county_sep24_table_3234b <- read_csv("Govt Spending/sep2024/County_sep24_table_735.csv")

### Department
county_sep24_table_3235a <- read_csv("Govt Spending/sep2024/County_sep24_table_736.csv")  |> 
  mutate(
    `1` = remove_na(paste(`1`, `2`)),
    `2` = NULL,
    `1` = ifelse(`0` == "Agriculture - Crop Management", "69.47 - 59.77 -",`1`),
    `6` = NULL
  )  |> 
  separate(
    col = 2,
    into = c("budget_rec", "budget_dev", "exch_rec", "exch_dev"),
    sep = "\\s+",
    fill = "right"
  )  |> 
  separate(
    col = 6,
    into = c("exp_rec", "exp_dev"),
    sep = "\\s+",
    fill = "right"
  )  |> 
  separate(
    col = 8,
    into = c("exp_exch_rec", "exp_exch_dev"),
    sep = "\\s+",
    fill = "right"
  )  |> 
  separate(
    col = 10,
    into = c("abs_rec", "abs_dev"),
    sep = "\\s+",
    fill = "right"
  )

county_sep24_table_3235b <- read_csv("Govt Spending/sep2024/County_sep24_table_737.csv")
names(county_sep24_table_3235b) <- names_department

county_sep24_table_3235 <- rbind(county_sep24_table_3235a, county_sep24_table_3235b)

rm(county_sep24_table_3235a)
rm(county_sep24_table_3235b)

nyamira_department_sep24 <- bind_rows(
  #merge_rows(county_sep24_table_3235, 1:3), #header
  county_sep24_table_3235  |> slice(4:21), #table 1
  #merge_rows(county_sep24_table_3235, 24:25), #header
  county_sep24_table_3235  |>  slice(26) #total
)  |> 
rename_and_clean()

rm(county_sep24_table_3235)

### Program
county_sep24_table_3236a <- read_csv("Govt Spending/sep2024/County_sep24_table_738.csv")
county_sep24_table_3236b <- read_csv("Govt Spending/sep2024/County_sep24_table_740.csv")
county_sep24_table_3236c <- read_csv("Govt Spending/sep2024/County_sep24_table_742.csv")
county_sep24_table_3236d <- read_csv("Govt Spending/sep2024/County_sep24_table_744.csv")
county_sep24_table_3236e <- read_csv("Govt Spending/sep2024/County_sep24_table_746.csv")

## Nyandarua ----

county_sep24_table_3237a <- read_csv("Govt Spending/sep2024/County_sep24_table_749.csv")
county_sep24_table_3237b <- read_csv("Govt Spending/sep2024/County_sep24_table_750.csv")

county_sep24_table_3238 <- read_csv("Govt Spending/sep2024/County_sep24_table_751.csv")

county_sep24_table_3239 <- read_csv("Govt Spending/sep2024/County_sep24_table_752.csv")

county_sep24_table_3240 <- read_csv("Govt Spending/sep2024/County_sep24_table_753.csv")

county_sep24_table_3241 <- read_csv("Govt Spending/sep2024/County_sep24_table_754.csv")

### Department
county_sep24_table_3242 <- read_csv("Govt Spending/sep2024/County_sep24_table_755.csv")  |> 
  separate(
    col = 2,
    into = c("budget_rec", "budget_dev"),
    sep = "\\s+",
    fill = "right"
  )  |> 
  separate(
    col = 4,
    into = c("exch_rec", "exch_dev"),
    sep = "\\s+",
    fill = "right"
  )  |> 
  separate(
    col = 6,
    into = c("exp_rec", "exp_dev"),
    sep = "\\s+",
    fill = "right"
  )  |> 
  separate(
    col = 8,
    into = c("abs_rec", "abs_dev"),
    sep = "\\s+",
    fill = "right"
  )

nyandarua_department_sep24 <- bind_rows(
  #merge_rows(county_sep24_table_3242, 1:3), # header
  county_sep24_table_3242  |> slice(4), # governor
  merge_rows(county_sep24_table_3242, 5:6), # secretary
  merge_rows(county_sep24_table_3242, 7:8), # attorney
  merge_rows(county_sep24_table_3242, 9:10), # psa
  county_sep24_table_3242  |>  slice(11), # cpsb
  merge_rows(county_sep24_table_3242, 12:13), # finance
  county_sep24_table_3242  |>  slice(14), # health
  merge_rows(county_sep24_table_3242, 15:17), # education
  merge_rows(county_sep24_table_3242, 18:20), # tourism
  merge_rows(county_sep24_table_3242, 21:22), # youth
  merge_rows(county_sep24_table_3242, 23:24), # environment
  merge_rows(county_sep24_table_3242, 25:26), # lands
  county_sep24_table_3242  |>  slice(29:31), # municipalities
  merge_rows(county_sep24_table_3242, 32:33), # agriculture
  county_sep24_table_3242  |>  slice(34:35) # total
)  |> 
rename_and_clean()  |> 
rowwise()  |> 
mutate(
  exp_exch_rec = round((exp_rec/exch_rec), 4) * 100,
  exp_exch_dev = round((exp_dev/exch_dev ), 4) * 100,
)  |> 
ungroup()

rm(county_sep24_table_3242)

### Program
county_sep24_table_3243a <- read_csv("Govt Spending/sep2024/County_sep24_table_756.csv")
county_sep24_table_3243b <- read_csv("Govt Spending/sep2024/County_sep24_table_757.csv")
county_sep24_table_3243c <- read_csv("Govt Spending/sep2024/County_sep24_table_759.csv")
county_sep24_table_3243d <- read_csv("Govt Spending/sep2024/County_sep24_table_761.csv")

## Nyeri ----
county_sep24_table_3244a <- read_csv("Govt Spending/sep2024/County_sep24_table_764.csv")
county_sep24_table_3244b <- read_csv("Govt Spending/sep2024/County_sep24_table_766.csv")

county_sep24_table_3245 <- read_csv("Govt Spending/sep2024/County_sep24_table_767.csv")

county_sep24_table_3246 <- read_csv("Govt Spending/sep2024/County_sep24_table_768.csv")

county_sep24_table_3247a <- read_csv("Govt Spending/sep2024/County_sep24_table_771.csv")
county_sep24_table_3247b <- read_csv("Govt Spending/sep2024/County_sep24_table_772.csv")

county_sep24_table_3248 <- read_csv("Govt Spending/sep2024/County_sep24_table_774.csv")

### Department 
county_sep24_table_3249a <- read_csv("Govt Spending/sep2024/County_sep24_table_775.csv")  |> 
mutate(
  `5` = paste(`5`, `6`),
  `6` = NULL
)  |> 
process_county_data()
county_sep24_table_3249b <- read_csv("Govt Spending/sep2024/County_sep24_table_776.csv")
names(county_sep24_table_3249b) <- names_department

county_sep24_table_3249 <- rbind(county_sep24_table_3249a, county_sep24_table_3249b)

rm(county_sep24_table_3249a)
rm(county_sep24_table_3249b)

nyeri_department_sep24 <- bind_rows(
  #merge_rows(county_sep24_table_3249, 1:3), # header
  merge_rows(county_sep24_table_3249, 4:5), # governor
  merge_rows(county_sep24_table_3249, 6:7), # secretary
  merge_rows(county_sep24_table_3249, 8:9), # finance
  merge_rows(county_sep24_table_3249, 10:12), # lands
  merge_rows(county_sep24_table_3249, 13:14), # health
  #merge_rows(county_sep24_table_3249, 15:16), # header
  county_sep24_table_3249  |> slice(17:27), # table 2
)  |> 
rename_and_clean()

rm(county_sep24_table_3249)

### Program 
county_sep24_table_3250a <- read_csv("Govt Spending/sep2024/County_sep24_table_777.csv")
county_sep24_table_3250b <- read_csv("Govt Spending/sep2024/County_sep24_table_779.csv")
county_sep24_table_3250c <- read_csv("Govt Spending/sep2024/County_sep24_table_781.csv")
county_sep24_table_3250d <- read_csv("Govt Spending/sep2024/County_sep24_table_783.csv")

## Samburu ----
county_sep24_table_3251a <- read_csv("Govt Spending/sep2024/County_sep24_table_786.csv")
county_sep24_table_3251b <- read_csv("Govt Spending/sep2024/County_sep24_table_788.csv")

county_sep24_table_3252 <- read_csv("Govt Spending/sep2024/County_sep24_table_789.csv")

county_sep24_table_3253 <- read_csv("Govt Spending/sep2024/County_sep24_table_790.csv")

county_sep24_table_3254a <- read_csv("Govt Spending/sep2024/County_sep24_table_791.csv")
county_sep24_table_3254b <- read_csv("Govt Spending/sep2024/County_sep24_table_792.csv")

county_sep24_table_3255a <- read_csv("Govt Spending/sep2024/County_sep24_table_793.csv")
county_sep24_table_3255b <- read_csv("Govt Spending/sep2024/County_sep24_table_795.csv")

### Department
county_sep24_table_3256 <- read_csv("Govt Spending/sep2024/County_sep24_table_796.csv")  |> 
mutate(
  `8` = remove_na(paste(`7`, `8`)),
  `7` = NULL
)  |> 
process_county_data()

samburu_department_sep24 <- bind_rows(
  #merge_rows(county_sep24_table_3256, 1:3), # header
  county_sep24_table_3256  |> slice(4), # county executive
  merge_rows(county_sep24_table_3256, 5:7), # finance
  merge_rows(county_sep24_table_3256, 8:13), # agriculture
  merge_rows(county_sep24_table_3256, 14:17), # water
  merge_rows(county_sep24_table_3256, 18:20), # education
  merge_rows(county_sep24_table_3256, 21:23), # health
  merge_rows(county_sep24_table_3256, 24:27), # lands
  merge_rows(county_sep24_table_3256, 28:29), # roads
  merge_rows(county_sep24_table_3256, 30:33), # tourism
  merge_rows(county_sep24_table_3256, 34:37), # culture
  county_sep24_table_3256  |>  slice(38:39) # assembly + total
)  |> 
rename_and_clean()

rm(county_sep24_table_3256)

### Program
county_sep24_table_3257a <- read_csv("Govt Spending/sep2024/County_sep24_table_797.csv")
county_sep24_table_3257b <- read_csv("Govt Spending/sep2024/County_sep24_table_798.csv")
county_sep24_table_3257c <- read_csv("Govt Spending/sep2024/County_sep24_table_800.csv")
county_sep24_table_3257d <- read_csv("Govt Spending/sep2024/County_sep24_table_802.csv")
county_sep24_table_3257e <- read_csv("Govt Spending/sep2024/County_sep24_table_804.csv")
county_sep24_table_3257f <- read_csv("Govt Spending/sep2024/County_sep24_table_806.csv")
county_sep24_table_3257g <- read_csv("Govt Spending/sep2024/County_sep24_table_808.csv")
county_sep24_table_3257h <- read_csv("Govt Spending/sep2024/County_sep24_table_810.csv")

## Siaya ----
county_sep24_table_3258 <- read_csv("Govt Spending/sep2024/County_sep24_table_812.csv")

county_sep24_table_3259 <- read_csv("Govt Spending/sep2024/County_sep24_table_813.csv")

county_sep24_table_3260 <- read_csv("Govt Spending/sep2024/County_sep24_table_814.csv")

county_sep24_table_3261a <- read_csv("Govt Spending/sep2024/County_sep24_table_816.csv")
county_sep24_table_3261b <- read_csv("Govt Spending/sep2024/County_sep24_table_818.csv")

county_sep24_table_3262 <- read_csv("Govt Spending/sep2024/County_sep24_table_819.csv")

### Department
county_sep24_table_3263a <- read_csv("Govt Spending/sep2024/County_sep24_table_820.csv")  |> 
  process_county_data()

county_sep24_table_3263b <- read_csv("Govt Spending/sep2024/County_sep24_table_821.csv")  |> 
mutate(
  `0` = ifelse(row_number() == 10, "Total", `0`)
)
names(county_sep24_table_3263b) <- names_department

county_sep24_table_3263 <- rbind(county_sep24_table_3263a, county_sep24_table_3263b)

rm(county_sep24_table_3263a)
rm(county_sep24_table_3263b)

siaya_department_sep24 <- bind_rows(
  #merge_rows(county_sep24_table_3263, 1:3), # header
  county_sep24_table_3263  |> slice(4), # assembly
  merge_rows(county_sep24_table_3263, 5:6), # finance
  county_sep24_table_3263  |>  slice(7:8), # health + admin
  #merge_rows(county_sep24_table_3263, 9:10), # header
  county_sep24_table_3263  |>  slice(11:18)
)  |> 
rename_and_clean()

rm(county_sep24_table_3263)

### Program
county_sep24_table_3264a <- read_csv("Govt Spending/sep2024/County_sep24_table_822.csv")
county_sep24_table_3264b <- read_csv("Govt Spending/sep2024/County_sep24_table_823.csv")
county_sep24_table_3264c <- read_csv("Govt Spending/sep2024/County_sep24_table_825.csv")
county_sep24_table_3264d <- read_csv("Govt Spending/sep2024/County_sep24_table_827.csv")

## Taita Taveta ----
county_sep24_table_3265a <- read_csv("Govt Spending/sep2024/County_sep24_table_829.csv")
county_sep24_table_3265b <- read_csv("Govt Spending/sep2024/County_sep24_table_830.csv")

county_sep24_table_3266 <- read_csv("Govt Spending/sep2024/County_sep24_table_831.csv")

county_sep24_table_3267 <- read_csv("Govt Spending/sep2024/County_sep24_table_835.csv")

county_sep24_table_3268 <- read_csv("Govt Spending/sep2024/County_sep24_table_836.csv")

county_sep24_table_3269 <- read_csv("Govt Spending/sep2024/County_sep24_table_837.csv")

### Department
county_sep24_table_3270 <- read_csv("Govt Spending/sep2024/County_sep24_table_838.csv")  |> 
  process_county_data()

taita_taveta_department_sep24 <- bind_rows(
  #merge_rows(county_sep24_table_3270, 1:3), # header
  county_sep24_table_3270  |> slice(4), # assembly
  merge_rows(county_sep24_table_3270, 5:6), # psa
  merge_rows(county_sep24_table_3270, 7:8), # governor
  merge_rows(county_sep24_table_3270, 9:10), # finance
  merge_rows(county_sep24_table_3270, 11:12), # agriculture
  county_sep24_table_3270  |> slice(13:15), # water + education + health
  merge_rows(county_sep24_table_3270, 16:17), # trade
  county_sep24_table_3270  |> slice(18), # cpsb
  merge_rows(county_sep24_table_3270, 19:20), # infrastructure
  merge_rows(county_sep24_table_3270, 21:22), # lands
  merge_rows(county_sep24_table_3270, 23:24), # youth
  county_sep24_table_3270  |>  slice(25) # total
)  |> 
rename_and_clean()

rm(county_sep24_table_3270)

### Program
county_sep24_table_3271a <- read_csv("Govt Spending/sep2024/County_sep24_table_839.csv")
county_sep24_table_3271b <- read_csv("Govt Spending/sep2024/County_sep24_table_840.csv")
county_sep24_table_3271c <- read_csv("Govt Spending/sep2024/County_sep24_table_842.csv")
county_sep24_table_3271d <- read_csv("Govt Spending/sep2024/County_sep24_table_844.csv")
county_sep24_table_3271e <- read_csv("Govt Spending/sep2024/County_sep24_table_846.csv")
county_sep24_table_3271f <- read_csv("Govt Spending/sep2024/County_sep24_table_848.csv")
county_sep24_table_3271g <- read_csv("Govt Spending/sep2024/County_sep24_table_850.csv")

## Tana River ----
county_sep24_table_3272 <- read_csv("Govt Spending/sep2024/County_sep24_table_852.csv")

county_sep24_table_3273 <- read_csv("Govt Spending/sep2024/County_sep24_table_860.csv")

county_sep24_table_3274 <- read_csv("Govt Spending/sep2024/County_sep24_table_861.csv")

county_sep24_table_3275 <- read_csv("Govt Spending/sep2024/County_sep24_table_863.csv")

county_sep24_table_3276 <- read_csv("Govt Spending/sep2024/County_sep24_table_865.csv")

### Department
county_sep24_table_3277a <- read_csv("Govt Spending/sep2024/County_sep24_table_866.csv")  |> 
  separate(
    col = 2,
    into = c("budget_rec", "budget_dev"),
    sep = "\\s+",
    fill = "right"
  )  |> 
  separate(
    col = 8,
    into = c("exch_exp_rec", "exch_exp_dev"),
    sep = "\\s+",
    fill = "right"
  )  |> 
  separate(
    col = 10,
    into = c("abs_rec", "abs_dev"),
    sep = "\\s+",
    fill = "right"
  )
names(county_sep24_table_3277a) <- names_department

county_sep24_table_3277b <- read_csv("Govt Spending/sep2024/County_sep24_table_867.csv")
names(county_sep24_table_3277b) <- names_department

county_sep24_table_3277 <- rbind(county_sep24_table_3277a, county_sep24_table_3277b)
county_sep24_table_3277$exp_dev <- as.numeric(county_sep24_table_3277$exp_dev)

rm(county_sep24_table_3277a)
rm(county_sep24_table_3277b)

tana_river_department_sep24 <- bind_rows(
  #merge_rows(county_sep24_table_3277, 1:3), # header
  county_sep24_table_3277  |>  slice(4), # assembly
  merge_rows(county_sep24_table_3277, 5:6)  |>  mutate(exp_dev = as.numeric(exp_dev)), # governor
  county_sep24_table_3277  |> slice(7), # finance
  merge_rows(county_sep24_table_3277, 8:9) |>  mutate(exp_dev = as.numeric(exp_dev)), #cpsb
  merge_rows(county_sep24_table_3277, 10:12) |>  mutate(exp_dev = as.numeric(exp_dev)), # trade
  merge_rows(county_sep24_table_3277, 13:14) |>  mutate(exp_dev = as.numeric(exp_dev)), # agriculture
  merge_rows(county_sep24_table_3277, 15:17) |>  mutate(exp_dev = as.numeric(exp_dev)), # culture
  merge_rows(county_sep24_table_3277, 18:19) |>  mutate(exp_dev = as.numeric(exp_dev)), # education
  merge_rows(county_sep24_table_3277, 20:21) |>  mutate(exp_dev = as.numeric(exp_dev)), # health
  county_sep24_table_3277  |>  slice(22), # special
  merge_rows(county_sep24_table_3277, 23:25) |>  mutate(exp_dev = as.numeric(exp_dev)), # roads
  merge_rows(county_sep24_table_3277, 26:28) |>  mutate(exp_dev = as.numeric(exp_dev)), # water
  merge_rows(county_sep24_table_3277, 29:31) |>  mutate(exp_dev = as.numeric(exp_dev)), # psa
  merge_rows(county_sep24_table_3277, 32:33) |>  mutate(exp_dev = as.numeric(exp_dev)), # lands
  county_sep24_table_3277  |> slice(34:35) # hola + total
)  |> 
rename_and_clean()

rm(county_sep24_table_3277)

### Program
county_sep24_table_3278a <- read_csv("Govt Spending/sep2024/County_sep24_table_868.csv")
county_sep24_table_3278b <- read_csv("Govt Spending/sep2024/County_sep24_table_869.csv")
county_sep24_table_3278c <- read_csv("Govt Spending/sep2024/County_sep24_table_871.csv")
county_sep24_table_3278d <- read_csv("Govt Spending/sep2024/County_sep24_table_873.csv")
county_sep24_table_3278e <- read_csv("Govt Spending/sep2024/County_sep24_table_875.csv")
county_sep24_table_3278f <- read_csv("Govt Spending/sep2024/County_sep24_table_877.csv")

## Tharaka Nithi ---- 
county_sep24_table_3279a <- read_csv("Govt Spending/sep2024/County_sep24_table_879.csv")
county_sep24_table_3279b <- read_csv("Govt Spending/sep2024/County_sep24_table_881.csv")

county_sep24_table_3280 <- read_csv("Govt Spending/sep2024/County_sep24_table_882.csv")

county_sep24_table_3281 <- read_csv("Govt Spending/sep2024/County_sep24_table_883.csv")

county_sep24_table_3282 <- read_csv("Govt Spending/sep2024/County_sep24_table_886.csv")

county_sep24_table_3283a <- read_csv("Govt Spending/sep2024/County_sep24_table_887.csv")
county_sep24_table_3283b <- read_csv("Govt Spending/sep2024/County_sep24_table_888.csv")

### Department
county_sep24_table_3284a <- read_csv("Govt Spending/sep2024/County_sep24_table_889.csv")  |> 
mutate(
  `9` = paste(`9`, `10`),
  `10` = NULL
)  |> 
process_county_data()

county_sep24_table_3284b <- read_csv("Govt Spending/sep2024/County_sep24_table_890.csv")
names(county_sep24_table_3284b) <- names_department

county_sep24_table_3284 <- rbind(county_sep24_table_3284a, county_sep24_table_3284b)

rm(county_sep24_table_3284a)
rm(county_sep24_table_3284b)

tharaka_nithi_department_sep24 <- bind_rows(
  #merge_rows(county_sep24_table_3284, 1:3), # header
  merge_rows(county_sep24_table_3284, 4:5), # governor
  merge_rows(county_sep24_table_3284, 6:7), # roads
  county_sep24_table_3284  |>  slice(8), # health
  merge_rows(county_sep24_table_3284, 9:11), # agriculture
  merge_rows(county_sep24_table_3284, 12:13), # admin
  merge_rows(county_sep24_table_3284, 14:15), # education
  merge_rows(county_sep24_table_3284, 16:17), # finance
  merge_rows(county_sep24_table_3284, 18:19), # tourism
  county_sep24_table_3284  |>  slice(20), # assembly
  merge_rows(county_sep24_table_3284, 21:22), # water
  merge_rows(county_sep24_table_3284, 23:24), # cpsb
  county_sep24_table_3284  |> slice(25:27), # health + youth + culture
  merge_rows(county_sep24_table_3284, 28:29), # revenue
  #merge_rows(county_sep24_table_3284, 30:31), # header
  county_sep24_table_3284  |> slice(32:36) # table 2
)  |> 
rename_and_clean()

rm(county_sep24_table_3284)

### Program
county_sep24_table_3285a <- read_csv("Govt Spending/sep2024/County_sep24_table_891.csv")
county_sep24_table_3285b <- read_csv("Govt Spending/sep2024/County_sep24_table_893.csv")
county_sep24_table_3285c <- read_csv("Govt Spending/sep2024/County_sep24_table_895.csv")
county_sep24_table_3285d <- read_csv("Govt Spending/sep2024/County_sep24_table_896.csv")

## Trans Nzoia ---- 
county_sep24_table_3286a <- read_csv("Govt Spending/sep2024/County_sep24_table_897.csv")
county_sep24_table_3286b <- read_csv("Govt Spending/sep2024/County_sep24_table_898.csv")

county_sep24_table_3287 <- read_csv("Govt Spending/sep2024/County_sep24_table_899.csv")

county_sep24_table_3288 <- read_csv("Govt Spending/sep2024/County_sep24_table_900.csv")

county_sep24_table_3289 <- read_csv("Govt Spending/sep2024/County_sep24_table_903.csv")

county_sep24_table_3290 <- read_csv("Govt Spending/sep2024/County_sep24_table_904.csv")

### Department
county_sep24_table_3291 <- read_csv("Govt Spending/sep2024/County_sep24_table_905.csv")  |> 
  process_county_data()

trans_nzoia_department_sep24 <- bind_rows(
  #merge_rows(county_sep24_table_3291, 1:3), # header
  county_sep24_table_3291  |>  slice(4:5), # agri + livestock
  merge_rows(county_sep24_table_3291, 6:7), # trade
  merge_rows(county_sep24_table_3291, 8:9), # environment
  merge_rows(county_sep24_table_3291, 10:12), # works
  county_sep24_table_3291  |> slice(13:14), # health + lands
  merge_rows(county_sep24_table_3291, 15:16), # kitale
  merge_rows(county_sep24_table_3291, 17:18), # gender
  county_sep24_table_3291  |> slice(19:23), 
  merge_rows(county_sep24_table_3291, 24:25), # planning
  county_sep24_table_3291  |> slice(26:28)
)  |> 
mutate(
  `0` = ifelse(is.na(`0`), "Total", `0`)
) |> 
rename_and_clean()  

rm(county_sep24_table_3291)

### Program
county_sep24_table_3292a <- read_csv("Govt Spending/sep2024/County_sep24_table_906.csv")
county_sep24_table_3292b <- read_csv("Govt Spending/sep2024/County_sep24_table_907.csv")
county_sep24_table_3292c <- read_csv("Govt Spending/sep2024/County_sep24_table_909.csv")
county_sep24_table_3292d <- read_csv("Govt Spending/sep2024/County_sep24_table_911.csv")

## Turkana ----
county_sep24_table_3293 <- read_csv("Govt Spending/sep2024/County_sep24_table_913.csv")

county_sep24_table_3294 <- read_csv("Govt Spending/sep2024/County_sep24_table_925.csv")

county_sep24_table_3295a <- read_csv("Govt Spending/sep2024/County_sep24_table_926.csv")
county_sep24_table_3295b <- read_csv("Govt Spending/sep2024/County_sep24_table_927.csv")

county_sep24_table_3296 <- read_csv("Govt Spending/sep2024/County_sep24_table_928.csv")

county_sep24_table_3297 <- read_csv("Govt Spending/sep2024/County_sep24_table_929.csv")

### Department
county_sep24_table_3298 <- read_csv("Govt Spending/sep2024/County_sep24_table_930.csv")  |> 
  mutate(
    `5` = NULL
  )  |> 
  process_county_data()

turkana_department_sep24 <- bind_rows(
  #merge_rows(county_sep24_table_3298, 1:3), # header
  county_sep24_table_3298  |> slice(4), # governor
  merge_rows(county_sep24_table_3298, 5:6), # dep. governor
  merge_rows(county_sep24_table_3298, 7:8), # finance
  merge_rows(county_sep24_table_3298, 9:11), # water
  merge_rows(county_sep24_table_3298, 12:13), # health
  merge_rows(county_sep24_table_3298, 14:15), # trade
  merge_rows(county_sep24_table_3298, 16:17), # education
  merge_rows(county_sep24_table_3298, 18:21), # psa
  merge_rows(county_sep24_table_3298, 22:24), # infra
  merge_rows(county_sep24_table_3298, 25:27), # agri
  merge_rows(county_sep24_table_3298, 28:29), # tourism
  merge_rows(county_sep24_table_3298, 30:32), # lands
  merge_rows(county_sep24_table_3298, 33:34), # assembly
  merge_rows(county_sep24_table_3298, 35:36), # cpsb
  merge_rows(county_sep24_table_3298, 37:38), # attorney
  merge_rows(county_sep24_table_3298, 39:40), # lodwar
  county_sep24_table_3298  |> slice(41:42) # total + kakuma
)  |> 
rename_and_clean()

rm(county_sep24_table_3298)

### Program
county_sep24_table_3299a <- read_csv("Govt Spending/sep2024/County_sep24_table_931.csv")
county_sep24_table_3299b <- read_csv("Govt Spending/sep2024/County_sep24_table_932.csv")
county_sep24_table_3299c <- read_csv("Govt Spending/sep2024/County_sep24_table_933.csv")
county_sep24_table_3299d <- read_csv("Govt Spending/sep2024/County_sep24_table_935.csv")
county_sep24_table_3299e <- read_csv("Govt Spending/sep2024/County_sep24_table_937.csv")
county_sep24_table_3299f <- read_csv("Govt Spending/sep2024/County_sep24_table_939.csv")
county_sep24_table_3299g <- read_csv("Govt Spending/sep2024/County_sep24_table_941.csv")
county_sep24_table_3299h <- read_csv("Govt Spending/sep2024/County_sep24_table_943.csv")

## Uasin Gishu ----
county_sep24_table_3300 <- read_csv("Govt Spending/sep2024/County_sep24_table_946.csv")

county_sep24_table_3301 <- read_csv("Govt Spending/sep2024/County_sep24_table_948.csv")

county_sep24_table_3302a <- read_csv("Govt Spending/sep2024/County_sep24_table_949.csv")
county_sep24_table_3302b <- read_csv("Govt Spending/sep2024/County_sep24_table_950.csv")

county_sep24_table_3303 <- read_csv("Govt Spending/sep2024/County_sep24_table_951.csv")

county_sep24_table_3304 <- read_csv("Govt Spending/sep2024/County_sep24_table_952.csv")

### Department
county_sep24_table_3305a <- read_csv("Govt Spending/sep2024/County_sep24_table_953.csv")
county_sep24_table_3305b <- read_csv("Govt Spending/sep2024/County_sep24_table_955.csv")

county_sep24_table_3305 <- rbind(county_sep24_table_3305a, county_sep24_table_3305b)
names(county_sep24_table_3305) <- names_department

rm(county_sep24_table_3305a)
rm(county_sep24_table_3305b)

uasin_gishu_department_sep24 <- bind_rows(
  #merge_rows(county_sep24_table_3305, 1:2), # header
  county_sep24_table_3305  |> slice(3:6), # table 1
  #merge_rows(county_sep24_table_3305, 7:8), # header
  county_sep24_table_3305  |> slice(9:30) # table 2
)  |> 
rename_and_clean()

rm(county_sep24_table_3305)

### Program 
county_sep24_table_3306a <- read_csv("Govt Spending/sep2024/County_sep24_table_957.csv")
county_sep24_table_3306b <- read_csv("Govt Spending/sep2024/County_sep24_table_958.csv")
county_sep24_table_3306c <- read_csv("Govt Spending/sep2024/County_sep24_table_960.csv")
county_sep24_table_3306d <- read_csv("Govt Spending/sep2024/County_sep24_table_962.csv")
county_sep24_table_3306e <- read_csv("Govt Spending/sep2024/County_sep24_table_964.csv")
county_sep24_table_3306f <- read_csv("Govt Spending/sep2024/County_sep24_table_966.csv")
county_sep24_table_3306g <- read_csv("Govt Spending/sep2024/County_sep24_table_968.csv")
county_sep24_table_3306h <- read_csv("Govt Spending/sep2024/County_sep24_table_970.csv")

## Vihiga ----
county_sep24_table_3307a <- read_csv("Govt Spending/sep2024/County_sep24_table_971.csv")
county_sep24_table_3307b <- read_csv("Govt Spending/sep2024/County_sep24_table_973.csv")

county_sep24_table_3308 <- read_csv("Govt Spending/sep2024/County_sep24_table_988.csv")

county_sep24_table_3309 <- read_csv("Govt Spending/sep2024/County_sep24_table_989.csv")

county_sep24_table_3310 <- read_csv("Govt Spending/sep2024/County_sep24_table_991.csv")

county_sep24_table_3311 <- read_csv("Govt Spending/sep2024/County_sep24_table_993.csv")

### Department
county_sep24_table_3312 <- read_csv("Govt Spending/sep2024/County_sep24_table_994.csv")  |> 
  process_county_data()

vihiga_department_sep24 <- bind_rows(
  #merge_rows(county_sep24_table_3312, 1:3), # header
  county_sep24_table_3312  |> slice(4), # governor
  merge_rows(county_sep24_table_3312, 5:6), # finance
  merge_rows(county_sep24_table_3312, 7:8), # agriculture
  county_sep24_table_3312  |> slice(9), # health
  merge_rows(county_sep24_table_3312, 10:11), # education
  merge_rows(county_sep24_table_3312, 12:14), # gender
  merge_rows(county_sep24_table_3312, 15:16), # trade
  merge_rows(county_sep24_table_3312, 17:18), # cpsb
  merge_rows(county_sep24_table_3312, 19:21), # environment
  merge_rows(county_sep24_table_3312, 22:23), # transport
  merge_rows(county_sep24_table_3312, 24:26), # lands
  county_sep24_table_3312  |> slice(27), # assembly
  merge_rows(county_sep24_table_3312, 28:29), # admin
  county_sep24_table_3312  |> slice(30:31)
  ) |>
  rename_and_clean()

rm(county_sep24_table_3312)

### Program
county_sep24_table_3313a <- read_csv("Govt Spending/sep2024/County_sep24_table_995.csv")
county_sep24_table_3313b <- read_csv("Govt Spending/sep2024/County_sep24_table_996.csv")
county_sep24_table_3313c <- read_csv("Govt Spending/sep2024/County_sep24_table_998.csv")
county_sep24_table_3313d <- read_csv("Govt Spending/sep2024/County_sep24_table_1000.csv")

## Wajir ----
county_sep24_table_3314 <- read_csv("Govt Spending/sep2024/County_sep24_table_1003.csv")

county_sep24_table_3315 <- read_csv("Govt Spending/sep2024/County_sep24_table_1014.csv")

county_sep24_table_3316 <- read_csv("Govt Spending/sep2024/County_sep24_table_1015.csv")

county_sep24_table_3317 <- read_csv("Govt Spending/sep2024/County_sep24_table_1016.csv")

county_sep24_table_3318a <- read_csv("Govt Spending/sep2024/County_sep24_table_1017.csv")
county_sep24_table_3318b <- read_csv("Govt Spending/sep2024/County_sep24_table_1018.csv")

### Department
county_sep24_table_3319 <- read_csv("Govt Spending/sep2024/County_sep24_table_1019.csv")  |> 
  mutate(
    `2` = remove_na(paste(`2`, `3`)),
    `3` = NULL,
    `8` = remove_na(paste(`8`, `9`)),
    `9` = NULL
  )  |> 
  process_county_data()

wajir_department_sep24 <- bind_rows(
  #merge_rows(county_sep24_table_3319, 1:3), # header
  county_sep24_table_3319  |> slice(4:5), # assembly + executive
  merge_rows(county_sep24_table_3319, 6:7), # finance
  merge_rows(county_sep24_table_3319, 8:9), # roads
  merge_rows(county_sep24_table_3319, 10:11), # water
  merge_rows(county_sep24_table_3319, 12:13), # energy
  county_sep24_table_3319  |>  slice(14), # health
  merge_rows(county_sep24_table_3319, 15:16), # education
  merge_rows(county_sep24_table_3319, 17:18), # agriculture
  merge_rows(county_sep24_table_3319, 19:20), # ict
  county_sep24_table_3319  |>  slice(21), # lands
  merge_rows(county_sep24_table_3319, 22:23), # public service
  county_sep24_table_3319  |>  slice(24:27)
)  |> 
rename_and_clean()

rm(county_sep24_table_3319)

### Program
county_sep24_table_3320a <- read_csv("Govt Spending/sep2024/County_sep24_table_1020.csv")
county_sep24_table_3320b <- read_csv("Govt Spending/sep2024/County_sep24_table_1021.csv")
county_sep24_table_3320c <- read_csv("Govt Spending/sep2024/County_sep24_table_1023.csv")
county_sep24_table_3320d <- read_csv("Govt Spending/sep2024/County_sep24_table_1025.csv")

## West Pokot ----
county_sep24_table_3321a <- read_csv("Govt Spending/sep2024/County_sep24_table_1027.csv")
county_sep24_table_3321b <- read_csv("Govt Spending/sep2024/County_sep24_table_1029.csv")

county_sep24_table_3322 <- read_csv("Govt Spending/sep2024/County_sep24_table_1030.csv")

county_sep24_table_3323 <- read_csv("Govt Spending/sep2024/County_sep24_table_1031.csv")

county_sep24_table_3324a <- read_csv("Govt Spending/sep2024/County_sep24_table_1033.csv")
county_sep24_table_3324b <- read_csv("Govt Spending/sep2024/County_sep24_table_1035.csv")

county_sep24_table_3325 <- read_csv("Govt Spending/sep2024/County_sep24_table_1036.csv")

### Department
county_sep24_table_3326a <- read_csv("Govt Spending/sep2024/County_sep24_table_1037.csv")  |> 
  process_county_data()

county_sep24_table_3326b <- read_csv("Govt Spending/sep2024/County_sep24_table_1038.csv")
names(county_sep24_table_3326b) <- names_department

county_sep24_table_3326 <- rbind(county_sep24_table_3326a, county_sep24_table_3326b)

rm(county_sep24_table_3326a)
rm(county_sep24_table_3326b)

west_pokot_department_sep24 <- bind_rows(
  #merge_rows(county_sep24_table_3326, 1:4), # header
  county_sep24_table_3326  |>  slice(5), # governor
  #merge_rows(county_sep24_table_3326, 6:7), # header
  county_sep24_table_3326  |> slice(8:21) # table 2
)  |> 
rename_and_clean()

rm(county_sep24_table_3326)

### Program
county_sep24_table_3327a <- read_csv("Govt Spending/sep2024/County_sep24_table_1039.csv")
county_sep24_table_3327b <- read_csv("Govt Spending/sep2024/County_sep24_table_1041.csv")
county_sep24_table_3327c <- read_csv("Govt Spending/sep2024/County_sep24_table_1043.csv")
county_sep24_table_3327d <- read_csv("Govt Spending/sep2024/County_sep24_table_1045.csv")

## End ----

# Make a combined table for each county: department
department_sep24 <- county_vector |>
  map_dfr(~ {
    # Dynamically reference the data frame
    df <- get(paste0(.x, "_department_sep24"))
    # Add the county column
    df |> mutate(county = .x)
  })

write_csv(department_sep24, "Govt Spending/sep2024/clean/department.csv")

# Make a combined table for each county: program (accept gaps etc)

# Jun 2024 (3/4 of the year) ----
  
