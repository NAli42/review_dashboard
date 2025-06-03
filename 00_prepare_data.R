# 00_prepare_data.R - One-time script to prepare all data files
# Run this script once to convert your current data structure to the new format

library(tidyverse)
library(haven)
library(janitor)

# Create data directory if it doesn't exist
# Not needed for R projects - use raw_data folder

# 1. Load and process main dataset ----------------------------------------

# Load the raw CSV with new structure (includes title, authors, doi)
# Try new file first, fall back to old if not found
if (file.exists('raw_data/with_dois_names.csv')) {
  dataset <- read.csv('raw_data/with_dois_names.csv', 
                      sep = ";", 
                      quote = "\"",
                      stringsAsFactors = FALSE,
                      encoding = "UTF-8")
  cat("Loaded with_dois_names.csv\n")
} else if (file.exists('raw_data/dataset_v5.csv')) {
  dataset <- read.csv('raw_data/dataset_v5.csv', 
                      sep = ";", 
                      quote = "\"",
                      stringsAsFactors = FALSE,
                      encoding = "UTF-8")
  cat("Loaded dataset_v5.csv\n")
} else {
  stop("No dataset file found. Please ensure either 'with_dois_names.csv' or 'dataset_v5.csv' exists in raw_data/")
}
keep <- !is.na(names(dataset)) & nzchar(names(dataset))
dataset <- dataset[, keep]

# Check the actual number of columns
cat("Number of columns in dataset:", ncol(dataset), "\n")
cat("First few column names:\n")
print(head(names(dataset), 20))

# If the number of columns is not what we expect, show a warning
# Check the actual number of columns
cat("Number of columns in dataset:", ncol(dataset), "\n")
cat("First few column names:\n")
print(head(names(dataset), 20))

# If the number of columns is not what we expect, show a warning
if (ncol(dataset) == 60) {
  cat("\nDataset has 60 columns (new extended format).\n")
  # Column mapping for 60-column format
  # Based on the CSV structure: ID, Include?, Summarised, If not included - why?, Title, Year, Authors, DOI...
  col_names <- c(
    "id", "inclusion", "summarised", "exclusion_reason", "title", "year", 
    "authors", "doi", "paper_type", "poverty", "adversity", "country", 
    "sample_comp", "exposure_period", "exposure_period2", "design", 
    "sample_size", "sample_size_clean", "poverty_oper"
  )
  
  # Apply names to the first set of columns
  if (length(col_names) <= ncol(dataset)) {
    colnames(dataset)[1:length(col_names)] <- col_names
  }
  
  # For the remaining columns, we'll map them based on patterns
  remaining_start <- length(col_names) + 1
  if (remaining_start <= ncol(dataset)) {
    # These should be the outcome variables
    remaining_cols <- c(
      "early_life_adv", "support", "stress", "nutrition", "toxins", 
      "substance", "m_depression", "m_mental_health", "m_health", 
      "neigh_safety", "housing", "sleep", "preterm", "attachment", 
      "neglect", "parenting", "cognitive_enr", "micronutrients", 
      "breastfeeding", "immune", "placenta_brain", "gut_microbiome", 
      "hpa_axis", "synaptic", "epigenetics", "telomere", "polygenic_snps", 
      "outcome_beh", "outcome_neuro", "outcome_sga", "outcome_mobility"
    )
    
    # Apply remaining column names
    end_idx <- min(remaining_start + length(remaining_cols) - 1, ncol(dataset))
    if (end_idx >= remaining_start) {
      num_cols_to_assign <- end_idx - remaining_start + 1
      colnames(dataset)[remaining_start:end_idx] <- remaining_cols[1:num_cols_to_assign]
    }
  }
  
} else if (ncol(dataset) == 46) {
  cat("\nDataset has 46 columns (old format without title/authors/doi).\n")
  # Original column names without the new additions
  colnames(dataset) <- c(
    "id", "inclusion", "year", "paper_type", "poverty", "adversity", 
    "country", "sample_comp", "exposure_period", "design", "sample_size", 
    "poverty_oper", "early_life_adv", "support", "stress", "nutrition", 
    "toxins", "substance", "m_depression", "m_mental_health", "m_health", 
    "neigh_safety", "housing", "sleep", "preterm", "attachment", "neglect", 
    "parenting", "cognitive_enr", "micronutrients", "breastfeeding", 
    "immune", "placenta_brain", "gut_microbiome", "hpa_axis", "synaptic", 
    "epigenetics", "telomere", "polygenic_snps", "outcome_beh", 
    "outcome_neuro", "outcome_sga", "outcome_mobility"
  )
  #   # Add placeholder columns for missing data
  #   dataset$title <- NA
  #   dataset$authors <- NA
  #   dataset$doi <- NA
  # } else {
  #   cat("\nWARNING: Unexpected number of columns:", ncol(dataset), "\n")
  #   cat("Attempting to map columns by name matching...\n")
  #   
  # Try to identify key columns by name
  col_lower <- tolower(names(dataset))
  
  # Find key columns
  id_col <- which(col_lower == "id")[1]
  title_col <- which(col_lower == "title")[1]
  authors_col <- which(col_lower == "authors")[1]
  doi_col <- which(col_lower == "doi")[1]
  year_col <- which(col_lower == "year")[1]
  poverty_col <- which(col_lower == "poverty")[1]
  adversity_col <- which(col_lower == "adversity")[1]
  
  cat("Found columns at positions:\n")
  cat("  ID:", id_col, "\n")
  cat("  Title:", title_col, "\n")
  cat("  Authors:", authors_col, "\n")
  cat("  DOI:", doi_col, "\n")
  cat("  Year:", year_col, "\n")
  cat("  Poverty:", poverty_col, "\n")
  cat("  Adversity:", adversity_col, "\n")
}

# Save column labels
attr(dataset, "column_labels") <- colnames(dataset)

# Save column labels
attr(dataset, "column_labels") <- colnames(dataset)

colnames(dataset) <- c(
  "id", "inclusion", "title", "year", "authors", "doi", "paper_type", 
  "poverty", "adversity", "country", "sample_comp", "exposure_period", 
  "design", "sample_size", "poverty_oper", "early_life_adv", "support", 
  "stress", "nutrition", "toxins", "substance", "m_depression", 
  "m_mental_health", "m_health", "neigh_safety", "housing", "sleep", 
  "preterm", "attachment", "neglect", "parenting", "cognitive_enr", 
  "micronutrients", "breastfeeding", "immune", "placenta_brain", 
  "gut_microbiome", "hpa_axis", "synaptic", "epigenetics", "telomere", 
  "polygenic_snps", "outcome_beh", "outcome_neuro", "outcome_sga", 
  "outcome_mobility"
)

# Clean up country names
country_name_mapping <- c(
  "US" = "United States of America",
  "USA" = "United States of America",
  "UK" = "United Kingdom",
  "Russia" = "Russian Federation",
  "Venezuela" = "Venezuela (Bolivarian Republic)",
  "Korea" = "Republic of Korea"
)

dataset <- dataset %>%
  mutate(country = recode(country, !!!country_name_mapping))

# Filter for included studies only (if inclusion column exists)
if ("inclusion" %in% names(dataset)) {
  cat("\nFiltering for included studies only...\n")
  dataset_all <- dataset
  dataset <- dataset %>% 
    filter(inclusion == 1 | inclusion == "1" | tolower(inclusion) == "yes")
  cat("Filtered from", nrow(dataset_all), "to", nrow(dataset), "included studies\n")
}

# Count and display poverty articles
dataset <- dataset %>%
  mutate(
    poverty = as.numeric(as.character(poverty)),
    adversity = as.numeric(as.character(adversity))
  )

poverty_count <- sum(dataset$poverty == 1, na.rm = TRUE)
cat("\nTotal poverty articles:", poverty_count, "\n")

# Check for any issues with the poverty/adversity columns
if (poverty_count == 0) {
  cat("WARNING: No poverty articles found. Checking unique values in poverty column:\n")
  print(table(dataset$poverty, useNA = "ifany"))
}

# Save clean dataset
write_rds(dataset, 'raw_data/dataset_clean.rds')

# 2. Create derived datasets ----------------------------------------------

# Define variables for processing - check which ones actually exist (removed 'synaptic')
potential_variables <- c(
  "early_life_adv", "support", "stress", "nutrition", "toxins", 
  "substance", "m_depression", "m_mental_health", "m_health", 
  "neigh_safety", "housing", "sleep", "preterm", "attachment", 
  "neglect", "parenting", "cognitive_enr", "micronutrients", 
  "breastfeeding", "immune", "placenta_brain", "gut_microbiome", 
  "hpa_axis", "epigenetics", "telomere", "polygenic_snps"
)

# Only use variables that actually exist in the dataset
variables_use <- potential_variables[potential_variables %in% names(dataset)]
cat("\nFound", length(variables_use), "category variables in the dataset\n")

if (length(variables_use) == 0) {
  cat("WARNING: No expected category variables found. Checking column names...\n")
  print(names(dataset))
}

# Labels for pie chart (removed 'synaptic' - infant brain connectivity)
legend_labels <- c(
  "num_neigh_safety" = "Neighbourhood safety",
  "num_substance" = "Maternal substance use",
  "num_housing" = "Housing challenges",
  "num_sleep" = "Sleep",
  "num_neglect" = "Neglect",
  "num_parenting" = "Parenting",
  "num_cognitive_enr" = "Cognitive enrichment",
  "num_breastfeeding" = "Breastfeeding",
  "num_immune" = "Immune system",
  "num_placenta_brain" = "Placenta-brain axis",
  "num_gut_microbiome" = "Gut microbiome", 
  "num_hpa_axis" = "HPA axis",
  "num_epigenetics" = "Epigenetics",
  "num_telomere" = "Telomere dynamics",
  "num_polygenic_snps" = "SNPs and Polygenic risk scores",
  "num_attachment" = "Attachment",
  "num_early_life_adv" = "Early life adversity",
  "num_support" = "Support",
  "num_stress" = "Stress",
  "num_nutrition" = "Nutrition",
  "num_toxins" = "Toxins",
  "num_m_mental_health" = "Maternal mental health",
  "num_m_health" = "Maternal health",
  "num_preterm" = "Infant premature birth",
  "num_micronutrients" = "Micronutrients",
  "num_m_depression" = "Maternal depression"
)

# Create pie chart data
pie_data <- dataset %>%
  filter(poverty == 1) %>%
  mutate(across(all_of(variables_use), 
                ~ as.numeric(as.character(.)), .names = "num_{col}")) %>%
  summarise(across(starts_with("num_"), ~ sum(replace_na(., 0)))) %>%
  pivot_longer(cols = everything(), names_to = "category", values_to = "count") %>%
  mutate(percentage = round(100 * count / sum(count), 1))

# Only keep categories that have valid labels
pie_data <- pie_data %>%
  mutate(category_label = legend_labels[category]) %>%
  filter(!is.na(category_label)) %>%
  mutate(category = factor(category, levels = names(legend_labels), labels = legend_labels)) %>%
  select(-category_label) %>%
  filter(!is.na(category))

cat("Pie data created with", nrow(pie_data), "categories\n")

write_rds(pie_data, 'raw_data/pie_data.rds')


# Create word cloud data - only include columns that exist
word_cols <- c(
  "m_mental_health" = "Maternal mental health",
  "stress" = "Stress", 
  "support" = "Support",
  "nutrition" = "Nutrition",
  "early_life_adv" = "Early life adversity",
  "m_depression" = "Maternal depression",
  "toxins" = "Toxins",
  "attachment" = "Attachment",
  "m_health" = "Maternal health",
  "preterm" = "Preterm"
)

# Filter to only existing columns
existing_word_cols <- names(word_cols)[names(word_cols) %in% names(dataset)]

# if (length(existing_word_cols) > 0) {
#   word_data <- dataset %>%
#     filter(poverty == 1) %>%
#     select(all_of(existing_word_cols)) %>%
#     summarise(across(everything(), ~ sum(as.numeric(as.character(.)), na.rm = TRUE))) %>%
#     rename(!!!word_cols[existing_word_cols])
#   
#   cat("Word data created with", ncol(word_data), "categories\n")
# } else {
#   # Create empty word_data if no columns found
#   word_data <- data.frame(Message = "No word cloud data available")
#   cat("WARNING: No word cloud columns found in dataset\n")
# }

existing_word_cols <- names(word_cols)[names(word_cols) %in% names(dataset)]

word_data <- dataset %>%
  filter(poverty == 1) %>%
  select(all_of(existing_word_cols)) %>%
  summarise(across(everything(),
                   ~ sum(as.numeric(as.character(.)), na.rm = TRUE))) %>%
  rename_with(~ word_cols[.x], .cols = existing_word_cols)
write_rds(word_data, 'raw_data/word_data.rds')

# 3. Copy poverty operationalisations CSV ---------------------------------

# Copy the CSV file to the data directory
# Make sure this file exists in your raw_data directory
if (file.exists("raw_data/poverty_operationalisations.csv")) {
  file.copy("raw_data/poverty_operationalisations.csv", 
            "data/poverty_operationalisations.csv",
            overwrite = TRUE)
} else {
  warning("poverty_operationalisations.csv not found in raw_data directory")
}

# 4. Verify all files were created ----------------------------------------

required_files <- c(
  "raw_data/dataset_clean.rds",
  "raw_data/pie_data.rds",
  "raw_data/word_data.rds",
  "raw_data/poverty_operationalisations.csv"
)

missing_files <- required_files[!file.exists(required_files)]

if (length(missing_files) == 0) {
  cat("\n✓ All data files successfully created!\n")
  
  # Print summary statistics
  cat("\nDataset summary:\n")
  cat("- Total articles in clean dataset:", nrow(dataset), "\n")
  cat("- Poverty articles:", sum(dataset$poverty == 1, na.rm = TRUE), "\n")
  cat("- Adversity articles:", sum(dataset$adversity == 1, na.rm = TRUE), "\n")
  cat("- Articles with both poverty and adversity:", 
      sum(dataset$poverty == 1 & dataset$adversity == 1, na.rm = TRUE), "\n")
  cat("- Categories tracked:", length(variables_use), "\n")
  cat("- Articles with title/author/DOI info:", 
      sum(!is.na(dataset$title) & !is.na(dataset$authors) & !is.na(dataset$doi)), "\n")
  
  cat("\n✓ You can now run the app with: shiny::runApp()\n")
} else {
  cat("\n✗ Missing files:\n")
  cat(paste("  -", missing_files, collapse = "\n"), "\n")
}