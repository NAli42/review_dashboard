# 02_data_processing.R - Data Loading and Processing Functions
# This file contains all data loading and processing functions

# Function to load all data
load_all_data <- function(data_path = "raw_data/") {
  list(
    main_data = readRDS(file.path(data_path, "dataset_clean.rds")),
    word_data = readRDS(file.path(data_path, "word_data.rds")),
    pie_data = readRDS(file.path(data_path, "pie_data.rds")),
    poverty_opers = read_csv2(file.path(data_path, "poverty_operationalisations.csv"), 
                              col_types = cols(), show_col_types = FALSE)
  )
}

# Process poverty operationalisations data
process_poverty_opers <- function(df_raw) {
  df_raw %>%
    mutate(
      study_id = row_number(),
      Family_SES_flag = if_any(c(
        "Family SES (education + occupation)",
        "Family SES (education + income)",
        "Family SES (other mixed)",
        "Postcode-based family SES",
        "Country-specific SES scale"
      ), ~ .x == 1),
      Material_hardship_flag = if_any(c(
        "Nutrition",
        "Food expenditure/insecurity",
        "Difficulties paying bills (self-reported)",
        "Welfare use"
      ), ~ .x == 1),
      Effect = case_when(
        `Effect of low SES? (0 = no, 1 = yes, 2 = mixed, 3 = contradicting hypothesis)` == 0 ~ "No",
        `Effect of low SES? (0 = no, 1 = yes, 2 = mixed, 3 = contradicting hypothesis)` == 1 ~ "Yes",
        `Effect of low SES? (0 = no, 1 = yes, 2 = mixed, 3 = contradicting hypothesis)` == 2 ~ "Mixed",
        TRUE ~ "Contradicting"
      )
    ) %>%
    select(-`Effect of low SES? (0 = no, 1 = yes, 2 = mixed, 3 = contradicting hypothesis)`)
}

# Process map data
process_map_data <- function(data) {
  # Load world map
  world <- ne_countries(scale = "medium", returnclass = "sf") %>%
    st_make_valid()
  
  # Calculate centroids
  world_centroids <- st_centroid(world)
  
  # Summarize country data
  country_counts <- data %>%
    group_by(country) %>%
    summarise(count = n(), .groups = "drop")
  
  # Join and process
  world_centroids %>%
    left_join(country_counts, by = c("name" = "country")) %>%
    mutate(
      lon = st_coordinates(geometry)[, 1],
      lat = st_coordinates(geometry)[, 2]
    ) %>%
    filter(!is.na(lon) & !is.na(lat) & !is.na(count)) %>%
    mutate(
      scaled_size = scales::rescale(count, to = c(3, 20)),
      # Fix France coordinates
      lon = ifelse(name == "France", 2.3522, lon),
      lat = ifelse(name == "France", 48.8566, lat)
    )
}

# Variable categories (removed 'synaptic' - infant brain connectivity)
VARIABLE_CATEGORIES <- list(
  external_system = c(
    "support", "stress", "nutrition", "toxins", "substance",
    "m_mental_health", "m_health", "neigh_safety", "housing", 
    "sleep", "attachment", "neglect", "parenting", "cognitive_enr", 
    "breastfeeding", "micronutrients", "m_depression", "early_life_adv", "preterm"
  ),
  internal_system = c(
    "immune", "placenta_brain", "gut_microbiome", "hpa_axis"
  ),
  cellular = c(
    "epigenetics", "telomere", "polygenic_snps"
  )
)

# Label mappings (removed 'synaptic' - infant brain connectivity)
VARIABLE_LABELS <- c(
  neigh_safety = "Neighbourhood safety",
  substance = "Maternal substance use",
  housing = "Housing challenges",
  sleep = "Sleep",
  neglect = "Neglect",
  parenting = "Parenting",
  cognitive_enr = "Cognitive enrichment",
  breastfeeding = "Breastfeeding",
  immune = "Immune system",
  placenta_brain = "Placenta-brain axis",
  gut_microbiome = "Gut microbiome",
  hpa_axis = "HPA axis",
  epigenetics = "Epigenetics",
  telomere = "Telomere dynamics",
  polygenic_snps = "SNPs and Polygenic risk scores",
  attachment = "Attachment",
  support = "Support",
  stress = "Stress",
  nutrition = "Nutrition",
  toxins = "Toxins",
  m_mental_health = "Maternal mental health",
  m_health = "Maternal health",
  preterm = "Infant premature birth",
  early_life_adv = "Early life adversity",
  m_depression = "Maternal depression",
  micronutrients = "Micronutrients"
)

# Poverty operationalisation labels
POVERTY_LABELS <- c(
  "0" = "Education",
  "1" = "Income",
  "2" = "SES/composite",
  "3" = "Neighbourhood",
  "4" = "Mixed",
  "5" = "Financial stress",
  "6" = "Social mobility",
  "7" = "Income-to-needs ratio"
)