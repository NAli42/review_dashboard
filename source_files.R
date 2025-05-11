# Source files for dashboard
library(tidyverse)
library(dplyr)
library(viridis)
colors <- viridis(3)
library(rnaturalearth)
library(rnaturalearthdata)


source("0001_data_cleanup.R")
# data <- readRDS('dataset_clean.rds')
# word_data <- readRDS('word_data.rds')
# pie_data <- readRDS('pie_data.rds')
# poverty_data <- readRDS('poverty_data.rds')


# Define the new categories and associated variables
external_system_vars <- c(
                         "support", "stress", "nutrition", "toxins", 
                         "substance", "m_mental_health", "m_health", 
                         "neigh_safety", "housing", "sleep","attachment", 
                         "neglect", "parenting", "cognitive_enr", 
                         "breastfeeding", "micronutrients"
)

internal_system_vars <- c("immune", 
                          "placenta_brain", 
                          "gut_microbiome", 
                          "hpa_axis", 
                          "synaptic")

cellular_vars <- c("epigenetics", 
                   "telomere", 
                   "polygenic_snps")

# Combine all variables for the Venn diagram tab
all_vars <- c(external_system_vars, internal_system_vars, cellular_vars)

# Define simplified variable names for word cloud
simplified_names <- c(
  "Maternal mental health", "Stress", "Support", "Nutrition",
  "Early life adversity", "Maternal depression", "Toxins",
  "Attachment", "Maternal health", "Preterm"
)


# World map
world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  st_make_valid() # Ensure all geometries are valid

# Calculate centroids for countries
world_centroids <- st_centroid(world)

# Summarize the country-level data from your dataset
country_counts <- data %>%
  group_by(country) %>%
  summarise(count = n()) %>%
  ungroup()

# Join the centroids data with the country counts
map_data <- left_join(world_centroids, country_counts, by = c("name" = "country"))

# Extract coordinates from centroids
map_data <- map_data %>%
  mutate(lon = st_coordinates(geometry)[, 1], lat = st_coordinates(geometry)[, 2]) %>%
  filter(!is.na(lon) & !is.na(lat) & !is.na(count)) # Omit rows with NA values

# Normalize article count to get appropriate circle size
map_data <- map_data %>%
  mutate(scaled_size = scales::rescale(count, to = c(3, 20))) # Adjust scaling range if needed

# TODO: drop ELA and see if everything runs fine
# Define the renaming map for variable names globally
rename_map <- c(
  "neigh_safety"    = "Neighbourhood safety",
  "substance"       = "Maternal substance use",
  "housing"         = "Housing challenges",
  "sleep"           = "Sleep",
  "neglect"         = "Neglect",
  "parenting"       = "Parenting",
  "cognitive_enr"   = "Cognitive enrichment",
  "breastfeeding"   = "Breastfeeding",
  "immune"          = "Immune system",
  "placenta_brain"  = "Placenta-brain axis",
  "gut_microbiome"  = "Gut microbiome",
  "hpa_axis"        = "HPA axis",
  "synaptic"        = "Infant brain connectivity",
  "epigenetics"     = "Epigenetics",
  "telomere"        = "Telomere dynamics",
  "polygenic_snps"  = "SNPs and Polygenic risk scores",
  "attachment"      = "Attachment",
  "support"         = "Support",
  "stress"          = "Stress",
  "nutrition"       = "Nutrition",
  "toxins"          = "Toxins",
  "m_mental_health" = "Maternal mental health",
  "m_health"        = "Maternal health",
  "preterm"         = "Infant premature birth"
)

# Define the list of all variables (these should correspond to the dataset)
all_vars <- names(rename_map) # If all_vars contains more variables, adjust accordingly


## Preparing the datasets
# Prepare color palette
extended_palette <- colorRampPalette(paletteer::paletteer_d("rcartocolor::Antique"))(length(unique(pie_data$category)))

# Clean labels for the treemap and Venn diagram
clean_labels <- c(
  "m_mental_health" = "Maternal Mental Health",
  "m_health"        = "Maternal Health",
  "neigh_safety"    = "Neighborhood Safety",
  "housing"         = "Housing",
  "sleep"           = "Sleep",
  "attachment"      = "Attachment",
  "neglect"         = "Neglect",
  "parenting"       = "Parenting",
  "cognitive_enr"   = "Cognitive Enrichment",
  "breastfeeding"   = "Breastfeeding",
  "support"         = "Support",
  "stress"          = "Stress",
  "nutrition"       = "Nutrition",
  "toxins"          = "Toxins",
  "substance"       = "Substance Exposure",
  "immune"          = "Immune System",
  "placenta_brain"  = "Placenta-Brain Axis",
  "gut_microbiome"  = "Gut Microbiome",
  "hpa_axis"        = "HPA Axis",
  "synaptic"        = "Synaptic Function",
  "epigenetics"     = "Epigenetics",
  "telomere"        = "Telomeres",
  "polygenic_snps"  = "Polygenic risk scores/SNPs"
)

# Custom plot theme function to reuse across multiple plots
custom_plot_theme <- function() {
  theme(
    axis.text.x = element_text(size = 13.5, color = "#331718FF"),
    axis.text.y = element_text(size = 13.5, color = "#331718FF"),
    axis.title.y = element_text(size = 13.5, color = "#331718FF", face = "bold"),
    axis.title.x = element_text(size = 13.5, color = "#331718FF", face = "bold"),
    plot.title = element_text(size = 15, color = "#331718FF", face = "bold"),
    legend.position = "none"
  )
}

# Dynamic height function for responsive plots
dynamic_height <- function(session, output_id, small_size_ratio = 0.9, large_size_ratio = 0.85) {
  return(function() {
    output_width <- session$clientData[[paste0("output_", output_id, "_width")]]
    if (output_width <= 600) {
      output_width * small_size_ratio
    } else {
      output_width * large_size_ratio
    }
  })
}


poverty_labels <- c(
  "0" = "Education",
  "1" = "Income",
  "2" = "SES/composite",
  "3" = "Neighbourhood",
  "4" = "Mixed",
  "5" = "Financial stress",
  "6" = "Social mobility",
  "7" = "Income-to-needs ratio"
)

map_data <- map_data %>%
  mutate(
    lon = ifelse(name == "France", 2.3522, lon),
    lat = ifelse(name == "France", 48.8566, lat)
  )


#
save.image(file = "my_environment.RData")
