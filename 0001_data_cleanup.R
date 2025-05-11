library(tidyverse)
library(dplyr)
library(janitor)

#TODO: 
# Fix the categories to be displayed 

#Data clean up 
data <- readRDS('raw_data/dataset_v5.rds')

variables_use <- c(colnames(data[, 14:39]))

#Clean up country names
country_name_mapping <- c(
  "US" = "United States of America",
  "USA" = "United States of America",
  "UK" = "United Kingdom",
  "Russia" = "Russian Federation",
  "Venezuela" = "Venezuela (Bolivarian Republic)",
  "Korea" = "Republic of Korea"
)
#Define the labels
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
  "num_synaptic" = "Infant brain connectivity",
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
  "num_preterm" = "Infant premature birth"
)

venn_labels <- c(
  "neigh_safety" = "Neighbourhood safety",
  "substance" = "Maternal substance use",
  "num_housing" = "Housing challenges",
  "sleep" = "Sleep",
  "neglect" = "Neglect",
  "parenting" = "Parenting",
  "cognitive_enr" = "Cognitive enrichment",
  "breastfeeding" = "Breastfeeding",
  "immune" = "Immune system",
  "placenta_brain" = "Placenta-brain axis",
  "gut_microbiome" = "Gut microbiome", 
  "hpa_axis" = "HPA axis",
  "synaptic" = "Infant brain connectivity",
  "epigenetics" = "Epigenetics",
  "telomere" = "Telomere dynamics",
  "polygenic_snps" = "SNPs and Polygenic risk scores",
  "attachment" = "Attachment",
  "early_life_adv" = "Early life adversity",
  "support" = "Support",
  "stress" = "Stress",
  "nutrition" = "Nutrition",
  "toxins" = "Toxins",
  "m_mental_health" = "Maternal mental health",
  "m_health" = "Maternal health",
  "preterm" = "Infant premature birth"
)

# Apply the mapping to standardize the country names in your dataset
data <- data %>%
  mutate(country = recode(country, !!!country_name_mapping))

#Data for the pie chart
pie_data <- data %>%
  filter(poverty == 1) %>%
  mutate(across(all_of(variables_use), 
                ~ as.numeric(as.character(.)), .names = "num_{col}")) %>%
  summarise(across(starts_with("num_"), ~ sum(replace_na(., 0)))) %>%
  pivot_longer(cols = everything(), names_to = "category", values_to = "count") %>%
  mutate(percentage = round(100 * count / sum(count), 1))  # Calculate percentage

pie_data$category <- factor(pie_data$category, levels = names(legend_labels), labels = legend_labels)
pie_data <- pie_data |> 
  filter(!is.na(category))


#Create a dataframe for the word cloud
word_data <- data %>%
  filter(poverty == 1) %>%
  summarise(
    `Maternal mental health` = sum(as.numeric(as.character(m_mental_health)), na.rm = TRUE),
    Stress = sum(as.numeric(as.character(stress)), na.rm = TRUE),
    Support = sum(as.numeric(as.character(support)), na.rm = TRUE),
    Nutrition = sum(as.numeric(as.character(nutrition)), na.rm = TRUE),
    `Early life adversity` = sum(as.numeric(as.character(early_life_adv)), na.rm = TRUE),
    `Maternal depression` = sum(as.numeric(as.character(m_depression)), na.rm = TRUE),
    Toxins = sum(as.numeric(as.character(toxins)), na.rm = TRUE),
    Attachment = sum(as.numeric(as.character(attachment)), na.rm = TRUE),
    `Maternal health` = sum(as.numeric(as.character(m_health)), na.rm = TRUE),
    Preterm = sum(as.numeric(as.character(preterm)), na.rm = TRUE)
  )

poverty_data <- data %>%
  filter(design == 1) %>%
  mutate(poverty_oper = factor(poverty_oper, levels = 0:6, labels = c(
    "Education", "Income", "SES/composite", "Neighbourhood SES", "Mixed", 
    "Financial Stress", "Social mobility"
  ))) %>%
  group_by(poverty_oper) %>%
  summarise(count = n())


#Trying to assign labels to the variables






#saving the datasets
write_rds(data, 'raw_data/dataset_clean.rds')
write_rds(word_data, 'raw_data/word_data.rds')
write_rds(pie_data, 'raw_data/pie_data.rds')
