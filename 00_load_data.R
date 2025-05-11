#Load the data
library(haven)
library(tidyverse)
library(janitor)
library(dplyr)

#Loads the csv file
dataset <- read.csv('raw_data/dataset_v5.csv', sep = ";")


#Saves the old variable names as column labels
attr(dataset, "column_labels") <- colnames(dataset)


#Renames the columns
colnames(dataset) <- c("id", "inclusion", "year", "paper_type", "poverty", "adversity", "country", "sample_comp", 
                        "exposure_period", "design", "same_size", "poverty_oper", "early_life_adv", "support", "stress",
                        "nutrition", "toxins", "substance", "m_depression", "m_mental_health", "m_health", "neigh_safety", 
                        "housing", "sleep", "preterm", "attachment", "neglect", "parenting", "cognitive_enr", "micronutrients",
                        "breastfeeding", "immune", "placenta_brain", "gut_microbiome", "hpa_axis", "synaptic", "epigenetics", 
                        "telomere", "polygenic_snps", "outcome_beh", "outcome_neuro", "outcome_sga", "outcome_mobility")


#loads the column labels in a separate object
original_labels <- attr(dataset, "column_labels")

#Saves the data in an R file
write_rds(dataset, 'raw_data/dataset_v5.rds')
