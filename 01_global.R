# 01_global.R - Global Configuration and Package Loading
# This file contains all global settings, theme configuration, and package loading

# Load all required packages
library(shiny)
library(shinydashboard)
library(tidyverse)
library(leaflet)
library(VennDiagram)
library(grid)
library(sf)
library(treemapify)
library(paletteer)
library(bslib)
library(hrbrthemes)
library(plotly)
library(DT)
library(rnaturalearth)
library(rnaturalearthdata)
library(scales)
library(visNetwork)
library(igraph)

# Global theme configuration with lighter, coral-ish colors
THEME_CONFIG <- list(
  # Color palette - lighter, coral-ish colors
  primary_colors = c("#FF7F7F", "#FFB366", "#FFD699", "#06D6A0", "#118AB2"),  # Lighter coral-ish red/orange
  accent_colors = c("#7BB3F0", "#118AB2", "#06D6A0", "#FFD166"),
  
  # System level colors for research focus areas - also lighter
  system_colors = c(
    "External System" = "#FF7F7F",  # Light coral
    "Internal System" = "#118AB2",  # Blue (kept same)
    "Cellular" = "#06D6A0"          # Green (kept same)
  ),
  
  # Text colors
  text_primary = "#2C3E50",
  text_secondary = "#7F8C8D",
  
  # Background colors
  bg_primary = "#FFFFFF",
  bg_secondary = "#F8F9FA",
  
  # Font settings
  font_family = "Helvetica Neue, Arial, sans-serif",
  font_size_base = 14,
  font_size_large = 16,
  font_size_small = 12
)

# Bootstrap theme
app_theme <- bs_theme(
  version = 5,
  bg = THEME_CONFIG$bg_primary,
  fg = THEME_CONFIG$text_primary,
  primary = THEME_CONFIG$primary_colors[1],
  secondary = THEME_CONFIG$primary_colors[2],
  base_font = font_google("Lato"),
  heading_font = font_google("Montserrat")
)

# Global options
options(
  shiny.maxRequestSize = 30*1024^2,  # 30MB max file size
  digits = 3
)