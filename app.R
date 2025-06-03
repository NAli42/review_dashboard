# app.R - Main Shiny Application
# This is the main entry point for the Poverty & Neurodevelopment Literature Review Dashboard

# Source all modular files in correct order
source("01_global.R")
source("02_data_processing.R")
source("03_ui_modules.R")
source("05_plotting_functions.R")
if (file.exists("05_plotting_functions_interactive.R")) {
  source("05_plotting_functions_interactive.R")
}
source("04_server_modules.R")
source("06_utils.R")

# Load all data
data_list <- load_all_data()

# Check data integrity
check_data_integrity(data_list)

# Define UI
ui <- navbarPage(
  title = "Poverty & Neurodevelopment Literature Review",
  theme = app_theme,
  
  # Custom CSS for improved styling and ZPL logo positioning
  tags$head(
    tags$style(HTML("
      /* Responsive sticky navbar: fixed on desktop, static on mobile */
      @media (min-width: 769px) {
        .navbar {
          position: fixed !important;
          top: 0;
          width: 100%;
          z-index: 1030;
          margin-bottom: 0;
          padding-right: 70px; /* Make room for logo */
        }
        body {
          padding-top: 70px;
        }
      }
      @media (max-width: 768px) {
        .navbar {
          position: static !important;
          width: 100%;
          margin-bottom: 0;
          padding-right: 70px; /* Make room for logo */
        }
        body {
          padding-top: 0;
        }
      }

      /* ZPL logo positioning */
      .navbar-brand {
        padding-top: 5px;
        padding-bottom: 5px;
      }
      
      .navbar-nav {
        margin-right: 70px; /* Make room for logo */
      }
      
      /* ZPL logo in navbar */
      .navbar-right-logo {
        position: fixed;
        right: 15px;
        top: 10px;
        z-index: 1040;
        height: 50px;
      }
      
      .navbar-right-logo img {
        height: 50px;
        width: auto;
      }
      
      /* Fix for network diagram overlap */
      .vis-network {
        position: relative !important;
      }
      
      /* Improve spacing between elements */
      h4 {
        margin-top: 30px !important;
        margin-bottom: 20px !important;
      }
      
      .plotly {
        margin-bottom: 20px;
      }
      
      /* Improve box spacing */
      .box {
        margin-bottom: 20px;
      }
      
      /* Fix table container spacing */
      .dataTables_wrapper {
        margin-top: 10px;
      }
    "))
  ),
  
  # Include all tabs
  ui_dashboard_tab(),
  ui_research_tab(),
  ui_overlap_tab(),
  ui_poverty_measures_tab(),
  ui_conceptual_tab(),
  ui_methodology_tab(),
  
  # Footer
  footer = create_footer()
)

# Add ZPL logo after navbar is created - Fixed logo reference
ui <- tagList(
  ui,
  tags$div(
    class = "navbar-right-logo",
    tags$a(
      href = "https://www.tilburguniversity.edu/research/institutes-and-research-groups/zero-poverty-lab",
      target = "_blank",
      # Fixed logo path - make sure logo is in www folder
      imageOutput("zpl_logo_navbar", height = "50px", width = "auto", inline = TRUE)
    )
  )
)

# Define server
server <- function(input, output, session) {
  
  # Add logo output for navbar
  output$zpl_logo_navbar <- renderImage({
    list(src = "www/zpl.png", alt = "Zero Poverty Lab", height = "50px", width = "auto")
  }, deleteFile = FALSE)
  
  # Initialize all server modules
  server_dashboard_plots(input, output, session, data_list)
  server_research_areas(input, output, session, data_list)
  server_venn_diagram(input, output, session, data_list)
  server_poverty_measures(input, output, session, data_list)
  server_static_images(input, output, session)
  
}

# Run the application
shinyApp(ui = ui, server = server)
