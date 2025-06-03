# 03_ui_modules.R - UI Component Functions
# This file contains all UI module functions for the dashboard

# Helper function for info boxes
create_info_box <- function(icon, text) {
  div(
    style = paste0(
      "padding: 10px; background-color: ", THEME_CONFIG$bg_secondary, "; ",
      "border-radius: 8px; margin-bottom: 15px; display: flex; align-items: center; ",
      "box-shadow: 0 2px 4px rgba(0,0,0,0.1);"
    ),
    imageOutput(icon, height = "30px", inline = TRUE),
    tags$strong(
      style = paste0("margin-left: 8px; color: ", THEME_CONFIG$text_primary, ";"),
      text
    )
  )
}

# Dashboard tab UI (removed lollipop chart)
ui_dashboard_tab <- function() {
  tabPanel(
    "Dashboard",
    
    # Info boxes
    fluidRow(
      column(3, create_info_box("database_icon", "Total articles from databases = 9,722")),
      column(3, create_info_box("asreview_icon", "Screened with ASReview = 1,240")),
      column(3, create_info_box("screened_icon", "Articles manually screened = 565")),
      column(3, create_info_box("relevant_icon", textOutput("poverty_article_count", inline = TRUE)))
    ),
    
    # Instruction text - updated to mention interactive map
    fluidRow(
      column(
        12,
        div(
          style = paste0(
            "background-color: ", THEME_CONFIG$bg_secondary,
            "; padding: 10px; border-radius: 5px; margin-bottom: 15px; ",
            "text-align: center; color: ", THEME_CONFIG$text_secondary, ";"
          ),
          "Click on any bar, category, network node, or country bubble to filter the studies table"
        )
      )
    ),
    
    # First row of visualizations
    fluidRow(
      column(4, plotlyOutput("poverty_adversity_barplot", height = "500px")),
      column(8, leafletOutput("worldmap", height = "500px"))
    ),
    
    # Combined table and network diagram side by side
    fluidRow(
      style = "margin-top: 30px; margin-bottom: 30px;",
      column(
        6,
        h4(
          "Selected Studies",
          style = paste0("text-align: center; color: ", THEME_CONFIG$text_primary, 
                         "; margin-top: 20px; margin-bottom: 15px;")
        ),
        div(
          textOutput("filter_status"),
          style = paste0(
            "color: ", THEME_CONFIG$accent_colors[1],
            "; font-weight: bold; margin-bottom: 10px;"
          )
        ),
        actionButton(
          "reset_filter", "Reset Filter",
          style = paste0(
            "background-color: ", THEME_CONFIG$accent_colors[1],
            "; color: white; margin-bottom: 15px;"
          )
        ),
        div(
          style = "height: 700px; overflow-y: auto;",
          DTOutput("main_filter_table")
        )
      ),
      column(
        6,
        h4(
          "Network of Co-occurring Categories",
          style = paste0("text-align: center; color: ", THEME_CONFIG$text_primary, 
                         "; margin-top: 20px; margin-bottom: 15px;")
        ),
        div(
          style = paste0(
            "font-size: 12px; color: ", THEME_CONFIG$text_secondary,
            "; text-align: center; margin-bottom: 15px;"
          ),
          "Connections show categories that appear together in 2+ papers. Click nodes to filter."
        ),
        visNetworkOutput("network_diagram", height = "700px", width = "100%")
      )
    )
  )
}

# Research focus areas tab (with ordered bars and system colors)
ui_research_tab <- function() {
  tabPanel(
    "Research focus areas",
    fluidRow(
      column(
        12,
        selectInput(
          "category", "Select Category",
          choices = c("All", "External system", "Internal system", "Cellular")
        ),
        plotlyOutput("category_barchart", height = "600px"),
        br(),
        h4(
          "Studies in Selected Categories",
          style = paste0("text-align: center; color: ", THEME_CONFIG$text_primary, 
                         "; margin-top: 20px; margin-bottom: 15px;")  # Added margins
        ),
        DTOutput("research_table")
      )
    )
  )
}

# Overlap tab (with improved Venn diagram)
ui_overlap_tab <- function() {
  tabPanel(
    "Overlap in categories",
    fluidRow(
      column(9, plotOutput("venn_diagram", height = "600px")),
      column(
        3,
        checkboxGroupInput(
          "venn_vars",
          HTML("<span style='font-size:16px;'><b>Select Variables for Venn Diagram</b></span>"),
          choices = setNames(names(VARIABLE_LABELS), VARIABLE_LABELS)
        )
      )
    ),
    fluidRow(
      column(
        12,
        br(),
        h4(
          "Studies with Selected Variable Overlaps",
          style = paste0("text-align: center; color: ", THEME_CONFIG$text_primary, 
                         "; margin-top: 20px; margin-bottom: 15px;")  # Added margins
        ),
        DTOutput("overlap_table")
      )
    )
  )
}

# Poverty measurements tab
ui_poverty_measures_tab <- function() {
  tabPanel(
    "Poverty measurements",
    fluidRow(
      box(
        width = 8, status = "primary", solidHeader = TRUE,
        title = "Mean Sample Size by Category",
        checkboxInput("exclusive_bar", "Exclusive measures only", FALSE),
        plotlyOutput("poverty_bar", height = "400px"),
        tags$div(
          style = paste0(
            "font-size: 12px; color: ", THEME_CONFIG$text_secondary,
            "; text-align: center; margin-top: 10px;"  # Added margin
          ),
          "Click on a bar to filter the table. Double-click to reset."
        )
      ),
      box(
        width = 4, status = "info", solidHeader = TRUE,
        title = "Effect & Study Details",
        plotlyOutput("poverty_pie", height = "250px"),
        hr(),
        DTOutput("poverty_table"),
        tags$div(
          style = paste0("font-size:80%; margin-top:10px; color:", THEME_CONFIG$text_secondary, ";"),
          strong("Legend:"),
          tags$ul(
            tags$li("N = Sample size"),
            tags$li("Hollingshead = Hollingshead Index"),
            tags$li("ADI = Area Deprivation Index"),
            tags$li("HDI = Human Development Index"),
            tags$li("Family SES = Aggregated family‐SES flags"),
            tags$li("Material hardship = Aggregated hardship flags"),
            tags$li("Level = 0:mixed, 1:external, 2:internal, 3:cellular")
          )
        )
      )
    )
  )
}

# Conceptual model tab
ui_conceptual_tab <- function() {
  tabPanel(
    "Conceptual model",
    fluidRow(
      column(8, imageOutput("review_figure", height = "auto")),
      column(
        4,
        h3("Conceptual Model Overview", style = "margin-top: 20px;"),  # Added margin
        HTML("<p>This is an overview of the categories we have identified so far, following a levels-of-explanation approach. The outer ring represents the <b>external factors</b> that can affect infant neurodevelopment.</p>"),
        HTML("<p>The inner 'systems' level represents the <b>internal systems</b> that mediate the biological embedding of those factors.</p>"),
        HTML("<p>Finally, the innermost circle represents the <b>molecular processes</b> studied that underlie those effects.</p>")
      )
    )
  )
}

# Methodology tab
ui_methodology_tab <- function() {
  tabPanel(
    "Methodology",
    fluidRow(
      column(8, imageOutput("review_process", height = "auto")),
      column(
        4,
        h3("Review process breakdown", style = "margin-top: 20px;"),  # Added margin
        HTML("
          <p>The figure on the left shows the search and screening process. The databases used are:</p>
          <ul>
            <li>PubMed</li>
            <li>PsycInfo</li>
            <li>Web of Science</li>
          </ul>"
        ),
        HTML("<p>Databases were first searched using a combination of synonyms and variations of the terms 'intergenerational'+ 'poverty' + 'neurodevelopment'. A total of 6 searches for poverty and 7 for adversity were conducted per database. Only articles published between 2015 and 2024 (including) were retained.</p>"),
        HTML("<p>After combining all the search results duplicates were removed. The abstracts of 1240 of the articles were screened using the open source machine learning tool <a href='https://asreview.nl/'>ASReview</a>.</p>"),
        HTML("<p>To compile the figures available in this dashboard the remaining 565 articles that were deemed potentially relevant were screened again, this time coding each for various categories.</p>"),
        HTML("<p>The categories were only coded as present if the topic is specifically investigated in the paper. This means that if, for example, income was simply <i>controlled for</i>, the paper is not coded as a 'poverty' paper. Alternatively, for studies that investigate certain adversity types within a <b>disadvantaged</b> sample, the paper is coded for poverty.</p>"),
        HTML("<p><b>Additional exclusion criteria are</b>:</p>
             <ul>
               <li>Age of exposure > 2 years</li>
               <li>There is no mention of 'poverty', 'socioeconomic', or 'income' anywhere in the article</li>
               <li>The study is retrospective (e.g. assessing early-life adversity exposure via a retrospective questionnaire)</li>
               <li>The outcome is infant mortality. While this is a highly relevant topic, due to the main question of the paper (Intergenerational transmission of poverty), it is not within its scope</li>
               <li>The study is an animal study. Review papers that review information from both human and animal models are included.</li>
             </ul>")
      )
    )
  )
}