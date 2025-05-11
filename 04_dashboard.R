#TODO: Filter to only empirical articles
# TODO: Update with the new included articles set
# TODO: Keep only the categories that are included in the review
# TODO: Integrate the paper titles + DOIs to alow users to click and be redirected to the DOI url 
# Load necessary libraries
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr, warn.conflicts = FALSE)
library(tidyr)
library(leaflet)
library(VennDiagram)
library(grid)
library(sf)
library(treemapify)
library(paletteer)
library(bslib)
library(hrbrthemes)
library(tm)
library(proustr)

# Load environment or data as necessary
load("my_environment.RData")

# UI ----------------------------------------------------------------------

ui <- navbarPage(
  title = "Categories of Poverty Articles", 
  theme = bs_theme(version = 5),
  
  # First tab (Dashboard)
  tabPanel(
    "Dashboard",
    
    # Add the boxes at the top of the dashboard
    fluidRow(
      column(
        width = 3,
        div(
          style = "padding: 10px; background-color: #f5f5f5; border-radius: 5px; margin-bottom: 15px; display: flex; align-items: center;",
          imageOutput("database_icon", height = "30px", inline = TRUE),
          tags$strong(style = "margin-left: 8px;", "Total articles from databases = 9,722")
        )
      ),
      column(
        width = 3,
        div(
          style = "padding: 10px; background-color: #f5f5f5; border-radius: 5px; margin-bottom: 15px; display: flex; align-items: center;",
          imageOutput("asreview_icon", height = "30px", inline = TRUE),
          tags$strong(style = "margin-left: 8px;", "Screened with ASReview = 1,240")
        )
      ),
      column(
        width = 3,
        div(
          style = "padding: 10px; background-color: #f5f5f5; border-radius: 5px; margin-bottom: 15px; display: flex; align-items: center;",
          imageOutput("screened_icon", height = "30px", inline = TRUE),
          tags$strong(style = "margin-left: 8px;", "Abstracts manually screened = 585")
        )
      ),
      column(
        width = 3,
        div(
          style = "padding: 10px; background-color: #f5f5f5; border-radius: 5px; margin-bottom: 15px; display: flex; align-items: center;",
          imageOutput("relevant_icon", height = "30px", inline = TRUE),
          tags$strong(style = "margin-left: 8px;", "Relevant articles included = 79")
        )
      )
    ),
    
    fluidRow(
      column(
        width = 12, class = "col-xs-12 col-md-4",
        plotOutput("poverty_adversity_barplot_main", height = "500px")
      ),
      column(
        width = 12, class = "col-xs-12 col-md-8",
        leafletOutput("worldmap_main", height = "500px")
      )
    ),
    
    fluidRow(
      column(
        width = 12, class = "col-xs-12 col-md-6",
        plotOutput("stacked_barplot", height = "500px")
      ),
      column(
        width = 12, class = "col-xs-12 col-md-6",
        plotOutput("piechart_main", height = "500px")  # Lollipop chart here
      )
    ),
    
    fluidRow(
      column(
        width = 12, class = "col-xs-12 col-md-12",
        plotOutput("treemap_main", height = "700px")
      )
    )
  ),
  
  # Second tab (Research Focus Areas)
  tabPanel(
    "Research focus areas",
    fluidRow(
      # Column for the plot
      column(
        width = 12, class = "col-xs-12 col-md-12",
        selectInput("category", "Select Category",
                    choices = c("All", "External system", "Internal system", "Cellular")),
        plotOutput("piechart", height = "800px")
      )
    )
  ),
  
  # Third tab (Overlap in categories)
  tabPanel(
    "Overlap in categories",
    fluidRow(
      column(
        width = 12, class = "col-xs-12 col-md-9",
        plotOutput("venn_diagram", height = "800px")
      ),
      column(
        width = 12, class = "col-xs-12 col-md-3",
        checkboxGroupInput("venn_vars", 
                           HTML("<span style='font-size:16px;'><b>Select Variables for Venn Diagram</b></span>"),
                           choices = setNames(all_vars, rename_map[all_vars]))
      )
    )
  ),
  
  # New tab (Conceptual model)
  tabPanel(
    "Conceptual model",  # Title for the new tab
    fluidRow(
      # Column for the static image, scaled down
      column(
        width = 8, class = "col-xs-12 col-md-8",
        imageOutput("review_figure", height = "auto")  # Adjusted for scaling the image
      ),
      
      # Column for the text description
      column(
        width = 4, class = "col-xs-12 col-md-4",
        h3("Conceptual Model Overview"),  # Title
        HTML("<p>This is an overview of the categories we have identified so far, following a levels-of-explanation approach. The outer ring represents the <b>external factors</b> that can affect infant neurodevelopment.</p>"),
        HTML("<p>The inner 'systems' level represents the <b>internal systems</b> that mediate the biological embedding of those factors.</p>"),
        HTML("<p>Finally, the innermost circle represents the <b>molecular processes</b> studied that underlie those effects.</p>")
      )
    )
  ),
  tabPanel(
    "Methodology",
    fluidRow(
      column(
        width = 8, class = "col-xs-12 col-md-8",
        imageOutput("review_process", height = "auto")
      ),
      column(
        width = 4, class = "col-xs-12 col-md-4",
        h3("Review process breakdown"),
        HTML("
    <p>The figure on the left shows the search and screening process. The databases used are:</p>
    <ul>
      <li>PubMed</li>
      <li>PsycInfo</li>
      <li>Web of Science</li>
    </ul>
  "),
        HTML("<p>Databases were first searched using a combination of synonyms and variations of the terms 'intergenerational'+ 'poverty' + 'neurodevelopment'. A total of 6 searches for poverty and 7 for adversity were conducted per database. </p>"),
        HTML("<p>After combining all the search results duplicates were removed. The abstracts of 1240 of the articles were screened using the open source machine learning tool ASReview.</p>"),
        HTML("<p>To compile the figures available in this dashboard the remaining 568 articles that were deemed potentially relevant were screened again, this time coding each for various categories.</p>"),
        HTML("<p>The categories were only coded as present if the topic is specifically investigated in the paper. This means that if, for example, income was simply <i>controlled for</i>, the paper is not coded as a 'poverty' paper. Alternatively, for studies that investigate certain adversity types within a <b>disadvantaged</b> sample, the paper is coded for poverty.</p>"),
        HTML("<p><b>Additional exclusion criteria are</b>:</p>
             <ul>
             <li> Age of exposure > 2 years</li>
             <li> There is no mention of 'poverty', 'socioeconomic', or 'income' anywhere in the article</li>
             <li> The study is retrospective (e.g. assessing early-life adversity exposure via a retrospective questionnaire)</li>
             <li> The outcome is infant mortality. While this is a highly relevant topic, due to the main question of the paper (Intergenerational transmission of poverty), it is not within its scope</li>
             </ul>")
      )
    )
  )
)


# Server ------------------------------------------------------------------

server <- function(input, output, session) {
  
  # Dataset 1: Overlap in Poverty and Adversity Literature
  poverty_data_adversity <- reactive({
    poverty_count <- data %>%
      filter(poverty == 1 & adversity == 0) %>%
      summarise(count = n()) %>%
      pull(count)
    
    poverty_adversity_count <- data %>%
      filter(poverty == 1 & adversity == 1) %>%
      summarise(count = n()) %>%
      pull(count)
    
    data.frame(
      category = c("Poverty only", "Poverty + Adversity"),
      count = c(poverty_count, poverty_adversity_count)
    )
  })
  
  # Overlap in literature ---------------------------------------------------
  
  
  # Plot for "Overlap in Poverty and Adversity Literature"
  output$poverty_adversity_barplot_main <- renderPlot({
    ggplot(poverty_data_adversity(), aes(x = category, y = count, fill = category)) +
      geom_bar(stat = "identity", width = 0.5) +
      geom_text(aes(label = count), vjust = -0.5, size = 5, color = "#331718FF") +
      labs(
        y = "Number of articles",
        x = "",
        title = "Overlap in the literature"
      ) +
      theme_minimal() +
      scale_fill_paletteer_d("rcartocolor::Antique") +
      custom_plot_theme()
  })
  
  # Reactive dataset for "World Map"
  output$worldmap_main <- renderLeaflet({
    leaflet(map_data) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~lon, lat = ~lat,
        radius = ~scaled_size,
        color = "#440154FF", fillOpacity = 0.6,
        popup = ~ paste(name, ": ", count)
      ) %>%
      addControl(
        html = "<h4 style='color:#331718FF; text-align:center;'>Countries of origin of the samples in the dataset</h4>",
        position = "topright"
      )
  })
  
  # Reactive dataset for "Poverty Operationalization"
  poverty_data_filtered <- reactive({
    data %>%
      filter(poverty == 1) %>%
      group_by(poverty_oper) %>%
      summarise(count = n()) %>%
      filter(!is.na(poverty_oper) & !is.na(count))
  })
  
  # Poverty operationalization ----------------------------------------------
  
  poverty_data_filtered <- reactive({
    data %>%
      filter(poverty == 1, !is.na(poverty_oper)) %>%  # Drop NAs explicitly
      group_by(poverty_oper) %>%
      summarise(count = n()) %>%
      mutate(poverty_oper_label = poverty_labels[as.character(poverty_oper)]) %>%  # Apply the correct labels
      filter(!is.na(poverty_oper_label))  # Ensure no NA labels remain
  })
  
  # Render the second plot for "Poverty Operationalization"
  output$stacked_barplot <- renderPlot({
    ggplot(poverty_data_filtered(), aes(x = reorder(poverty_oper_label, count), y = count, fill = poverty_oper_label)) +  # Use the labeled column
      geom_bar(stat = "identity") +
      geom_text(aes(label = count),
                hjust = -0.2,
                size = 5,
                color = "#331718FF"
      ) +
      coord_flip() +
      scale_fill_paletteer_d("rcartocolor::Antique") +
      labs(fill = "Poverty indicators", title = "Poverty operationalization", y = "Article count") +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5, color = "#331718FF"),
        axis.title.y = element_blank(),
        axis.title.x = element_text(size = 16, color = "#331718FF"),
        axis.text.y = element_text(size = 16, color = "#331718FF"),
        legend.position = "none"
      )
  })
  
  # Lollipop chart ---------------------------------------------------------
  
  output$piechart_main <- renderPlot({
    pie_data <- pie_data %>%
      arrange(count) %>%
      mutate(category = factor(category, levels = category))
    
    ggplot(pie_data, aes(x = category, y = count)) +
      geom_segment(aes(x = category, xend = category, y = 0, yend = count), color = "grey") +
      geom_point(size = 5, color = "#467378FF") +
      ylim(0, max(pie_data$count) + 10) +
      coord_flip() +
      labs(x = "Categories", y = "Article count", title = "Article count per category") +
      theme_minimal() +
      custom_plot_theme()
  })
  
  # Research areas bar chart ------------------------------------------------
  
  output$piechart <- renderPlot({
    vars <- switch(input$category,
                   "External system" = external_system_vars,
                   "Internal system" = internal_system_vars,
                   "Cellular" = cellular_vars,
                   c(external_system_vars, internal_system_vars, cellular_vars)
    )
    
    pie_data <- data %>%
      filter(poverty == 1) %>%
      mutate(across(all_of(vars), ~ as.numeric(as.character(.)), .names = "num_{col}")) %>%
      summarise(across(starts_with("num_"), ~ sum(replace_na(., 0)))) %>%
      pivot_longer(cols = everything(), names_to = "category", values_to = "count") %>%
      filter(!is.na(category) & category %in% names(legend_labels)) %>%
      filter(count > 0) %>%
      mutate(percentage = round(100 * count / sum(count), 1))
    
    pie_data$category <- factor(pie_data$category, levels = names(legend_labels), labels = legend_labels)
    
    ggplot(pie_data, aes(x = reorder(category, count), y = count, fill = category)) +
      geom_bar(stat = "identity", width = 0.7) +
      geom_text(aes(label = paste0(count)), hjust = -0.1) +
      coord_flip() +
      labs(x = "Category", y = "Article count", fill = "Category") +
      scale_fill_manual(values = extended_palette) +
      theme_minimal() +
      theme(
        axis.text.y = element_text(size = 16),
        axis.text.x = element_text(size = 16),
        legend.position = "none",
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_blank()
      )
  })
  
  output$review_figure <- renderImage({
    # Path to the image
    list(
      src = "www/review_figure.png",   # Path to image inside www folder
      alt = "Review Figure",           # Alternative text in case the image doesn't load
      width = "100%",                  # Responsive width
      height = "auto"                  # Maintain aspect ratio
    )
  }, deleteFile = FALSE) 
  
  output$review_process <- renderImage({
    # Path to the image
    list(
      src = "www/review_process.png",   # Path to image inside www folder
      alt = "Article search and screening process",           # Alternative text in case the image doesn't load
      width = "100%",                  # Responsive width
      height = "auto"                  # Maintain aspect ratio
    )
  }, deleteFile = FALSE)
  
  
  # Treemap -----------------------------------------------------------------
  
  output$treemap_main <- renderPlot({
    req(data)
    
    treemap_data <- data %>%
      filter(poverty == 1) %>%
      select(
        m_mental_health, m_health, neigh_safety, housing, sleep, attachment,
        neglect, parenting, cognitive_enr, breastfeeding, support, stress,
        nutrition, toxins, substance, immune, placenta_brain, gut_microbiome,
        hpa_axis, synaptic, epigenetics, telomere, polygenic_snps
      ) %>%
      summarise(across(everything(), ~ sum(. == 1, na.rm = TRUE))) %>%
      pivot_longer(cols = everything(), names_to = "words", values_to = "freqs") %>%
      filter(freqs > 0) %>%
      mutate(category = case_when(
        words %in% external_system_vars ~ "External System",
        words %in% internal_system_vars ~ "Internal System",
        words %in% cellular_vars ~ "Cellular",
        TRUE ~ NA_character_
      )) %>%
      filter(!is.na(category))
    
    treemap_data$clean_words <- clean_labels[treemap_data$words]
    treemap_data$label <- paste0(treemap_data$clean_words, " (", treemap_data$freqs, ")")
    treemap_data$category <- factor(treemap_data$category, levels = c("External System", "Internal System", "Cellular"))
    
    ggplot(treemap_data, aes(area = freqs, fill = category, label = label)) +
      geom_treemap() +
      geom_treemap_text(
        colour = "white",
        place = "topleft",
        grow = FALSE,
        size = 16,
        fontface = "bold",
        reflow = TRUE
      ) +
      scale_fill_paletteer_d("rcartocolor::Antique") +
      labs(fill = "System level", title = "Distribution of Research Focus Areas") +
      theme(
        legend.position = "top",
        legend.text = element_text(size = 14, color = "#331718FF"),
        legend.title = element_blank(),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5, color = "#331718FF")
      )
  })
  
  # Venn diagram ------------------------------------------------------------
  
  output$venn_diagram <- renderPlot({
    selected_vars <- input$venn_vars
    num_selected <- length(selected_vars)
    
    if (num_selected >= 2 && num_selected <= 3) {
      venn_data <- data %>%
        filter(poverty == 1) %>%
        select(all_of(selected_vars)) %>%
        mutate(across(everything(), ~ as.numeric(as.character(.))))
      
      venn_list <- lapply(selected_vars, function(var) which(venn_data[[var]] == 1))
      
      renamed_vars <- sapply(selected_vars, function(var) {
        if (var %in% names(rename_map)) return(rename_map[[var]]) else return(var)
      })
      
      venn_colors <- c("#440154ff", '#21908dff', '#68855CFF')[1:num_selected]
      venn_fills <- sapply(venn_colors, function(color) alpha(color, 0.3))
      
      venn.plot <- venn.diagram(
        x = venn_list,
        category.names = renamed_vars,
        filename = NULL,
        output = TRUE,
        height = 480,
        width = 480,
        resolution = 300,
        lwd = 1,
        col = venn_colors,
        fill = venn_fills,
        cex = 1.5,
        fontfamily = "sans",
        cat.cex = 1.5,
        cat.default.pos = "outer",
        cat.pos = c(-27, 27, 135, 0)[1:num_selected],
        cat.dist = c(0.055, 0.055, 0.085, 0.05)[1:num_selected],
        cat.fontfamily = "sans",
        cat.col = venn_colors
      )
      grid.draw(venn.plot)
      
    } else {
      plot.new()
      if (num_selected > 3) {
        text(0.5, 0.5, "Venn Diagrams with more than 3 categories are not supported.", cex = 1.5)
      } else {
        text(0.5, 0.5, "Select at least 2 variables", cex = 1.5)
      }
    }
  }, height = dynamic_height(session, "venn_diagram"))
  
  # Icons ------------------------------------------------------------------
  
  
  #Icon images
  output$database_icon <- renderImage({
    list(
      src = "www/find.png",
      alt = "Screened Articles Icon",
      width = "30px",
      height = "auto"
    )
  }, deleteFile = FALSE)
  
  output$asreview_icon <- renderImage({
    list(
      src = "www/asreview.png",
      alt = "Relevant Articles Icon",
      width = "30px",
      height = "auto"
    )
  }, deleteFile = FALSE)
  
  output$screened_icon <- renderImage({
    list(
      src = "www/review.png",
      alt = "Relevant Articles Icon",
      width = "30px",
      height = "auto"
    )
  }, deleteFile = FALSE)
  
  output$relevant_icon <- renderImage({
    list(
      src = "www/checked.png",
      alt = "Relevant Articles Icon",
      width = "30px",
      height = "auto"
    )
  }, deleteFile = FALSE)
  
}

# Run the app
shinyApp(ui = ui, server = server)
