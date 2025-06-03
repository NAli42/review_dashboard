# 04_server_modules.R - Server Logic Functions
# This file contains all server module functions

# Helper function for creating consistent study tables
create_study_table <- function(data, height = "400px") {
# Clean authors display - fixed for []; [] format
  clean_authors <- function(author_string) {
    if (is.na(author_string) || author_string == "" || author_string == "Unknown") {
      return("Author not available")
    }
    
    # Convert to character
    author_string <- as.character(author_string)
    
    # First check if it's already in "et al." format
    if (grepl("et al\\.?", author_string, ignore.case = TRUE)) {
      # Extract the first author before "et al"
      first_part <- sub("\\s*et al\\.?.*$", "", author_string, ignore.case = TRUE)
      first_part <- gsub("\\[|\\]", "", first_part)  # Remove brackets
      first_author <- trimws(first_part)
      
      if (nzchar(first_author)) {
        return(paste0(first_author, " et al."))
      }
    }
    
    # Handle [author]; [author] format specifically
    if (grepl("\\[.*\\];\\s*\\[.*\\]", author_string)) {
      # Extract content between first set of brackets
      first_author_match <- regmatches(author_string, regexpr("\\[([^\\]]+)\\]", author_string))
      if (length(first_author_match) > 0) {
        # Remove the brackets
        first_author <- gsub("\\[|\\]", "", first_author_match[1])
        first_author <- trimws(first_author)
        
        # Check if there are multiple bracketed authors
        all_brackets <- gregexpr("\\[([^\\]]+)\\]", author_string)
        num_authors <- length(regmatches(author_string, all_brackets)[[1]])
        
        if (nzchar(first_author)) {
          if (num_authors > 1) {
            return(paste0(first_author, " et al."))
          } else {
            return(first_author)
          }
        }
      }
    }
    
    # Remove all types of brackets for other processing
    cleaned <- gsub("\\[|\\]|\\{|\\}|\\(|\\)", "", author_string)
    
    # Handle "and" separator
    cleaned <- gsub("\\s+and\\s+", ", ", cleaned, ignore.case = TRUE)
    cleaned <- gsub("\\s+&\\s+", ", ", cleaned)
    
    # Try to split by semicolon first (most common in your format)
    if (grepl(";", cleaned)) {
      authors_list <- strsplit(cleaned, ";")[[1]]
    } else if (grepl(",", cleaned)) {
      # For comma-separated, check if it's "Last, First" format
      parts <- strsplit(cleaned, ",")[[1]]
      if (length(parts) == 2 && nchar(trimws(parts[2])) < 10) {
        # Likely a single author in "Last, First" format
        authors_list <- paste(trimws(parts[1]), trimws(parts[2]), sep = ", ")
      } else {
        authors_list <- parts
      }
    } else {
      # Single author or use other separators
      authors_list <- strsplit(cleaned, "[|\\n\\r\\t]")[[1]]
    }
    
    # Clean up the list
    authors_list <- sapply(authors_list, trimws, USE.NAMES = FALSE)
    authors_list <- authors_list[authors_list != ""]
    authors_list <- authors_list[!is.na(authors_list)]
    
    if (length(authors_list) > 0) {
      first_author <- authors_list[1]
      
      # Basic cleaning of the first author name
      # Remove leading/trailing special characters but keep periods and hyphens
      first_author <- gsub("^[^a-zA-Z]+", "", first_author)
      first_author <- gsub("[^a-zA-Z\\s\\.-]+$", "", first_author)
      first_author <- trimws(first_author)
      
      # Make sure we have a valid name
      if (nzchar(first_author) && nchar(first_author) >= 2) {
        if (length(authors_list) > 1) {
          return(paste0(first_author, " et al."))
        } else {
          return(first_author)
        }
      }
    }
    
    # Last resort: extract first name-like pattern
    name_pattern <- regexpr("[A-Za-z][A-Za-z\\s\\.-]{1,30}", cleaned)
    if (name_pattern > 0) {
      name_match <- regmatches(cleaned, name_pattern)
      if (length(name_match) > 0) {
        return(paste0(trimws(name_match[1]), " et al."))
      }
    }
    
    return("Author not available")
  }
  
  # Prepare display data with country column
  display_data <- data.frame(
    Authors = sapply(data$authors, clean_authors),
    Year = ifelse(is.na(data$year), "N/A", as.character(data$year)),
    Country = ifelse(is.na(data$country) | data$country == "", "Not specified", as.character(data$country)),
    Title = ifelse(
      is.na(data$title) | data$title == "",
      "Title not available",
      str_trunc(as.character(data$title), 80)
    ),
    DOI = ifelse(
      is.na(data$doi) | data$doi == "",
      "No DOI",
      paste0("<a href='https://doi.org/", data$doi, "' target='_blank' style='color:",
             THEME_CONFIG$primary_colors[5], ";'>", data$doi, "</a>")
    ),
    stringsAsFactors = FALSE
  )
  
  # Create datatable with adjusted column widths
  datatable(
    display_data,
    escape = FALSE,
    filter = "top",
    extensions = c("Buttons"),
    options = list(
      pageLength = 10,
      scrollX = TRUE,
      dom = 'Bfrtip',
      buttons = c('copy', 'csv', 'excel'),
      columnDefs = list(
        list(width = '20%', targets = 0),  # Authors
        list(width = '8%', targets = 1),   # Year (narrower)
        list(width = '12%', targets = 2),  # Country
        list(width = '35%', targets = 3),  # Title
        list(width = '25%', targets = 4)   # DOI
      )
    )
  ) %>%
    formatStyle(
      columns = 1:5,
      backgroundColor = THEME_CONFIG$bg_primary,
      color = THEME_CONFIG$text_primary
    )
}

# Dashboard plots server logic (removed lollipop chart)
server_dashboard_plots <- function(input, output, session, data_list) {
  
  # Article count output
  output$poverty_article_count <- renderText({
    data_with_numeric <- data_list$main_data %>%
      mutate(poverty = as.numeric(as.character(poverty)))
    poverty_count <- sum(data_with_numeric$poverty == 1, na.rm = TRUE)
    paste("Poverty articles included =", poverty_count)
  })
  
  # Source interactive plotting functions
  if (!exists("create_poverty_overlap_plot_interactive")) {
    source("05_plotting_functions_interactive.R")
  }
  
  # Reactive value for current filter
  current_filter <- reactiveValues(type = "all", value = NULL, country = NULL)
  
  # Shared data source for consistency
  shared_data <- reactive({
    if (file.exists("raw_data/with_dois_names.csv")) {
      original_data <- read.csv("raw_data/with_dois_names.csv",
                                sep = ";", stringsAsFactors = FALSE, encoding = "UTF-8")
      
      keep <- !is.na(names(original_data)) & nzchar(names(original_data))
      original_data <- original_data[, keep]
      
      expected_cols <- c(
        "id", "inclusion", "title", "year", "authors", "doi", "paper_type",
        "poverty", "adversity", "country", "sample_comp", "exposure_period",
        "design", "sample_size", "poverty_oper", "early_life_adv", "support",
        "stress", "nutrition", "toxins", "substance", "m_depression",
        "m_mental_health", "m_health", "neigh_safety", "housing", "sleep",
        "preterm", "attachment", "neglect", "parenting", "cognitive_enr",
        "micronutrients", "breastfeeding", "immune", "placenta_brain",
        "gut_microbiome", "hpa_axis", "synaptic", "epigenetics", "telomere",
        "polygenic_snps", "outcome_beh", "outcome_neuro", "outcome_sga", "outcome_mobility"
      )
      
      if (ncol(original_data) >= length(expected_cols)) {
        names(original_data)[1:length(expected_cols)] <- expected_cols
      }
      
      return(original_data %>%
               filter(inclusion %in% c(1, "1", "yes"), poverty %in% c(1, "1")))
    } else {
      return(NULL)
    }
  })
  
  # Poverty overlap barplot
  output$poverty_adversity_barplot <- renderPlotly({
    create_poverty_overlap_plot_interactive(data_list$main_data)
  })
  
  # Handle click events
  observeEvent(event_data("plotly_click", source = "overlap_plot"), {
    click_data <- event_data("plotly_click", source = "overlap_plot")
    if (!is.null(click_data)) {
      current_filter$type <- "overlap"
      current_filter$value <- click_data$customdata
    }
  })
  
  # World map
  output$worldmap <- renderLeaflet({
    if (!exists("create_world_map")) {
      source("05_plotting_functions.R")
    }
    map_data <- process_map_data(data_list$main_data)
    create_world_map(map_data)
  })
  
  # Handle map click events for country filtering
  observeEvent(input$worldmap_marker_click, {
    click <- input$worldmap_marker_click
    
    tryCatch({
      if (!is.null(click)) {
        # Get the clicked country name
        country_name <- NULL
        
        # Method 1: From layerId (most reliable)
        if (!is.null(click$id) && !is.na(click$id) && nzchar(click$id)) {
          country_name <- click$id
        }
        
        # Method 2: From popup text
        if (is.null(country_name) || is.na(country_name) || !nzchar(country_name)) {
          if (!is.null(click$popup) && !is.na(click$popup) && nzchar(click$popup)) {
            country_name <- str_extract(click$popup, "^[^:]+")
            country_name <- str_trim(country_name)
          }
        }
        
        # Method 3: From label if popup doesn't work
        if (is.null(country_name) || is.na(country_name) || !nzchar(country_name)) {
          if (!is.null(click$label) && !is.na(click$label) && nzchar(click$label)) {
            country_name <- str_extract(click$label, "^[^:]+")
            country_name <- str_trim(country_name)
          }
        }
        
        if (!is.null(country_name) && !is.na(country_name) && nzchar(country_name)) {
          current_filter$type <- "country"
          current_filter$country <- country_name
          current_filter$value <- NULL  # Clear other filters
        }
      }
    }, error = function(e) {
      # Silently handle errors to prevent crash
      message("Map click error: ", e$message)
    })
  })
  
  # Network diagram
  output$network_diagram <- renderVisNetwork({
    create_network_diagram(data_list$main_data)
  })
  
  observeEvent(input$network_diagram_selected, {
    if (!is.null(input$network_diagram_selected)) {
      current_filter$type <- "variable"
      current_filter$value <- input$network_diagram_selected
    }
  })
  
  # Filter status
  output$filter_status <- renderText({
    if (current_filter$type == "all" || (is.null(current_filter$value) && is.null(current_filter$country))) {
      "Showing all poverty studies"
    } else if (current_filter$type == "overlap") {
      if (current_filter$value == "poverty_only") {
        "Filtered: Poverty only studies"
      } else {
        "Filtered: Poverty + Adversity studies"
      }
    } else if (current_filter$type == "category") {
      paste("Filtered by category:", current_filter$value)
    } else if (current_filter$type == "variable") {
      variable_label <- VARIABLE_LABELS[current_filter$value]
      if (!is.na(variable_label)) {
        paste("Filtered by variable:", variable_label)
      } else {
        paste("Filtered by variable:", current_filter$value)
      }
    } else if (current_filter$type == "country") {
      paste("Filtered by country:", current_filter$country)
    }
  })
  
  # Main filter table
  output$main_filter_table <- renderDT({
    tryCatch({
      base_data <- shared_data()
      if (is.null(base_data)) {
        return(datatable(data.frame(Error = "Data not available")))
      }
      
      filtered_data <- base_data
      
      # Apply filters based on current selection
      if (current_filter$type == "overlap" && !is.null(current_filter$value)) {
        if (current_filter$value == "poverty_only") {
          filtered_data <- filtered_data %>%
            filter(is.na(adversity) | adversity == 0 | adversity == "0")
        } else if (current_filter$value == "poverty_adversity") {
          filtered_data <- filtered_data %>%
            filter(adversity == 1 | adversity == "1")
        }
      } else if (current_filter$type == "category" && !is.null(current_filter$value)) {
        var_name <- names(VARIABLE_LABELS)[VARIABLE_LABELS == current_filter$value]
        if (length(var_name) > 0 && var_name %in% names(filtered_data)) {
          filtered_data <- filtered_data %>% filter(.data[[var_name]] == 1)
        }
      } else if (current_filter$type == "variable" && !is.null(current_filter$value)) {
        if (current_filter$value %in% names(filtered_data)) {
          filtered_data <- filtered_data %>% filter(.data[[current_filter$value]] == 1)
        }
      } else if (current_filter$type == "country" && !is.null(current_filter$country)) {
        # Create a list of possible country name variations to search for
        possible_names <- c(
          current_filter$country,  # The clicked name from map
          # Add common abbreviations/variations
          case_when(
            current_filter$country == "United States of America" ~ "US",
            current_filter$country == "United Kingdom" ~ "UK",
            current_filter$country == "Russian Federation" ~ "Russia",
            current_filter$country == "Venezuela (Bolivarian Republic)" ~ "Venezuela",
            current_filter$country == "Republic of Korea" ~ "Korea",
            TRUE ~ current_filter$country
          ),
          # Also check the reverse mappings
          case_when(
            current_filter$country == "US" ~ "United States of America",
            current_filter$country == "UK" ~ "United Kingdom", 
            current_filter$country == "Russia" ~ "Russian Federation",
            current_filter$country == "Venezuela" ~ "Venezuela (Bolivarian Republic)",
            current_filter$country == "Korea" ~ "Republic of Korea",
            TRUE ~ current_filter$country
          )
        )
        
        # Remove duplicates and filter
        possible_names <- unique(possible_names)
        filtered_data <- filtered_data %>% 
          filter(country %in% possible_names)
      }
      
      create_study_table(filtered_data, height = "350px")
      
    }, error = function(e) {
      datatable(data.frame(Error_Message = e$message))
    })
  })
  
  observeEvent(input$reset_filter, {
    current_filter$type <- "all"
    current_filter$value <- NULL
    current_filter$country <- NULL
  })
}

# Research focus areas server logic (fixed ordering to show highest count on top)
server_research_areas <- function(input, output, session, data_list) {
  
  # Reactive to store the clicked category label
  clicked_category <- reactiveVal(NULL)
  
  # When dropdown changes, clear clicked_category
  observeEvent(input$category, {
    clicked_category(NULL)
  })
  
  # Render interactive bar chart with proper ordering and colors
  output$category_barchart <- renderPlotly({
    if (!exists("custom_theme")) {
      source("05_plotting_functions.R")
    }
    
    vars <- switch(input$category,
                   "External system" = VARIABLE_CATEGORIES$external_system,
                   "Internal system" = VARIABLE_CATEGORIES$internal_system,
                   "Cellular" = VARIABLE_CATEGORIES$cellular,
                   "All" = unlist(VARIABLE_CATEGORIES))
    
    existing_vars <- intersect(vars, names(data_list$main_data))
    
    pie_data <- data_list$main_data %>%
      filter(poverty == 1) %>%
      mutate(across(all_of(existing_vars), ~ as.numeric(as.character(.)), .names = "num_{col}")) %>%
      summarise(across(starts_with("num_"), ~ sum(replace_na(., 0)))) %>%
      pivot_longer(cols = everything(), names_to = "category", values_to = "count") %>%
      filter(count > 0) %>%
      mutate(
        category_clean = gsub("num_", "", category),
        category_label = VARIABLE_LABELS[category_clean],
        # Add system level for coloring
        system_level = case_when(
          category_clean %in% VARIABLE_CATEGORIES$external_system ~ "External System",
          category_clean %in% VARIABLE_CATEGORIES$internal_system ~ "Internal System",
          category_clean %in% VARIABLE_CATEGORIES$cellular ~ "Cellular",
          TRUE ~ "Other"
        )
      ) %>%
      filter(!is.na(category_label)) %>%
      # Sort by count in descending order for highest on top
      arrange(desc(count)) %>%
      mutate(category_label = factor(category_label, levels = category_label))
    
    # Create plot with colors by system level - highest count on top
    plot_ly(
      pie_data,
      x = ~count,
      y = ~category_label,  # Use factor levels to maintain order
      type = 'bar',
      orientation = 'h',
      key = ~category_label,
      marker = list(color = ~THEME_CONFIG$system_colors[system_level]),
      hovertemplate = paste('<b>%{y}</b><br>Count: %{x}<br>System: ', pie_data$system_level, '<extra></extra>'),
      source = "research_bar"
    ) %>%
      layout(
        title = list(text = "Category occurrences across articles",
                     font = list(size = 18, color = THEME_CONFIG$text_primary)),
        xaxis = list(title = "Article count"),
        yaxis = list(
          title = "", 
          categoryorder = "array", 
          categoryarray = rev(levels(pie_data$category_label))  # Reverse to show highest on top
        ),
        plot_bgcolor = THEME_CONFIG$bg_primary,
        paper_bgcolor = THEME_CONFIG$bg_primary,
        font = list(family = THEME_CONFIG$font_family, color = THEME_CONFIG$text_primary),
        margin = list(l = 180)
      ) %>%
      event_register("plotly_click") %>%
      event_register("plotly_doubleclick")
  })
  
  # Capture click on bar
  observeEvent(event_data("plotly_click", source = "research_bar"), {
    click_data <- event_data("plotly_click", source = "research_bar")
    if (!is.null(click_data)) {
      clicked_category(click_data$key)
    }
  })
  
  # Reset on double-click
  observeEvent(event_data("plotly_doubleclick", source = "research_bar"), {
    clicked_category(NULL)
  })
  
  # Render filtered table
  output$research_table <- renderDT({
    vars <- switch(input$category,
                   "External system" = VARIABLE_CATEGORIES$external_system,
                   "Internal system" = VARIABLE_CATEGORIES$internal_system,
                   "Cellular" = VARIABLE_CATEGORIES$cellular,
                   "All" = unlist(VARIABLE_CATEGORIES))
    
    existing_vars <- intersect(vars, names(data_list$main_data))
    
    base_data <- data_list$main_data %>%
      filter(poverty == 1)
    
    if (!is.null(clicked_category())) {
      # If a bar is clicked, filter by that single category
      var_name <- names(VARIABLE_LABELS)[VARIABLE_LABELS == clicked_category()]
      if (length(var_name) > 0 && var_name %in% names(base_data)) {
        filtered_data <- base_data %>% filter(.data[[var_name]] == 1)
      } else {
        filtered_data <- base_data
      }
    } else {
      # Otherwise filter by any of the vars under the selected dropdown
      filtered_data <- base_data %>%
        filter(if_any(all_of(existing_vars), ~ . == 1))
    }
    
    create_study_table(filtered_data)
  })
}

# Venn diagram server logic (fixed to prevent file generation)
server_venn_diagram <- function(input, output, session, data_list) {
  
  output$venn_diagram <- renderPlot({
    selected_vars <- input$venn_vars
    num_selected <- length(selected_vars)
    
    if (num_selected >= 2 && num_selected <= 3) {
      venn_data <- data_list$main_data %>%
        filter(poverty == 1) %>%
        select(all_of(selected_vars)) %>%
        mutate(across(everything(), ~ as.numeric(as.character(.))))
      
      venn_list <- lapply(selected_vars, function(var) which(venn_data[[var]] == 1))
      names(venn_list) <- VARIABLE_LABELS[selected_vars]
      
      venn_colors <- THEME_CONFIG$primary_colors[1:num_selected]
      venn_fills <- sapply(venn_colors, function(color) alpha(color, 0.3))
      
      # Create venn diagram without file output
      venn.plot <- venn.diagram(
        x = venn_list,
        filename = NULL,  # This prevents file generation
        output = TRUE,
        height = 480,
        width = 480,
        resolution = 300,
        lwd = 2,
        col = venn_colors,
        fill = venn_fills,
        cex = 1.5,
        fontfamily = THEME_CONFIG$font_family,
        cat.cex = 1.25,
        cat.default.pos = "outer",
        cat.fontfamily = THEME_CONFIG$font_family,
        cat.col = venn_colors,
        # Improve label positioning
        cat.dist = if(num_selected == 2) c(0.08, 0.08) else c(0.08, 0.08, 0.08),
        cat.pos = if(num_selected == 2) c(-20, 20) else c(-40, 40, 180),
        margin = 0.1                       # Add margin
      )
      grid.draw(venn.plot)
    } else {
      plot.new()
      text(0.5, 0.5,
           ifelse(num_selected > 3,
                  "Venn Diagrams with more than 3 categories are not supported.",
                  "Select at least 2 variables"),
           cex = 1.5, col = THEME_CONFIG$text_primary)
    }
  })
  
  # Table for overlap studies
  output$overlap_table <- renderDT({
    selected_vars <- input$venn_vars
    
    if (length(selected_vars) >= 2) {
      filtered_data <- data_list$main_data %>%
        filter(poverty == 1) %>%
        filter(if_all(all_of(selected_vars), ~ . == 1))
      
      if (nrow(filtered_data) > 0) {
        create_study_table(filtered_data)
      } else {
        datatable(data.frame(Message = "No studies contain all selected variables"))
      }
    } else {
      datatable(data.frame(Message = "Select at least 2 variables to see overlapping studies"))
    }
  })
}

# Poverty measurements server logic
server_poverty_measures <- function(input, output, session, data_list) {
  
  # Process data
  df_processed <- reactive({
    process_poverty_opers(data_list$poverty_opers)
  })
  
  # Column groups
  fh_cols <- c(
    "Family SES (education + occupation)",
    "Family SES (education + income)",
    "Family SES (other mixed)",
    "Postcode-based family SES",
    "Country-specific SES scale"
  )
  
  mh_cols <- c(
    "Nutrition",
    "Food expenditure/insecurity",
    "Difficulties paying bills (self-reported)",
    "Welfare use"
  )
  
  index_cols <- c(
    "Hollingshead Index",
    "Area Deprivation Index (ADI)",
    "Human Development Index (HDI): country based"
  )
  
  flag_exclude <- c(
    "study_id", "Authors", "Country", "Country (index)",
    "Sample size", "Effect", "DOI",
    "Level (0 = mixed, 1 = external, 2 = internal, 3 = cellular)",
    fh_cols, mh_cols, index_cols
  )
  
  single_cols <- reactive({
    setdiff(names(df_processed()), flag_exclude)
  })
  
  # Long format data
  df_long <- reactive({
    df_processed() %>%
      select(
        study_id, Authors, Country, `Sample size`, Effect,
        Family_SES_flag, Material_hardship_flag,
        all_of(index_cols), all_of(single_cols())
      ) %>%
      pivot_longer(
        cols = -c(study_id, Authors, Country, `Sample size`, Effect),
        names_to = "Category",
        values_to = "Used"
      ) %>%
      filter(Used == 1) %>%
      mutate(
        Category = recode(
          Category,
          Family_SES_flag = "Family SES",
          Material_hardship_flag = "Material hardship"
        )
      )
  })
  
  # Filter function
  filter_by_cat <- function(data, sel) {
    if (is.null(sel)) return(data)
    if (sel == "Family SES") return(filter(data, Family_SES_flag))
    if (sel == "Material hardship") return(filter(data, Material_hardship_flag))
    filter(data, .data[[sel]] == 1)
  }
  
  # Bar chart data
  bar_studies <- reactive({
    if (!input$exclusive_bar) {
      unique(df_long()$study_id)
    } else {
      df_long() %>%
        count(study_id) %>%
        filter(n == 1) %>%
        pull(study_id)
    }
  })
  
  bar_data <- reactive({
    df_long() %>%
      filter(study_id %in% bar_studies()) %>%
      group_by(Category) %>%
      summarise(
        mean_N = mean(`Sample size`, na.rm = TRUE),
        count = n(),
        .groups = "drop"
      ) %>%
      mutate(Category = fct_reorder(Category, mean_N, .desc = TRUE))
  })
  
  # Click handling
  clicked_category <- reactiveVal(NULL)
  
  observeEvent(event_data("plotly_click", source = "bar_src"), {
    click_data <- event_data("plotly_click", source = "bar_src")
    clicked_category(if(!is.null(click_data)) click_data$key else NULL)
  })
  
  observeEvent(event_data("plotly_doubleclick", source = "bar_src"), {
    clicked_category(NULL)
  })
  
  # Bar chart
  output$poverty_bar <- renderPlotly({
    bd <- bar_data()
    max_range <- 10000
    
    plot_ly(
      bd,
      x = ~mean_N,
      y = ~Category,
      type = "bar",
      orientation = "h",
      key = ~Category,
      marker = list(color = THEME_CONFIG$accent_colors[1]),
      customdata = ~count,
      hovertemplate = paste0(
        "Category: %{y}<br>",
        "Mean N: %{x:.0f}<br>",
        "Studies: %{customdata}<extra></extra>"
      ),
      source = "bar_src"
    ) %>%
      layout(
        xaxis = list(
          title = "Mean sample size (capped at 10,000)",
          range = c(0, max_range)
        ),
        yaxis = list(
          title = list(text = "Measure category", standoff = 20),
          autorange = "reversed"
        ),
        margin = list(l = 140),
        plot_bgcolor = THEME_CONFIG$bg_primary,
        paper_bgcolor = THEME_CONFIG$bg_primary,
        font = list(family = THEME_CONFIG$font_family, color = THEME_CONFIG$text_primary)
      ) %>%
      event_register("plotly_click") %>%
      event_register("plotly_doubleclick")
  })
  
  # Pie chart data
  pie_data <- reactive({
    sel <- clicked_category()
    
    if (is.null(sel)) {
      df_processed() %>%
        count(Effect) %>%
        rename(label = Effect, value = n)
    } else {
      filter_by_cat(df_processed(), sel) %>%
        count(Effect) %>%
        rename(label = Effect, value = n)
    }
  })
  
  # Pie chart
  pie_colors <- c(
    Yes = THEME_CONFIG$accent_colors[1],
    No = THEME_CONFIG$primary_colors[3],
    Mixed = THEME_CONFIG$primary_colors[4],
    Contradicting = THEME_CONFIG$primary_colors[2]
  )
  
  output$poverty_pie <- renderPlotly({
    pd <- pie_data()
    plot_ly(
      pd,
      labels = ~label,
      values = ~value,
      type = "pie",
      marker = list(colors = pie_colors[pd$label]),
      textinfo = "percent+label",
      textposition = "inside",
      hoverinfo = "label+value"
    ) %>%
      layout(
        margin = list(l = 0, r = 0, t = 0, b = 0),
        plot_bgcolor = THEME_CONFIG$bg_primary,
        paper_bgcolor = THEME_CONFIG$bg_primary,
        font = list(family = THEME_CONFIG$font_family)
      )
  })
  
  # Data table with country column
  output$poverty_table <- renderDT({
    sel <- clicked_category()
    
    table_data <- if (is.null(sel)) {
      df_processed() %>% filter(study_id %in% bar_studies())
    } else {
      filter_by_cat(df_processed(), sel) %>% filter(study_id %in% bar_studies())
    }
    
    table_data %>%
      transmute(
        Study = Authors,
        Country = Country,
        Level = `Level (0 = mixed, 1 = external, 2 = internal, 3 = cellular)`,
        N = `Sample size`,
        Effect = Effect,
        DOI = paste0(
          "<a href='https://doi.org/", DOI,
          "' target='_blank' style='color:", THEME_CONFIG$primary_colors[1], ";'>",
          DOI, "</a>"
        )
      ) %>%
      datatable(
        filter = "top",
        escape = FALSE,
        extensions = c("Scroller"),
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          scrollY = "400px",
          scroller = TRUE,
          dom = 'Bfrtip',
          columnDefs = list(
            list(width = '20%', targets = 0),  # Study
            list(width = '15%', targets = 1),  # Country
            list(width = '10%', targets = 2),  # Level
            list(width = '10%', targets = 3),  # N
            list(width = '15%', targets = 4),  # Effect
            list(width = '30%', targets = 5)   # DOI
          )
        )
      ) %>%
      formatStyle(
        columns = 1:6,
        backgroundColor = THEME_CONFIG$bg_primary,
        color = THEME_CONFIG$text_primary
      )
  })
}

# Static images server logic
server_static_images <- function(input, output, session) {
  
  # Icon images
  output$database_icon <- renderImage({
    list(src = "www/find.png", alt = "Database Icon", width = "30px", height = "auto")
  }, deleteFile = FALSE)
  
  output$asreview_icon <- renderImage({
    list(src = "www/asreview.png", alt = "ASReview Icon", width = "30px", height = "auto")
  }, deleteFile = FALSE)
  
  output$screened_icon <- renderImage({
    list(src = "www/review.png", alt = "Screened Icon", width = "30px", height = "auto")
  }, deleteFile = FALSE)
  
  output$relevant_icon <- renderImage({
    list(src = "www/checked.png", alt = "Relevant Icon", width = "30px", height = "auto")
  }, deleteFile = FALSE)
  
  output$zpl_icon <- renderImage({
    list(src = "www/zpl.png", alt = "Zero Poverty Lab Icon", width = "50px", height = "auto")
  }, deleteFile = FALSE)
  
  # Conceptual model images
  output$review_figure <- renderImage({
    list(src = "www/review_figure.png", alt = "Review Figure", width = "100%", height = "auto")
  }, deleteFile = FALSE)
  
  output$review_process <- renderImage({
    list(src = "www/review_process.png", alt = "Review Process", width = "100%", height = "auto")
  }, deleteFile = FALSE)
}