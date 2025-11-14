library(shiny)
library(shinydashboard)
library(sf)
library(leaflet)
library(classInt)
library(dplyr)
library(DT)
library(ggplot2)
library(mapview)

## User interface setup

ui <- dashboardPage(
  dashboardHeader(title = "SAP Basin Ranking App",
                  titleWidth = 450),
  dashboardSidebar(
    div(
      style = "background-color:#9AC0CD; color:black; height:100%;",
      sidebarMenu(
        menuItem("Map and Table", tabName = "maptable", icon = icon("map"), selected = TRUE)
      ),
      
      # Inputs accordion
      tags$div(
        class = "panel-group",
        tags$div(
          class = "panel panel-default",
          tags$div(
            class = "panel-heading",
            tags$h4(class = "panel-title",
                    tags$a("Inputs", "data-toggle" = "collapse", href = "#inputsPanel",
                           style = "color:black;font-size:18px;font-weight:bold;"))
          ),
          tags$div(
            id = "inputsPanel", class = "panel-collapse collapse in",
            tags$div(class = "panel-body",
                     fileInput("csvfile", "Upload CSV File", accept = ".csv"),
                     fileInput("shpzip", "Upload Zipped Shapefile (.zip)", accept = ".zip")
            )
          )
        )
      ),
      
      # Score Attribute Selector
      tags$div(
        class = "panel-group",
        tags$div(
          class = "panel panel-default",
          tags$div(
            class = "panel-heading",
            tags$h4(class = "panel-title",
                    tags$a("Metric Selector", "data-toggle" = "collapse", href = "#attrPanel",
                           style = "color:black;font-size:18px;font-weight:bold;"))
          ),
          tags$div(
            id = "attrPanel", class = "panel-collapse collapse in",
            tags$div(
              class = "panel-body",
              uiOutput("attr_selector")
            )
          )
        )
      ),
      
      
      # Multipliers accordion
      tags$div(
        class = "panel-group",
        tags$div(
          class = "panel panel-default",
          tags$div(
            class = "panel-heading",
            tags$h4(class = "panel-title",
                    tags$a("Metric Multipliers", "data-toggle" = "collapse", href = "#multPanel",
                           style = "color:black;font-size:18px;font-weight:bold;"))
          ),
          tags$div(
            id = "multPanel", class = "panel-collapse collapse in",
            tags$div(class = "panel-body", uiOutput("multiplier_inputs"))
          )
        )
      ),
      # Break sliders accordion
      tags$div(
        class = "panel-group",
        tags$div(
          class = "panel panel-default",
          tags$div(
            class = "panel-heading",
            tags$h4(class = "panel-title",
                    tags$a("Rank Thresholds", "data-toggle" = "collapse", href = "#breakPanel",
                           style = "color:black;font-size:18px;font-weight:bold;"))
          ),
          tags$div(
            id = "breakPanel", class = "panel-collapse collapse in",
            tags$div(class = "panel-body",
                     uiOutput("break_sliders_ui"),
                     tags$small(
                       "Low: ≤ 1st break, Medium: > 1st break and ≤ 2nd break, High: > 2nd break"
                     )
            )
          )
        )
      ),
      hr(),
      
      # Add basin count text to side panel
      textOutput("total_basin_count"),
      hr(),
      
      
      downloadButton("downloadData", "Download Ranked Results CSV",
                     style = "color: white; 
                   background-color: #007BFF;
                   border-color: #007BFF; 
                   font-weight: bold;
                   font-size: 14px;")
    )
  ),
  dashboardBody(
    # Sidebar color override CSS
    tags$style(HTML("
      .skin-blue .main-sidebar, .skin-blue .left-side {
        background-color: #ADD8E6 !important;
      }
      .skin-blue .sidebar-menu>li>a, .skin-blue .sidebar-menu>li>a span, 
      .skin-blue .sidebar-menu>li.active>a {
        color: #000 !important;
        font-weight: bold;
      }
    ")),
    tabItems(
      tabItem(tabName = "maptable",
              fluidRow(
                box(width = 12, leafletOutput("map", height = 600),
                    # Map export button
                    downloadButton("download_map_html", "Download Map as HTML",
                                   style = "color:white;background-color:#008CBA;font-weight:bold;padding:10px 20px;") 
                )
              ),
              fluidRow(
                box(width = 12,
                    # Basin Rank Table Accordion
                    tags$div(
                      class = "panel-group",
                      tags$div(
                        class = "panel panel-default",
                        tags$div(
                          class = "panel-heading",
                          tags$h4(class = "panel-title",
                                  tags$a("Basin Rank Table", "data-toggle" = "collapse", href = "#tablePanel",
                                         style = "color:black;font-size:18px;font-weight:bold;"))
                        ),
                        tags$div(
                          id = "tablePanel", class = "panel-collapse collapse",
                          tags$div(class = "panel-body", DT::dataTableOutput("ranked_table"))
                        )
                      )
                    )
                )
              ),
              fluidRow(
                box(width = 12,
                    # Ranking Chart Accordion
                    tags$div(
                      class = "panel-group",
                      tags$div(
                        class = "panel panel-default",
                        tags$div(
                          class = "panel-heading",
                          tags$h4(class = "panel-title",
                                  tags$a("Ranking Chart", "data-toggle" = "collapse", href = "#chartPanel",
                                         style = "color:black;font-size:18px;font-weight:bold;"))
                        ),
                        tags$div(
                          id = "chartPanel", class = "panel-collapse collapse",
                          tags$div(class = "panel-body", plotOutput("score_plot", height = "800px"))
                        )
                      )
                    )
                )
              )
      )
    )
  )
)


## Server Setup

server <- function(input, output, session) {
  
  data <- reactive({
    req(input$csvfile)
    read.csv(input$csvfile$datapath, check.names = FALSE, stringsAsFactors = FALSE)
  })
  
  # Reactive: All numeric columns available for selection
  numeric_cols <- reactive({
    df <- data()
    if (is.null(df)) return(NULL)
    num_cols <- names(df)[sapply(df, is.numeric)]
    setdiff(num_cols, names(df)[1]) # Exclude first column (e.g. Basin)
  })
  
  output$attr_selector <- renderUI({
    cols <- numeric_cols()
    req(cols)
    checkboxGroupInput(
      inputId = "selected_attrs",
      label = "Select Metrics to Include in Total Score",
      choices = cols,
      selected = cols  # default all selected
    )
  })
  
  output$multiplier_inputs <- renderUI({
    req(numeric_cols())
    lapply(numeric_cols(), function(col) {
      numericInput(
        inputId = paste0("mult_", col),
        label = paste(col, "Multiplier"),
        value = 1,
        min = 0
      )
    })
  })
  
  shapefile <- reactive({
    req(input$shpzip)
    temp_dir <- tempdir()
    # Remove old shapefile content in temp_dir before unzipping the new upload
    # Remove only files, not folders/subdirectories
    file.remove(list.files(temp_dir, full.names = TRUE))
    unzip(input$shpzip$datapath, exdir = temp_dir)
    shp_path <- list.files(temp_dir, pattern = "\\.shp$", full.names = TRUE)
    if (length(shp_path) == 0) {
      stop("No shapefile (.shp) found in the uploaded zip")
    }
    sf::st_read(shp_path[1], quiet = TRUE) %>% 
      sf::st_transform(4326)
  })
  
  
  ranked_data <- reactive({
    df <- data()
    cols <- numeric_cols()
    selected_cols <- input$selected_attrs
    req(cols, selected_cols)
    use_cols <- intersect(cols, selected_cols)
    weighted_df <- df[, c(names(df)[1], use_cols), drop = FALSE]
    for (col in use_cols) {
      multiplier <- input[[paste0("mult_", col)]]
      if (is.null(multiplier) || multiplier == "" || is.na(multiplier)) {
        multiplier <- 1
      }
      weighted_df[[col]] <- weighted_df[[col]] * multiplier
    }
    weighted_df$`Total Score` <- rowSums(weighted_df[, use_cols], na.rm = TRUE)
    weighted_df %>%
      arrange(desc(`Total Score`)) %>%
      mutate(Rank = row_number()) 
  })
  
  default_breaks <- reactive({
    scores <- ranked_data()[["Total Score"]]
    if (length(scores) == 0) return(NULL)
    quantile(scores, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE)
  })
  
  tags$small(
    "Low: ≤ first break, Medium: > first break and ≤ second break, High: > second break"
  )
  
  
  output$break_sliders_ui <- renderUI({
    req(default_breaks())
    breaks <- default_breaks()
    tagList(
      sliderInput("break1", "Low/Medium Break:",
                  min = floor(breaks[1]),
                  max = ceiling(breaks[4]),
                  value = breaks[2], step = 0.5),
      sliderInput("break2", "Medium/High Break:",
                  min = floor(breaks[1]),
                  max = ceiling(breaks[4]),
                  value = breaks[3], step = 0.5)
    )
  })
  
  pal <- reactive({
    req(input$break1, input$break2)
    bins <- sort(c(0, input$break1, input$break2, Inf))
    colorBin(
      palette = c("red", "yellow", "olivedrab3"),
      domain = ranked_data()$`Total Score`,
      bins = bins,
      na.color = "transparent",
      right = TRUE
    )
  })
  
  
  joined_sf <- reactive({
    req(shapefile(), ranked_data())
    shp <- shapefile()
    rankdf <- ranked_data()
    if (!("Basin" %in% names(shp))) {
      stop("Shapefile must contain a 'Basin' column matching CSV first column")
    }
    shp %>% left_join(rankdf, by = setNames(names(rankdf)[1], "Basin"))
  })
  
  output$map <- renderLeaflet({
    shp <- joined_sf()
    req(shp)
    pal_func <- pal()
    
    leaflet(shp) %>%
      clearShapes() %>% 
      addProviderTiles("Esri.WorldTopoMap", group = "Base Map") %>%
      addPolygons(
        fillColor = ~pal_func(`Total Score`),
        weight = 2,
        opacity = 1,
        color = "black",
        dashArray = "3",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 3,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.5,
          bringToFront = TRUE),
        label = ~paste0(Basin, ": ", round(`Total Score`, 2)),
        group = "Basins"
      ) %>%
      addLabelOnlyMarkers(
        data = shp,
        lng = ~st_coordinates(st_centroid(geometry))[,1],
        lat = ~st_coordinates(st_centroid(geometry))[,2],
        label = ~Basin,
        labelOptions = labelOptions(noHide = TRUE, 
                                    direction = "center",
                                    textOnly = TRUE, 
                                    textsize = "11px", # Sets font size, label box auto-sizes around it
                                    style = list(
                                      "font-weight" = "bold",
                                      "width" = "auto",
                                      "color" = "black",
                                      # "font-family" = "serif",
                                      # "font-size" = "12px",
                                      "text-shadow" = "-1px -1px 0 white,  
                                                        1px -1px 0 white,
                                                       -1px 1px 0 white,
                                                        1px 1px 0 white")),
        group = "Labels"
      ) %>%
      addLegend(
        colors = c("green", "yellow", "red"),
        labels = c("High", "Medium", "Low"),
        title = "Basin Rank",
        position = "bottomright") %>% 
      addLayersControl(
        baseGroups = c("Base Map"),
        overlayGroups = c("Basins", "Labels"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
  
  output$ranked_table <- DT::renderDataTable({
    df <- ranked_data()
    req(df)
    bins <- sort(c(0, input$break1, input$break2, Inf))
    colors <- c("red", "yellow", "green")
    
    # Add ColorClass for row coloring
    df$ColorClass <- cut(
      df$`Total Score`,
      breaks = bins,
      labels = colors,
      include.lowest = TRUE,   # includes the lowest value in the first bin
      right = TRUE             # upper boundary is inclusive (x <= bin[i+1])
    )
    
    
    # Add Rank as first column
    df$Rank <- seq_len(nrow(df))
    df <- df[, c("Rank", setdiff(names(df), c("Rank", "ColorClass")), "ColorClass")]
    
    # JS callback to color rows
    rowCallback <- DT::JS(
      "function(row, data, index) {",
      "  var color = data[data.length - 1];",
      "  if(color == 'red') { $(row).css('background-color', '#ffcccc'); }",
      "  else if(color == 'yellow') { $(row).css('background-color', '#ffffcc'); }",
      "  else if(color == 'green') { $(row).css('background-color', '#ccffcc'); }",
      "}"
    )
    
    # Hide ColorClass column by index
    DT::datatable(
      df,
      options = list(
        pageLength = 25,
        rowCallback = rowCallback,
        columnDefs = list(
          list(visible = FALSE, targets = which(names(df) == "ColorClass"))
        )
      )
    )
  })
  
  output$score_plot <- renderPlot({
    df <- ranked_data()
    req(df)
    bins <- sort(c(-Inf, input$break1, input$break2, Inf))
    colors <- c("red", "yellow", "green")
    
    # Assign color class for break bins
    df$ColorClass <- cut(
      df$`Total Score`,
      breaks = bins,
      labels = colors,
      include.lowest = TRUE,
      right = TRUE
    )
    
    # Keep basins in order by rank for plotting
    df$Basin <- factor(df$Basin, levels = df$Basin[order(df$Rank)])
    
    # Set legend labels
    legend_labels <- c(red = "Low", yellow = "Medium", green = "High")
    
    # Map fill colors to break bins
    color_map <- c(red = "red", yellow = "yellow", green = "green")
    
    # Lollipop chart
    ggplot(df, aes(x = Basin, y = `Total Score`, color = ColorClass)) +
      geom_segment(aes(xend = Basin, y = 0, yend = `Total Score`), size = 1.5) +
      geom_point(size = 5) +
      coord_flip() +
      # scale_y_continuous(limits = c(0,0) +
      scale_color_manual(
        values = color_map,
        name = "Score Group",
        labels = legend_labels
      ) +
      theme_linedraw() +
      theme(
        legend.position = "top",
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 18),
        axis.title.x = element_text(size = 18),
        axis.text.x = element_text(size = 16)
      ) +
      labs(x = "Basin", y = "Total Score", title = "")
  })
  
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("ranked_results_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(ranked_data(), file, row.names = FALSE)
    }
  )
  
  
  output$total_basin_count <- renderText({
    count <- nrow(ranked_data())
    paste("Total Basin Count:", count)
  })
  
  output$download_map_html <- downloadHandler(
    filename = function() {
      paste0("watershed_map_", Sys.Date(), ".html")
    },
    content = function(file) {
      shp <- joined_sf()
      req(shp)
      pal_func <- pal()
      
      m <-  leaflet(shp) %>%
        clearShapes() %>% 
        addProviderTiles("Esri.WorldTopoMap", group = "Base Map") %>%
        addPolygons(
          fillColor = ~pal_func(`Total Score`),
          weight = 2,
          opacity = 1,
          color = "black",
          dashArray = "3",
          fillOpacity = 0.7,
          highlight = highlightOptions(
            weight = 3,
            color = "#666",
            dashArray = "",
            fillOpacity = 0.5,
            bringToFront = TRUE),
          label = ~paste0(Basin, ": ", round(`Total Score`, 2)),
          group = "Basins"
        ) %>%
        addLabelOnlyMarkers(
          data = shp,
          lng = ~st_coordinates(st_centroid(geometry))[,1],
          lat = ~st_coordinates(st_centroid(geometry))[,2],
          label = ~Basin,
          labelOptions = labelOptions(noHide = TRUE, 
                                      direction = "center",
                                      textOnly = TRUE, 
                                      textsize = "14px", # Sets font size, label box auto-sizes around it
                                      style = list(
                                        "font-weight" = "bold",
                                        "width" = "auto",
                                        "color" = "black",
                                        # "font-family" = "serif",
                                        # "font-size" = "12px",
                                        "text-shadow" = "-1px -1px 0 white,  
                                                        1px -1px 0 white,
                                                       -1px 1px 0 white,
                                                        1px 1px 0 white")),
          group = "Labels"
        ) %>%
        addLegend(
          colors = c("green", "yellow", "red"),
          labels = c("High", "Medium", "Low"),
          title = "Basin Rank",
          position = "bottomright") %>% 
        addLayersControl(
          baseGroups = c("Base Map"),
          overlayGroups = c("Basins", "Labels"),
          options = layersControlOptions(collapsed = TRUE)
        )
      # Save as html file
      mapview::mapshot(m, url = file, selfcontained = TRUE)
    }
  )
  
  
}

# Launch Shiny App

shinyApp(ui, server)