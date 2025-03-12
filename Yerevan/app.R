library(shiny)
library(httr2)
library(dplyr)
library(lubridate)
library(leaflet)
library(DT)
library(plotly)
library(crosstalk)
library(htmlwidgets)
library(bslib)
library(tidyr)
library(data.table)
library(bsicons)

org <- "techno6IOV"
target_metric <- "pm2_5ConcMass1HourMean"
tz <- "Asia/Yerevan"


construction_sites <- fluidPage(
  tags$script(HTML(
    "Shiny.addCustomMessageHandler('scrollToRow', function(rowIndex) {
      var table = $('#table table').DataTable();
      var rowNode = table.row(rowIndex).node();
      if (rowNode) {
        rowNode.scrollIntoView({ behavior: 'smooth', block: 'center' });
      }
    });
    "
  )),
  tags$head(
    tags$style(HTML("
      table.dataTable tr.selected td, table.dataTable tr.selected {
        box-shadow: inset 0 0 0 9999px clarityBlue !important;
        color: white !important;
      }         
      /* Let the html/body fill the entire browser area */
      html, body {
        # height: 100%;
        margin: 0;
        padding: 0;
      }


    /* Make the sidebar fill the viewport height minus title bar */
      #side-panel {
        height: calc(100vh - 135px);
        overflow-y: auto;
      }
      

      /* Main panel also fills that same vertical space */
      #main-panel {
        height: calc(100vh - 135px);
        display: flex;
        flex-direction: column;
        margin: 0;
        padding: 0;
      }

      /* Divide main-panel into two flex children (map + plot),
         each taking 50% of the height. */
      #map-container, #plotly-container {
        flex: 1 1 50%;
        display: flex;
        flex-direction: column;
        overflow: hidden; /* no scrolling in each half */
      }

      /* Force Leaflet and Plotly to fill their parent's space */
      #map, #time_series_plot {
        flex: 1;
        width: 100%;
        height: 100% !important; /* override defaults */
      }

      /* For Plotly specifically, ensure the .plotly/html-widget container is also forced to 100% */
      .plotly.html-widget {
        height: 100% !important;
        width: 100% !important;
      }
    "))
  ),
  
  h4(HTML(paste0("Construction Site PM",tags$sub("2.5"), ' Excess Above Background Tool'))),
  sidebarLayout(
    sidebarPanel(
      width = 5,
      id = "side-panel",
      fluidRow(
        column(3, actionButton("refresh", "Refresh Data")),
        column(9, selectInput("background_def",
                              span(
                                'Select "Background" Defintion',
                                tooltip(
                                  bs_icon("info-circle"),
                                  HTML("Where <i>Excess = Concentration - Background</i>"),
                                  placement = "bottom"
                                )
                              ),
                              choices = c("15th Percentile Within 2 km",
                                          "15th Percentile Across All of Yerevan",
                                          "Minimum by Site"),
                              selected = "15th Percentile Within 2 km"))
      ),
      DTOutput("table")
    ),
    
    mainPanel(
      width = 7,
      id = "main-panel",
      
      # Upper half: the map
      div(
        id = "map-container",
        leafletOutput("map")
      ),
      
      # Lower half: the plotly chart
      div(
        id = "plotly-container",
        plotlyOutput("time_series_plot", height = "100%", width = "100%")
      )
    )
  )
  # )
  
)

ui <- navbarPage(span(img(src = "clarity-logo.png", height = 25), "Yerevan Custom Dashboard"),
                 theme = bs_theme(brand = TRUE, 
                                  "navbar-bg" = "#F1F9FF"),
                 tabPanel("Construction Sites", construction_sites)
)

server <- function(input, output, session) {
  
  ### FUNCTIONS ###
  showPopup <- function(selected_data) {
    leafletProxy("map") %>%
      clearPopups() %>%  # Clear existing popups
      addPopups(
        lng = selected_data$lon,
        lat = selected_data$lat,
        popup = paste0(
          "<h5>", selected_data$Name, "</h5>",
          "<b>Datasource ID: </b>", selected_data$`Datasource ID`, "<br>",
          "<b>Last Reported At: </b>", selected_data$Date, "<br>",
          "<b>Excess: </b>", selected_data$Excess, " µg/m³", "<br>",
          "<b>Concentration: </b>", selected_data$Value, " µg/m³"
        )
      )
    
    # Store the last popup location
    last_popup(list(lon = selected_data$lon, lat = selected_data$lat))
  }
  
  
  # Function to query API using httr2 and parse the response
  
  clarityAPIRequestBase <- function() {
    api_base <- "https://clarity-data-api.clarity.io"
    
    request(api_base) %>%
      req_headers(`x-api-key` = Sys.getenv("API_KEY"), .redact = "x-api-key") %>%
      httr2::req_error(is_error = function(resp) FALSE)
  }
  
  clarityAPIPerformRequest <- function(req){
    resp <- req %>% req_perform()
    
    if (httr2::resp_is_error(resp)) {
      stop("API request failed with status: ", httr2::resp_status_desc(resp))
    } else {
      if (httr2::resp_content_type(resp) == "text/csv") {
        respData <- readr::read_csv(httr2::resp_body_string(resp), show_col_types = FALSE)
      } else {
        respData <- httr2::resp_body_json(resp, simplifyVector = TRUE)
      }
      return(respData)
    }
  }
  
  requestDatasourcesPerOrgSummary <- function(org) {
    req <- clarityAPIRequestBase() %>%
      req_template("GET v2/datasources?org=:org") 
    
    clarityAPIPerformRequest(req)
    
  }
  
  requestRecentMeasurements <- function(org, metric_select) {
    req <- clarityAPIRequestBase() %>%
      req_template("POST v2/recent-datasource-measurements-query") %>%
      req_body_json(list(
        "org" = org,
        "allDatasources" = TRUE,
        "outputFrequency" = "hour",
        "qcAssessment" = TRUE,
        "metricSelect" = metric_select
      )) 
    
    clarityAPIPerformRequest(req)
  }
  
  # Function to get most recent data
  loadData <- function(org, target_metric, tz) {
    datasources <- requestDatasourcesPerOrgSummary(org)
    recentMeasurements <- requestRecentMeasurements(org, 
                                                    metric_select = 
                                                      paste("none +", target_metric))
    
    locations <- recentMeasurements$locations
    datasourceNames <- datasources$datasources %>%
      unnest(orgAnnotations) %>%
      select(datasourceId, Name = name)
    
    recentDataProcessed <- recentMeasurements$data %>%
      filter(qcAssessment == "valid", status == "calibrated-ready") %>%
      select(-raw) %>%
      mutate(Date = lubridate::ymd_hms(time) %>% with_tz(tz)) %>%
      left_join(datasourceNames, by = "datasourceId") %>%
      left_join(locations, by = "datasourceId") %>%
      rename(
        `Datasource ID` = datasourceId,
        `Metric` = metric,
        `Value` = value,
        `QC Assessment` = qcAssessment
      ) %>%
      relocate(Name) 
    
    return(recentDataProcessed)
  }
  
  # Define the haversine distance function
  haversine_distance <- function(lon1, lat1, lon2, lat2) {
    R <- 6371000  # Earth's radius in meters
    # Convert degrees to radians
    lon1 <- lon1 * pi / 180
    lat1 <- lat1 * pi / 180
    lon2 <- lon2 * pi / 180
    lat2 <- lat2 * pi / 180
    # Compute differences
    dlon <- lon2 - lon1
    dlat <- lat2 - lat1
    # Haversine formula
    a <- sin(dlat / 2)^2 + cos(lat1) * cos(lat2) * sin(dlon / 2)^2
    c <- 2 * atan2(sqrt(a), sqrt(1 - a))
    R * c
  }
  
  get_text_color <- function(hex_colors) {
    # Convert hex to RGB channels
    rgb_mat <- col2rgb(hex_colors)
    # Compute luminance/brightness using standard formula
    brightness <- 0.299*rgb_mat[1, ] + 0.587*rgb_mat[2, ] + 0.114*rgb_mat[3, ]
    ifelse(brightness > 127, "black", "white")
  }
  
  # Reactive value holding the recent data
  recent <- reactiveVal()
  last_popup <- reactiveVal(NULL)

  # Load data on startup
  observeEvent(TRUE, {
    recent(loadData(org, target_metric, tz))
  }, once = TRUE
  )
  
  # Refresh data when button is clicked
  observeEvent(input$refresh, {
    recent(loadData(org, target_metric, tz))
  })
  excessCalcd <- reactive({
    # Work on a copy of your data
    dt <- setDT(recent())
    
    # Calculate background across Yerevan (15th percentile across group)
    dt[, `Background Across Yerevan` := quantile(Value, probs = 0.15, na.rm = TRUE), by = .(Date, Metric)]
    
    # Calculate background within 2 km (15th percentile within each row's 2 km radius)
    dt[, `Background within 2 km` := sapply(seq_len(.N), function(j) {
      dists <- haversine_distance(lon[j], lat[j], .SD$lon, .SD$lat)
      nearby_values <- .SD$Value[dists <= 2000]
      if (length(nearby_values) == 0) NA_real_ else quantile(nearby_values, probs = 0.15, na.rm = TRUE)
    }), by = .(Date, Metric)]
    
    # Calculate background by site (minimum Value for each Date, Metric, and Name)
    dt[, `Background By Site` := min(Value, na.rm = TRUE), by = .(Date, Metric, Name)]
    
    # Select the appropriate background column based on the user's choice
    dt[, Background := if (input$background_def == "15th Percentile Across All of Yerevan") {
      `Background Across Yerevan`
    } else if (input$background_def == "15th Percentile Within 2 km") {
      `Background within 2 km`
    } else {
      `Background By Site`
    }]
    
    # Compute Excess as Value minus Background
    dt[, Excess := Value - Background]
    dt[, `:=`(Value = round(Value, 1), Excess = round(Excess, 1))]
    
    as.data.frame(dt)
  })
  
  mostRecent <- reactive({
    req(excessCalcd())
    excessCalcd() %>%
      group_by(`Datasource ID`) %>%
      filter(Date == max(Date)) %>%
      ungroup() %>%
      arrange(desc(`Excess`))
  })
  
  sharedTimeSeries <- reactive({
    req(excessCalcd())
    SharedData$new(excessCalcd()%>%
                     select(`Datasource ID`,Name, Date, Metric, Concentration = Value, Excess) %>%
                     tidyr::pivot_longer(cols = -c(`Datasource ID`,Name, Date, Metric), values_to = "Value"),
                   key = ~`Datasource ID`, group = "ts")
  })
  # sharedTimeSeries <- recentDataProcessed %>%
  #   select(`Datasource ID`,Name, Date, Metric, Concentration = Value, Excess) %>%
  #   tidyr::pivot_longer(cols = -c(`Datasource ID`,Name, Date, Metric), values_to = "Value")
  
  sharedMap <- reactive({
    req(mostRecent())
    SharedData$new(mostRecent() %>% arrange(Excess), key = ~`Datasource ID`, group = "map")
  })
  
  sharedTable <-reactive({
    req(mostRecent())
    SharedData$new(
      mostRecent() %>% 
        select(Site = Name, `Datasource ID`, `Concentration µg/m³` = Value, `Excess µg/m³` = Excess, `Last Reported` = Date)
      , key = ~`Datasource ID`, group = "table")
  })
  # sharedTable <- sharedMap %>% 
  #   select(`Datasource ID`, `Last Reported` = Date, Value, `Excess`)
  valPal <- reactive({
    req(mostRecent())
    
    colorNumeric("viridis", mostRecent()$`Excess`)
  })
  
  # Render the data table (linked to shared data)
  output$table <- DT::renderDataTable({
    datatable(
      sharedTable() ,
      selection = "single",
      options = list(paging = FALSE, autoWidth = TRUE,
                     order = list(list(3, "desc")),
                     scrollY = "calc(100vh - 370px)",
                     info = FALSE, rowId = ~`Datasource ID`
      ),
      extensions = "Scroller",
      class = "compact",
      rownames = FALSE
    ) %>%
      formatDate("Last Reported", "toLocaleString",
                 params = list(
                   # First item: the locale, e.g. "en-US"
                   "en-US",
                   # Second item: a list of options, including timeZone
                   list(
                     hour12        = FALSE,
                     timeZone      = "Asia/Yerevan",
                     timeZoneName  = "short",
                     year          = "numeric",
                     month         = "2-digit",
                     day           = "2-digit",
                     hour          = "2-digit",
                     minute        = "2-digit"
                   )
                 )) %>%
      formatStyle('Excess µg/m³', backgroundColor = styleEqual(mostRecent()$Excess,valPal()(mostRecent()$Excess)),
                  color = styleEqual(mostRecent()$Excess, get_text_color(valPal()(mostRecent()$Excess))))
  }, server = FALSE)
  
  # Render the leaflet map (linked to shared data)
  output$map <- renderLeaflet({
    
    leaflet(sharedMap()) %>%
      addProviderTiles("Stadia.AlidadeSmooth") %>%
      addCircleMarkers(
        lng = ~lon,
        lat = ~lat,
        layerId = ~`Datasource ID`,
        fillColor = ~valPal()(`Excess`),
        weight = 1,
        label = ~`Excess`,
        color = "#333",
        fillOpacity = 0.9
      ) %>%
      addLegend(
        "bottomright",
        pal = valPal(),
        values = ~`Excess`,
        title = "Excess (µg/m³)",
        opacity = 1
      )
  })
  
  output$time_series_plot <- renderPlotly({
    gg <- ggplot(sharedTimeSeries(),
                 aes(x = Date, y = Value, group = `Datasource ID`, label = Name)) +
      geom_line(alpha = 0.6, color = "#464646") +
      facet_wrap(~name, ncol = 1, scales = "free_y") +
      ylab("PM2.5 (µg/m³)") +
      theme_minimal()
    
    p <- ggplotly(gg, tooltip = "all", dynamicTicks = TRUE, source = "time_series_plot") %>%
      highlight(
        color = "#2994E5",
        opacityDim = 1,
        on = "plotly_click",
        off = "plotly_doubleclick",
        selectize = FALSE, 
        dynamic = FALSE,
        persistent = FALSE
      ) 
    
    # Explicitly register the click event
    event_register(p, "plotly_click") %>%
      event_register("plotly_doubleclick")

    return(p)
  })
  
  selectedRow <- reactive({
    input$table_rows_selected
  })
  
  observeEvent(input$mytable_rows_selected,ignoreNULL = FALSE, {
    if(length(input$mytable_rows_selected) == 0) {
      # The datatable has been deselected (or nothing is selected)
      cat("No row is selected\n")
    } else {
      cat("Row", input$mytable_rows_selected, "is selected\n")
    }
  })
  
  sel <- reactive({!is.null(input$table_rows_selected)})  
  
  observeEvent(sel(), {
    if(!sel()) {
      cat("clear")
      sharedMap()$selection(sharedMap()$data()$`Datasource ID`)
      sharedTimeSeries()$selection(NA)
      
      leafletProxy("map") %>%
        clearPopups()
    }
  })
  
  observeEvent(selectedRow(), {
    # cat(selectedRow())
    if (!is.null(selectedRow()) && length(selectedRow()) > 0) {
      selected_data <- mostRecent() %>% slice(selectedRow())
      if (nrow(selected_data) > 0) {
        showPopup(selected_data)
        newselection <- c(selected_data$`Datasource ID`, NA)
        sharedMap()$selection(newselection)
        sharedTimeSeries()$selection(newselection)
      }
    } else {
      # cat("clear")
      sharedMap()$selection(sharedMap()$data()$`Datasource ID`)
      leafletProxy("map") %>%
        clearPopups()
    }
  })
  
  observeEvent(input$map_marker_click, {
    clickId <- input$map_marker_click$id
    
    if (!is.null(clickId)) {
      # Get clicked marker data
      selected_data <- mostRecent() %>% filter(`Datasource ID` == clickId)
      
      if (nrow(selected_data) > 0) {
        showPopup(selected_data)
        
        tableRow <- which(sharedTable()$`Datasource ID` == clickId)
        
        dataTableProxy("table") %>%
          selectRows(tableRow)
        session$sendCustomMessage("scrollToRow", tableRow)
        
        newselection <- c(clickId, NA)

        sharedMap()$selection(newselection)
        sharedTimeSeries()$selection(newselection)
      }
    }
  })
  
  observeEvent(event_data("plotly_doubleclick", source = "time_series_plot"), {
    # cat("clear")
    sharedMap()$selection(sharedMap()$data()$`Datasource ID`)
    sharedTable()$selection(sharedTable()$data()$`Datasource ID`)
    
    leafletProxy("map") %>%
      clearPopups()
  }
  )
  
  observeEvent(event_data("plotly_click", source = "time_series_plot"), {
    plotly_event <- event_data("plotly_click", source = "time_series_plot")

    if (!is.null(plotly_event)) {
      selectedDatasourceId <- plotly_event$key  # This should match `Datasource ID`
      tableDf <- sharedTable()$data() %>% as.data.frame()
      
      # Find corresponding location and data
      selected_data <- tableDf %>% filter(`Datasource ID` == selectedDatasourceId)

      if (nrow(selected_data) > 0) {

        showPopup(sharedMap()$data()[sharedMap()$data()$`Datasource ID` == selectedDatasourceId,])
        tableRow <- which(tableDf$`Datasource ID` == selectedDatasourceId)
        cat(tableRow)
        dataTableProxy("table") %>%
          selectRows(tableRow)
        session$sendCustomMessage("scrollToRow", tableRow)
        
        newselection <- c(selectedDatasourceId, NA)
        sharedMap()$selection(newselection)
        sharedTimeSeries()$selection(newselection)
      } 
    }
  })
  
}
shinyApp(ui, server)
