library(shiny)
library(tidyverse)
library(readr)
library(hrbrthemes)
library(here)
library(ISLR)
library(tigris)
library(leaflet)
library(stringr)
library(shinythemes)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # shiny app theme
  theme = shinytheme("flatly"),
  
  # app title
  titlePanel("Association Between Income, Race and Tax Audit Rates"),
  
  
  tabsetPanel(type = "tabs", 
                        tabPanel("Graph", 
                                 sidebarLayout(
                                   sidebarPanel(
                                     checkboxGroupInput(inputId = "scatter_plot",
                                                        label = "Select model type:",
                                                        choices = c("Linear" = "1",
                                                                    "Quadratic" = "2",
                                                                    "Higher level polynomial" = "3"),
                                                        selected = NULL),
                                     
                                     selectInput("select",
                                                 "State",
                                                 choices = append(c("All"), sort(unique(joined_cnty$state.x))),
                                                 selected = "All"),
                                   ),
                                   mainPanel(plotOutput("scatter_plot")))),
                                 tabPanel("Map",
                                          fluidRow(
                                            column(12, 
                                                   leafletOutput("map_plot", height = 600))
                                            ))
                        )
  
)





# define server logic to draw plot and map
server <- function(input, output) {
 
  # load data 
  audits <- here::here("data/cleaned/auditsData_2019.04.03.csv") %>%
    read_csv()
  race_county <- here::here("data/cleaned/race_eth_county.csv") %>%
    read_csv()
  incomes <- here::here("data/cleaned/incomes.csv") %>%
    read_csv()
  joined <- full_join(audits, race_county, by = "fips") %>%
    full_join(., incomes, by = "fips") %>% 
    select(fips, state.x, county, n_returns, estimated_exams, audit_rate, white, black, indian, asian, hawaiian, other, two, non_his, hispanic, median_income)
 
  # wrangle data 
  joined <- joined %>%
    mutate(non_white = black + indian + asian + hawaiian + other + two, 
           pred_white = as.logical(ifelse(white >= non_white, 1, 0)),
           pred_white2 = factor(pred_white, labels = c("People of Color", "White")))
  
  cnty <- counties(state = FALSE, cb = TRUE, resolution = "20m")
  cnty <- cnty %>%
    unite(fips, c(STATEFP, COUNTYFP), sep = "", remove = FALSE) %>%
    mutate(fips = str_remove(fips, "^0+"),
             fips = as.numeric(fips))

  joined_cnty <- full_join(cnty, joined, by = "fips")
  
  joined_cnty <- na.omit(joined_cnty)
    
  pal1 <- colorNumeric(palette = "YlOrRd", domain = joined_cnty$audit_rate)
  
  pal2 <- colorNumeric(palette = "YlGnBu", domain = joined_cnty$median_income)
  
  pal3 <- colorFactor(palette = c("grey34","grey88"), domain = joined_cnty$pred_white2)
  
  
  # create output plots
  output$scatter_plot <- renderPlot({
    
    #if all of the states are selected show joined_cnty data set else filter by the selected state
    sort_state <- if ("All" == input$select) {
      joined_cnty
    } else {
      joined_cnty %>% filter(state.x == input$select)
    }
    
    # make sort_state something the user can iotneract with
    react <- reactive({
      sort_state
    })
  
    # draw baseline scatter plot 
    base_plot <- ggplot(data = react(), aes(x = median_income, y = audit_rate, color = pred_white2)) +
      geom_point() +
      scale_x_continuous(labels = scales::dollar) +
      scale_y_continuous(labels = function(x) paste0(x, '%')) +
      labs(x = "Median Income",
           y = "Percent of Tax Returns Audited",
           title = "",
           subtitle = "Each point represents one US county", 
           color = "Predominant Racial Identity")

    # conditionals to add regression lines
    if(is.null(input$scatter_plot)) {out <- base_plot;
    }
    else if(identical(c("1"), input$scatter_plot)) {out <- base_plot + 
      geom_smooth(method = lm, color = "#f9766e");
    }
    else if(identical(c("2"), input$scatter_plot)) {out <- base_plot + 
      stat_smooth(method = lm, formula = y ~ poly(x, 2, raw = TRUE), color = "#7caf00");
    }
    else if(identical(c("3"), input$scatter_plot)) {out <- base_plot + 
      stat_smooth(method = lm, formula = y ~ poly(x, 3, raw = TRUE), color = "#c77cff");
    }
    else if(identical(c("1", "2"), input$scatter_plot)) {out <- base_plot +
            geom_smooth(method = lm, color = "#f9766e") +
            stat_smooth(method = lm, formula = y ~ poly(x, 2, raw = TRUE), color = "#7caf00");
    }
    else if(identical(c("1", "3"), input$scatter_plot)) {out <- base_plot +
      geom_smooth(method = lm, color = "#f9766e") +
      stat_smooth(method = lm, formula = y ~ poly(x, 3, raw = TRUE), color = "#c77cff");
    }
    else if(identical(c("2", "3"), input$scatter_plot)) {out <- base_plot +
      stat_smooth(method = lm, formula = y ~ poly(x, 2, raw = TRUE), color = "#7caf00") +
      stat_smooth(method = lm, formula = y ~ poly(x, 3, raw = TRUE), color = "#c77cff");
    }
    else if(identical(c("1", "2", "3"), input$scatter_plot)) {out <- base_plot +
      geom_smooth(method = lm, color = "#f9766e") +
      stat_smooth(method = lm, formula = y ~ poly(x, 2, raw = TRUE), color = "#7caf00") +
      stat_smooth(method = lm, formula = y ~ poly(x, 3, raw = TRUE), color = "#c77cff");
    }
    
    # display out (baseline scatter) plot
    out
    
    
    
  })
  
  
  output$map_plot <- renderLeaflet({
    
    leaflet(joined_cnty) %>%
      addTiles() %>%
      addPolygons(group = "Percentage of Taxes Audited",
                  fillColor = ~pal1(audit_rate), 
                  color = "#b2aeae", # use hex colors for line color
                  fillOpacity = 0.7, 
                  weight = 1, 
                  smoothFactor = 0.2,
                  popup = ~ paste(county, state.x, audit_rate, median_income)) %>%
      addPolygons(group = "Median Income",
                  fillColor = ~pal2(median_income),
                  color = "#b2aeae", # you need to use hex colors
                  fillOpacity = 0.7,
                  weight = 1,
                  smoothFactor = 0.2,
                  popup = ~ paste(county, state.x, audit_rate, median_income)) %>%
      addPolygons(group = "Racial Identity",
                  fillColor = ~pal3(pred_white2),
                  color = "#b2aeae", # you need to use hex colors
                  fillOpacity = 0.7,
                  weight = 1,
                  smoothFactor = 0.2,
                  popup = ~ paste(county, state.x, audit_rate, median_income)) %>%
      addLayersControl(overlayGroups = c("Percentage of Taxes Audited", "Median Income", "Racial Identity"),
                       options = layersControlOptions(collapsed = FALSE),
                       position = "topright") %>%
      htmlwidgets::onRender("function() {
            $('.leaflet-control-layers-overlays').prepend('<label style=\"text-align:center\">Select Layers</label>');
      }") %>% 
      addLegend(pal = pal1, 
                values = joined_cnty$audit_rate, 
                position = "topright", 
                title = "Percent of Taxes Audited",
                labFormat = labelFormat(suffix = "%")) %>%
      addLegend(pal = pal2, 
                values = joined_cnty$median_income, 
                position = "topright", 
                title = "Median Income") %>%
      addLegend(pal = pal3, 
                values = joined_cnty$pred_white2, 
                position = "topright", 
                title = "Racial Identity") %>%
      hideGroup("Median Income") %>% 
      hideGroup("Racial Identity") 
    
  })

}

# Run the application
shinyApp(ui = ui, server = server)



