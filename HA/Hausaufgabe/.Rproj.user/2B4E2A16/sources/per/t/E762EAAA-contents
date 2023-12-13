ortsauswahl_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    data_selector_ui(
      id = ns("id_data_selector")
    ),
    
    actionButtonQW(
      inputId = ns("add_summarize_table"),
      label = NULL,
      icon = icon("table"),
      tooltip = "Öffne Summarize Table"
    ),
    
    selectInput(
      inputId = ns("add_selected_histogram"),
      label = "Choose data for Histogram",
      choices = c("Grundgesamheit","MA", "EW", "H", "ER")
    ),
    
    actionButtonQW(
      inputId = ns("add_histogram"),
      label = NULL,
      icon = icon("area-chart"),
      tooltip = "Öffne Histogramm"
    ),
    
    selectInput(
      inputId = ns("add_selected_qq"),
      label = "Choose data for QQplot",
      choices = c( "Grundgesamheit", "MA", "EW", "H", "ER")
    ),
    
    actionButtonQW(
      inputId = ns("add_qqplot"),
      label = NULL,
      icon = icon("area-chart"),
      tooltip = "Öffne Q-Q Plot"
  ))
}

ortsauswahl_server <- function(id, .values) {
  
  shiny::moduleServer(
    id, 
    function(input, output, session) {
      ns <- session$ns
      
      data_r <- reactive({
        data_selector_return$data_r()
      })
      
      our_data <-reactive({data<-data_r()}) 
      
#summarized table

      ortsauswahl_summ_app <- reactive({ 
        data1 <- data_r() %>% group_by(Ort) %>% 
          summarize(mean_flugdauer = mean(Flugdauer),
                    var_flugdauer = var(Flugdauer)*((n()-1)/n()),
                    sd_flugdauer = (var_flugdauer)**(1/2),
                    Cp = 1/(6*sd_flugdauer))
        data2 <- data_r() %>% 
          summarize(mean_flugdauer = mean(Flugdauer),
                    var_flugdauer = var(Flugdauer)*((n()-1)/n()),
                    sd_flugdauer = (var_flugdauer)**(1/2),
                    Cp = 1/(6*sd_flugdauer)) %>%  mutate(Ort = "Gesamt")
        data <- data1 %>% full_join(data2)
      })  
      
      observeEvent(input$add_summarize_table, {
        table_output_id <- paste0("table_", data_selector_return$name_r())
        
        .values$viewer$append_tab(
          tab = tabPanel(
            title = paste("Table:", data_selector_return$name_r()),
            value = paste0("table_", data_selector_return$name_r()),
            tableOutput(
              outputId = ns(table_output_id)
            )
          )
        )
        
        if (!hasName(output, table_output_id)) {
          output[[table_output_id]] <- renderTable({
            ortsauswahl_summ_app()
          })
        }
      })
# ~ kelar table

# Histogram data yang harus di pilih
      histogram_r <- reactive({
        ggplot(data = our_data(), mapping = aes(x = Flugdauer)) +
          geom_histogram(bins = nclass.Sturges(our_data()$Flugdauer)) +
          theme_bw()
      })
      
      observeEvent(input$add_histogram, {
        plot_output_id <- paste0("histogram_", data_selector_return$name_r())
        
        .values$viewer$append_tab(
          tab = tabPanel(
            title = paste("Histogram:", data_selector_return$name_r()),
            value = paste0("histogram_", data_selector_return$name_r()),
            plotOutput(
              outputId = ns(plot_output_id)
            )
          )
        )
        
        if (!hasName(output, plot_output_id)) {
          output[[plot_output_id]] <- renderPlot({
            histogram_r()
          })
        }
      })
      
# ~ kelar histogram
      
# # QQplot data yang harus di pilih
#       qqplot_r <- reactive({
#         ggplot(data = our_data()) +
#           geom_point() +
#           theme_bw()
#       })
#       
#       observeEvent(input$add_histogram, {
#         plot_output_id <- paste0("histogram_", data_selector_return$name_r())
#         
#         .values$viewer$append_tab(
#           tab = tabPanel(
#             title = paste("Histogram:", data_selector_return$name_r()),
#             value = paste0("histogram_", data_selector_return$name_r()),
#             plotOutput(
#               outputId = ns(plot_output_id)
#             )
#           )
#         )
#         
#         if (!hasName(output, plot_output_id)) {
#           output[[plot_output_id]] <- renderPlot({
#             histogram_r()
#           })
#         }
#       })
#       
# # ~ kelar qqplot
      
      
      data_selector_return <- data_selector_server(
        id = "id_data_selector",
        .values = .values
      )
    }
  )
}

