ortsauswahl_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    data_selector_ui(
      id = ns("id_data_selector")
    ),
    
    actionButtonQW(
      inputId = ns("add_summarize_table"),
      label = "Show Table of the Data",
      icon = icon("table"),
      tooltip = "Öffne Summarize Table"
    ),
    
    selectInput(
      inputId = ns("add_selected_histogram"),
      label = "Choose data for Histogram",
      choices = c("MA", "EW", "H", "ER")
    ),
    
    actionButtonQW(
      inputId = ns("add_his_single"),
      label = NULL,
      icon = icon("area-chart"),
      tooltip = "Öffne Histogramm"
    ),
    
    actionButtonQW(
      inputId = ns("add_histogram"),
      label = "gesamheit Histogramm",
      icon = icon("area-chart"),
      tooltip = "Öffne Grundgesamheit Histogramm"
    ),
    
    selectInput(
      inputId = ns("add_selected_qq"),
      label = "Choose data for QQplot",
      choices = c( "MA", "EW", "H", "ER")
    ),
    
    actionButtonQW(
      inputId = ns("add_qqplot_single"),
      label = NULL,
      icon = icon("area-chart"),
      tooltip = "Öffne Q-Q Plot"
    ),
    
    actionButtonQW(
      inputId = ns("add_qqplot"),
      label = "gesamtheit Q-Q Plot",
      icon = icon("area-chart"),
      tooltip = "Öffne Grundgesamheit Q-Q Plot"
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
      
# Histogramm macem2 
      
      histogram_single <- reactive({
        
        specific_data <- data<-data_r() %>% filter(Ort == input$add_selected_histogram)
        ggplot(data = specific_data, mapping = aes(x = Flugdauer)) +
          geom_histogram(bins = nclass.Sturges(specific_data$Flugdauer)) +
          theme_bw()
      })
      
      observeEvent(input$add_his_single, {
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
            histogram_single()
          })
        }
      })
      
      
# ~ kelar Historgramm macem2 

# Histogram data with Grundgesamheit 
      histogram_r <- reactive({
        ggplot(data = our_data(), mapping = aes(x = Flugdauer)) +
          geom_histogram(bins = nclass.Sturges(our_data()$Flugdauer)) +
          theme_bw()
      })
      
      observeEvent(input$add_histogram, {
        plot_output_id2 <- paste0("histogram_", data_selector_return$name_r())
        
        .values$viewer$append_tab(
          tab = tabPanel(
            title = paste("Histogram:", data_selector_return$name_r()),
            value = paste0("histogram_", data_selector_return$name_r()),
            plotOutput(
              outputId = ns(plot_output_id2)
            )
          )
        )
        
        if (!hasName(output, plot_output_id2)) {
          output[[plot_output_id2]] <- renderPlot({
            histogram_r()
          })
        }
      })
      
# ~ kelar histogram

      # QQplot macem2 
      qqplot_single <- reactive({
        specific_data2 <- data<-data_r() %>% filter(Ort == input$add_selected_qq)
        ggplot(mapping = aes(sample = specific_data2$Flugdauer)) +
          stat_qq_point() +
          stat_qq_line() +
          xlab("Theoritical Quantiles") +
          ylab("Sample Quantiles") +
          ggtitle("Normal Q-Q Plot") +
          theme(plot.title = element_text(hjust = 0.5))
      })
      
      observeEvent(input$add_qqplot_single, {
        plot_output_id3 <- paste0("qqplot_", data_selector_return$name_r())
        
        .values$viewer$append_tab(
          tab = tabPanel(
            title = paste("Q-Q Plot:", data_selector_return$name_r()),
            value = paste0("qqplot_", data_selector_return$name_r()),
            plotOutput(
              outputId = ns(plot_output_id3)
            )
          )
        )
        
        if (!hasName(output, plot_output_id3)) {
          output[[plot_output_id3]] <- renderPlot({
            qqplot_single()
          })
        }
      })
      
      #~ kelar QQplot macem2 
      
# QQplot data with Grundgesamheit
      qqplot_r <- reactive({
        ggplot(mapping = aes(sample = our_data()$Flugdauer)) +
          stat_qq_point() +
          stat_qq_line() +
          xlab("Theoritical Quantiles") +
          ylab("Sample Quantiles") +
          ggtitle("Normal Q-Q Plot") +
          theme(plot.title = element_text(hjust = 0.5))
      })

      observeEvent(input$add_qqplot, {
        plot_output_id4 <- paste0("qqplot_", data_selector_return$name_r())

        .values$viewer$append_tab(
          tab = tabPanel(
            title = paste("Q-Q Plot2:", data_selector_return$name_r()),
            value = paste0("qqplot_", data_selector_return$name_r()),
            plotOutput(
              outputId = ns(plot_output_id4)
            )
          )
        )

        if (!hasName(output, plot_output_id4)) {
          output[[plot_output_id4]] <- renderPlot({
            qqplot_r()
          })
        }
      })

# ~ kelar qqplot
      
      
      data_selector_return <- data_selector_server(
        id = "id_data_selector",
        .values = .values
      )
    }
  )
}

