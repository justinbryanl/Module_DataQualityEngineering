standardisierung_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    data_selector_ui(
      id = ns("id_data_selector")
    ),
    actionButtonQW(
      inputId = ns("add_histogram"),
      label = "Show Histogram",
      icon = icon("area-chart"),
      tooltip = "Öffne Histogramm"
    ),
    actionButtonQW(
      inputId = ns("add_qqplot"),
      label = "Show Q-Q Plot",
      icon = icon("area-chart"),
      tooltip = "Öffne QQ Plot"
    ),
    actionButtonQW(
      inputId = ns("add_pcrplot"),
      label = "Show PCR",
      icon = icon("area-chart"),
      tooltip = "Öffne PCR Plot"
    ),
    actionButtonQW(
      inputId = ns("add_summarize_table"),
      label = "Show Table of the Data",
      icon = icon("table"),
      tooltip = "Öffne Summarize Table"
    )
  )
}

standardisierung_server <- function(id, .values) {
 
   moduleServer(
    id, 
    function(input, output, session) {
      ns <- session$ns
      
      data_r <- reactive({
        data_selector_return$data_r()
      })
        
      our_data <-reactive({data<-data_r()})
      
      #mulai
      standardisierung_summ_app <- reactive({ 
        data <- data_r() %>% 
          summarize(mean_flugdauer = mean(Flugdauer),
                    var_flugdauer = var(Flugdauer)*((n()-1)/n()),
                    sd_flugdauer = (var_flugdauer)**(1/2),
                    Cp = 1/(6*sd_flugdauer))
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
            standardisierung_summ_app()
          })
        }
      })
      
      #histogram
      histogram_r <- reactive({
        ggplot(data = our_data(), mapping = aes(x = Flugdauer)) +
          geom_histogram(bins = nclass.Sturges(our_data()$Flugdauer)) +
          theme_bw() +
          ggtitle("Histogram") +
          theme(plot.title = element_text(hjust = 0.5))
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
      
      #qq plot
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
        plot_output_id <- paste0("qqplot_", data_selector_return$name_r())
        
        .values$viewer$append_tab(
          tab = tabPanel(
            title = paste("QQ Plot:", data_selector_return$name_r()),
            value = paste0("qqplot_", data_selector_return$name_r()),
            plotOutput(
              outputId = ns(plot_output_id)
            )
          )
        )
        
        if (!hasName(output, plot_output_id)) {
          output[[plot_output_id]] <- renderPlot({
            qqplot_r()
          })
        }
      })
      
      #pcr plot
      pcrplot_r <- reactive({
        pcr(our_data()$Flugdauer)
      })
      
      observeEvent(input$add_pcrplot, {
        plot_output_id <- paste0("pcrplot_", data_selector_return$name_r())
        
        .values$viewer$append_tab(
          tab = tabPanel(
            title = paste("PCR Plot:", data_selector_return$name_r()),
            value = paste0("pcrplot_", data_selector_return$name_r()),
            plotOutput(
              outputId = ns(plot_output_id)
            )
          )
        )
        
        if (!hasName(output, plot_output_id)) {
          output[[plot_output_id]] <- renderPlot({
            pcrplot_r()
          })
        }
      })
      #slese
        
      data_selector_return <- data_selector_server(
        id = "id_data_selector",
        .values = .values
      )
    }
  )
}
