steepest_ascent_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    data_selector_ui(
      id = ns("id_data_selector")
    ),
    
    actionButtonQW(
      inputId = ns("add_SAplot"),
      label = "Steepest Ascent Plot",
      icon = icon("area-chart"),
      tooltip = "Öffne Steepest Ascent Plot"
      ),
    
    actionButtonQW(
      inputId = ns("add_SAtable"),
      label = "Steepest Ascent Table",
      icon = icon("table"),
      tooltip = "Öffne Steepest Ascent Table"
    ),
    
    actionButtonQW(
      inputId = ns("add_difftable"),
      label = "Difference Table",
      icon = icon("table"),
      tooltip = "Öffne Vergleichstabelle"
    ),
    
    sliderInput(
      inputId = ns("add_slider"),
      label = NULL ,
      min = 0, 
      max = 1,
      value = 0.65, 
      step = 0.05
    ) 
    )
}

steepest_ascent_server <- function(id, .values) {
  
  moduleServer(
    id, 
    function(input, output, session) {
      ns <- session$ns
      
      data_r <- reactive({
        data_selector_return$data_r()
      })
      
      our_data <-reactive({data<-data_r()}) 
      
      # START Plot ----------------------------------------------
      add_SAplot <- reactive({
    
       colnames_ourdata <- colnames(our_data())
       Aname <- colnames_ourdata[6]
       Bname <- colnames_ourdata[7]
       Cname <- colnames_ourdata[8]
       Dname <- colnames_ourdata[9]
       responsename <- colnames_ourdata[5]

       Amin <- min(our_data()[,6])
       Bmin <- min(our_data()[,7])
       Cmin <- min(our_data()[,8])
       Dmin <- min(our_data()[,9])

       Amax <- max(our_data()[,6])
       Bmax <- max(our_data()[,7])
       Cmax <- max(our_data()[,8])
       Dmax <- max(our_data()[,9])

        #fracDesign
        if (!require(qualityTools)) {
          install.packages("qualityTools")
          require(qualityTools)
        }
        vp <- facDesign(
          k = 4,
          replicates = 2,
          centerCube = 16
        )

        names(vp) <- c(Aname, Bname, Cname, Dname)
        lows(vp) <- c(Amin, Bmin, Cmin, Dmin)
        highs(vp) <- c(Amax, Bmax, Cmax, Dmax)
        units(vp) <- c("mm", "mm", "mm", "g/mm^2")
        vp_in_df <- as.data.frame(vp)
        vp_in_tibble <- as_tibble(vp_in_df)
        vp_sortiert_flugdauer <- vp_in_tibble %>% 
          arrange(D) %>% 
          arrange(C) %>% 
          arrange(B) %>% 
          arrange(A)
        
        vp_input_flugdauer <- our_data() %>% 
          arrange(Papierstaerke) %>% 
          arrange(Einschnitt) %>% 
          arrange(Koerperlaenge) %>% 
          arrange(Fluegellaenge)
        
        vp_sortiert_flugdauer <- vp_sortiert_flugdauer %>% 
          mutate(Flugdauer = vp_input_flugdauer$Flugdauer) %>% 
          arrange(RunOrder) 
        
        Flugdauer <- vp_sortiert_flugdauer$Flugdauer
        response(vp)<- Flugdauer
        
        #LinearRegression
        fdo_to_df <- function(fdo) {
          if (nrow(fdo@centerCube) == 0) {
            fdo_df <- data.frame(fdo@cube, response = fdo@response[[1]][1:(nrow(fdo@cube))]) 
          } else { 
            fdo_cube <- data.frame(fdo@cube, response = fdo@response[[1]][1:(nrow(fdo@cube))], group = 1) 
            fdo_center <- data.frame(fdo@centerCube, response = fdo@response[[1]][(nrow(fdo@cube) + 1):nrow(fdo@response)], group = 0) 
            fdo_df <- bind_rows(fdo_cube, fdo_center) 
          }
          return(fdo_df)
        }
        
        vp_df <- fdo_to_df(vp)
        vp_df_isolated <- vp_df %>% filter(group == 1)
        
        lm_haupt <- lm(response ~ A + B + C + D, data = vp_df_isolated)
        
        #SA
        sa <- steepAscent(c("A","B", "C", "D"), response = "Flugdauer", size = input$add_slider, data = vp)
        
        data_sa <- base::data.frame(A = sa@X$A.coded, B = sa@X$B.coded, C = sa@X$C.coded, D = sa@X$D.coded)
        
        flugdauer2 <- predict(lm_haupt, data_sa)
        
        flugdauer3 <- our_data()$Flugdauer
        
        delta <- 0:5
        ggplot(data = data.frame(x = rep(delta, times = 2), y = c(flugdauer3, flugdauer2), group = rep(c("Excel", "SA"), each = 6)), mapping = aes(x = x, y = y, col = group)) +
          geom_point() +
          geom_line() +
          theme_bw() +
          theme(panel.border = element_blank()) +
          labs(x = "Delta", y = "Flugdauer", col = "")
        
      })
      
      observeEvent(input$add_SAplot, {
        plot_output_id <- paste0("plot_", data_selector_return$name_r())
        
        .values$viewer$append_tab(
          tab = tabPanel(
            title = paste("Plot:", data_selector_return$name_r()),
            value = paste0("plot_", data_selector_return$name_r()),
            plotOutput(
              outputId = ns(plot_output_id)
            )
          )
        )
        
        if (!hasName(output, plot_output_id)) {
          output[[plot_output_id]] <- renderPlot({
            add_SAplot()
          })
        }
      })
      # END Plot------------------------------------------------    
      
      #START Tabelle -------------------------------------------
      
      add_SAtable <- reactive({
        #fracDesign
        colnames_ourdata <- colnames(our_data())
        Aname <- colnames_ourdata[6]
        Bname <- colnames_ourdata[7]
        Cname <- colnames_ourdata[8]
        Dname <- colnames_ourdata[9]
        responsename <- colnames_ourdata[5]
        
        Amin <- min(our_data()[,6])
        Bmin <- min(our_data()[,7])
        Cmin <- min(our_data()[,8])
        Dmin <- min(our_data()[,9])
        
        Amax <- max(our_data()[,6])
        Bmax <- max(our_data()[,7])
        Cmax <- max(our_data()[,8])
        Dmax <- max(our_data()[,9])
        
        #fracDesign
        if (!require(qualityTools)) {
          install.packages("qualityTools")
          require(qualityTools)
        }
        vp <- facDesign(
          k = 4,
          replicates = 2,
          centerCube = 16
        )
        
        names(vp) <- c(Aname, Bname, Cname, Dname)
        lows(vp) <- c(Amin, Bmin, Cmin, Dmin)
        highs(vp) <- c(Amax, Bmax, Cmax, Dmax)
        units(vp) <- c("mm", "mm", "mm", "g/mm^2")
        vp_in_df <- as.data.frame(vp)
        vp_in_tibble <- as_tibble(vp_in_df)
        vp_sortiert_flugdauer <- vp_in_tibble %>% 
          arrange(D) %>% 
          arrange(C) %>% 
          arrange(B) %>% 
          arrange(A)
        
        vp_input_flugdauer <- our_data() %>% 
          arrange(Papierstaerke) %>% 
          arrange(Einschnitt) %>% 
          arrange(Koerperlaenge) %>% 
          arrange(Fluegellaenge)
        
        vp_sortiert_flugdauer <- vp_sortiert_flugdauer %>% 
          mutate(Flugdauer = vp_input_flugdauer$Flugdauer) %>% 
          arrange(RunOrder) 
        
        Flugdauer <- vp_sortiert_flugdauer$Flugdauer
        response(vp)<- Flugdauer

        #LinearRegression
        fdo_to_df <- function(fdo) {
          if (nrow(fdo@centerCube) == 0) {
            fdo_df <- data.frame(fdo@cube, response = fdo@response[[1]][1:(nrow(fdo@cube))])
          } else {
            fdo_cube <- data.frame(fdo@cube, response = fdo@response[[1]][1:(nrow(fdo@cube))], group = 1)
            fdo_center <- data.frame(fdo@centerCube, response = fdo@response[[1]][(nrow(fdo@cube) + 1):nrow(fdo@response)], group = 0)
            fdo_df <- bind_rows(fdo_cube, fdo_center)
          }
          return(fdo_df)
        }

        vp_df <- fdo_to_df(vp)
        vp_df_isolated <- vp_df %>% filter(group == 1)

        lm_haupt <- lm(response ~ A + B + C + D, data = vp_df_isolated)
        flugdauer2 <- predict(lm_haupt, data_sa)
        #SA
        sa <- steepAscent(c("A","B", "C", "D"), response = "Flugdauer", size = input$add_slider, data = vp)
        SA_clean <- sa[, 1: 10]
        })
      
      observeEvent(input$add_SAtable, {
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
            add_SAtable()
          })
        }
      })
      #END Table -----------------------------------------------
      
      # START Difference Table--------------------------------
      
      add_difftable <- reactive({
        #fracDesign
        colnames_ourdata <- colnames(our_data())
        Aname <- colnames_ourdata[6]
        Bname <- colnames_ourdata[7]
        Cname <- colnames_ourdata[8]
        Dname <- colnames_ourdata[9]
        responsename <- colnames_ourdata[5]
        
        Amin <- min(our_data()[,6])
        Bmin <- min(our_data()[,7])
        Cmin <- min(our_data()[,8])
        Dmin <- min(our_data()[,9])
        
        Amax <- max(our_data()[,6])
        Bmax <- max(our_data()[,7])
        Cmax <- max(our_data()[,8])
        Dmax <- max(our_data()[,9])
        
        #fracDesign
        if (!require(qualityTools)) {
          install.packages("qualityTools")
          require(qualityTools)
        }
        vp <- facDesign(
          k = 4,
          replicates = 2,
          centerCube = 16
        )
        
        names(vp) <- c(Aname, Bname, Cname, Dname)
        lows(vp) <- c(Amin, Bmin, Cmin, Dmin)
        highs(vp) <- c(Amax, Bmax, Cmax, Dmax)
        units(vp) <- c("mm", "mm", "mm", "g/mm^2")
        vp_in_df <- as.data.frame(vp)
        vp_in_tibble <- as_tibble(vp_in_df)
        vp_sortiert_flugdauer <- vp_in_tibble %>% 
          arrange(D) %>% 
          arrange(C) %>% 
          arrange(B) %>% 
          arrange(A)
        
        vp_input_flugdauer <- our_data() %>% 
          arrange(Papierstaerke) %>% 
          arrange(Einschnitt) %>% 
          arrange(Koerperlaenge) %>% 
          arrange(Fluegellaenge)
        
        vp_sortiert_flugdauer <- vp_sortiert_flugdauer %>% 
          mutate(Flugdauer = vp_input_flugdauer$Flugdauer) %>% 
          arrange(RunOrder) 
        
        Flugdauer <- vp_sortiert_flugdauer$Flugdauer
        response(vp)<- Flugdauer
        
        #LinearRegression
        fdo_to_df <- function(fdo) {
          if (nrow(fdo@centerCube) == 0) {
            fdo_df <- data.frame(fdo@cube, response = fdo@response[[1]][1:(nrow(fdo@cube))])
          } else {
            fdo_cube <- data.frame(fdo@cube, response = fdo@response[[1]][1:(nrow(fdo@cube))], group = 1)
            fdo_center <- data.frame(fdo@centerCube, response = fdo@response[[1]][(nrow(fdo@cube) + 1):nrow(fdo@response)], group = 0)
            fdo_df <- bind_rows(fdo_cube, fdo_center)
          }
          return(fdo_df)
        }
        
        vp_df <- fdo_to_df(vp)
        vp_df_isolated <- vp_df %>% filter(group == 1)
        
        lm_haupt <- lm(response ~ A + B + C + D, data = vp_df_isolated)
        flugdauer2 <- predict(lm_haupt, data_sa)
        #SA
        sa <- steepAscent(c("A","B", "C", "D"), response = "Flugdauer", size = input$add_slider, data = vp)
          
         SA_vgl <- sa[,c(1,7:10)]
         vgldatabeobachtung <- as_tibble(our_data()[,c(1,5:9)])
         Vergleichstabelle <- SA_vgl  %>% mutate(lm = predict(lm_haupt, data_sa)) %>% right_join(vgldatabeobachtung, by =c("Run" = "Wurf"))
      })
      
      observeEvent(input$add_difftable, {
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
            add_difftable()
          })
        }
      })
      
      # END Difference Table ----------------------------------
      data_selector_return <- data_selector_server(
        id = "id_data_selector",
        .values = .values
      )
    }
  )
}