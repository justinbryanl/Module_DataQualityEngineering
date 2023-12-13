versuchsplan_ui <- function(id) {
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
      inputId = ns("add_effectplot"),
      label = "Show Effect Plot",
      icon = icon("area-chart"),
      tooltip = "Öffne Effect Plot"
    ) ,
    selectInput(
      inputId = ns("add_selected_effectplot"),
      label = "Choose data for Effect Plot",
      choices = c("A" = "A","B" = "B", "C" = "C", "D" ="D")
    ),
    actionButtonQW(
      inputId = ns("add_InteractionPlot"),
      label = "Show Interaction Plot",
      icon = icon("area-chart"),
      tooltip = "Öffne Interaction Plot"
    ),
    radioButtons(
      inputId = ns("add_radio_inter_x"),
      label = " X-Achse",
      choices = list("A" = 1 , "B" = 2, "C" = 3, "D" = 4 ),
      selected = 1 
    
    ),
    radioButtons(
      inputId = ns("add_radio_inter_y"),
      label = " Y-Achse",
      choices = list("A" = 1 , "B" = 2, "C" = 3, "D" = 4 ),
      selected = 1 
    ),
    actionButtonQW(
      inputId = ns("add_ParetoPlot"),
      label = "Show Pareto Plot",
      icon = icon("area-chart"),
      tooltip = "Öffne Pareto Plot"
    ),
    actionButtonQW(
      inputId = ns("add_ContourPlot"),
      label = "Show Contour Plot",
      icon = icon("area-chart"),
      tooltip = "Öffne Contour Plot"
    ),
    actionButtonQW(
      inputId = ns("add_SurfacePlot"),
      label = "Show Surface Plot",
      icon = icon("area-chart"),
      tooltip = "Öffne Surface Plot"
    )
  )
}

versuchsplan_server <- function(id, .values) {

  moduleServer(
    id, 
    function(input, output, session) {
      ns <- session$ns
      
      data_r <- reactive({
        data_selector_return$data_r()
      })
      
      our_data <- reactive({data<- data_r()})
      
#Histogram      
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
#Effect Plot   (masih salah grgr belom ada yang automatisierung data ke ABCD)   
      
     effectplot_r <- reactive({
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
       
       our_data2 <<- vp
      
       #ini methode di übung
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
        
       dist <- noquote(switch(input$add_selected_effectplot,
                      "A" = "A" ,
                      "B" = "B",
                      "C" = "C",
                      "D" = "D"))
       
       #ini dipake buat linear model (runoder ga vorhanden disini)
       vp_df <- fdo_to_df(our_data2)
       vp_df_isolated <- vp_df %>% filter(group == 1)
       vp_df_kruemmungtest <- vp_df %>% filter(group == 0)
       
       lm_komplett <- lm(response ~ A * B * C * D, data = vp_df_isolated)
   
       lm_haupt <- lm(response ~ A + B + C + D, data = vp_df_isolated)
       lm_haupt_global <<- lm_haupt
       # 
       # lm_AD <- lm(response ~ A + D, data = vp_df_isolated)
       # lm_AD_global <<- lm_AD
       # 
       # lm_AB <- lm(response ~ A + B , data = vp_df_isolated)
       # lm_AB_global <<- lm_AB
       # 
       # lm_AC <- lm(response ~ A + C , data = vp_df_isolated)
       # lm_AC_global <<- lm_AC
       # 
       # lm_BC <- lm(response ~ B + C, data = vp_df_isolated)
       # lm_BC_global <<- lm_BC
       # 
       # lm_BD <- lm(response ~ B + D, data = vp_df_isolated)
       # lm_BD_global <<- lm_BD
       # 
       # lm_CD <- lm(response ~ C + D, data = vp_df_isolated)
       # lm_CD_global <<- lm_CD
      
       # hw_macem2 <- vp_df_isolated %>%
       #   group_by(dist) %>% 
       #   summarise(mean = mean(response))
       # 
       dist
         # ggplotly(data = hw_macem2, mapping = aes(x = dist, y = mean)) + 
         # geom_point(col = "red") + 
         # geom_line() + 
         # theme_bw() + 
         # labs( y = "Flugdauer", title = "Effekt-Plot")
       
      
      })
      observeEvent(input$add_effectplot, {
        plot_output_id <- paste0("effectplot_", data_selector_return$name_r())
        
        .values$viewer$append_tab(
          tab = tabPanel(
            title = paste("Effect Plot:", data_selector_return$name_r()),
            value = paste0("effectplot_", data_selector_return$name_r()),
            plotlyOutput(
              outputId = ns(plot_output_id)
            )
          )
        )
        
        if (!hasName(output, plot_output_id)) {
          output[[plot_output_id]] <- renderPlotly({
            effectplot_r()
          })
        }
      })
#effek plot kelar ~
      
#interaction Plot
      interaction <- reactive({
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
        
        #facDesign
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
        
        our_data2 <<- vp
        
        #ini methode di übung
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
        
        #ini dipake buat linear model (runoder ga vorhanden disini)
        vp_df <- fdo_to_df(our_data2)
        
        jika <- function(input_x,input_y){
          if(input_x==input_y){
            salah <- renderText({"ERROR! DATANYA SAMA"})
            salah()
            next
          } else {
            ll <- (filter(vp_df, noquote(input_x) == -1, noquote(input_y) == -1) %>% summarise(mean = mean(response)))$mean
            lh <- (filter(vp_df, noquote(input_x) == -1, noquote(input_y) ==  1) %>% summarise(mean = mean(response)))$mean
            hl <- (filter(vp_df, noquote(input_x) ==  1, noquote(input_y) == -1) %>% summarise(mean = mean(response)))$mean
            hh <- (filter(vp_df, noquote(input_x) ==  1, noquote(input_y) ==  1) %>% summarise(mean = mean(response)))$mean
            
            df <- data.frame(y = c(ll, lh, hl, hh), x = c(-1, -1, 1, 1), col = c(-1, 1, -1, 1))
            
            ggplot(data = df, mapping = aes(x = x, y = y)) +
              geom_point(col = "red") +
              geom_line(mapping = aes(x = x, y = y, col = factor(col))) +
              theme_bw() +
              theme(plot.title = element_text(hjust = 0.5),
                    panel.border = element_blank()) +
              labs(title = "Interaktions-Plot") 
          }
        }
        
        jika(input$add_radio_inter_x, input$add_radio_inter_y)
        
        # ll <- (filter(vp_df, noquote(input$add_radio_inter_x) == -1, noquote(input$add_radio_inter_y) == -1) %>% summarise(mean = mean(response)))$mean
        # lh <- (filter(vp_df, noquote(input$add_radio_inter_x) == -1, noquote(input$add_radio_inter_y) ==  1) %>% summarise(mean = mean(response)))$mean
        # hl <- (filter(vp_df, noquote(input$add_radio_inter_x) ==  1, noquote(input$add_radio_inter_y) == -1) %>% summarise(mean = mean(response)))$mean
        # hh <- (filter(vp_df, noquote(input$add_radio_inter_x) ==  1, noquote(input$add_radio_inter_y) ==  1) %>% summarise(mean = mean(response)))$mean
        # 
        # df <- data.frame(y = c(ll, lh, hl, hh), x = c(-1, -1, 1, 1), col = c(-1, 1, -1, 1))
        # 
        # ggplot(data = df, mapping = aes(x = x, y = y)) +
        #   geom_point(col = "red") +
        #   geom_line(mapping = aes(x = x, y = y, col = factor(col))) +
        #   theme_bw() +
        #   theme(plot.title = element_text(hjust = 0.5),
        #         panel.border = element_blank()) +
        #   labs(title = "Interaktions-Plot")
        # 
        
      })
      
      observeEvent(input$add_InteractionPlot, {
        plot_output_id <- paste0("Pareto_plot_", data_selector_return$name_r())
        
        .values$viewer$append_tab(
          tab = tabPanel(
            title = paste("Interaction Plot:", data_selector_return$name_r()),
            value = paste0("interactionplot_", data_selector_return$name_r()),
            plotOutput(
              outputId = ns(plot_output_id)
            )
          )
        )
        
        if (!hasName(output, plot_output_id)) {
          output[[plot_output_id]] <- renderPlot({
            interaction()
          })
        }
        
      })
      
#kelar interaction ~

#Pareto Plot 
      pareto <- reactive({
        
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
        
        our_data2 <<- vp
        
        #ini methode di übung
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
        
        dist <- switch(input$add_selected_effectplot,
                       "A" = "A" ,
                       "B" = "B",
                       "C" = "C",
                       "D" = "D")
        
        #ini dipake buat linear model (runoder ga vorhanden disini)
        vp_df <- fdo_to_df(our_data2)
        vp_df_isolated <- vp_df %>% filter(group == 1)
        lm_komplett <- lm(response ~ A * B * C * D, data = vp_df_isolated)
        
        lm_haupt <- lm(response ~ A + B + C + D, data = vp_df_isolated)
        lm_haupt_global <<- lm_haupt
        
        # lm_AD <- lm(response ~ A + D, data = vp_df_isolated)
        # lm_AD_global <<- lm_AD
        # 
        # lm_AB <- lm(response ~ A + B , data = vp_df_isolated)
        # lm_AB_global <<- lm_AB
        # 
        # lm_AC <- lm(response ~ A + C , data = vp_df_isolated)
        # lm_AC_global <<- lm_AC
        # 
        # lm_BC <- lm(response ~ B + C, data = vp_df_isolated)
        # lm_BC_global <<- lm_BC
        # 
        # lm_BD <- lm(response ~ B + D, data = vp_df_isolated)
        # lm_BD_global <<- lm_BD
        # 
        # lm_CD <- lm(response ~ C + D, data = vp_df_isolated)
        # lm_CD_global <<- lm_CD
        # 
        
        
        # Funktion zur Erstellung eines Pareto-Plots: 
        pareto_plot <- function(lm, alpha = 0.05) {
          erklaert <- "t-Wert"
          effects <- summary(lm)$coefficients[,3][2:length(summary(lm)$coefficients[,3])]
          effect_names <- names(effects)
          data <- data.frame(name = effect_names, effects = abs(effects))
          data$name <- factor(data$name, levels = data$name[order(data$effects, decreasing = TRUE)])
          t_sig <- abs(qt(alpha/2, df = df.residual(lm)))
          plot <- ggplot(data = data) + 
            geom_col(mapping = aes(x = name, y = effects), fill = "lightblue") +
            geom_hline(yintercept = t_sig, col = "red") +
            scale_x_discrete(name = NULL) +
            scale_y_continuous(name = erklaert) +
            theme_bw() +
            theme(panel.border = element_blank(),
                  plot.title = element_text(hjust = 0.5, size = 20),
                  plot.subtitle = element_text(hjust = 0.5, size = 18),
                  axis.title = element_text(size = 18),
                  axis.text = element_text(size = 16),
                  legend.title = element_text(size = 18),
                  legend.text = element_text(size = 16)) +
            labs(title = "Standardisierte Haupteffekte und Wechselwirkungen",
                 subtitle = substitute(paste(t[Krit] == t_sig, " für ", alpha == a), list(t_sig = round(t_sig, 3), a = alpha)))
          return(plot)
        }
        
        pareto_plot(lm_komplett, alpha = 0.05)
      })
        
        observeEvent(input$add_ParetoPlot, {
          plot_output_id <- paste0("Pareto_plot_", data_selector_return$name_r())
          
          .values$viewer$append_tab(
            tab = tabPanel(
              title = paste("Pareto Plot:", data_selector_return$name_r()),
              value = paste0("Paretoplot_", data_selector_return$name_r()),
              plotOutput(
                outputId = ns(plot_output_id)
              )
            )
          )
          
          if (!hasName(output, plot_output_id)) {
            output[[plot_output_id]] <- renderPlot({
              pareto()
            })
          }
        
      })
        
#kelar Pareto Plot
        
#Contour Plot
#kelar Contour Plot

      data_selector_return <- data_selector_server(
        id = "id_data_selector",
        .values = .values
      )
    }
  )  
}