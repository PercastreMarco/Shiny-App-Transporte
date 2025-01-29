##### :::::::::: CARGAR PAQUETES :::::::::::::::::: ####
library(tidyverse)
library(discrtr) # A companion package for the book Introduction to Discrete Choice Analysis with `R`
library(dplyr) # A Grammar of Data Manipulation
library(ggplot2) # Create Elegant Data Visualisations Using the Grammar of Graphics
library(ggridges)
library(discrtr)
library(shiny)
library(bslib)
library(plotly)
library(tidyr)
library(viridis)
library(leaflet)
library(leaflet.extras)

#### ::::::::::: IMPORTAR DATOS ::::::::::::::::::::: ####
#if (!require("remotes"))
# install.packages("remotes", repos = "https:/cran.rstudio.org")
#if(!require("discrtr"))
#remotes::install_github("paezha/discrtr")

mc_mode_choice <- read_csv(system.file("extdata",
                                       "mc_commute.csv",
                                       package = "discrtr"),
                           show_col_types = FALSE)

# Seleccionar las variables que nos interesan
mc_mode_choice <- mc_mode_choice %>% dplyr::select(RespondentID, 
                                                   choice, 
                                                   avcycle,
                                                   avwalk,
                                                   avhsr,
                                                   avcar,
                                                   timecycle,
                                                   timewalk,
                                                   accesshsr,
                                                   waitingtimehsr,
                                                   timecar,
                                                   gender,
                                                   LAT,
                                                   LONG)

# Sumar el tiempo para HSR
mc_mode_choice <- mc_mode_choice %>%
  mutate(timehsr = accesshsr + waitingtimehsr)

# Inicialmente R identifica las variables `RespondentID` y `choice` como numéricas
# Convertimos la variable `choice` (y asignar etiquetas o niveles  específicos) de la siguiente manera:
mc_mode_choice$choice <- factor(mc_mode_choice$choice,
                                labels = c("Cycle", "Walk", "HSR","Car"))


#Limpiamos los datos de tiempo para Cycle, Walk, HSR y Car.
mc_mode_choice <- mc_mode_choice %>%
  mutate(across(contains("time"), ~ ifelse(. == 100000, NA, .)))



####### ::::: APP SHINY :::::: #######

ui <- page_fluid(
  tags$head(
    tags$style(HTML("
      body {
        font-family: 'Arial', sans-serif;
        background-color: #f9f9f9;
        margin: 0;
        padding: 0;
      }
      .header {
        background-color: #7A003C;
        color: white;
        padding: 20px;
        text-align: center;
      }
      .header h1 {
        margin: 0;
        font-size: 2.5rem;
      }
      .header h2 {
        margin-top: 5px;
        font-size: 1.5rem;
        font-weight: 300;
        color: #d9e3f0;
      }
      .content {
        padding: 20px;
      }
      .footer {
        background-color: #7A003C;
        color: white;
        text-align: center;
        padding: 10px;
        margin-top: 20px;
      }
      .main-image {
        width: 100%;
        height: auto;
        max-height: 400px;
        margin-bottom: 20px;
        border-radius: 8px;
      }
      .value-box {
        text-align: center;
        padding: 20px;
        margin: 10px;
        border-radius: 8px;
        background-color: white;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
      }
    "))
  ),
  
  div(class = "header",
      h1("Encuesta de Viajes a la Universidad McMaster"),
      h2("Analiza cómo los estudiantes viajan y toman decisiones de transporte")
  ),
  
  div(class = "content",
      img(src = "https://campusguides.ca/wp-content/uploads/2020/06/mcmaster.jpg", 
          alt = "Universidad McMaster", 
          class = "main-image"),
      
      navset_bar(
        title = "Opciones de Análisis",
        nav_panel("Resumen General", 
                 h3("Resumen General de la Encuesta"),
                 
                 # Tarjeta de introducción 1
                 card(
                   card_header("Acerca de esta aplicación"),
                   div(style = "padding: 15px;",
                       p(style = "font-size: 16px;", 
                         "Bienvenido a la aplicación de análisis de la Encuesta de Viajes de la Universidad McMaster. 
                         Esta herramienta interactiva ha sido diseñada para visualizar y analizar los patrones de 
                         movilidad de la comunidad estudiantil."),
                       p(style = "font-size: 16px;", "Los objetivos principales de este análisis son:"),
                       tags$ul(style = "font-size: 16px;",
                               tags$li("Comprender los diferentes modos de transporte utilizados por los estudiantes "),
                               tags$li("Analizar los atributos relevantes de los medios de transporte disponibles"),
                               tags$li("Analizar los atributos relevantes de la comunidad estudiantil de la Universidad McMaster"),
                               tags$li("Analizar la demanda mediante un modelo de elección discreta")
                       ),
                       p(style = "font-size: 16px;", 
                         "Esta información es fundamental para la planificación de servicios de transporte, 
                         mejora de la accesibilidad al campus y desarrollo de iniciativas de movilidad para facilitar el traslado estudiantil.")
                   )
                 ),
                 
                 # Tarjeta de introducción 2
                 card(
                   card_header("Acerca del conjunto de datos"),
                   div(style = "padding: 15px;",
                       p(style = "font-size: 16px;", 
                         "Un archivo de texto delimitado que contiene información sobre los estudiantes que viajan a la Universidad McMaster. 
                         Los datos fueron recopilados mediante una encuesta de viajes en el otoño de 2010. 
                         Se les preguntó a los encuestados sobre su modo de viaje a la Universidad McMaster, en Hamilton, Canadá. 
                         También se les preguntó sobre los modos de transporte disponibles para ellos. 
                         Las características de los viajes fueron autoinformadas o imputadas. 
                         El conjunto de datos también contiene atributos relevantes sobre los encuestados. 
                         El formato de la tabla es largo, con cada fila representando una situación de elección."),
                       p(style = "font-size: 16px;", "El conjunto de datos contiene las siguientes variables:"),
                       tags$ul(style = "font-size: 16px;",
                               tags$li("RespondentID: Identificador único para los encuestados."), 
                               tags$li("choice: Variable numérica que indica los modos de transporte: (1) Bicicleta, (2) Caminar, (3) HSR (transporte local), (4) Coche."),
                               tags$li("avcycle: Variable indicadora de disponibilidad de bicicleta: (1) Sí, (0) No."),
                               tags$li("avwalk: Variable indicadora de disponibilidad para caminar: (1) Sí, (0) No."),
                               tags$li("avhsr: Variable indicadora de disponibilidad de HSR: (1) Sí, (0) No."),
                               tags$li("avcar: Variable indicadora de disponibilidad de coche: (1) Sí, (0) No."),
                               tags$li("timecycle: Tiempo de viaje en bicicleta en minutos (cuando el modo no está disponible se codifica como 100000)."),
                               tags$li("timewalk: Tiempo de viaje caminando en minutos (cuando el modo no está disponible se codifica como 100000)."),
                               tags$li("accesshsr: Tiempo de acceso a HSR en minutos."),
                               tags$li("waitingtimehsr: Tiempo de espera al viajar en HSR en minutos."),
                               tags$li("timecar: Tiempo de viaje en coche en minutos (cuando el modo no está disponible se codifica como 100000)."),
                               tags$li("gender: Variable indicadora de género: (1) Mujer, (0) Hombre.")
                       )
                   )),
                 br(),
                 
        ),
        nav_panel("Modos de Transporte", 
                 h3("Análisis de los modos disponibles"),
                 # Primera fila para los value-boxes
                 fluidRow(
                   column(2, offset = 1,
                          div(class = "value-box",
                              h4("Total Encuestados"),
                              h2(formatC(n_distinct(mc_mode_choice$RespondentID), big.mark = ",")),
                              p("estudiantes")
                          )
                   ),
                   column(2,
                          div(class = "value-box",
                              h4("Caminar"),
                              h2(formatC(sum(mc_mode_choice$choice == "Walk"), big.mark = ",")),
                              p("estudiantes")
                          )
                   ),
                   column(2,
                          div(class = "value-box",
                              h4("Automovil"),
                              h2(formatC(sum(mc_mode_choice$choice == "Car"), big.mark = ",")),
                              p("estudiantes")
                          )
                   ),
                   column(2,
                          div(class = "value-box",
                              h4("Transporte local"),
                              h2(formatC(sum(mc_mode_choice$choice == "HSR"), big.mark = ",")),
                              p("estudiantes")
                          )
                   ),
                   column(2,
                          div(class = "value-box",
                              h4("Bicicleta"),
                              h2(formatC(sum(mc_mode_choice$choice == "Cycle"), big.mark = ",")),
                              p("estudiantes"))
                   )
                 ),
                 
                 br(),
                 br(),
                 
                 # Selector de graficos
                 fluidRow(
                   column(3,
                          div(class = "card",
                              div(class = "card-body",
                                  selectInput(
                                    inputId = "modo_transporte",
                                    label = "Modo de transporte de los encuestados:",
                                    choices = c("Walk", "Car", "HSR", "Cycle"),
                                    selected = "Car",
                                    multiple = TRUE
                                  ),
                                  selectInput(
                                    inputId = "genero",
                                    label = "Género:",
                                    choices = c("Todos" = "all",
                                                "Mujer" = "1",
                                                "Hombre" = "0"),
                                    selected = "all"
                                  )
                              )
                          )
                   ),
                   column(4,
                          div(class = "card",
                              div(class = "card-body",
                                  h4("Disponibilidad de elegir otro modo de transporte"),
                                  plotlyOutput("plot_disponibilidad")
                              )
                          )
                   ),
                   column(5,
                          div(class = "card",
                              div(class = "card-body",
                                  h4("Disponibilidad de elegir otro modo de transporte"),
                                  plotlyOutput("plot_heatmap")
                              )
                          )
                   )
                 ),
                 
                 br(),
                 
                 # Sección para el gráfico de densidad
                 fluidRow(
                   column(12,
                          div(class = "card",
                              div(class = "card-body",
                                  h4("Distribución de tiempos de viaje"),
                                  plotlyOutput("plot_density")
                              )
                          )
                   )
                 )
        )
      )
  ),
  
  div(class = "footer",
      p("© 2010 Universidad McMaster | Encuesta de Viajes | Creado con Shiny por Marco Percastre")
  )
)

server <- function(input, output, session) {
  
  # Creo reactivo para filtrar datos
  data_reactive <- reactive({
    req(input$modo_transporte)
    
    datos_filtrados <- mc_mode_choice %>%
      { if(input$genero != "all") filter(., gender == as.numeric(input$genero)) else . } %>%
      filter(choice %in% input$modo_transporte)
  })
  
  output$plot_disponibilidad <- renderPlotly({
    req(input$modo_transporte)
    
    datos_filtrados <- data_reactive() %>%
      select(choice, avcycle, avwalk, avhsr, avcar) %>% 
      gather(disponibilidad, valor, -choice) %>%
      mutate(
        disponibilidad = case_when(
          disponibilidad == "avcycle" ~ "Bicicleta",
          disponibilidad == "avwalk" ~ "Caminar",
          disponibilidad == "avhsr" ~ "Trans. Público",
          disponibilidad == "avcar" ~ "Automóvil"
        )
      )
    
    p <- ggplot(datos_filtrados, 
                aes(x = disponibilidad, 
                    y = valor, 
                    fill = choice,
                    text = paste("Modo elegido:", choice,
                                 "<br>Disponibilidad:", disponibilidad,
                                 "<br>Porcentaje:", scales::percent(mean(valor))))) +
      geom_bar(stat = "summary", 
               fun = "mean", 
               position = "dodge",
               width = 0.7) +
      scale_y_continuous(labels = scales::percent) +
      scale_fill_brewer(palette = "Set2") +
      theme_minimal() +
      labs(
        x = "Modo disponible",
        y = "Porcentaje de disponibilidad",
        fill = "Modo elegido"
      ) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(fill = NA, color = "gray90"),
        legend.position = "bottom",
        plot.background = element_rect(fill = "#f9f9f9"),
        panel.background = element_rect(fill = "#f9f9f9")
      )
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        showlegend = TRUE,
        legend = list(orientation = "h", y = -0.2),
        margin = list(b = 100)
      )
  })
  
  output$plot_heatmap <- renderPlotly({
    req(input$modo_transporte)
    
    datos_heatmap <- data_reactive() %>%
      { if(input$genero != "all") filter(., gender == as.numeric(input$genero)) else . } %>%
      filter(choice %in% input$modo_transporte) %>%
      group_by(choice) %>%
      summarise(
        Bicicleta = mean(avcycle),
        Caminar = mean(avwalk),
        `Trans. Público` = mean(avhsr),
        Automóvil = mean(avcar)
      ) %>%
      gather(disponibilidad, valor, -choice)
    
    p <- ggplot(datos_heatmap, 
                aes(x = choice, 
                    y = disponibilidad, 
                    fill = valor,
                    text = paste("Modo elegido:", choice,
                                 "<br>Modo disponible:", disponibilidad,
                                 "<br>Disponibilidad:", scales::percent(valor)))) +
      geom_tile() +
      scale_fill_viridis_c(labels = scales::percent, option = "D") +
      theme_minimal() +
      labs(
        x = "Modo elegido",
        y = "Modo disponible",
        fill = "Disponibilidad"
      ) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank(),
        legend.position = "right",
        plot.background = element_rect(fill = "#f9f9f9"),
        panel.background = element_rect(fill = "#f9f9f9")
      )
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        margin = list(b = 80),
        showlegend = TRUE
      )
  })
  
  output$plot_density <- renderPlotly({
    
    # filtered_data <- if (input$genero != "all") {
    #   mc_mode_choice[mc_mode_choice$gender == input$genero, ]
    # } else {
    #   mc_mode_choice
    # }
    
    # Crear el plot base
    p <- plot_ly()
    
    
    color_map <- c("Walk" = "#FFA07A", 
                   "Car" = "#87CEEB",
                   "HSR" = "#98FB98",
                   "Cycle" = "#DDA0DD")
    
    
    for(modo in input$modo_transporte) {
      
      time_col <- switch(modo,
                         "Walk" = data_reactive()$timewalk,
                         "Car" = data_reactive()$timecar,
                         "HSR" = data_reactive()$timehsr,
                         "Cycle" = data_reactive()$timecycle)
      
      
      dens <- density(time_col, na.rm = TRUE)
      
      
      p <- p %>% add_trace(x = dens$x,
                           y = dens$y,
                           name = modo,
                           type = 'scatter',
                           mode = 'lines',
                           fill = 'tonexty',
                           fillcolor = adjustcolor(color_map[modo], alpha.f = 0.6),
                           line = list(color = color_map[modo]),
                           hoverinfo = "x+y+name")
    }
    
    
    p %>% layout(
      xaxis = list(title = "Tiempo (minutos)",
                   zeroline = FALSE),
      yaxis = list(title = "Densidad",
                   zeroline = FALSE),
      showlegend = TRUE,
      title = list(
        text = "",
        x = 0.5
      ),
      legend = list(
        orientation = "h",
        xanchor = "center",
        x = 0.5,
        y = -0.2
      ),
      plot_bgcolor = "#f9f9f9",
      paper_bgcolor = "#f9f9f9"
    )
  })
}



shinyApp(ui, server)

