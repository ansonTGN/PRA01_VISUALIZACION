
# Instalar el paquete Shiny si no está instalado
if (!require("shiny", quietly = TRUE)) {
    install.packages("shiny")
    library(shiny)
}

# Instalar ggplot2 para la visualización de datos si no está instalado
if (!require("ggplot2", quietly = TRUE)) {
    install.packages("ggplot2")
    library(ggplot2)
}

# Instalar readr para leer archivos CSV si no está instalado
if (!require("readr", quietly = TRUE)) {
    install.packages("readr")
    library(readr)
}

# Instalar tidyr si no está instalado (para trabajar con datos en formato "largo")
if (!require("tidyr", quietly = TRUE)) {
    install.packages("tidyr")
    library(tidyr)
}

# Instalar tidyr si no está instalado (para trabajar con datos en formato "largo")
if (!require("rsconnect", quietly = TRUE)) {
    install.packages("rsconnect")
    library(rsconnect)
}

# Instalar tidyr si no está instalado (para trabajar con datos en formato "largo")
if (!require("shinydashboard", quietly = TRUE)) {
  install.packages("shinydashboard")
  library(shinydashboard)
}


library(shiny)
library(ggplot2)
library(dplyr)
library(readxl)
library(DT)

# Función para cargar y transformar los datos
load_and_transform_data <- function(file_path) {
  tryCatch({
    data <- read_excel(file_path)
    # Convertir todas las columnas (excepto la primera) a tipo numérico
    data <- data %>% mutate(across(-1, ~as.numeric(as.character(.))))
    # Transformar los datos de formato ancho a largo
    data_long <- pivot_longer(data, cols = -1, names_to = "Year", values_to = "Count")
    return(data_long)
  }, error = function(e) {
    return(data.frame())
  })
}

# Cargar los datos con manejo de errores
data1 <- load_and_transform_data("Taula14_Ingresos_Anorexia2017-2022.xlsx")
data2 <- load_and_transform_data("Taula12_IngresosUrgenciasJovenesPor edad.xlsx")
data3 <- load_and_transform_data("Taula11_ReingresosSexoEdad.xlsx")
data6 <- load_and_transform_data("Taula13_Anorexia2017-2022.xlsx")
data7 <- load_and_transform_data("Taula10_ReingresosAdultos30dias.xlsx")
data8 <- load_and_transform_data("Taula3_Evolucion_Personas_atendidas2017-2022.xlsx")
data9 <- load_and_transform_data("Taula4_Taxa1000habitantes_jovenes.xlsx")
data10 <- load_and_transform_data("Taula2_Media_Visitas_2022.xlsx")

# Lista completa de textos para cada conjunto de datos
datasetTexts <- list(
  "Ingresos Transtorno de Conducta" = "¿Cuantas personas ingresan por transtorno de conducta?",
  "Ingresos Urgencias Jóvenes" = "¿Cuantos Ingresos Urgencias Jóvenes?",
  "Reingresos por Sexo y Edad" = "¿Cuantos Reingresos por Sexo y Edad",
  "Datos Anorexia" = "Datos Anorexia",
  "Reingresos Adultos 30 dias" = "¿Reingresos Adultos 30 días?",
  "Evolucion Personas atendidas 2017-2022" = "Evolución de Personas Atendidas 2017-2022",
  "Tasa 1000 habitantes jovenes" = "¿Cual es el nivel del problema?",
  "Media Visitas 2022" = "¿Cual es la Media de Visitas por persona segun tipologia?"
)

# Lista completa de textos para cada conjunto de datos
datasetTexts1 <- list(
  "Ingresos Transtorno de Conducta" = "Los ingresos por transtorno de conducta de adolescentes (hombres) duplican el de las mujeres de la misma edad",
  "Ingresos Urgencias Jóvenes" = "El 62,3% de los ingresos psiquiatricos de menores van a ser urgentes. Por grupos de edad los que mas han aumentado son los de niños y niñas menores de once años",
  "Reingresos por Sexo y Edad" = "Se observa que las mujeres reingresan más que los hombres en todos los grupos de edad. Especialmente entre los 18 y 24 años",
  "Datos Anorexia" = "Los ingresos hospitalarios por transtornos de conducta alimentaria han aumentado progresivamente desde el 2018",
  "Reingresos Adultos 30 dias" = "Por sexo y edad se observa que las mujeres reingresan mas que los hombres, en todos los rangos de edad pero especialmente entre 18 y 24 años",
  "Evolucion Personas atendidas 2017-2022" = "La media de visitas por personas atendidas en el 2022 es de 7,4 y ha ido aumentando desde el 2017",
  "Tasa 1000 habitantes jovenes" = "A partir del 2020 se incrementa la tasa de niños y jovenes atendidos en los centros de salud mental",
  "Media Visitas 2022" = "En el año 2022 el 39,1% de las personas atendidas son pacientes cronicos de salud mental"
)

# Interfaz de usuario
ui <- fluidPage(
  
  titlePanel("Datos de Salud Mental (Cataluña)"),
    tags$head(
      tags$style(HTML("
      .selected-dataset-text {
        font-size: 18px; /* Tamaño del texto */
        color: #2E86C1; /* Color del texto, en formato hexadecimal */
        font-family: 'Arial', sans-serif; /* Tipo de letra */
        margin-bottom: 20px; /* Espacio debajo del texto */
        padding: 10px; /* Relleno alrededor del texto */
        text-align: center; /* Alineación del texto */
        background-color: #EAF2F8; /* Color de fondo */
        border-radius: 5px; /* Bordes redondeados */
        box-shadow: 0px 4px 8px rgba(0, 0, 0, 0.1); /* Sombra sutil */
      }
    "))
    ),
  sidebarLayout(
    sidebarPanel(
      selectInput("dataset", "Seleccionar conjunto de datos", choices = c("Ingresos Transtorno de Conducta", "Ingresos Urgencias Jóvenes", "Reingresos por Sexo y Edad", "Datos Anorexia", "Reingresos Adultos 30 dias", "Evolucion Personas atendidas 2017-2022", "Tasa 1000 habitantes jovenes", "Media Visitas 2022")),
      uiOutput("yearInput"), 
      actionButton("showModal", "Mostrar Ventana"),
      # Recuadro mejorado para el texto del dataset1
      wellPanel(
        tags$head(
          tags$style(HTML("
            .well {
              background-color: #F7F7F7; /* Color de fondo */
              border: 1px solid #E3E3E3; /* Borde del recuadro */
              border-radius: 10px; /* Bordes redondeados */
              box-shadow: 3px 3px 5px rgba(0, 0, 0, 0.1); /* Sombra sutil */
              padding: 15px; /* Espaciado interno */
              margin-top: 20px; /* Espacio superior */
              margin-bottom: 20px; /* Espacio inferior */
              font-family: 'Arial', sans-serif; /* Tipo de fuente */
            }
            .well h4 {
              color: #333333; /* Color del título */
              margin-top: 0; /* Espacio superior del título */
            }
          "))
        ),
        h4("Información Adicional"),  # Título del recuadro
        textOutput("selectedDataset1Text")
      )
    ),
    mainPanel(
      # Envolver textOutput en un div para aplicar estilos CSS
      div(textOutput("selectedDatasetText"), class = "selected-dataset-text"),
      plotOutput("linePlot"),
      textOutput("colorExplanation"),
      dataTableOutput("yearTable")
    )
  ),


  # Añadir un pie de página con la fuente de los datos
  tags$footer(
    tags$p("Fuente: Central de Resultats (Salut, Agéncia de Qualitat i Avaluació Sanitàrias de Catalunya)", 
           style = "text-align: center; color: grey; font-size: 0.8em; padding-top: 10px;")
  )
  ## Ventana emergente
  #modalDialog(
  #  title = "Mensaje de Prueba",
  #  "Hola Prueba",
  #  easyClose = TRUE,
  #  footer = NULL,
  #  id = "testModal" # Identificador de la ventana emergente
  #)
)

# Funciones del servidor
server <- function(input, output) {
  # Actualizar dinámicamente la selección del año
  output$yearInput <- renderUI({
    dataset <- switch(input$dataset,
                      "Ingresos Transtorno de Conducta" = data1,
                      "Ingresos Urgencias Jóvenes" = data2,
                      "Reingresos por Sexo y Edad" = data3,
                      "Ingresos Urgencias Jovenes Por edad" = data4,
                      "Reingresos Sexo Edad" = data5,
                      "Datos Anorexia" = data6,
                      "Reingresos Adultos 30 dias" = data7,
                      "Evolucion Personas atendidas 2017-2022" = data8,
                      "Tasa 1000 habitantes jovenes" = data9,
                      "Media Visitas 2022" = data10)
    selectInput("year", "Año", choices = unique(dataset$Year))
  })

  # Actualizar el texto basado en la selección del conjunto de datos
  output$selectedDatasetText <- renderText({
    selectedDataset <- input$dataset
    if (selectedDataset %in% names(datasetTexts)) {
      return(datasetTexts[[selectedDataset]])
    } else {
      return("Texto por defecto para conjuntos de datos no especificados")
    }
  })
  
  # Función para mostrar el texto del dataset1
  output$selectedDataset1Text <- renderText({
    selectedDataset <- input$dataset
    if (selectedDataset %in% names(datasetTexts1)) {
      return(datasetTexts1[[selectedDataset]])
    } else {
      return("Texto por defecto para conjuntos de datos no especificados en dataset1")
    }
  })
  
output$linePlot <- renderPlot({
  data_selected <- switch(input$dataset,
                          "Ingresos Transtorno de Conducta" = data1,
                          "Ingresos Urgencias Jóvenes" = data2,
                          "Reingresos por Sexo y Edad" = data3,
                          "Ingresos Urgencias Jovenes Por edad" = data4,
                          "Reingresos Sexo Edad" = data5,
                          "Datos Anorexia" = data6,
                          "Reingresos Adultos 30 dias" = data7,
                          "Evolucion Personas atendidas 2017-2022" = data8,
                          "Tasa 1000 habitantes jovenes" = data9,
                          "Media Visitas 2022" = data10)

  if (nrow(data_selected) == 0) {
    return(NULL)
  }

  first_column_name <- names(data_selected)[1] # Asumiendo que es la columna de agrupación
  category_column <- sym(names(data_selected)[2])

  ggplot(data_selected, aes(x = Year, y = Count, fill = !!category_column)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    geom_text(aes(label = Count), vjust = -0.3, position = position_dodge(0.9)) +
    facet_wrap(~ get(first_column_name), scales = "free_x", ncol = 3) + # Añadir facet_wrap
    theme_minimal() +
    theme(text = element_text(size = 12),
          legend.position = "bottom",
          plot.title = element_text(size = 14, face = "bold"),
          strip.text.x = element_text(size = 10, face = "bold")) + # Ajustar el tamaño del texto de los paneles
    labs(title = paste("Evolución Anual -", input$dataset),
         x = "Año",
         y = "Conteo") +
    scale_fill_viridis_d()
})

# Mostrar datos del año seleccionado
  output$yearData <- renderText({
    data_selected <- switch(input$dataset,
                            "Ingresos Transtorno de Conducta" = data1,
                            "Ingresos Urgencias Jóvenes" = data2,
                            "Reingresos por Sexo y Edad" = data3,
                            "Ingresos Urgencias Jovenes Por edad" = data4,
                            "Reingresos Sexo Edad" = data5,
                            "Datos Anorexia" = data6,
                            "Reingresos Adultos 30 dias" = data7,
                            "Evolucion Personas atendidas 2017-2022" = data8,
                            "Tasa 1000 habitantes jovenes" = data9,
                            "Media Visitas 2022" = data10)
    selected_year_data <- filter(data_selected, Year == input$year)

    if (nrow(selected_year_data) == 0) {
      return("No hay datos disponibles para este año.")
    } else {
      return(paste("Datos del año", input$year, ":", 
                   toString(selected_year_data$Count)))
    }
  })
  
# Mostrar tabla de datos del año seleccionado
  output$yearTable <- renderDataTable({  
    data_selected <- switch(input$dataset,
                            "Ingresos Transtorno de Conducta" = data1,
                            "Ingresos Urgencias Jóvenes" = data2,
                            "Reingresos por Sexo y Edad" = data3,
                            "Ingresos Urgencias Jovenes Por edad" = data4,
                            "Reingresos Sexo Edad" = data5,
                            "Datos Anorexia" = data6,
                            "Reingresos Adultos 30 dias" = data7,
                            "Evolucion Personas atendidas 2017-2022" = data8,
                            "Tasa 1000 habitantes jovenes" = data9,
                            "Media Visitas 2022" = data10)
selected_year_data <- filter(data_selected, Year == input$year)

if (nrow(selected_year_data) == 0) {
  return(data.frame())
} else {
  return(selected_year_data)
}
}, options = list(pageLength = 5)) # Configuración opcional para limitar el número de filas mostradas
}


## Observar el evento del botón showModal
#observeEvent(input$showModal, {
#  # Mostrar la ventana emergente
#  showModal(modalDialog(
#    title = "PRA01 UCO Visualizacion",
#    "Hola Prueba",
#    easyClose = TRUE,  # Permite cerrar la ventana haciendo clic fuera de ella
#    footer = modalButton("Cerrar")  # Botón para cerrar explícitamente la ventana
# ))
#}
#)

# Ejecutar la aplicación
shinyApp(ui, server)




