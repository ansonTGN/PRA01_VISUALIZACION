
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

# Interfaz de usuario
ui <- fluidPage(
  titlePanel("Datos de Salud Mental (Cataluña)"),
  sidebarLayout(
    sidebarPanel(
      selectInput("dataset", "Seleccionar conjunto de datos", choices = c("Ingresos Anorexia", "Ingresos Urgencias Jóvenes", "Reingresos por Sexo y Edad", "Datos Anorexia", "Reingresos Adultos 30 dias", "Evolucion Personas atendidas 2017-2022",  "Tasa 1000 habitantes jovenes", "Media Visitas 2022")),
      uiOutput("yearInput") # Selector de año
    ),
    mainPanel(
      plotOutput("linePlot"),
      textOutput("colorExplanation"), # Texto explicativo para la leyenda de colores
      dataTableOutput("yearTable")  # Elemento para la tabla
    )
  ),
  # Añadir un pie de página con la fuente de los datos
  tags$footer(
    tags$p("Fuente: Datos Central de Resultados (Departament Salut Generalitat)", 
           style = "text-align: center; color: grey; font-size: 0.8em; padding-top: 10px;")
  )
)

# Funciones del servidor
server <- function(input, output) {
  # Actualizar dinámicamente la selección del año
  output$yearInput <- renderUI({
    dataset <- switch(input$dataset,
                      "Ingresos Anorexia" = data1,
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

output$linePlot <- renderPlot({
  data_selected <- switch(input$dataset,
                          "Ingresos Anorexia" = data1,
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
                            "Ingresos Anorexia" = data1,
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
                            "Ingresos Anorexia" = data1,
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
# Ejecutar la aplicación
shinyApp(ui, server)




