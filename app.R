library(shiny)
library(shinydashboard)
library(tidyverse)
library(reactable)
library(gt)
library(patchwork)
library(glue)
library(here)

df <- read_rds("analisis2.rds")
df2 <- df %>% pluck("df")

ui <- 
  dashboardPage(
    dashboardHeader(disable = TRUE),
    dashboardSidebar(disable = TRUE, collapsed = TRUE, width = 0),
    dashboardBody(
      fluidRow(
        column(
          width = 6,
          selectInput(
            "select_sector",
            label = "Seleccionar sector",
            choices = df2 %>% names() %>% str_split_i("_", 1) %>% sort()
          )
        ),
        column(
          width = 6,
          selectInput(
            "select_nivel",
            label = "Seleccionar nivel",
            choices = df2 %>% names() %>% str_split_i("_", 2) %>% as.numeric() %>% sort()
          )
        )
      ),
      fluidRow(
        tabBox(
          width = 12,
          tabPanel(
            title = "Correlación",
            plotOutput("plot5")
          ),
          tabPanel(
            title = "Distribución NL",
            tabBox(
              width = 12,
              tabPanel(
                title = "Agregado",
                plotOutput("plot2")
              ),
              tabPanel(
                title = "Por Año",
                plotOutput("plot4")
              )
            )
          ),
          tabPanel(
            title = "Comparación por NL",
            fluidRow(
              column(plotOutput("plot3"), width = 9),
              column(gt_output("corr"), width = 3)
            )
          ),
          tabPanel(
            title = "Simulación puntos de corte",
            fluidRow(
              column(plotOutput("plot1"), width = 9),
              column(gt_output("table2"), width = 3)
            )
          ),
          tabPanel(
            title = "Data",
            reactableOutput("react_df")
          )
        )
      )
    )
  )

server <- function(input, output) {

  output$plot1 <- 
    renderPlot({
      df %>% 
        pluck("puntaje_corte", glue("{input$select_sector}_{input$select_nivel}"), "plot_cortesAbsolutos")
    })
  
  output$plot2 <-
    renderPlot({
      df %>%
        pluck("distribucion", glue("{input$select_sector}_{input$select_nivel}"), "plot1")
    })
  
  output$plot3 <-
    renderPlot({
      df %>%
        pluck("distribucion", glue("{input$select_sector}_{input$select_nivel}"), "plot2")
    })
  
  output$plot4 <-
    renderPlot({
      df %>%
        pluck("distribucion_year", glue("{input$select_sector}_{input$select_nivel}"))
    })
  
  output$corr <-
    render_gt({
      
      temp <- 
        df %>% 
        pluck("ranks", glue("{input$select_sector}_{input$select_nivel}"), "correlaciones")
      
      temp %>% 
        gt() %>% 
        cols_align("center") %>% 
        fmt_number(columns = c("p_value", "r"), decimals = 2)
    })
  
  output$table2 <-
    render_gt({
      
      temp <- 
        df %>% 
        pluck("puntaje_corte", glue("{input$select_sector}_{input$select_nivel}"), "interpolacion")
      
      temp %>% 
        gt() %>% 
        cols_align("center")
      
    })
  
  output$plot5 <-
    renderPlot({
      df %>% 
        pluck("correlaciones", glue("{input$select_sector}_{input$select_nivel}"), "plot")
    })
  
  data_natural <- 
    reactive({
      df %>% 
        pluck("deltas", glue("{input$select_sector}_{input$select_nivel}"))
    })
  
  output$react_df <-
    renderReactable({
      temp <- data_natural()
      
      temp %>% 
        reactable(
          pagination = FALSE, 
          compact = TRUE,
          filterable = TRUE,
          searchable = TRUE,
          height = "60vh",
          defaultColDef = colDef(
            align = "center"
          )
        )
    })
}

shinyApp(ui, server)