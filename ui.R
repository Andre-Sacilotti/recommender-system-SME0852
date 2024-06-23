library(shiny)
library(shinydashboard)
library(recommenderlab)
library(data.table)
library(ShinyRatingInput)
library(shinyjs)
library(dplyr)
library(shinyWidgets)
library(DT)
header <- dashboardHeader(
  title = "Recomendação de Filmes", disable=TRUE
)

model_column <- box(width = NULL, status = "warning",
    selectInput(
        "model", "Selecione o Modelo Desejado:",
        list("UBCF","SVD++"), multiple=F
    ),

    actionButton("update_model", "Atualizar Modelo")
)

filter_column <- column(width = 12,
    box(width = NULL, status = "warning",
        textInput("nome", "Nome do Filme", ""),
        selectInput(
            "genre", "Selecione o Genero do Filme:",
            generos_filtrados, multiple=TRUE
        ),
          airDatepickerInput(
          inputId = "ano",
          label = "Select a range of years:",
          view = "years",
          minView = "years",
          dateFormat = "yyyy",
          range = TRUE,
          minDate = "1902-01-01",
          maxDate = "2019-12-31",
          clearButton = TRUE
        ),
   
        
        #checkboxInput("avaliados", "Exibir Apenas Filmes Avaliados", FALSE),
        
        actionButton("update_filter", "Limpar Filtros")
    ),
    model_column
)




movies_column <- box(width = NULL, solidHeader = TRUE,
                     tabsetPanel(
                    tabPanel(
                titlePanel("Avaliação"),
                column(width = 12,br(),
                  
                      uiOutput('movies_grid'),
              
                      fluidRow(
                        column(2,offset =4, 
                          actionButton("previous_page", "Pagina Anterior")),
                        column(1,
                          textOutput("page")),
                        column(2,
                          actionButton("next_page", "Proxima Pagina")
                      )
                  ))),
              tabPanel(titlePanel("Avaliados"),
                       column(width = 12, br(),
                              uiOutput("movies_grid_avaliados"),
                              fluidRow(
                                column(2,offset =4, 
                                       actionButton("previous_page_avaliados", "Pagina Anterior")),
                                column(1,
                                       textOutput("page_avaliados")),
                                column(2,
                                       actionButton("next_page_avaliados", "Proxima Pagina")
                                )
                              )
                              )),
              
              tabPanel(titlePanel("Recomendação"),
                           column(width = 12, br(),
                                  uiOutput('movies_grid_recomendados')))))
      
body <- dashboardBody(
#  dropdownButton(
#    tags$h3("List of Input"),
#    selectInput(inputId = 'xcol', label = 'X Variable', choices = names(iris)),
#    selectInput(inputId = 'ycol', label = 'Y Variable', choices = names(iris), selected = names(iris)[[2]]),
#    sliderInput(inputId = 'clusters', label = 'Cluster count', value = 3, min = 1, max = 9),
#    circle = TRUE,
#    status = "danger", 
#    icon = icon("gear"), width = "300px",
#    tooltip = tooltipOptions(title = "Click to see inputs !")
#  ),
  fluidRow(column(3,filter_column),
           column(9,movies_column))
)

ui <- dashboardPage(
  header,
  dashboardSidebar(disable = TRUE),
  body
)

