library(shiny)
library(shinydashboard)
library(recommenderlab)
library(data.table)
library(ShinyRatingInput)
library(shinyjs)


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

filter_column <- column(width = 3,
    box(width = NULL, status = "warning",
        textInput("nome", "Nome do Filme", ""),
        selectInput(
            "genre", "Selecione o Genero do Filme:",
            list("Action", "Drama", "Horror", "Romance"), multiple=TRUE
        ),
        dateRangeInput("ano", "Ano de Estreia do Filme:",
            start = "1920-01-01", end = "2018-12-31", format = "yyyy",separator = " Até "
        ),
        checkboxInput("avaliados", "Exibir Apenas Filmes Avaliados", FALSE),
        
        actionButton("update_filter", "Atualizar Filtros")
    ),
    model_column
)




movies_column <- column(width = 9,
    box(width = NULL, solidHeader = TRUE,
        uiOutput('movies_grid'),

        fluidRow(
            actionButton("previous_page", "Pagina Anterior"),
            textOutput("page"),
            actionButton("next_page", "Proxima Pagina")
        )
    ),
    box(width = NULL,
        actionButton("gen_recommendation", "Gerar Recomendação"),
        uiOutput('recommendation')
    )
)

body <- dashboardBody(
  fluidRow(
    filter_column,
    movies_column
  )
)

ui <- dashboardPage(
  header,
  dashboardSidebar(disable = TRUE),
  body
)