library(shinyRatings)


### Local ##
## Lendo a base



if(.Platform$OS.type == "unix") {
  movies = read.csv("./data/movies.csv", sep=",")   #, colClasses = c(NA, "NULL"), header = F)

} else {
  movies = read.csv("C:/Users/zabuz/Documents/GitHub/recommender-system-SME0852/data/movies.csv", sep=",")   #, colClasses = c(NA, "NULL"), header = F)

}



names(movies) <- c('movieID','name','genres','image','ano_lanc')

movies$ano_lanc <- as.integer(gsub(".*\\((.*)\\).*", "\\1", movies$name))

###
#Join
#movies_metadata = read.csv("C:/Users/zabuz/Documents/GitHub/recommender-system-SME0852/data/ml-1m/ml1m_images.csv")
#total_movies = nrow(movies_metadata)
#
#movies <- merge(movies_metadata, movies, by=c("item_id"))

###
#Base de ratings


if(.Platform$OS.type == "unix") {
  ratings = read.csv("./data/ratings.csv", sep=",")

} else {
  ratings = read.csv("C:/Users/zabuz/Documents/GitHub/recommender-system-SME0852/data/ratings.csv", sep=",")

}




ratings$userId <- as.factor(ratings$userId)
ratings$movieId <- as.factor(ratings$movieId)
ratings$rating <- as.numeric(ratings$rating)
ratings <- as(ratings, "realRatingMatrix")

#Chamando ratings de train
train <- ratings#[1:5000]



#### Criando a lista dos generos
generos_unidos <- paste(unique(movies$genres), collapse = "|")
generos_separados <- unlist(strsplit(generos_unidos, "\\|"))
generos_unicos <- unique(generos_separados)
generos_para_remover <- c("", " Miami Beach (1988)")
generos_filtrados <- setdiff(generos_unicos, generos_para_remover)


####




server <- function(input, output, session) {

    actual_page <- reactiveValues(data = 1)
    actual_page_avaliados <- reactiveValues(data = 1)
    filtered_movies <- reactiveVal(NULL)
    lista_movies <- reactiveVal(NULL)
    ratings_df <- reactiveVal(NULL)
    n_rows <- reactiveValues(data = 3)
    n_movies_per_row <- reactiveValues(data = 4)
    
    check_genres <- function(genres, genre_list) {
      any(sapply(genre_list, grepl, genres))
    }
    
    #Filtro 
    filtered_movies <- reactive({
      Base <- NULL
      if (gsub(" ", "", input$nome) == ""){
        Base <- movies
      }
      else{
        Base <- movies[grepl(tolower(input$nome), tolower(movies$name)),]
      }
      
      if (length(input$genre) > 0){
        Base <- Base[sapply(Base$genres, check_genres, input$genre), ]
        
      }
      
      if (!is.null(input$ano)) {
        if (length(input$ano) == 2){
        start_year <- format(input$ano[1], "%Y")
        end_year <- format(input$ano[2], "%Y")
        Base <- Base[Base$ano_lanc >= start_year & Base$ano_lanc <= end_year,]}
        else {
          start_year <- format(input$ano[1], "%Y")
          Base <- Base[Base$ano_lanc == start_year ,]}
        }
      
      actual_page$data <- 1
      Base
     
    })
    
    observeEvent(input$update_filter,{ 
      updateCheckboxGroupInput(session, "nome", selected = character(0))
      updateCheckboxGroupInput(session, "genre", selected = character(0))
      updateAirDateInput(session, "ano", clear = TRUE)
      
      })
    
    
    


    observeEvent(input$previous_page, {
        print(actual_page$data)
        if (actual_page$data > 1){
            actual_page$data = actual_page$data - 1
        }
    })
    
    observeEvent(input$previous_page_avaliados, {
      print(actual_page_avaliados$data)
      if (actual_page_avaliados$data > 1){
        actual_page_avaliados$data = actual_page_avaliados$data - 1
      }
    })

    observeEvent(input$next_page, {
        n_rows = n_rows$data
        n_movies_per_row = n_movies_per_row$data
        n_pages = as.numeric(nrow(filtered_movies())/(n_rows*n_movies_per_row))

        if (actual_page$data < n_pages){
            actual_page$data = actual_page$data + 1
        }
    })
    
    observeEvent(input$next_page_avaliados, {
      n_rows = n_rows$data
      n_movies_per_row = n_movies_per_row$data
      n_pages = as.numeric(nrow(ratings_df())/(n_rows*n_movies_per_row))
      if (actual_page_avaliados$data < n_pages){
        actual_page_avaliados$data = actual_page_avaliados$data + 1
      }
    })

    output$page <- renderText({ actual_page$data })
    output$page_avaliados <- renderText({ actual_page_avaliados$data })
    
    
    
    ratings <- reactiveValues(data = list(), rated_movies = list())

    
    
    output$movies_grid <- renderUI({
      n_rows <- n_rows$data
      n_movies_per_row <- n_movies_per_row$data
      actual_page <- actual_page$data
      n_pages <- as.integer(nrow(filtered_movies()) / (n_rows * n_movies_per_row))
      linhas_base <- nrow(filtered_movies())

      mm <- filtered_movies()
      print("Teste?")
      
      isolate({
      lapply(1:n_rows, function(i) {
        list(fluidRow(lapply(1:n_movies_per_row, function(j) {
          index <- (i - 1) * n_movies_per_row + j + (actual_page - 1) * (n_rows * n_movies_per_row)
          if (index <= linhas_base) {
            print("Teste2?")
            movie_id <- mm$movieID[index]
            rating_id <- paste0("select_", movie_id)
            ratings$rated_movies[[rating_id]] <- movie_id
            
            if (is.null(ratings$data[[rating_id]])) {
              ratings$data[[rating_id]] <- 0
              ratings$rated_movies[[rating_id]] <- NULL
            }
            
            list(column(width = 3, height = 250,
                        div(
                          style = "text-align:center; padding-bottom: 13px;",
                          div(
                            style = "text-align:center",
                            img(src = mm$image[index], height = 150)
                          ),
                          div(
                            style = "text-align:center; max-height: 40px; height: 40px",
                            strong(mm$name[index])
                          ),
                          div(
                            style = "text-align:center; font-size: 150%; color: #f0ad4e;",
                            ratingInput(
                              inputId = rating_id,
                              label = "",
                              value = ratings$data[[rating_id]],
                              dataStop = 5, includeBootstrapCSS=T
                            )
                          )
                        )
            ))
          } else {
            list(column(width = 3, height = 250,
                        div(
                          style = "text-align:center; padding-bottom: 13px;",
                          div(
                            style = "text-align:center; height: 150px;"
                          ),
                          div(
                            style = "text-align:center; max-height: 40px; height: 40px"
                          ),
                          div(
                            style = "text-align:center; font-size: 150%; color: #f0ad4e;"
                          )
                        )
            ))
          }
        })))
      })})
    })
    
    
    output$movies_grid_avaliados <- renderUI({
      K <- ratings_df()
      K$movieID <- as.integer(K$movieID)
      K <- left_join(K,filtered_movies(), by = "movieID")
      K <- na.omit(K)
      
      n_rows <- n_rows$data
      n_movies_per_row <- n_movies_per_row$data
      actual_page <- actual_page_avaliados$data

      linhas_base <- nrow(K)
      start_index <- 1 + (actual_page - 1) * n_rows * n_movies_per_row
      
      lapply(1:n_rows, function(i) {
        fluidRow(lapply(1:n_movies_per_row, function(j) {
          index <- start_index + (i - 1) * n_movies_per_row + (j - 1)
          if (index <= linhas_base) {
            movie <- K[index, ]
            nota <- movie$rating
            column(width = 3, height = 250,
                   div(
                     style = "text-align:center; padding-bottom: 13px;",
                     div(
                       style = "text-align:center",
                       img(src = movie$image, height = 150)
                     ),
                     div(
                       style = "text-align:center; max-height: 40px; height: 40px",
                       strong(movie$name)
                     ),
                     div(
                       style = "text-align:center; font-size: 150%; color: #f0ad4e;",
                       ratingInput(
                         inputId = paste0("rating_", index),
                         label = "",
                         value = nota,
                         dataStop = 5,
                         disabled = TRUE, includeBootstrapCSS=T
                       )
                     )
                   )
            )
          }
        }))
      })
    })
    
      
   
    
    observe({
      lapply(names(ratings$data), function(rating_id) {
        observeEvent(input[[rating_id]], {
          if (!is.null(input[[rating_id]]) && input[[rating_id]] > 0) {
            ratings$data[[rating_id]] <- input[[rating_id]]
            ratings$rated_movies[[rating_id]] <- substr(rating_id, 8, nchar(rating_id))  # Extract movieID
          } else {
            ratings$data[[rating_id]] <- 0
            ratings$rated_movies[[rating_id]] <- NULL  
          }
          
          all_keys <- unique(c(names(ratings$data), names(ratings$rated_movies)))
          
          ratings_df_value <- data.frame(
            rating_id = all_keys,
            movieID = sapply(all_keys, function(rating_id) {
              if (!is.null(ratings$rated_movies[[rating_id]])) {
                ratings$rated_movies[[rating_id]]
              } else {
                NA
              }
            }),
            rating = sapply(all_keys, function(rating_id) {
              if (!is.null(ratings$data[[rating_id]])) {
                ratings$data[[rating_id]]
              } else {
                NA
              }
            })
          )
          
          ratings_df_value <- ratings_df_value[!is.na(ratings_df_value$movieID),]
          ratings_df_value <- ratings_df_value[!is.null(ratings_df_value$movieID),]
          
          
          ratings_df(ratings_df_value)
        })
        })
        })
      
    recom <- reactive({
      print("Changing recommender")
      print(input$model)
      if (input$model == "HybridRecommender"){
        # Recommender(b_train, method = input$model)
        HybridRecommender(
          Recommender(train, method = "POPULAR"),
          Recommender(train, method = "RANDOM"),
          Recommender(train, method = "UBCF", param=list(method=input$sim_method, nn=500)),
          weights = c(.2, .5, .3)
        )
      }else if(input$model == "UBCF"){
        
        Recommender(train, method = input$model, param=list(method=input$sim_method, nn=500))
      }
      else{
        Recommender(train, method = input$model)
      }
      

    })
            
    output$movies_grid_recomendados <- renderUI({
        user <- c()
        item <- c()
        rating <- c()

        K <- ratings_df()
        K$movieID <- as.integer(K$movieID)
        

        ratings_rec <- matrix(
          NA, nrow = 1, ncol = ncol(train)
        )
        ratings_rec <- matrix(
          sample(c(NA,0:0),ncol(train), replace=TRUE, prob=c(.99,rep(.001/6,1))),
          
          nrow=1, ncol=ncol(train), dimnames = list(
          user=paste('u', 1:1, sep=''),
          item=paste('i', 1:ncol(train), sep='')
        ))

        total_ratings <- 0
        for (i in 1:nrow(K)){
          
          movie_id = K[i,]$movieID
          rating_ = K[i,]$rating


          if (!is.null(rating_)){
            
            ratings_rec[, movie_id] <- as.double(rating_)
            total_ratings <- total_ratings + 1
          }
          
        }

        if (total_ratings < 5){
          return ("Avalie pelo menos 5 filmes")
        }


        # print(ratings_rec[,1:10])
        ratings_conv <- as(ratings_rec, "realRatingMatrix")



        n_rec <- 5
        pred <- predict(
          recom(),
          ratings_conv,
          n = n_rec
        )

        rec = as(pred, "list")[[1]]



        K <- movies

        n_rows <- 2
        n_movies_per_row <- 4
        linhas_base <- n_rec
        print(n_rec)
        print(rec)
        start_index <- 1
        
        lapply(1:n_rows, function(i) {
            fluidRow(lapply(1:n_movies_per_row, function(j) {
                index <- start_index + (i - 1) * n_movies_per_row + (j - 1)
                print("index")
                print(index)
                print(rec[index])
                if (index <= linhas_base) {
                  print(as.integer(rec[index]))
                  if (input$model=="HybridRecommender"){
                    rec[index] = strsplit(rec[index], "i", fixed=T)[[1]][2]
                  }
                  print(as.integer(rec[index]))

                  movie <- K[as.integer(rec[index]), ]
                  
                  print("movie")
                  print(movie)
                  column(width = 3, height = 250,
                   div(
                     style = "text-align:center; padding-bottom: 13px;",
                     div(
                       style = "text-align:center",
                       img(src = movie$image, height = 150)
                     ),
                     div(
                       style = "text-align:center; max-height: 40px; height: 40px",
                       strong(movie$name)
                     )
                  ))
                }
            }))
        })




    })    

}


if(.Platform$OS.type == "unix") {
} else {
  shinyApp(ui = ui, server = server)
}
