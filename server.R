#.s

movies = read.csv("./data/ml-1m/movies.dat", sep=":", colClasses = c(NA, "NULL"), header = F)
names(movies) <- c('item_id','name','genres')

movies_metadata = read.csv("./data/ml-1m/ml1m_images.csv")
total_movies = nrow(movies_metadata)

movies <- merge(movies_metadata, movies, by=c("item_id"))

ratings = read.csv("./data/ml-1m/ratings.dat", sep = ":", colClasses = c(NA, "NULL"), header = F)
names(ratings) <- c('id','item_id','rating', 'user_id')
ratings$user_id <- as.factor(ratings$user_id)
ratings$item_id <- as.factor(ratings$item_id)
ratings$rating <- as.numeric(ratings$rating)
ratings <- as(ratings, "realRatingMatrix")

train <- ratings
rec <- Recommender(train, method = "UBCF")

generos_unidos <- paste( unique(movies$genres), collapse = "|")
generos_separados <- unlist(strsplit(generos_unidos, "\\|"))
generos_unicos <- unique(generos_separados)
generos_para_remover <- c("", " Miami Beach (1988)")
generos_filtrados <- setdiff(generos_unicos, generos_para_remover)



server <- function(input, output) {

    actual_page <- reactiveValues(data = 1)
    filtered_movies <- reactiveVal(NULL)
    n_rows <- reactiveValues(data = 3)
    n_movies_per_row <- reactiveValues(data = 4)
    recommender <- reactiveValues(object = rec)
    
    filtered_movies <- reactive({
      Base <- NULL
      if (gsub(" ", "", input$nome) == ""){
        Base <- movies
      }
      else{
        Base <- movies[grepl(input$nome, movies$name),]
      }
      
      if (length(input$genre) > 0){
        Base <- Base[grepl(input$genre, movies$genres), ]
      }
      #n_rows$data <- (nrow(Base) %% 12) %% 4
      
      actual_page$data <- 1
      Base
     
    })
    
    
    output$dataframe <- renderTable({
      filtered_movies()
    })
    


    observeEvent(input$previous_page, {
        print(actual_page$data)
        if (actual_page$data > 1){
            actual_page$data = actual_page$data - 1
        }
    })

    observeEvent(input$next_page, {
        n_rows = n_rows$data
        n_movies_per_row = n_movies_per_row$data
        n_pages = as.integer(nrow(filtered_movies())/(n_rows*n_movies_per_row))
        print(actual_page$data)
        print(n_pages)
        if (actual_page$data < n_pages){
            actual_page$data = actual_page$data + 1
        }
    })

    output$page <- renderText({ actual_page$data })
    
    observeEvent(input$model, {
        if (input$model == "UBCF"){
            recommender$object <- Recommender(train, method = "UBCF")
        }else if (input$model == "SVD++"){

        }
        # ... outros
    })

    observeEvent(input$gen_recommendation, {

        user <- c()
        item <- c()
        rating <- c()

        for (i in filtered_movies()$data$item_id){
            title = movies[movies$item_id==i,][['name']]
            if (!is.null(input[[paste0("select_",i)]])){
                print("val")
                print(input[[paste0("select_",i)]])
                if (input[[paste0("select_",i)]] != ""){
                    print("ADICIONADO")
                    user <- c(user, 999999)
                    item <- c(item, title)
                    rating <- c(rating, input[[paste0("select_",i)]])
                }else{
                    user <- c(user, 999999)
                    item <- c(item, title)
                    rating <- c(rating, 0)
                }
            }else{  
            
                    user <- c(user, 999999)
                    item <- c(item, title)
                    rating <- c(rating, 0)
                }
            
        }
        print(user)
        print(item)
        print(rating)
        ratings <- data.frame(
            user = user, 
            item = item,
            rating = rating
        )
        print(ratings)
        ratings <- as(ratings, "realRatingMatrix")
        print(ratings)
        pre <- predict(recommender$object, ratings, n = 5)
        print(as(pre, "list"))
    })
    
    

    ratings <- reactiveValues(data = list())
    
    output$movies_grid <- renderUI({
      n_rows <- n_rows$data
      n_movies_per_row <- n_movies_per_row$data
      actual_page <- actual_page$data
      n_pages <- as.integer(nrow(filtered_movies()) / (n_rows * n_movies_per_row))
      w <- nrow(filtered_movies())
      
      lapply(1:n_rows, function(i) {
        list(fluidRow(lapply(1:n_movies_per_row, function(j) {
          index <- (i - 1) * n_movies_per_row + j + (actual_page - 1) * (n_rows * n_movies_per_row)
          if (index <= linhas_base) {
            movie_id <- filtered_movies()$item_id[index]
            rating_id <- paste0("select_", movie_id)
            
            # Se a classificação para esse filme ainda não existe, inicialize com NULL
            if (is.null(ratings$data[[rating_id]])) {
              ratings$data[[rating_id]] <- 0
            }
            
            list(column(width = 3, height = 250,
                        div(
                          style = "text-align:center; padding-bottom: 13px;",
                          div(
                            style = "text-align:center",
                            img(src = filtered_movies()$image[index], height = 150)
                          ),
                          div(
                            style = "text-align:center; max-height: 40px; height: 40px",
                            strong(filtered_movies()$name[index])
                          ),
                          div(
                            style = "text-align:center; font-size: 150%; color: #f0ad4e;",
                            ratingInput(
                              inputId = rating_id,
                              label = "",
                              value = ratings$data[[rating_id]],
                              dataStop = 5
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
      })
    })
    
    
    observe({
      lapply(names(ratings$data), function(rating_id) {
        observeEvent(input[[rating_id]], {
          ratings$data[[rating_id]] <- input[[rating_id]]
        })
      })
    })
    
    #output$movies_grid_avaliados <- renderUI({
    #  avaliados <- names(ratings$data)[sapply(ratings$data, function(x) x > 0)]
    #  filmes_avaliados <- filtered_movies()[filtered_movies()$item_id %in% sub("select_", "", avaliados),]
    #  
    #  n_rows <- n_rows$data
    #  n_movies_per_row <- n_movies_per_row$data
    #  linhas_base <- nrow(filmes_avaliados)
    #  
    #  lapply(1:n_rows, function(i) {
    #    list(fluidRow(lapply(1*n_movies_per_row, function(j) {
    #      index <- (i - 1) * n_movies_per_row + j
    #      if (index <= linhas_base) {
    #        movie_id <- filmes_avaliados$item_id[index]
    #        rating_id <- paste0("select_", movie_id)
    #        
    #        list(column(width = 3, height = 250,
    #                    div(
    #                      style = "text-align:center; padding-bottom: 13px;",
    #                      div(
    #                        style = "text-align:center",
    #                        img(src = filmes_avaliados$image[index], height = 150)
    #                      ),
    #                      div(
    #                        style = "text-align:center; max-height: 40px; height: 40px",
    #                        strong(filmes_avaliados$name[index])
    #                      ),
    #                      div(
    #                        style = "text-align:center; font-size: 150%; color: #f0ad4e;",
    #                        ratingInput(
    #                          inputId = rating_id,
    #                          label = "",
    #                          value = ratings$data[[rating_id]],
    #                          dataStop = 5
    #                        )
    #                      )
    #                    )
    #        ))
    #      } else {
    #        list(column(width = 3, height = 250,
    #                    div(
    #                      style = "text-align:center; padding-bottom: 13px;",
    #                      div(
    #                        style = "text-align:center; height: 150px;"
    #                      ),
    #                      div(
    #                        style = "text-align:center; max-height: 40px; height: 40px"
    #                      ),
    #                      div(
    #                        style = "text-align:center; font-size: 150%; color: #f0ad4e;"
    #                      )
    #                    )
    #        ))
    #      }
    #    })))
    #  })
    #})           
            
        

}
shinyApp(ui = ui, server = server)
