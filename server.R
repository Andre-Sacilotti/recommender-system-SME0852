
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


server <- function(input, output) {

    actual_page <- reactiveValues(data = 1)
    filtered_movies <- reactiveValues(data = movies)
    n_rows <- reactiveValues(data = 3)
    n_movies_per_row <- reactiveValues(data = 4)
    recommender <- reactiveValues(object = rec)




    observeEvent(input$previous_page, {
        print(actual_page$data)
        if (actual_page$data > 1){
            actual_page$data = actual_page$data - 1
        }
    })

    observeEvent(input$next_page, {
        n_rows = n_rows$data
        n_movies_per_row = n_movies_per_row$data
        n_pages = as.integer(nrow(filtered_movies$data)/(n_rows*n_movies_per_row))
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

        for (i in filtered_movies$data$item_id){
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

    output$movies_grid <- renderUI({
        n_rows = n_rows$data
        n_movies_per_row = n_movies_per_row$data
        

        actual_page = actual_page$data

        filtered_movies = filtered_movies$data

        n_pages = as.integer(nrow(filtered_movies)/(n_rows*n_movies_per_row))

        lapply(1:n_rows,
            function(i) {
                list(fluidRow(lapply(1:n_movies_per_row, function(j) {
                    list(column(width = 3, height=250,
                        div(
                            style = "text-align:center; padding-bottom: 13px;",
                            div(
                                style = "text-align:center",
                                img(src = filtered_movies$image[(i - 1) * n_movies_per_row + j + (actual_page-1)*(n_rows*n_movies_per_row)],height = 150)
                            ),
                            div(
                                style = "text-align:center; max-height: 40px; height: 40px",
                                strong(filtered_movies$name[(i - 1) * n_movies_per_row + j + (actual_page-1)*(n_rows*n_movies_per_row)])
                            ),
                            div(
                                style = "text-align:center; font-size: 150%; color: #f0ad4e;",
                                ratingInput(paste0("select_", filtered_movies$item_id[(i - 1) * n_movies_per_row + j + (actual_page-1)*(n_rows*n_movies_per_row)]), label = "", dataStop = 5)
                            ),
                            # div(
                            #     paste0("select_", filtered_movies$item_id[(i - 1) * n_movies_per_row + j + (actual_page-1)*(n_rows*n_movies_per_row)])
                            # )
                        )
                            
                        )
                    )
                })))
            }
        
        )


    })

}


print("Oi, Funcionou!")