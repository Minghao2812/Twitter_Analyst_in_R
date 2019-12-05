source("global.r")


ui <- fluidPage(
    
    # Application title
    titlePanel("Twitter Analyst"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            textInput("hashtag", 
                      h3("Hashtag Input"), value = NULL),
            sliderInput("amount",
                        "Number of Users:",
                        min = 100,
                        max = 1000,
                        value = 100,
                        step = 100),
            actionButton("go", "RUN")
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(type = "tabs",
                        tabPanel("Word Cloud", plotOutput("wordcloud")),
                        tabPanel("Word Frequency", plotOutput("wordfrequency")),
                        tabPanel("Tweet Length", plotOutput("TweetLength")),
                        tabPanel("Follower Number", plotOutput("FollowerNum")),
                        tabPanel("Friend Number", plotOutput("FriNum")),
                        tabPanel("Like Number", plotOutput("LikeNum")),
                        tabPanel("Sentiment", plotOutput("sentiment"))
                        
            )
        )
    )
)



# Define server logic required to draw a histogram
server <- function(input, output) {
    
    token <- create_token(app = "twitter_analysis_hkbu", 
                          consumer_key = "GIDTwKtSL6zmalcKNuxziO3t5",
                          consumer_secret = "jbgCGxuJGK1sLN1V9Qi2InX2LYHsauD8vFcMTSRKwTyWYsTJXw",
                          access_token = "1184726061742821376-WzaQqrotIUlCquPO9V9jmnzr7FeFY1",
                          access_secret = "3sfdNVGnetoHPrcfVZthhEL689FtUlee4YYzT5lhyxoBs",
                          set_renv = TRUE)
    
    cleaned <- eventReactive(input$go, {
        abstract(input$amount, input$hashtag) %>% clean()
    })
    notcleaned <- eventReactive(input$go, {
        abstract(input$amount, input$hashtag)
    })

    output$wordcloud <- renderPlot({
        cloud(cleaned())
    })

    output$wordfrequency <- renderPlot({
        wordfrequency(cleaned())
    })

    output$TweetLength <- renderPlot({
        tweet_len_his(notcleaned())
    })

    output$FollowerNum <- renderPlot({
        follower_num_his(notcleaned())
    })
    
    output$FriNum <- renderPlot({
        friend_num_his(notcleaned())
    })
    
    output$LikeNum <- renderPlot({
        like_num_his(notcleaned())
    })
    
    output$sentiment <- renderPlot({
        sentiment_counts(notcleaned(), input$amount)
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)


