shinyUI(fluidPage(
    
    titlePanel("Who's Faster?"),

    sidebarLayout(
        
        sidebarPanel(
            h3("Data Entry"),
            textInput(inputId = "p1_name",
                      label = h4("Player 1 Name")),
            br(),
            h4("Player 1 Measurements"),
            br(),
            textInput(inputId = "p1_obs1",
                      label = NULL),
            br(),
            textInput(inputId = "p1_obs2",
                      label = NULL),
            br(),
            textInput(inputId = "p1_obs3",
                      label = NULL),
            br(),
            textInput(inputId = "p1_obs4",
                      label = NULL),
            br(),
            textInput(inputId = "p1_obs5",
                      label = NULL),
            br(),
            textInput(inputId = "p2_name",
                      label = h4("Player 2 Name")),
            br(),
            h4("Player 2 Measurements"),
            textInput(inputId = "p2_obs1",
                      label = NULL),
            br(),
            textInput(inputId = "p2_obs2",
                      label = NULL),
            br(),
            textInput(inputId = "p2_obs3",
                      label = NULL),
            br(),
            textInput(inputId = "p2_obs4",
                      label = NULL),
            br(),
            textInput(inputId = "p2_obs5",
                      label = NULL),
            br(),
            br(),
            actionButton("update", "Update")
            ),
        mainPanel(
            h3("Results"),
            plotOutput("scatter_plot1")
            )
        )
    ))
