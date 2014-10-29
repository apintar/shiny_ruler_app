shinyUI(fluidPage(
    
    titlePanel("Who's Faster?"),

    sidebarLayout(
        
        sidebarPanel(
            h3("Data Entry"),
            textInput(inputId = "p1_name",
                      label = h4("Player 1 Name")),
            br(),
            h4("Player 1 Measurements (inches)"),
            br(),
            sliderInput(inputId = "p1_obs1",min=0,max=30,value=0,step=0.25,
                      label = NULL),
#            br(),
            sliderInput(inputId = "p1_obs2",min=0,max=30,value=0,step=0.25,
                      label = NULL),
#            br(),
            sliderInput(inputId = "p1_obs3",min=0,max=30,value=0,step=0.25,
                      label = NULL),
#            br(),
            sliderInput(inputId = "p1_obs4",min=0,max=30,value=0,step=0.25,
                      label = NULL),
#            br(),
            sliderInput(inputId = "p1_obs5",min=0,max=30,value=0,step=0.25,
                      label = NULL),
            br(),
            textInput(inputId = "p2_name",
                      label = h4("Player 2 Name")),
            br(),
            h4("Player 2 Measurements (inches)"),
            br(),         
            sliderInput(inputId = "p2_obs1",min=0,max=30,value=0,step=0.25,
                      label = NULL),
#            br(),
            sliderInput(inputId = "p2_obs2",min=0,max=30,value=0,step=0.25,
                      label = NULL),
#            br(),
            sliderInput(inputId = "p2_obs3",min=0,max=30,value=0,step=0.25,
                      label = NULL),
#            br(),
            sliderInput(inputId = "p2_obs4",min=0,max=30,value=0,step=0.25,
                      label = NULL),
#            br(),
            sliderInput(inputId = "p2_obs5",min=0,max=30,value=0,step=0.25,
                      label = NULL),
#            br(),
            br(),
            actionButton("update", "Update")
            ),
        mainPanel(
            h3("Results"),
            plotOutput("scatter_plot1")
            )
        )
    ))
