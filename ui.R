shinyUI(fluidPage(
    
    titlePanel("Who's Faster?"),

    sidebarLayout(
        
        sidebarPanel(
            
            h3("Data Entry"),
            selectInput("n_play","Number of Players",choices=1:4,selected=1),
                     
            ## Player 1
            textInput(inputId = "p1_name",
                      label = h4("Player 1 Name")),
            br(),
            selectInput("p1_gender",label=h4("Player 1 Gender"),
                        choices=list("Undeclared"=1,"Male"=2,"Female"=3),
                        selected=1),
            selectInput("p1_age",label=h4("Player 1 Age"),
                        choices=list("Undeclared"=1,"K-2nd Grade"=2,
                            "3rd-5th Grade"=3, "6th-8th Grade"=4,
                            "9th-12th Grade"=5,"Adult"=6),
                        selected=1),
            h4("Player 1 Measurements (inches)"),
            br(),
            sliderInput(inputId = "p1_obs1",min=0,max=30,value=0,step=0.25,
                        label = NULL),
            
            ## Allow up to 7 observations          
            conditionalPanel(

                condition="input.p1_obs1>0",
                sliderInput(inputId = "p1_obs2",
                            min=0,max=30,value=0,step=0.25,
                            label = NULL),

                conditionalPanel(

                    condition="input.p1_obs2>0",
                    sliderInput(inputId = "p1_obs3",
                                min=0,max=30,value=0,step=0.25,
                                label = NULL),

                    conditionalPanel(

                        condition="input.p1_obs3>0",
                        sliderInput(inputId = "p1_obs4",
                                    min=0,max=30,value=0,step=0.25,
                                    label = NULL),

                        conditionalPanel(

                            condition="input.p1_obs4>0",
                            sliderInput(inputId = "p1_obs5",
                                        min=0,max=30,value=0,step=0.25,
                                        label = NULL),

                            conditionalPanel(

                                condition="input.p1_obs5>0",
                                sliderInput(inputId = "p1_obs6",
                                            min=0,max=30,value=0,step=0.25,
                                            label = NULL),
                                
                                conditionalPanel(

                                    condition="input.p1_obs6>0",
                                    sliderInput(inputId = "p1_obs7",
                                                min=0,max=30,value=0,step=0.25,
                                                label = NULL))))))),

            br(),

            ## Input for Player 2, if present         
            conditionalPanel(

                condition="input.n_play>1",
                textInput(inputId = "p2_name",
                          label = h4("Player 2 Name")),
                br(),
                selectInput("p2_gender",label=h4("Player 2 Gender"),
                            choices=list("Undeclared"=1,"Male"=2,"Female"=3),
                            selected=1),
                selectInput("p2_age",label=h4("Player 2 Age"),
                            choices=list("Undeclared"=1,"K-2nd Grade"=2,"3rd-5th Grade"=3,
                                "6th-8th Grade"=4,"9th-12th Grade"=5,"Adult"=6),
                            selected=1),
                h4("Player 2 Measurements (inches)"),
                br(),
                sliderInput(inputId = "p2_obs1",min=0,max=30,value=0,step=0.25,
                            label = NULL),

                conditionalPanel(

                    condition="input.p2_obs1>0",
                    sliderInput(inputId = "p2_obs2",
                                min=0,max=30,value=0,step=0.25,
                                label = NULL),

                    conditionalPanel(
                        condition="input.p2_obs2>0",
                        sliderInput(inputId = "p2_obs3",
                                    min=0,max=30,value=0,step=0.25,
                                    label = NULL),

                        conditionalPanel(

                            condition="input.p2_obs3>0",
                            sliderInput(inputId = "p2_obs4",
                                        min=0,max=30,value=0,step=0.25,
                                        label = NULL),
                            conditionalPanel(

                                condition="input.p2_obs4>0",
                                sliderInput(inputId = "p2_obs5",
                                            min=0,max=30,value=0,step=0.25,
                                            label = NULL),
                                conditionalPanel(

                                    condition="input.p2_obs5>0",
                                    sliderInput(inputId = "p2_obs6",
                                                min=0,max=30,value=0,step=0.25,
                                                label = NULL),
                                    conditionalPanel(

                                        condition="input.p2_obs6>0",
                                        sliderInput(inputId = "p2_obs7",
                                                    min=0,max=30,value=0,step=0.25,
                                                    label = NULL)))))))),
            
             br(),
                     
            ### Input for Player 3, if present                  
            conditionalPanel(

                condition="input.n_play>2",
                textInput(inputId = "p3_name",
                          label = h4("Player 3 Name")),
                br(),
                selectInput("p3_gender",label=h4("Player 3 Gender"),
                            choices=list("Undeclared"=1,"Male"=2,"Female"=3),
                            selected=1),
                selectInput("p3_age",label=h4("Player 3 Age"),
                            choices=list("Undeclared"=1,"K-2nd Grade"=2,"3rd-5th Grade"=3,
                                "6th-8th Grade"=4,"9th-12th Grade"=5,"Adult"=6),
                            selected=1),
                h4("Player 3 Measurements (inches)"),
                br(),
                sliderInput(inputId = "p3_obs1",min=0,max=30,value=0,step=0.25,
                            label = NULL),
                
                conditionalPanel(

                    condition="input.p3_obs1>0",
                    sliderInput(inputId = "p3_obs2",
                                min=0,max=30,value=0,step=0.25,
                                label = NULL),
                    
                    conditionalPanel(

                        condition="input.p3_obs2>0",
                        sliderInput(inputId = "p3_obs3",
                                    min=0,max=30,value=0,step=0.25,
                                    label = NULL),
                        
                        conditionalPanel(

                            condition="input.p3_obs3>0",
                            sliderInput(inputId = "p3_obs4",
                                        min=0,max=30,value=0,step=0.25,
                                        label = NULL),
                            
                            conditionalPanel(

                                condition="input.p3_obs4>0",
                                sliderInput(inputId = "p3_obs5",
                                            min=0,max=30,value=0,step=0.25,
                                            label = NULL),
                                
                                conditionalPanel(

                                    condition="input.p3_obs5>0",
                                    sliderInput(inputId = "p3_obs6",
                                                min=0,max=30,value=0,step=0.25,
                                                label = NULL),
                                    
                                    conditionalPanel(

                                        condition="input.p3_obs6>0",
                                        sliderInput(inputId = "p3_obs7",
                                                    min=0,max=30,value=0,step=0.25,
                                                    label = NULL)))))))),
            
            br(),
            
            ## Input for Player 4, if present          
            conditionalPanel(

                condition="input.n_play>3",
                textInput(inputId = "p4_name",
                          label = h4("Player 4 Name")),
                br(),
                selectInput("p4_gender",label=h4("Player 4 Gender"),
                            choices=list("Undeclared"=1,"Male"=2,"Female"=3),
                            selected=1),
                selectInput("p4_age",label=h4("Player 4 Age"),
                            choices=list("Undeclared"=1,"K-2nd Grade"=2,"3rd-5th Grade"=3,
                                "6th-8th Grade"=4,"9th-12th Grade"=5,"Adult"=6),
                            selected=1),
                h4("Player 4 Measurements (inches)"),
                br(),
                sliderInput(inputId = "p4_obs1",min=0,max=30,value=0,step=0.25,
                            label = NULL),

                conditionalPanel(

                    condition="input.p4_obs1>0",
                    sliderInput(inputId = "p4_obs2",
                                min=0,max=30,value=0,step=0.25,
                                label = NULL),
                    
                    conditionalPanel(

                        condition="input.p4_obs2>0",
                        sliderInput(inputId = "p4_obs3",
                                    min=0,max=30,value=0,step=0.25,
                                    label = NULL),
                        
                        conditionalPanel(
                            condition="input.p4_obs3>0",
                            sliderInput(inputId = "p4_obs4",
                                        min=0,max=30,value=0,step=0.25,
                                        label = NULL),
                            
                            conditionalPanel(

                                condition="input.p4_obs4>0",
                                sliderInput(inputId = "p4_obs5",
                                            min=0,max=30,value=0,step=0.25,
                                            label = NULL),
                                
                                conditionalPanel(

                                    condition="input.p4_obs5>0",
                                    sliderInput(inputId = "p4_obs6",
                                                min=0,max=30,value=0,step=0.25,
                                                label = NULL),
                                    
                                    conditionalPanel(
                                        condition="input.p4_obs6>0",
                                        sliderInput(inputId = "p4_obs7",
                                                    min=0,max=30,value=0,step=0.25,
                                                    label = NULL)))))))),
            
            
            actionButton("update", "Update Graphs"),

            br(),
            br(),
            br(),
            br(),

            textInput(inputId = "record_observation",
                      label = "Record Observation")
        ),
        
        mainPanel(
            
            h3("Results For Current Players"),
            plotOutput("scatter_plot1", height = 13*72),
            h3("Player's Comparison to History"),
            plotOutput("scatter_plot2")))))
