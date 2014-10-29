shinyServer(function(input, output){
  
  output$scatter_plot1 <- renderPlot({

    input$update

    p1_name <- isolate(as.character(input$p1_name))
    p2_name <- isolate(as.character(input$p2_name))

    p1_vals <- c(isolate(as.numeric(input$p1_obs1)),
                 isolate(as.numeric(input$p1_obs2)),
                 isolate(as.numeric(input$p1_obs3)),
                 isolate(as.numeric(input$p1_obs4)),
                 isolate(as.numeric(input$p1_obs5)))
    p2_vals <- c(isolate(as.numeric(input$p2_obs1)),
                 isolate(as.numeric(input$p2_obs2)),
                 isolate(as.numeric(input$p2_obs3)),
                 isolate(as.numeric(input$p2_obs4)),
                 isolate(as.numeric(input$p2_obs5)))

    plot(c(rep(x = 1, times = 5),
           rep(x = 2, times = 5)),
         c(p1_vals, p2_vals),
         ylab = "Measurements",
         xlab = "",
         xaxt = "n",
         bty = "l",
         cex.axis = 2,
         cex.lab = 2)
    axis(side = 1, at = c(1, 2), labels = c(p1_name, p2_name),
         cex.lab = 2, cex.axis = 2)
    
  }, width = 72*10, height = 72*10)
})
