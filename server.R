library(plotrix)

## Make semi-transparent colors.  Code for this function was taken
## from Nick Sabbe's post on stackoverflow.com
makeTransparent <- function(someColor, alpha = 100) {

  newColor <- col2rgb(someColor)
  apply(newColor, 2, function(curcoldata) {
    rgb(red = curcoldata[1], green = curcoldata[2], blue = curcoldata[3], alpha = alpha, 
        maxColorValue = 255)
  })
}

shinyServer(function(input, output, session){

  observe({

    if (input$n_play == 1) {

      for (i in 2:4) {
        
        updateTextInput(session,
                        inputId = paste0("p", i, "_name"),
                        value = "")
        updateSelectInput(session,
                          inputId = paste0("p", i, "_gender"),
                          selected = 1)
        updateSelectInput(session,
                          inputId = paste0("p", i, "_age"),
                          selected = 1)

        for (j in 1:7) {
          
          updateSliderInput(session,
                            inputId = paste0("p", i, "_obs", j),
                            value = 0)
        }
      }
    }

    if (input$n_play == 2) {

      for (i in 3:4) {
        
        updateTextInput(session,
                        inputId = paste0("p", i, "_name"),
                        value = "")
        updateSelectInput(session,
                          inputId = paste0("p", i, "_gender"),
                          selected = 1)
        updateSelectInput(session,
                          inputId = paste0("p", i, "_age"),
                          selected = 1)

        for (j in 1:7) {
          
          updateSliderInput(session,
                            inputId = paste0("p", i, "_obs", j),
                            value = 0)
        }
      }
    }

    if (input$n_play == 3) {

      for (i in 4:4) {
        
        updateTextInput(session,
                        inputId = paste0("p", i, "_name"),
                        value = "")
        updateSelectInput(session,
                          inputId = paste0("p", i, "_gender"),
                          selected = 1)
        updateSelectInput(session,
                          inputId = paste0("p", i, "_age"),
                          selected = 1)

        for (j in 1:7) {
          
          updateSliderInput(session,
                            inputId = paste0("p", i, "_obs", j),
                            value = 0)
        }
      }
    }

    if (input$record_observation == "el") {

      for (i in 1:input$n_play) {

        for (j in 1:7) {
          time<-Sys.time()
          if (eval(parse(text = paste0("input$p", i, "_obs", j))) > 0) {

            tmp <- data.frame(time,
                              eval(parse(text = paste0("input$p", i, "_name"))),
                              eval(parse(text = paste0("input$p", i, "_gender"))),
                              eval(parse(text = paste0("input$p", i, "_age"))),
                              eval(parse(text = paste0("input$p", i, "_obs", j))))
            write.table(x = tmp,
                        file = ".player_history",
                        append = TRUE,
                        row.names = FALSE,
                        col.names = FALSE)
          }
        }
      }

      updateSelectInput(session,
                        inputId = "n_play",
                        selected = 1)

      for (i in 1:4) {
        
        updateTextInput(session,
                        inputId = paste0("p", i, "_name"),
                        value = "")
        updateSelectInput(session,
                          inputId = paste0("p", i, "_gender"),
                          selected = 1)
        updateSelectInput(session,
                          inputId = paste0("p", i, "_age"),
                          selected = 1)

        for (j in 1:7) {
          
          updateSliderInput(session,
                            inputId = paste0("p", i, "_obs", j),
                            value = 0)
        }
      }

      updateTextInput(session,
                      inputId = "record_observation",
                      value = "")
    }
  })
  
  output$scatter_plot1 <- renderPlot({
    
    input$update

    ## Collect inputs
    n_play<-isolate(input$n_play)
    p1_name <- isolate(as.character(input$p1_name))
    p2_name <- isolate(as.character(input$p2_name))
    p3_name <- isolate(as.character(input$p3_name))
    p4_name <- isolate(as.character(input$p4_name))
    
    p1_vals <- c(isolate(input$p1_obs1),
                 isolate(input$p1_obs2),
                 isolate(input$p1_obs3),
                 isolate(input$p1_obs4),
                 isolate(input$p1_obs5),
                 isolate(input$p1_obs6),
                 isolate(input$p1_obs7))
    
    p2_vals <- c(isolate(input$p2_obs1),
                 isolate(input$p2_obs2),
                 isolate(input$p2_obs3),
                 isolate(input$p2_obs4),
                 isolate(input$p2_obs5),
                 isolate(input$p2_obs6),
                 isolate(input$p2_obs7))

     p3_vals <- c(isolate(input$p3_obs1),
                 isolate(input$p3_obs2),
                 isolate(input$p3_obs3),
                 isolate(input$p3_obs4),
                 isolate(input$p3_obs5),
                 isolate(input$p3_obs6),
                 isolate(input$p3_obs7))

     p4_vals <- c(isolate(input$p4_obs1),
                 isolate(input$p4_obs2),
                 isolate(input$p4_obs3),
                 isolate(input$p4_obs4),
                 isolate(input$p4_obs5),
                 isolate(input$p4_obs6),
                 isolate(input$p4_obs7))
    
    nms <- c( p1_name,p2_name, p3_name, p4_name)
    nms[nms==""] <- paste("Player",which(nms==""))
    
    obs <- list(p1_vals,p2_vals,p3_vals,p4_vals)
    names(obs) <- nms

    ## Filter down to reported number of players
    obs <- obs[1:n_play]
    
    ## Remove observations of 0
    obs <- lapply(obs,function(x)x[x>0])

    ## Remove players without any non-0 entries
    obs <- obs[sapply(obs,function(x)sum(x>0))>0]
    
    ## How many (non-0) observations from each player
    n <- unlist(sapply(obs,length))
        
    ## Display results when observations have been reported
    if (sum(n)>0) {

      ## Setup display depending on number of participants
      if (length(n)==1) layout(matrix(1:2,2,1),heights=c(.8,1))
      if (length(n)>1) layout(matrix(1:3,3,1),heights=c(.8,1,1))
 
      ## Display convertion from distance fallen to reaction time
      player <- rep(names(obs),n)
      player_ind <- rep(1:length(n),n)
      obs_vec <- unlist(obs)
      time_vec <- sqrt(obs_vec*0.0254*2/9.80665)

      xx2 <- seq(0,30,by=.01)
      par(mai=c(.8,1,1,.5),mgp=c(3.5,1,0))
      plot(xx2,sqrt(xx2*0.0254*2/9.80665),
           main="Convert Distance to Reaction Time",
           xlab="Distance (inches)",ylab="Time (sec)",
           type="l",cex.lab=3,cex.main=3,cex.axis=3,lwd=4)
      grid()

      j <- 0  ## index for point color
      for (i in unique(player)) {

        j<-j+1
        points(obs_vec[player==i],time_vec[player==i],
               col=makeTransparent(j),lwd=6,cex=3)
      }

      ## text(24,.17,expression("Time(s)="*sqrt(("Dist(in)*0.0254*(m/in)*2")/(9.8*"(m/s"^2*")"))))
      
      text(20,.1,
           expression("Dist(in)=0.5*39.27(in/m)*9.8(m/s"^2*")*Time(s)"^2),
           cex=3)

      ## Plot means and distributions for each players 
      df <- sum(n)-length(n)
      n <- table(player)[unique(player)]
    
      if (length(n) > 1) {

        fit <- lm(time_vec~player)
        pool_sd <- summary(fit)$sigma
        mn <- predict(fit ,newdata=data.frame('player'=unique(player)))
      } else {

        pool_sd <- sd(time_vec)
        mn <- mean(time_vec)
      }

      par(mai=c(1,1,.8,.5),mgp=c(3.5,1,0))
      plot(player_ind,
           time_vec,
           xlim=c(.8,length(n)+.5),
           ylim=c(max(0,min(time_vec)-.05),min(.4,max(time_vec)+.05)),
           ylab = "Time (sec)",
           xlab = "",
           main="Confidence Distributions for Average Reaction Times",
           xaxt = "n",
           bty = "l",
           lwd=6,
           col=player_ind,
           cex.axis = 3,
           cex.lab = 3,
           cex.main=3)
      axis(side = 1, at = 1:length(n), labels = names(n),
           cex.lab = 3, cex.axis = 3)
      grid(ny=NULL,nx=NA)

      if (sum(n) > length(n)) {

        xx1 <- c(-100,seq(-.20,.60,by=.001),100)
        for (i in 1:length(n)) {
          mn_dist <- dt((xx1-mn[i])/pool_sd*sqrt(n[i]),df)
          polygon(x=i+c(mn_dist,mn_dist[1]),
                  y=c(xx1,xx1[1]),
                  col=makeTransparent(i))
        }
        ## legend("bottom",title="Dist. of Estimated Mean
        ## Reaction Time (sec)",legend=names(n),fill=makeTransparent(1:length(n)),cex=1.75,bty="n")
      }
      
      if (length(n) > 1) {
        k <- 0
        par(bty="n",mgp=c(3.5,1,0))
        plot(NULL,NULL,ylim=c(-.30,.30),xlim=c(0.4,choose(length(n),2)+.5),
             col="white",
             main="Confidence Distributions for Differences in Average Reaction Times (sec)",
             ylab="Time Difference (sec)",
             xlab="",xaxt="n",cex.axis = 3,cex.main=3,yaxt="n",
             cex.lab = 3)
        axis(2,at=(-3:3)/10,labels=abs(-3:3)/10,cex.axis=3)
        grid(ny=NULL,nx=NA)
        lab <- NULL
        for (j1 in 1:(length(n)-1)) {
          for (j2 in (j1+1):length(n)) {
            k<-k+1   ## index for comparison plots
            p1<-unique(player)[j1]
            p2<-unique(player)[j2]
            lab<-c(lab,paste(p1,"vs.",p2))
         
            xx3<-seq(-1,1,by=.001)
            p1_xx<-c(-1,seq(-.30,0,by=.0005))
            p2_xx<-c(seq(0,.30,by=.0005),1)
            t_den<-dt((xx3-(mn[j1]-mn[j2]))/(pool_sd*sqrt(1/n[j1]+1/n[j2])),df)
            
            polygon(y=c(p1_xx,p1_xx[length(p1_xx)],p1_xx[1]),
                    x=k+c(dt((p1_xx-(mn[j1]-mn[j2]))/
                               (pool_sd*sqrt(1/n[p1]+1/n[p2])),df),0,0),
                    col=makeTransparent(j1))
            polygon(y=c(p2_xx[1],p2_xx,p2_xx[1]),
                    x=k+c(0,dt((p2_xx-(mn[j1]-mn[j2]))/
                                 (pool_sd*sqrt(1/n[p1]+1/n[p2])),df),0),
                    col=makeTransparent(j2))
            conf<-pt((0-(mn[j1]-mn[j2]))/
                       (pool_sd*sqrt(1/n[p1]+1/n[p2])),df)*100
            p1_win<-mn[p1]<mn[p2]
            
            boxed.labels(k,-.3+(k%%2)/15,paste0(p1," faster: ", round(conf,1),"%"),
                         col=j1,cex=2,bg="white",border=FALSE)
            boxed.labels(k,.3-(k%%2)/15,paste0(p2," faster: ",round(100-conf,1),"%"),
                         col=j2,cex=2,bg="white",border=FALSE)
         
            ## text(0,1.25*max(t_den),
            ## paste0("We are ",p1_win*conf+(1-p1_win)*(100-conf),"% confident ",
            ## unique(player)[j1*p1_win+j2*(1-p1_win)],
            ## " reacts faster on average than ",
            ## unique(player)[j1*(1-p1_win)+j2*p1_win]),cex=2)
          }
        }
        axis(1,at=1:k,labels=lab,cex.axis=2,line=2,lwd=0)
     
      }
    } else {  ## If no observations have been entered, display message
   
      plot(NULL,NULL,xlim=0:1,ylim=0:1,xlab="",ylab="",xaxt="n",yaxt="n")
      text(.5,.5,"Please enter measurements",cex=2)
    }
  })#, height = 13*72, width = 17*72)

  output$scatter_plot2 <- renderPlot({

    input$update

    ## Collect inputs
    n_play<-isolate(input$n_play)
    p1_name <- isolate(as.character(input$p1_name))
    p2_name <- isolate(as.character(input$p2_name))
    p3_name <- isolate(as.character(input$p3_name))
    p4_name <- isolate(as.character(input$p4_name))
    
    p1_vals <- c(isolate(input$p1_obs1),
                 isolate(input$p1_obs2),
                 isolate(input$p1_obs3),
                 isolate(input$p1_obs4),
                 isolate(input$p1_obs5),
                 isolate(input$p1_obs6),
                 isolate(input$p1_obs7))
    
    p2_vals <- c(isolate(input$p2_obs1),
                 isolate(input$p2_obs2),
                 isolate(input$p2_obs3),
                 isolate(input$p2_obs4),
                 isolate(input$p2_obs5),
                 isolate(input$p2_obs6),
                 isolate(input$p2_obs7))

     p3_vals <- c(isolate(input$p3_obs1),
                 isolate(input$p3_obs2),
                 isolate(input$p3_obs3),
                 isolate(input$p3_obs4),
                 isolate(input$p3_obs5),
                 isolate(input$p3_obs6),
                 isolate(input$p3_obs7))

     p4_vals <- c(isolate(input$p4_obs1),
                 isolate(input$p4_obs2),
                 isolate(input$p4_obs3),
                 isolate(input$p4_obs4),
                 isolate(input$p4_obs5),
                 isolate(input$p4_obs6),
                 isolate(input$p4_obs7))
    
    nms <- c( p1_name,p2_name, p3_name, p4_name)
    nms[nms==""] <- paste("Player",which(nms==""))
    
    obs <- list(p1_vals,p2_vals,p3_vals,p4_vals)
    names(obs) <- nms

    ## Filter down to reported number of players
    obs <- obs[1:n_play]
    
    ## Remove observations of 0
    obs <- lapply(obs,function(x)x[x>0])

    ## Remove players without any non-0 entries
    obs <- obs[sapply(obs,function(x)sum(x>0))>0]
    
    ## How many (non-0) observations from each player
    n <- unlist(sapply(obs,length))
        
    ## Display results when observations have been reported
    if (sum(n)>0) {

      ## Display convertion from distance fallen to reaction time
      player <- rep(names(obs),n)
      player_ind <- rep(1:length(n),n)
      obs_vec <- unlist(obs)
      time_vec <- sqrt(obs_vec*0.0254*2/9.80665)

      n <- table(player)[unique(player)]
    
      if (length(n) > 1) {

        fit <- lm(time_vec~player)
        pool_sd <- summary(fit)$sigma
        mn <- predict(fit ,newdata=data.frame('player'=unique(player)))
      } else {

        pool_sd <- sd(time_vec)
        mn <- mean(time_vec)
      }
    }
    if (file.exists(".player_history")) {
      dat <- read.table(file = ".player_history",header=FALSE)
      hist.time<-as.factor(dat[,1])
      hist.player<-as.factor(dat[,2])
      hist.gender<-as.factor(dat[,3])
      hist.age<-as.factor(dat[,4])
      hist.time.vec<-sqrt(as.numeric(dat[,5])*0.0254*2/9.80665)
      id<-as.factor(paste(hist.time,hist.player))
      dat$id<-id
      fit <- lm(hist.time.vec~id)
      pool_sd <- summary(fit)$sigma
      
      hist.mn <- predict(fit ,newdata=data.frame('id'=unique(id)))
      tab<-table(id)[unique(id)]
      XLIM<-range(hist.mn)
      if (sum(n)>0) XLIM<-range(c(mn,hist.mn))
      
      plot(NULL,NULL,xlim=XLIM,#+pool_sd*c(-2,2),
           ylim=c(0,2),#dnorm(0,pool_sd/sqrt(max(table(id))))),
           xlab="Mean Reaction Time (sec)",ylab="",yaxt="n",
           cex.lab=2,cex.axis=2)

      gen.lab<-c("Undeclared"=5,"Male"=2,"Female"=4)
      age.lab<-c("Undeclared"=1,"K-2nd Grade"=2,
                 "3rd-5th Grade"=3, "6th-8th Grade"=4,
                 "9th-12th Grade"=5,"Adult"=6)
      COL<-c(1:4,"purple","orange")
      for(i in 1:length(unique(id))){
        lines(rep(hist.mn[i],2),0:1,lty=gen.lab[as.numeric(dat[id==unique(id)[i],3][1])],col=makeTransparent(COL[as.numeric(dat[id==unique(id)[i],4][1])],150),lwd=6)
       # t.x<-seq(hist.mn[i]-3*pool_sd/sqrt(tab[i]),hist.mn[i]+3*pool_sd/sqrt(tab[i]))
       # lines(t.x,dnorm(t.x,hist.mn[i],pool_sd/sqrt(tab[i])),lty=as.numeric(t.dat[i,3]),col=as.numeric(t.dat[i,4]),lwd=3)
       # polygon(y=c(0,dnorm(t.x,hist.mn[i],pool_sd/sqrt(tab[i])),0,0),
        #          x=sort(c(min(t.x),t.x,range(t.x))),
         #         col=makeTransparent(as.numeric(t.dat[i,4])),alpha=50)
      }
      legend("topleft",title="Age",fill=makeTransparent(COL[age.lab[unique(dat[,4])]],150),
             legend=names(age.lab[unique(dat[,4])]),ncol=ceiling(length(unique(dat[,4]))/3),cex=2)
      legend("topright",title="Gender",lty=gen.lab[unique(dat[,3])],
             legend=names(gen.lab[unique(dat[,3])]),lwd=6,cex=2)
      if (sum(n)>0) {
        for(i in 1:length(mn)) lines(rep(mn[i],2),0:1,col=i,lwd=3)
        legend("top",legend=unique(player),col=1:length(unique(player)),lwd=6,cex=2)
      }
    } else {
      plot(NULL,NULL,xlim=0:1,ylim=0:1,xlab="",ylab="",xaxt="n",yaxt="n")
      text(.5,.5,"No .player_history yet",cex=2)
    }
  })


})
