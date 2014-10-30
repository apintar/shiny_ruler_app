shinyServer(function(input, output){
  
  output$scatter_plot1 <- renderPlot({

    input$update

### Make semi-transparent colors.  Code for this function was taken from Nick Sabbe's post on stackoverflow.com

makeTransparent <- function(someColor, alpha = 100) {
  newColor <- col2rgb(someColor)
  apply(newColor, 2, function(curcoldata) {
    rgb(red = curcoldata[1], green = curcoldata[2], blue = curcoldata[3], alpha = alpha, 
      maxColorValue = 255)
  })
}

    ## Collect inputs
    n.play<-isolate(input$n_play)
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
if(0){
    n.play<-4
    p1_vals<-1:4
    p2_vals<-6:4
    p3_vals<-8:5
    p4_vals<-1:4
    p1_name <- "A"
    p2_name <- "B"
    p3_name <- "C"
    p4_name <- "D"
}
    
    nms<-c( p1_name,p2_name, p3_name, p4_name)
    nms[nms==""]<-paste("Player",which(nms==""))

    obs<-list(p1_vals,p2_vals,p3_vals,p4_vals)
    names(obs)<-nms

    #Filter down to reported number of players
    obs<-obs[1:n.play]
    
    #Remove observations of 0
    obs<-lapply(obs,function(x)x[x>0])

    #Remove players without any non-0 entries
    obs<-obs[sapply(obs,function(x)sum(x>0))>0]
    
    ## How many (non-0) observations from each player
    n<-unlist(sapply(obs,length))
        
    ### Display results when observations have been reported
    if(sum(n)>0){

      ### Setup display depending on number of participants
      if(length(n)==1) layout(matrix(1:2,2,1),heights=c(.8,1))
      if(length(n)>1) layout(matrix(1:3,3,1),heights=c(.8,1,1))
 
      ### Display convertion from distance fallen to reaction time
      player <- rep(names(obs),n)
      player.ind <- rep(1:length(n),n)
      obs.vec<-unlist(obs)
      time.vec<-sqrt(obs.vec*0.0254*2/9.80665)

      xx2<-seq(0,30,by=.01)
      par(mai=c(.8,1,.7,.5))
      plot(xx2,sqrt(xx2*0.0254*2/9.80665),main="Convert Distance to Reaction Time", xlab="Distance (inches)",ylab="Time (sec)",type="l",cex.lab=2,cex.main=2,cex.axis=2)
      grid()

      j<-0  ## index for point color
      for(i in unique(player)){
        j<-j+1
        points(obs.vec[player==i],time.vec[player==i],col=makeTransparent(j),lwd=4,cex=2)       
      }
#      text(24,.17,expression("Time(s)="*sqrt(("Dist(in)*0.0254*(m/in)*2")/(9.8*"(m/s"^2*")"))))
      text(20,.1,expression("Dist(in)=0.5*39.27(in/m)*9.8(m/s"^2*")*Time(s)"^2),cex=2)


      ### Plot means and distributions for each players 
      df<-sum(n)-length(n)
      n<-table(player)[unique(player)]
    
      if(length(n)>1){
        fit<-lm(time.vec~player)
        pool.sd<-summary(fit)$sigma
        mn<-predict(fit ,newdata=data.frame('player'=unique(player)))
      }   
      else pool.sd<-sd(time.vec)

      par(mai=c(1,1,.2,.5))
      plot(player.ind,
         time.vec,
         xlim=c(.8,length(n)+.5),
         ylim=c(max(0,min(time.vec)-.05),min(.4,max(time.vec)+.05)),
         ylab = "Time (seconds)",
         xlab = "",
         main="Confidence Distributions for Average Reaction Times",
         xaxt = "n",
         bty = "l",
         cex.axis = 2,
         cex.lab = 2,
         cex.main=2)
      axis(side = 1, at = 1:length(n), labels = names(n),
         cex.lab = 2, cex.axis = 2)
      grid(ny=NULL,nx=NA)

      if(sum(n)>length(n)){
        xx1<-c(-100,seq(-.20,.60,by=.001),100)
        for(i in 1:length(n)){
          mn.dist<- dt((xx1-mn[i])/pool.sd*sqrt(n[i]),df)
          polygon(x=i+c(mn.dist,mn.dist[1]),y=c(xx1,xx1[1]),col=makeTransparent(i))
        }
     #   legend("bottom",title="Dist. of Estimated Mean
#Reaction Time (sec)",legend=names(n),fill=makeTransparent(1:length(n)),cex=1.75,bty="n")
      }
      
   if(length(n)>1){
     k<-0
     par(bty="n")
     plot(NULL,NULL,ylim=c(-.30,.30),xlim=c(.8,choose(length(n),2)+.5),col="white",
           main="Confidence Distributions for Differences in Average Reaction Times (sec)",
           ylab="Time Difference (sec)",
           xlab="",xaxt="n",cex.axis = 2,cex.main=2,yaxt="n",
           cex.lab = 2)
     axis(2,at=(-3:3)/10,labels=abs(-3:3)/10,cex.axis=2)
     grid(ny=NULL,nx=NA)
     lab<-NULL
     for(j1 in 1:(length(n)-1)){
       for(j2 in (j1+1):length(n)){
         k<-k+1   ## index for comparison plots
         p1<-unique(player)[j1]
         p2<-unique(player)[j2]
         lab<-c(lab,paste(p1,"vs.
",p2))

         
         xx3<-seq(-1,1,by=.001)
         p1.xx<-c(-1,seq(-.30,0,by=.0005))
         p2.xx<-c(seq(0,.30,by=.0005),1)
         t.den<-dt((xx3-(mn[j1]-mn[j2]))/(pool.sd*sqrt(1/n[j1]+1/n[j2])),df)
         
         polygon(y=c(p1.xx,p1.xx[length(p1.xx)],p1.xx[1]),
             x=k+c(dt((p1.xx-(mn[j1]-mn[j2]))/
               (pool.sd*sqrt(1/n[p1]+1/n[p2])),df),0,0),
             col=makeTransparent(j1))
         polygon(y=c(p2.xx[1],p2.xx,p2.xx[1]),
             x=k+c(0,dt((p2.xx-(mn[j1]-mn[j2]))/
               (pool.sd*sqrt(1/n[p1]+1/n[p2])),df),0),
             col=makeTransparent(j2))
         conf<-pt((0-(mn[j1]-mn[j2]))/
                        (pool.sd*sqrt(1/n[p1]+1/n[p2])),df)*100
         p1.win<-mn[p1]<mn[p2]

        boxed.labels(k,-.25,paste0(p1," faster:
", round(conf,1),"%"),col=j1,cex=1.2,bg="white")
         boxed.labels(k,.25,paste0(p2," faster:
",round(100-conf,1),"%"),col=j2,cex=1.2,bg="white")
         
  #       text(0,1.25*max(t.den),
  #            paste0("We are ",p1.win*conf+(1-p1.win)*(100-conf),"% confident ",
  #                   unique(player)[j1*p1.win+j2*(1-p1.win)],
  #                   " reacts faster on average than ",
  #                   unique(player)[j1*(1-p1.win)+j2*p1.win]),cex=2)
       }
     }
     axis(1,at=1:k,labels=lab,cex.axis=2,line=2,lwd=0)
     
    }   
  }
  
   else{  ## If no observations have been entered, display message
   
     plot(NULL,NULL,xlim=0:1,ylim=0:1,xlab="",ylab="",xaxt="n",yaxt="n")
     text(.5,.5,"Please enter measurements",cex=2)
   } 
    
  }, width = 72*15, height = 72*15)
  
})
