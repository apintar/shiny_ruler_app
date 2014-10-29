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
    
    p1_name <- isolate(as.character(input$p1_name))
    p2_name <- isolate(as.character(input$p2_name))


    p1_vals <- c(isolate(input$p1_obs1),
                 isolate(input$p1_obs2),
                 isolate(input$p1_obs3),
                 isolate(input$p1_obs4),
                 isolate(input$p1_obs5))
    
    p2_vals <- c(isolate(input$p2_obs1),
                 isolate(input$p2_obs2),
                 isolate(input$p2_obs3),
                 isolate(input$p2_obs4),
                 isolate(input$p2_obs5))
                    
    p2_vals<-p2_vals[p2_vals>0]
    p1_vals<-p1_vals[p1_vals>0]
    p1_time<-sqrt(p1_vals*0.0254*2/9.80665)
    p2_time<-sqrt(p2_vals*0.0254*2/9.80665)
    
    n1<-length(p1_vals)
    n2<-length(p2_vals)
    df.corr<-as.integer((n1>0)+(n2>0))
    
    if(n1>0&p1_name=="")p1_name<-"Player 1"
    if(n2>0&p2_name=="")p2_name<-"Player 2"
    nm<-c(p1_name,p2_name)

    if(n1>1&n2>1)pool.sd<-sqrt(((n1-1)*var(p1_time)+(n2-1)*var(p2_time))/(n1+n2-2))
    else pool.sd<-sd(c(p1_time,p2_time))

    xx1<-c(-100,seq(-.20,.60,by=.001),100)
    p1_dist<-dt((xx1-mean(p1_time))/pool.sd*sqrt(n1),n1+n2-df.corr)
    p2_dist<-dt((xx1-mean(p2_time))/pool.sd*sqrt(n2),n1+n2-df.corr)

    if(n1+n2>0){
      layout(matrix(1:2,2,1),heights=c(1,1.5))
      if(n1>0&n2>0) layout(matrix(1:3,3,1),heights=c(1,1.5,1))
      xx2<-seq(0,30,by=.01)
      par(mai=c(1,1,.7,.5))
      plot(xx2,sqrt(xx2*0.0254*2/9.80665),main="Convert Distance to Reaction Time", xlab="Distance (inches)",ylab="Time (sec)",type="l",cex.lab=2,cex.main=2,cex.axis=2)
      grid()
      points(p1_vals,p1_time,col=makeTransparent(2),lwd=4,cex=2)
      points(p2_vals,p2_time,col=makeTransparent(4),lwd=4,cex=2)
#      text(24,.17,expression("Time(s)="*sqrt(("Dist(in)*0.0254*(m/in)*2")/(9.8*"(m/s"^2*")"))))
      text(20,.1,expression("Dist(in)=0.5*39.27(in/m)*9.8(m/s"^2*")*Time(s)"^2),cex=2)
      
      par(mai=c(1,1,.2,.5))
    plot(c(rep(x = 1, times = n1),
           rep(x = 2, times = n2)),
         c(p1_time, p2_time),
         ylim=c(max(0,min(c(p1_time,p2_time))-.05),min(.4,max(c(p1_time,p2_time))+.05)),
         ylab = "Time (seconds)",
         xlab = "",
         main="Evaluate Average Reaction Times",
         xaxt = "n",
         bty = "l",
         cex.axis = 2,
         cex.lab = 2,
         cex.main=2)
    axis(side = 1, at = c(1, 2), labels = nm,
         cex.lab = 2, cex.axis = 2)
    grid(ny=NULL,nx=NA)

      
   if(n1>0) polygon(x=1+c(p1_dist,p1_dist[1]),y=c(xx1,xx1[1]),col=makeTransparent(2))
   if(n2>0) polygon(x=2-c(p2_dist,p2_dist[1]),y=c(xx1,xx1[1]),col=makeTransparent(4))
   if(n1>1|n2>1)legend("bottom",title="Dist. of Estimated Mean
Reaction Time (sec)",legend=nm[c(n1,n2)>0],fill=makeTransparent(c(2,4)[c(n1,n2)>0]),cex=1.75,bty="n")
   if(n1>0&n2>0){
     xx3<-seq(-1,1,by=.001)
     p1.xx<-c(-1,seq(-.30,0,by=.0005))
     p2.xx<-c(seq(0,.30,by=.0005),1)
     t.den<-dt((xx3-(mean(p1_time)-mean(p2_time)))/(pool.sd*sqrt(1/n1+1/n2)),n1+n2-2)
     plot(NULL,NULL,xlim=c(-.30,.30),ylim=range(t.den)*c(0,1.5),col="white",
          main="Compare Mean Reaction Times",
          xlab="Dist. of Estimated Difference in Mean Reaction Time (sec)",ylab="Density",cex.axis = 2,cex.main=2,
         cex.lab = 2)
     polygon(x=c(p1.xx,p1.xx[length(p1.xx)],p1.xx[1]),
             y=c(dt((p1.xx-(mean(p1_time)-mean(p2_time)))/(pool.sd*sqrt(1/n1+1/n2)),n1+n2-2),0,0),col=makeTransparent(2))
     polygon(x=c(p2.xx[1],p2.xx,p2.xx[1]),
             y=c(0,dt((p2.xx-(mean(p1_time)-mean(p2_time)))/(pool.sd*sqrt(1/n1+1/n2)),n1+n2-2),0),col=makeTransparent(4))
     conf<-round(pt((0-(mean(p1_time)-mean(p2_time)))/(pool.sd*sqrt(1/n1+1/n2)),n1+n2-2),3)*100
     p1.win<-mean(p1_time)<mean(p2_time)

     text(0,1.25*max(t.den),paste0("We are ",p1.win*conf+(1-p1.win)*(100-conf),"% confident ",nm[2-p1.win]," reacts faster on average than ",nm[1+p1.win]),cex=2)
   }   
  }
   else{
     plot(NULL,NULL,xlim=0:1,ylim=0:1,xlab="",ylab="",xaxt="n",yaxt="n")
     text(.5,.5,"Please enter measurements",cex=2)
   } 
    
  }, width = 72*10, height = 72*10)
  
})
