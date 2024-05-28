tic_tac_toe<-function(User1,User2){
  winner<-NULL
  start_U1<-matrix(rep(0,9),3)
  start_U2<-matrix(rep(0,9),3)
  plot(NA,xlim=c(1,4),ylim=c(1,4),xaxt="n",yaxt="n",
       xlab="",ylab="",axes=T)
  
  combin<-rbind(c(1.5,3.5),
                c(1.5,2.5),
                c(1.5,1.5),
                c(2.5,3.5),
                c(2.5,2.5),
                c(2.5,1.5),
                c(3.5,3.5),
                c(3.5,2.5),
                c(3.5,1.5))
  
  segments(3,1,3,4)
  segments(2,1,2,4)
  segments(1,2,4,2)
  segments(1,3,4,3)
  
  repeat{  
    
    clic<-locator(1)
    choosen<-c(clic$x,clic$y)
    choosen_grid<-which.min(sqrt(colSums(apply(combin,1,function(x) (x-choosen)^2))))
    points(combin[choosen_grid,1],combin[choosen_grid,2],pch=4,col="red",
           cex=4,lwd=3)
    combin[choosen_grid]<-NA
    start_U1[choosen_grid]=1
    
    Hrows<-rowSums(start_U1)
    Hcols<-colSums(start_U1)
    Hdia1<-rep(sum(diag(start_U1)),3)
    Hdia2<-rep(sum(diag(t(apply(start_U1, 2, rev)))),3)
    Htot<-rbind(Hrows,Hcols,Hdia1,Hdia2)
    if(3%in%c(Hrows,Hcols,Hdia1,Hdia2)){
      title(paste(User1,"win!"))
      winner<-User1
      break
    }   
    
    clic<-locator(1)
    choosen<-c(clic$x,clic$y)
    choosen_grid<-which.min(sqrt(colSums(apply(combin,1,function(x) (x-choosen)^2))))
    points(combin[choosen_grid,1],combin[choosen_grid,2],pch=1,col="black",
           cex=4,lwd=3)
    combin[choosen_grid]<-NA
    start_U2[choosen_grid]=1
    
    # Check User 2
    Hrows<-rowSums(start_U2)
    Hcols<-colSums(start_U2)
    Hdia1<-rep(sum(diag(start_U2)),3)
    Hdia2<-rep(sum(diag(t(apply(start_U2, 2, rev)))),3)
    Htot<-rbind(Hrows,Hcols,Hdia1,Hdia2)
    if(3%in%c(Hrows,Hcols,Hdia1,Hdia2)){
      title(paste(User2,"win!"))
      winner<-User2
      break
    }
    
    if(length(which(((start_U1+start_U2)%in%0)))<1){
      title("draw!")
      break
    }
    
    
  }
  return(winner)
}

tic_tac_toe("Giocatore1","Giocatore2")
