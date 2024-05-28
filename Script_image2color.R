euclDist<-function(punto1,punto2){
  
  step1<-(punto1-punto2)^2
  step2<-sum(step1)
  step3<-sqrt(step2)
  return(step3)
}

pickcolors<-function(path,gridx=20,gridy=20,n=10){
  require(jpeg)
  require(raster) 
  img<-readJPEG(path)
  x<-seq(1,dim(img)[1],length=gridx)
  y<-seq(1,dim(img)[2],length=gridy)
  grid<-round(as.matrix(expand.grid(x,y)),0)
  
  red<-img[,,1]
  blu<-img[,,2]
  gre<-img[,,3]
  
  cols<-NULL
  for(i in 1:nrow(grid)){
    r<-red[grid[i,1],grid[i,2]]
    g<-blu[grid[i,1],grid[i,2]]
    b<-gre[grid[i,1],grid[i,2]]
    cols[i]<-rgb(r,g,b)
  }
  
  
  
  
  x11()
  plot(grid,pch=19,col=1,cex=3)
  points(grid,col=cols,pch=19,cex=2)
  
  pickcols<-NULL
  for(i in 1:n){
    sel<-locator(n = 1)
    selpoi<-c(sel$x,sel$y)
    
    dists<-NULL
    for(j in 1:nrow(grid)){
      dists[j]<-euclDist(selpoi,grid[j,])
    }
    
    pickcols[i]<-cols[which.min(dists)]
  }
  
  
  plot(1:n,rep(1,n),pch=15,cex=3,col=pickcols)
  text(1:n,rep(1,n),labels=pickcols,pos=3)
  
  return(pickcols)
}

ciccio<-pickcolors("pan.jpg",100,100,n=5)





puntoA<-c(5,3)
puntoB<-c(7,10)
euclDist(puntoA,puntoB)
plot(rbind(puntoA,puntoB))


img<-readJPEG("pan.jpg")
dim(img)
plot(NA, xlim = c(0, 753), 
     ylim = c(0, 1455), axes = F,xaxt='n',yaxt='n',ann=FALSE)
rasterImage(img,753, 1455, 0, 0)


x<-seq(1,10,4)
y<-seq(1,10,4)

grid<-round(as.matrix(expand.grid(x,y)),0)
plot(grid)
text(grid,labels=1:9,pos=1)
sel<-locator(1)
sel<-unlist(sel)
points(sel[1],sel[2],col="blue",pch=19)

euclDist(sel,grid[1,])
euclDist(sel,grid[2,])
euclDist(sel,grid[2,])
euclDist(sel,grid[5,])
euclDist(sel,grid[8,])
euclDist(sel,grid[9,])

dists<-NULL
for(i in 1:nrow(grid)){
  dists[i]<- euclDist(sel,grid[i,])
}
which.min(dists)

