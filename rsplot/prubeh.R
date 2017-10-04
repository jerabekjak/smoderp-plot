library(raster)
library(manipulate)
setwd("C:/Users/jakub/xubuntuShared/")
library(fields)



delej = function(dir_,png_=FALSE,pngjmeno='HH')
{
  files = list.files(dir_,pattern = '*.asc')
  n = length(files)
  D = c()
  rr = c()
  jjj = c()
  for (i in 1:n){
    rr = range(rr,values(raster(paste(dir_,files[i],sep=''))),na.rm = TRUE)
    D = c(D,raster(paste(dir_,files[i],sep='')))
    jjj = c(jjj,files[i])
    print (paste(i,files[i]))
  }
  n = length(D)
  rr = c()
  range(as.array(D[[1]]),na.rm = TRUE)
  for (i in (1:n)){
    rr = range(rr,range(as.array(D[[i]]),na.rm = TRUE))
  }
  print (rr)
  for (i in (1:n)){
    jmeno = paste(sprintf('%08d',i),pngjmeno,'.png',sep='')
    if (png_) {png(jmeno)}
    # print (jmeno)
    # if (png_) {layout(matrix(c(1,2,2),ncol=1))}
    par(mar=c(4,4,4,6))
    # plot(H$X..Time.s.,H$Rainfall.m.,type='h')
    # abline(v=H$X..Time.s.[i])
    # image(D[[i]],breaks = seq(rr[1],rr[2],length=51), col=c(rgb(0.95,0.95,0.95),terrain.colors(60)[50:2]),main=files[i])
    image(D[[i]],breaks = seq(rr[1],rr[2],length=51), col=c(terrain.colors(60)[50:1]),main=files[i])
    # plot(D[[i]],main=jjj[i],breaks = seq(rr[1],rr[2],length=20), col=(20:1)/20)
    # plot(D[[i]],main=jjj[i])
    # mtext('X [m]',side = 1,line = 2)
    # mtext('Y [m]',side = 2,line = 2)
    # imageScale(D[[i]])
    # mtext(jjj[i],side=3,line=3,cex = 2.0)
    grid()
    image.plot(D[[1]], legend.only = TRUE,breaks = seq(rr[1],rr[2],length=51),  col=c(terrain.colors(60)[50:1]))
    if (png_) {dev.off()}
  }
}


# manipulate({
#   layout(matrix(c(1,2,2),ncol=1))
#   plot(H$X..Time.s.,H$Rainfall.m.)
#   abline(v=H$X..Time.s.[i])
#   plot(D[[i]])
#   },i=slider(1,length(D)))


delej('prubeh/h/',FALSE)




