############################################
############### nastaveni ##################
############################################
# povinna knihovna 
library(raster)
library(rasterVis)
library(manipulate)
library(calibrate)
# s
# cesta k datum
data   <- "~/Documents/Smoderp/smoderp-nazv/smoderp/out/"
############################################
######### konec nastaveni ##################
############################################


dir_ = data

pp_kategorie = function(s,jmeno){
  # rat <- levels(as.factor(s))[[1]]
  
  # print (rat)
  # rat[['asdf']] <- c('0','1','1000','1001')
  # levels(s) <- rat
  # levelplot(s)
  plot(log10(s),jmeno)
}


pp_ = function(i,j,dva,addpoints){
  if (dva) {
    par(mar=c(3,3,3,7))
    layout(matrix(c(1,2),ncol = 2))
      plot(D[[i]],main=files[i])
    grid()
      plot(D[[j]],main=files[j])
    grid()
  } else {
    layout(matrix(c(1),ncol = 1))
      plot(D[[i]],main=files[i])

    grid()
  }
  if (addpoints) {points(p$V2,p$V3)}
  if (addpoints) {textxy(p$V2,p$V3,p$V1)}
  mtext('X [m]',side = 1,line = 3)
  mtext('Y [m]',side = 2,line = 3)
}

files = list.files(dir_,pattern = '*.asc')
n = length(files)
D = c()
for (i in 1:n){
  wrk = raster(paste(dir_,files[i],sep=''))
  # NAvalue(wrk) =  -3.4e+38
  D = c(D,wrk)
  
  # plot(D,main=files[i])
  print (paste(i,files[i]))
}


p = read.table(paste(dir_,'points.txt',sep=''))

wrk = as.data.frame(D[[2]])


manipulate(pp_(i,j,dva,addpoints),i = slider(1,n,initial = 6),
           dva = checkbox(FALSE),
           addpoints = checkbox(TRUE),
           j = slider(1,n,initial = 7))


