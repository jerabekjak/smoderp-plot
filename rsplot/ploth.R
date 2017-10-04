############################################
############### nastaveni ##################
############################################
# povinna knihovna 
library('manipulate')
# instalace prikazem: 
# install.packages("manipulate")
#
# cesta k datum
data   <- "~/Documents/smoderp-plot/test-data/data"
# zobrazi data ze vsech bodu 
# pro dane veliciny (pozice souplce v hydrogramu)
sloupce = c(3,12,17,20)
############################################
######### konec nastaveni ##################
############################################



# setwd('/home/jakub/Documents/smoderp/')
dir_ = data



pp = function(D,co_){
  n = length(D)
  jmena  = names(D)
  jmena_ = names(D[[1]])
  rr = c()
  
  for (i in (1:n)){
    rr = range(rr,range(D[[i]][[co_]],na.rm = TRUE))
  }
  par(mar=c(4,4,3,10))
  plot(NA,xlim=range(D[[1]]),ylim=rr,xlab='cas',ylab=names(D[[1]][co_]))
  grid()
  for (i in (1:n)){
    lines(D[[i]][[1]],D[[i]][[co_]],col=i)
  }
  
  par(xpd=TRUE)
  legend(max(D[[i]][[1]] + 0.1*D[[i]][[1]]),min(rr),legend = jmena,col=(1:n),lty = 1,yjust = 0,cex=0.5)
  par(xpd=FALSE)
}





# takhle to funguje jako predtim dva grafy z jednoho bodu
# id1_ = 1
# id2_ = 1
sep_  = ';'
skip_ = 3
extension_ = '*.dat'
#################################


files = list.files(dir_,pattern = '*.dat')
pixel = read.table(paste(dir_,'/',files[1],sep = ''),skip=2,nrows = 1,comment.char = '')
pixel = as.numeric(pixel[2])
H = list()

for (file_ in files) {
  print (file_)
  name_ = substr(file_,1,8)
  dd = read.table(paste(dir_,'/',file_,sep = ''),sep = sep_,header = TRUE,skip=skip_,comment.char = '')
  if (length(dd$X..Time.s.)>5){
    H[[name_]] = dd
  }
}

# layout(matrix(c(1,2),ncol=1))
nl = length(sloupce)
layout(matrix(1:nl,ncol=1))

for (is in sloupce) {
  pp(H,is)
}

