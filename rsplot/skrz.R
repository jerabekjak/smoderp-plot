############################################
############### nastaveni ##################
############################################
# cesta k datum
data   <- c("~/Documents/smoderp-plot/test-data/data",
            "~/Documents/smoderp-plot/test-data/data2/")
# podari sloupce pro zobrazeni
co = 12
############################################
######### konec nastaveni ##################
############################################



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



read = function(dir_){

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
  return(H)
}



D = list()
for (id in data){
  D[[id]] = read(id)
}



nl = length(data)
layout(matrix(1:nl,ncol=1))


for (id in data) {
  pp(D[[id]],co)
}
