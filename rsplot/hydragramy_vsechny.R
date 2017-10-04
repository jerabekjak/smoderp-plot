############################################
############### nastaveni ##################
############################################
# povinna knihovna 
library('manipulate')
# instalace prikazem: 
# install.packages("manipulate")
#
# cesta ke R skriptum
rsplot <-  "~/Documents/smoderp-plot/rsplot"
# cesta k datum
data   <- "../test-data/data"
############################################
######### konec nastaveni ##################
############################################

setwd(rsplot)
source('hydragramy_vsechny_fce.R')
dir_ = data


sep_  = ';'
skip_ = 3
extension_ = '*.dat'



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

plot_(H)
