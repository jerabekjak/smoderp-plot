############################################
############### nastaveni ##################
############################################
# povinna knihovna 
library('manipulate')
# instalace prikazem: 
# install.packages("manipulate")
#
# cesta ke R skriptum
rsplot <-  "~/Documents/Smoderp/smoderp-plot/rsplot"
# cesta k datum
dir_ = '~/Documents/Smoderp/smoderp/bash/test-out/out-konk01-krabice-velka/'

dir_ = c('/tmp/out-konk01-krabice-velka/',
         '/tmp/out-konv01-krabice-velka/')

# vyber bodu (soubor .dat v adresari)
# point000.dat -> id = 1
# point001.dat -> id = 2
# atd...
id1_ = 7
id2_ = 14
#
############################################
######### konec nastaveni ##################
############################################





setwd(rsplot)

# nacita funkce pro tvorbu grafu
source('hydragramy_fnc.R')


sep_  = ';'
skip_ = 3
extension_ = '*.dat'


files  = c()
for (idir_ in dir_) {
  files = c(files,list.files(idir_,pattern = '*.dat',full.names = TRUE))
}


pixel = read.table(paste(files[1],sep = ''),skip=2,nrows = 1,comment.char = '')
pixel = as.numeric(pixel[2])
H = list()
for (file_ in files) {
  # print (file_)
  name_ = substr(file_,1,8)
  name_ = file_
  
  H[[name_]] = read.table(file_,sep = sep_,header = TRUE,skip=skip_,comment.char = '')
}

plot_(id1_,id2_)



