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
# vyber bodu (soubor .dat v adresari)
# point000.dat -> id = 1
# point001.dat -> id = 2
# atd...
id1_ = 5
id2_ = 5
#
############################################
######### konec nastaveni ##################
############################################





setwd(rsplot)

# nacita funkce pro tvorbu grafu
source('hydragramy_fnc.R')

dir_ = data

sep_  = ';'
skip_ = 3
extension_ = '*.dat'




files = list.files(dir_,pattern = '*.dat')
pixel = read.table(paste(dir_,'/',files[1],sep = ''),skip=2,nrows = 1,comment.char = '')
pixel = as.numeric(pixel[2])
H = list()
for (file_ in files) {
  # print (file_)
  name_ = substr(file_,1,8)
  
  H[[name_]] = read.table(paste(dir_,'/',file_,sep = ''),sep = sep_,header = TRUE,skip=skip_,comment.char = '')
}

plot_(id1_,id2_)



