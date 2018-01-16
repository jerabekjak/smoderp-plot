############################################
############### nastaveni ##################
############################################
# povinna knihovna 
library(shiny)
# rm(list=ls())
# Adresar s dat vysledky
# to je jedine co musis zadat
# NAZEV PROMENNE dir_  POVINNY
# muzes zadat jeden adresar:
dir_ = '~/Documents/Smoderp/smoderp-nazv/smoderp/out/'
# nebo nekolikt takto:
# dir_ <- c("~/Documents/Smoderp/smoderp/outnazv/",
          # "~/Documents/Smoderp/smoderp-nazv/smoderp/out/")
#
# POZN. nevim jak to funguje
# pokud by v jednom adresari byly 
# vysledky s rejhama a v druhy (tretim)
# bez.
############################################
######### konec nastaveni ##################
#############################################






ctenidat = function(dir_){
  
  
  
  ndir_ = length(dir_)
  sep_  = ';'
  skip_ = 3
  extension_ = '*.dat'
  
  files = c()
  for (i in 1:ndir_){
    files = c(files,paste(dir_[i],'/',list.files(dir_[i],pattern = '*.dat'),sep=''))
  }
  
  
  pixel = read.table(paste(files[1],sep = ''),skip=2,nrows = 1,comment.char = '')
  pixel = as.numeric(pixel[2])
  H = list()
  names_ = list()
  for (file_ in files) {
    print (file_)
    H[[file_]] = read.table(file_,sep = sep_,header = TRUE,skip=skip_,comment.char = '')
    names_[[file_]] = names(H[[file_]])
  }
  
  
  
  return(list(H,files,names_))
  
}

out = ctenidat(dir_)
H      = out[[1]]
files  = out[[2]]
names_ = out[[3]]
rm(out)





ui <- shinyUI(fluidPage(
  
  titlePanel("Dynamic sliders"),
  fluidRow(
  column(2, 
         numericInput("num", 
                      h3("pocet souboru"), 
                      value = 1)) ,
  
  # sidebarLayout(
    column(4, 
      uiOutput("sliders")
    ),
  column(3,
         uiOutput("sliders2")
  )

  ),
  
  fluidRow(
    uiOutput("text")
  )
    # mainPanel(
      # plotOutput("distPlot")
    # 
  
  

))

server <- shinyServer(function(input, output) {
  
  #Render the sliders
  output$sliders <- renderUI({
    # First, create a list of sliders each with a different name
    sliders <- lapply(1:input$num, function(i) {
      var = paste('soub',i,sep='')
      # print (var)
      selectInput(var, "vyber pravy soubor:",files)
      # selectInput("var", "vyber pravy soubor:",c('a','v'))
    })
    # Create a tagList of sliders (this is important)
    do.call(tagList, sliders)
  })
  
  
  #Render the sliders
  output$sliders2 <- renderUI({
    # First, create a list of sliders each with a different name
    sliders2 <- lapply(1:input$num, function(i) {
      fil = paste('soub',i,sep='')
      var = paste('soup',i,sep='')
      selectInput(var, "vyber pravy soubor:",names_[[input[[fil]]]],multiple = TRUE)
    })
    # Create a tagList of sliders (this is important)
    do.call(tagList, sliders2)
  })
  
  
  output$text <- renderText(
    names(H[[input$soub1]])
    
  )

})

shinyApp(ui = ui, server = server)