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
dir_ = '~/Documents/Smoderp/smoderp-plot/test-data/data'
# nebo nekolikt takto:
# dir_ <- c("~/Documents/smoderp-plot/test-data/data",
#           "~/Documents/smoderp-plot/test-data/data2")
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
  for (file_ in files) {
    H[[file_]] = read.table(file_,sep = sep_,header = TRUE,skip=skip_,comment.char = '')
  }
  
  
  names_ = names(H[[1]])
  
  return(list(H,files,names_))
  
}

out = ctenidat(dir_)
H      = out[[1]]
files  = out[[2]]
names_ = out[[3]]
rm(out)


# pomocna plot funkce
pp = function(fil,fil2,fil3,var,var2,var3,yr1,yr2,yr3,xr,stejne){
  if (stejne){
    plot(NA,ylim=range(yr1,yr2),xlim=xr,xlab = 'Cas [s]',ylab = '',axes = FALSE)
  } else {
    plot(NA,ylim=yr1,xlim=xr,xlab = 'Cas [s]',ylab = '',axes = FALSE)
  }
  grid()
  axis(1)
  leglty = c()
  legcol = c()
  legleg = c()
  if (!is.null(var)) {
    axis(2)
    nvar   = min(3,length(var))
    for (i in 1:nvar){
      lines(H[[fil]]$X..Time.s.,H[[fil]][[var[i]]],lty=i)
      mtext(fil,3,3,adj = 0)
      leglty = c(leglty,i)
      legcol = c(legcol,1)
      legleg = c(legleg,var[i])
    }
  }
  if (!is.null(var2)) {
    if (!stejne) {
      par(new=TRUE)
      plot(NA,ylim=yr2,xlim=xr,axes=FALSE,xlab = 'Cas [s]',ylab = '')
    }
    nvar2   = min(3,length(var2))
    for (i in 1:nvar2){
      lines(H[[fil2]]$X..Time.s.,H[[fil2]][[var2[i]]],col = 2,lty=i)
      mtext(fil2,3,3,adj = 1,col=2)
      axis(4)
      leglty = c(leglty,i)
      legcol = c(legcol,2)
      legleg = c(legleg,var2[i])
    }
  }
  
  if (!is.null(var3)) {
    if (!stejne) {
      par(new=TRUE)
      plot(NA,ylim=yr2,xlim=xr,axes=FALSE,xlab = 'Cas [s]',ylab = '')
    }
    nvar2   = min(3,length(var3))
    for (i in 1:nvar2){
      lines(H[[fil3]]$X..Time.s.,H[[fil3]][[var3[i]]],col = 3,lty=i)
      mtext(fil3,3,3,adj = 1,col=3)
      axis(4)
      leglty = c(leglty,i)
      legcol = c(legcol,3)
      legleg = c(legleg,var3[i])
    }
  }
  
  
  legend('topright',legend = legleg, col=legcol, lty = leglty)
}

delejyr <- function(fil,fil2,fil3,var,var2,var3,stejne) {
  yr1 = NA
  # pokud mam pro soubor 1  vybrane promenne
  # udelam y range
  if (!is.null(var)) {
    nvar   = min(3,length(var))
    for (i in 1:nvar){
      yr1 = range(yr1,range(H[[fil]][[var[i]]]),na.rm = TRUE)
    }
  }
  # pokud mam pro soubor 2 vybrane promenne
  # udelam y range
  if (stejne) {
    yr2 = yr1
  } else {
    yr2 = NA
  }
  if (!is.null(var2)) {
    nvar2   = min(3,length(var2))
    for (i in 1:nvar2){
      yr2 = range(yr2,range(H[[fil2]][[var2[i]]]),na.rm = TRUE)
    }
  }
  
  # pokud mam pro soubor 2 vybrane promenne
  # udelam y range
  if (stejne) {
    yr3 = yr1
  } else {
    yr3 = NA
  }
  if (!is.null(var3)) {
    nvar3   = min(3,length(var3))
    for (i in 1:nvar3){
      yr3 = range(yr3,range(H[[fil3]][[var3[i]]]),na.rm = TRUE)
    }
  }
  
  
  return(list(yr1,yr2,yr3))
}



plot_  = function(fil,fil2,fil3,var,var2,var3,ranges = NULL, stejne){
  # pokud nezoomoju
  if (is.null(ranges)){
    xr = range(H[[1]]$X..Time.s.)
    out = delejyr(fil,fil2,fil3,var,var2,var3,stejne)
    yr1 = out[[1]]
    yr2 = out[[2]]
    yr3 = out[[3]]
    pp(fil,fil2,fil3,var,var2,var3,yr1,yr2,yr3,xr,stejne)
  } else {
    xr = ranges$x
    if (stejne){
      yr1 = ranges$y
      yr2  = yr1
      yr3  = yr1
    } else {
      out = delejyr(fil,fil2,fil3,var,var2,var3,stejne)
      yr1 = out[[1]]
      yr2 = out[[2]]
      yr3 = out[[3]]
    }
    pp(fil,fil2,fil3,var,var2,var3,yr1,yr2,yr3,xr,stejne)
  }
}




saveinput = function(input,ranges){
  write(input$somevalue,file='.saveshiny')
  write(input$soubor,file='.saveshiny',append = TRUE)
  write(length(input$sloupec),file='.saveshiny',append = TRUE)
  if (length(input$sloupec) > 0 ){
    write(input$sloupec,file='.saveshiny',append = TRUE)
  }
  write(input$soubor2,file='.saveshiny',append = TRUE)
  write(length(input$sloupec2),file='.saveshiny',append = TRUE)
  if (length(input$sloupec2) > 0 ){
    write(input$sloupec2,file='.saveshiny',append = TRUE)
  }
  if (is.null(ranges$x)){
    write(0,file='.saveshiny',append = TRUE)
    write(0,file='.saveshiny',append = TRUE)
  } else {
    write(ranges$x[1],file='.saveshiny',append = TRUE)
    write(ranges$x[2],file='.saveshiny',append = TRUE)
    write(ranges$y[1],file='.saveshiny',append = TRUE)
    write(ranges$y[2],file='.saveshiny',append = TRUE)
  }
}


# nacteni ulozenehos stavu
# nebo vytvoreni pocatecniho nastaveni
# pokus neni zadny stav ulozeny
init = function(){
  f <- './.saveshiny'
  initr = c()
  initr$x = c()
  initr$y = c()
  if (file.exists(f)) {
    L = readLines(f)
    l = 1
    stejne = as.logical(L[l])
    l = l + 1
    initfile1 = L[l]
    l = l + 1
    nsloupec = as.numeric(L[l])
    if (nsloupec>0){
      initval1 = c()
      for (i in 1:nsloupec){
        initval1 = c(initval1,L[l+i])
      }
      l = l + nsloupec
    } else {
      initval1 = NULL
    }
    l = l + 1
    initfile2 = L[l]
    l = l + 1
    nsloupec = as.numeric(L[l])
    if (nsloupec>0){
      initval2 = c()
      for (i in 1:nsloupec){
        initval2 = c(initval2,L[l+i])
      }
      l = l  + nsloupec
    } else {
      initval2 = NULL
    }
    l = l + 1
    if (L[l] == 0){
      initx = NULL
      inity = NULL
      
      return(list(initstejne = TRUE,
                  initfile1 = initfile1,
                  initval1  = initval1,
                  initfile2 = initfile2,
                  initval2  = initval2,
                  initr = data.frame(x  = initx,y  = inity ))
      )
    } else {
      initx1 = as.numeric(L[l])
      initx2 = as.numeric(L[l+1])
      inity1 = as.numeric(L[l+2])
      inity2 = as.numeric(L[l+3])
      return(list(initstejne = TRUE,
                  initfile1 = initfile1,
                  initval1  = initval1,
                  initfile2 = initfile2,
                  initval2  = initval2,
                  initr = data.frame(x  = c(initx1,initx2),y  = c(inity1,inity2) ))
      )
    }
    
  } else {
    initfile1 = files[1]
    initval1  = names_[6]
    initfile2 = NULL
    initval2  = NULL
    initx     = NULL
    inity     = NULL
    return(list(initstejne = TRUE,
                initfile1 = initfile1,
                initval1  = initval1,
                initfile2 = initfile2,
                initval2  = initval2,
                initr = data.frame(x  = initx,y  = inity ))
    )
  }
  
  
}


# resetuje .saveshiny soubor
resetsave <- function(input) {
  write(TRUE,file='.saveshiny')
  write(files[1],file='.saveshiny',append = TRUE)
  write(1,file='.saveshiny',append = TRUE)
  write(names_[6],file='.saveshiny',append = TRUE)
  write(files[1],file='.saveshiny',append = TRUE)
  write(0,file='.saveshiny',append = TRUE)
  write(0,file='.saveshiny',append = TRUE)
  write(0,file='.saveshiny',append = TRUE)
}



initd = init()


helptext <-       "ve skriptu:\t musis zadat cestu a adresari (adresarum) \n\t\t s dat soubory, jak na to je na zacatku skriptu
SAVE \t\t nastaveni si muzes ulozit a az skript restartujes, ukaze se ti ulozeny stav
RESET \t\t vymazes ulozeny stav, projevi se to po restartovani skriptu, nebo po refresh
REFRESH \t pokud zmenis data muzes zmenene soubory znovu nacist  bez vypinani skriptu
png PLOT \t ulozi se png soubor, ve setwd() adresari

note: \t s diakritikou to pada, sorry
\t na ose y nejsou jednotky, protoze si muzes vynest nekokit ruznuch velicin (dodelam v budoucnu)"

helptext2 <-  "TREU: \t Stejne meritko leveho a praveho souboru. Zoomovat lze podle osy y i x.
FALSE \t Ruzne meritko praveho a leveho souboru. Zoomovat lze POUZE podle osy x."


ui <- fluidPage(
  fluidRow(column(width = 6, class = "well",
           h4("Vysleddky"),
           # helpText("help: musis zadat cestu a adresari (adresarum)
                    # s dat soubory, jak na to je na zacatku skriptu"),
           checkboxInput("somevalue", "Stejne meritko praveho a leveho grafu", TRUE),
           verbatimTextOutput("value")
          ),
          column(width = 6, class = "well",
                 checkboxInput('help', "Napoveda", FALSE),
                 verbatimTextOutput("help1")
          )
          
  ),
  fluidRow(
    column(width = 3, class = "well",
           # selectInput("soubor", "vyber soubor:",files,selected = initd$initfile1),
           uiOutput("selsoub1"),
           uiOutput("selsloup1")
           # selectInput("sloupec", "vyber soubor (max 3):",multiple = TRUE, names_,selected = initd$initval1)
           
    ),      
    column(width = 3, class = "well",
           uiOutput("selsoub2"),
           uiOutput("selsloup2")
           # selectInput("soubor2", "vyber soubor:",files,selected = initd$initfile2),
           # selectInput("sloupec2", "vyber soubor (max 3):",multiple = TRUE, names_,selected = initd$initval2)
    ),
    column(width = 3, class = "well",
           uiOutput("selsoub3"),
           uiOutput("selsloup3")
           # selectInput("soubor2", "vyber soubor:",files,selected = initd$initfile2),
           # selectInput("sloupec2", "vyber soubor (max 3):",multiple = TRUE, names_,selected = initd$initval2)
    ),
    column(width = 3, class = "well",
             actionButton("save", "SAVE"),
             # helpText("Nastaveni si muzes ulozit a az skript restartujes, ukaze se ti ulozeny stav."),
             actionButton("reset", "RESET"),
             # helpText("Vymazes ulozeny stav, projevi se to po restartovani skriptu. "),
             actionButton("refresh", "REFRESH"),
             helpText('nazev png obrazku (bez pripony)'),
             textInput("pngplotjmeno",label = NULL),
             actionButton("pngplot", "png PLOT")
    ),
    fluidRow(
      
      column(width = 12, class = "well",
             # helpText("Pomoci mysi muzes zoomovat v levem plotu, vysledek se zobrazi v pravem plotu"),
             
             uiOutput("t1"),
             
             plotOutput("plot2", height = 500)
      )
    )
    
  )
)

server <- function(input, output) {
  
  
  ranges2 <- reactiveValues(x = NULL, y = NULL)
  
  
  observeEvent(input$save, { saveinput(input,ranges2)  })
  observeEvent(input$reset, { resetsave(input)  })
  
  
  observeEvent(input$pngplot, { 
    png(paste(input$pngplotjmeno,'.png',sep = ''),height = 1500,width = 2500,pointsize = 5,res=600)
    par(mar=c(4,4,4,4))
    if (is.null(ranges2$x)){
      plot_(input$soubor,input$soubor2,input$soubor3,input$sloupec,input$sloupec2,input$sloupec3,NULL,input$somevalue)
    } else {
      plot_(input$soubor,input$soubor2,input$soubor3,input$sloupec,input$sloupec2,input$sloupec3,ranges2,input$somevalue)
    }
    dev.off()
    
    })
  
  
  output$selsoub1 = renderUI({
    input$refresh
    out = ctenidat(dir_)
    H      = out[[1]]
    files  = out[[2]]
    names_ = out[[3]]
    rm(out)
    input$refresh
    initd = init()
    selectInput("soubor", "vyber levy soubor:",files,selected = initd$initfile1)
  })
  output$selsloup1 = renderUI({
    input$refresh
    initd = init()
    selectInput("sloupec", "vyber soupec (max 3):",multiple = TRUE, names_,selected = initd$initval1)
  })
  output$selsoub2 = renderUI({
    input$refresh
    out = ctenidat(dir_)
    H      = out[[1]]
    files  = out[[2]]
    names_ = out[[3]]
    rm(out)
    input$refresh
    initd = init()
    selectInput("soubor2", "vyber pravy soubor:",files,selected = initd$initfile2)
  })
  output$selsloup2 = renderUI({
    input$refresh
    initd = init()
    selectInput("sloupec2", "vyber sloupec (max 3):",multiple = TRUE, names_,selected = initd$initval2)
  })
  
  output$selsoub3 = renderUI({
    input$refresh
    out = ctenidat(dir_)
    H      = out[[1]]
    files  = out[[2]]
    names_ = out[[3]]
    rm(out)
    input$refresh
    initd = init()
    selectInput("soubor3", "vyber pravy soubor:",files,selected = initd$initfile2)
  })
  output$selsloup3 = renderUI({
    input$refresh
    initd = init()
    selectInput("sloupec3", "vyber sloupec (max 3):",multiple = TRUE, names_,selected = initd$initval2)
  })
  
  
  output$value <- renderText({
    if (input$help) {
      helptext2
    } else {
      NULL
    }
  })
  
  
  output$help1 <- renderText({
    if (input$help) {
      helptext
    } else {
      NULL
    }
  })
  
  output$t1 <- renderUI({
    if (input$somevalue) {
      plotOutput("plot1", height = 500,
                 brush = brushOpts(
                   id = "plot2_brush",
                   resetOnNew = TRUE
                 )
      )
    } else {
      plotOutput("plot1", height = 500,
                 brush = brushOpts(direction = 'x',
                                   id = "plot2_brush",
                                   resetOnNew = TRUE
                 )
      )
      
    }
  })
  
  output$plot1 <- renderPlot({
    par(mar=c(4,4,4,4))
    if (is.null(input$sloupec)){
      plot(NA,ylim = c(0,1),xlim = c(0,1),frame.plot = TRUE, axes = FALSE, xlab = '', ylab = '')
      text(0.5,0.5,"Vyber sloupce z leveho souboru",cex=5)
    } else {
      plot_(input$soubor,input$soubor2,input$soubor3,input$sloupec,input$sloupec2,input$sloupec3,NULL,input$somevalue)
    }
    
  })
  
  output$plot2 <- renderPlot({
    par(mar=c(4,4,4,4))
    if (is.null(ranges2$x)){
      plot(NA,ylim = c(0,1),xlim = c(0,1),frame.plot = TRUE, axes = FALSE, xlab = '', ylab = '')
      text(0.5,0.5,"Vyber mysi z grafu nad.",cex=5)
    } else {
      plot_(input$soubor,input$soubor2,input$soubor3,input$sloupec,input$sloupec2,input$sloupec3,ranges2,input$somevalue)
    }
    
  })
  
  
  observe({
    brush <- input$plot2_brush
    if (!is.null(brush)) {
      ranges2$x <- c(brush$xmin, brush$xmax)
      ranges2$y <- c(brush$ymin, brush$ymax)
    } else {
      ranges2$x <- initd$initr$x
      ranges2$y <- initd$initr$y
    }
  })
  
  
}

shinyApp(ui, server)

