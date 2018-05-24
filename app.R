############# app.R ###############
library(shiny)
library(shinydashboard)
if(!require("shinyalert"))
{
  install.packages('shinyalert')
}
if(!require("tidyverse"))
{
  install.packages('tidyverse')
}
library(tidyverse)

if(!require("DT"))
{
  install.packages('DT')
}
if(!require("ggplot2"))
{
  install.packages('ggplot2')
}
library('ggplot2')


############## UI ###################

ui <- ui <- dashboardPage(
  dashboardHeader(title = "File Visualizer_v2"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Import file", tabName = "table"),
      menuItem('Dot plot', tabName = "dotplot"),
      menuItem('Bar plot', tabName = "barplot"),
      menuItem('Box plot', tabName = "boxplot"),
      menuItem('Retour au site', href = 'https://floriancha.github.io/stage/')
    )
  ),
  dashboardBody(
    useShinyalert(),
    tabItems(
################# First tab content : Import file  + Table #####################
      tabItem(tabName = "table",
              fluidRow(
                box(title = 'Choose File',status = "primary",solidHeader = TRUE,uiOutput('fileImport'),actionButton("Done", "Done")),
                box(title = "Option",status = "primary",solidHeader = TRUE,checkboxInput("header2", "Header", TRUE),
                    radioButtons("sep2", "Separator",choices = c(Comma = ",",Semicolon = ";",Tab = "\t"),selected = ",")),
                
                box(width=12,title = 'Table',solidHeader = TRUE,status = "success",uiOutput('table'))
                
              )
      ),
################# Second tab content : Dot Plot #####################

      tabItem(tabName = "dotplot",
              fluidRow(
                column( width=3,
                tabBox(title = NULL,width = NULL,
                    tabPanel("Axis X",
                        width=3,
                        uiOutput('X'),
                        uiOutput('NewNameX'),
                        uiOutput('echX')),
                    tabPanel("Axis Y",
                        width=3,
                        uiOutput('Y'),
                        uiOutput('NewNameY'),
                        uiOutput('echY')),
                    tabPanel("Title",
                             width=3,
                             textInput("title",label = 'Give title at your plot',value = NULL), 
                             selectInput('formTitle',label = 'Form title',choices = list('Plain' = 'plain','Bold' = 'bold','italic' = 'italic','Bold and Italic'='bold.italic'),selected = 'Plain'),
                             selectInput('colorTitle',label = 'color title',choices = list('black','red','blue','green','purple','orange','yellow'),selected = 'black')
                            )),
                box(title = 'Graph Option',width = NULL, status = "primary",collapsible = TRUE, solidHeader = TRUE,
                    selectInput('typeColor',label = 'Color type',choices = c('Simple color','Color by selection','Color by group'),selected = 'Simple color'),
                    uiOutput('typeDot'),
                    radioButtons('correlation',label =NULL,choices= c('No regression','Add regression curve','Add right of regression')))),
                column( width=9,
                  box(title = "Graphique",width = NULL,status = "success",solidHeader = TRUE,plotOutput("dotplot",height = "500px"),
                        downloadButton('downloadPlot', 'Download Plot')),
                  box(title = 'Color Option',width = NULL, status = "primary",collapsible = TRUE, solidHeader = TRUE,
                      column( width=4,
                              uiOutput('SelectGroupe'),
                              uiOutput('selection'),uiOutput('line')),
                      column( width=4,
                              uiOutput('StrOrInt'),
                              uiOutput('selectColor')),
                      column( width=4,
                              uiOutput('unselectColor'))))
                
              )
      ),
################# third tab content : Bar Plot #####################
      tabItem(tabName = "barplot",
              fluidRow(
                column( width=3,
                        tabBox(title = NULL,width = NULL,
                               tabPanel("Axis X",
                                        width=3,
                                        uiOutput('X1'),
                                        uiOutput('NewNameX1')),
                               tabPanel("Axis Y",
                                        width=3,
                                        uiOutput('Y1'),
                                        uiOutput('NewNameY1')),
                               
                               tabPanel("Title",
                                        width=3,
                                        textInput("title1",label = 'Give title at your plot',value = NULL), 
                                        selectInput('formTitle1',label = 'Form title',choices = list('Plain' = 'plain','Bold' = 'bold','italic' = 'italic','Bold and Italic'='bold.italic'),selected = 'Plain'),
                                        selectInput('colorTitle1',label = 'color title',choices = list('black','red','blue','green','purple','orange','yellow'),selected = 'black')
                               )),
                        box(title = 'Graph Option',width = NULL, status = "primary",collapsible = TRUE, solidHeader = TRUE,
                            selectInput('histvar',label = 'type barplot', choices = c('Count variable x','Distribution of variable x','x vs y') ,selected = NULL),
                            uiOutput('typeColo'),
                            uiOutput('GroupOption'),uiOutput("typeb"))),
                column( width=9,
                        box(title = "Graphique",width = NULL,status = "success",solidHeader = TRUE,plotOutput("barplot",height = "500px"),
                            downloadButton('downloadPlot1', 'Download Plot')),
                        box(title = 'Color Option', width = NULL, status = 'primary',solidHeader = TRUE,
                            column(width = 4,
                                   uiOutput('col'),
                                   uiOutput('PositionLegende')),
                            column(width = 4
                                   ,uiOutput('colFIll'),
                                   tags$br(),
                                   uiOutput('value2')),
                            column(width = 4
                                   ,uiOutput('groupBox'),
                                   tags$br(),
                                   uiOutput('value1'),
                                   uiOutput('errorBarOpt'))
                            ))
              )
      ),
################# fourth tab content : Box Plot #####################
tabItem(tabName = "boxplot",
        fluidRow(
          column( width=3,
                  tabBox(title = NULL,width = NULL,
                         tabPanel("Axis X",
                                  width=3,
                                  uiOutput('X2'),
                                  uiOutput('NewNameX2')),
                         tabPanel("Axis Y",
                                  width=3,
                                  uiOutput('Y2'),
                                  uiOutput('NewNameY2')),
                         
                         tabPanel("Title",
                                  width=3,
                                  textInput("title2",label = 'Give title at your plot',value = NULL), 
                                  selectInput('formTitle2',label = 'Form title',choices = list('Plain' = 'plain','Bold' = 'bold','italic' = 'italic','Bold and Italic'='bold.italic'),selected = 'Plain'),
                                  selectInput('colorTitle2',label = 'color title',choices = list('black','red','blue','green','purple','orange','yellow'),selected = 'black')
                         )),
                  box(title = 'Graph Option',width = NULL, status = "primary",collapsible = TRUE, solidHeader = TRUE,
                      uiOutput('typeColo1'),
                      uiOutput('GroupOption1'))),
          column( width=9,
                  box(title = "Graphique",width = NULL,status = "success",solidHeader = TRUE,plotOutput("boxplot",height = "500px"),
                      downloadButton('downloadPlot2', 'Download Plot')),
                  box(title = 'Color Option', width = NULL, status = 'primary',solidHeader = TRUE,
                      column(width = 4,
                             uiOutput('col1'),
                             uiOutput('PositionLegende1')),
                      column(width = 4
                             ,uiOutput('colFIll1'),
                             tags$br()),
                      column(width = 4
                             ,uiOutput('groupBox1'))
                  ))
        )
)
    )
  )
)


############## Server ###################

server <- function(input, output) {
    output$fileImport<- renderUI({
    fileInput("file2", "",multiple = TRUE,accept = NULL,buttonLabel = "Browse...",placeholder = "No file selected") })
    observeEvent(input$Done, {
    if (length(colnames(df())) >= 3)
    {
      showModal(modalDialog(title='Message',
                            'Now that you import and parse your file, you can go to graphique tab for make graph'))
    }
    else 
    {
      shinyalert(title='Error!',type='error',
                 text ="Your file isn't parser correctly, please check that your file is a csv or txt file and that the params are well selected")
    }
    
  })
    output$table = renderUI({
    req(input$file2)
    if (length(colnames(df())) >= 2 && input$Done == TRUE)
    {
      DT::dataTableOutput("contents")
    }
    })
    output$contents <- DT::renderDataTable({
        df <- read.csv(input$file2$datapath,
                     header = input$header2,
                     sep = input$sep2)

         dft = datatable(df,escape = FALSE, colnames = colnames(df),extensions = 'Buttons', options = list(dom = 'Blfrtip',buttons = c('copy', 'csv', 'excel', 'pdf', 'print')))
        return(dft)
    })
############ DotPlot########################
    dotPlot <- reactive({
    req(input$file2)
    df <- read.csv(input$file2$datapath,header = input$header2,sep = input$sep2)
    df <- data.frame(rownames(df),df)
    colnames(df) <- c('Id',colnames(df)[2:ncol(df)])
    if (!is.null(input$nameY))
    {
      p <- ggplot(df,aes(x = df[,colnames(df)==input$nameX],y = df[,colnames(df)==input$nameY]))
      p <- p + labs(y = input$newnameY)
      Y <- df[,colnames(df) == input$nameY]
      if (is.numeric(df[,colnames(df)==input$nameY]))
      {
        p <- p +  ylim(input$echelleY)
      }
    }
    else if (is.null(input$nameY))
    {
      p <- ggplot(df,aes(x = df[,colnames(df)==input$nameX],y = df[,3]))
      Y <- df[,3]
      p <- p + labs(y = colnames(df)[3])
    }
    p <- p + labs(x = input$newnameX)
    if (is.numeric(df[,colnames(df)==input$nameX]))
    {
      p <- p + xlim(input$echelleX)
    }
   
    if (input$typeColor == 'Simple color')
    {
      p <- p + geom_point(col=input$Col,shape=as.integer(input$formPoint))
    }
    else if   (input$typeColor == 'Color by group')
    {
      group <- df[,colnames(df) == input$group]
      if (input$typeGroup == 'Character')
      {
        p <- p + geom_point(aes(color=as.character(group)),shape=as.integer(input$formPoint))
        p <- p +labs(color=input$group)
      }
      else if (input$typeGroup == 'Integer')
      {
        p <- p + geom_point(aes(color=group),shape=as.integer(input$formPoint))
        p <- p +labs(color=input$group)
      }
    }
    else if (!is.null(input$slider1) && input$typeColor == 'Color by selection')
    {
      test <- rep(input$unselectC,length(Y))
      min <- strsplit(paste(input$slider1,collapse = ' '),' ')[[1]][1]
      max <- strsplit(paste(input$slider1,collapse = ' '),' ')[[1]][2]
      min <- as.numeric(min)
      max <- as.numeric(max)
      select <- Y >= min & Y <= max
      test[select] <- input$selectC
      p <- p + geom_point(shape=as.integer(input$formPoint), color = test)
      if ('min' %in% input$line)
      {
        p <- p + geom_hline(yintercept=min, linetype="dashed")
      }
      if ('max' %in% input$line)
      {
        p <- p + geom_hline(yintercept=max, linetype="dashed")
      }
    }
    if (!is.null(input$correlation))
    {
      if (input$correlation == 'Add regression curve')
      {
        p <- p + stat_smooth()
      }
      else if (input$correlation == 'Add right of regression')
      {
        p <- p + stat_smooth(method = "lm")
      }
    }
    
    p <- p + ggtitle(input$title)
    p <- p + theme(plot.title = element_text(colour=input$colorTitle, size=14, face=input$formTitle))
    return(p)

    })
    df <- function(){ 
      req(input$file2)
      df <- read.table(input$file2$datapath,header = input$header2,sep = input$sep2)
      df <- data.frame(rownames(df),df)
      colnames(df) <- c('Id',colnames(df)[2:ncol(df)])
      return(df)}
    nbDecimal <- function(x){
      if (x%%1 ==0) 
        { 
      return(0)
        }
      else
        {
        return(nchar(strsplit(toString(x),'[.]')[[1]][2]))
          }
 
    }
    step <- function(x){
      step <- c()
      for (elt in x)
      {
        step <- c(step,nbDecimal(elt))
      }
      step <- 10^-max(step)
      return(step)
    }
    output$X <- renderUI({
      selectInput(inputId = 'nameX', label = 'Variable',choices = colnames(df()),selected = colnames(df())[2])
    })
    output$SelectGroupe <- renderUI({
      req(input$file2)
      if   (input$typeColor == 'Color by group')
      {
        selectInput(inputId = 'group', label = 'Color by group',choices = c('Id',colnames(df())),selected = colnames(df())[2])
      }
      else if (input$typeColor == 'Simple color')
      {
        selectInput('Col','Choose a color',choices = c('black','red','blue','green','yellow','orange','purple','white'))
      }
    })
    output$StrOrInt <- renderUI({
      req(input$file2)
      if (input$typeColor == 'Color by group')
      {
        radioButtons(inputId = 'typeGroup', label = 'Type of group variable',choices = c('Integer','Character'),selected = NULL,inline = T)
      }
    })
    output$echX <- renderUI({
      df <- df ()
      if (is.numeric(df[,colnames(df)==input$nameX]))
      {
        X <- input$nameX
        variableX <- as.character(df[,colnames(df)== X])
        sliderInput("echelleX",
                    paste("Scale :"),
                    min = min(as.numeric(variableX)),
                    max =  max(as.numeric(variableX)),
                    step = step(Xdata()),
                    value = c(min(as.numeric(variableX)),max(as.numeric(variableX))))
      }
     
      })
    output$Y <- renderUI({
      selectInput(inputId = 'nameY', label = 'Variable',choices = colnames(df()),selected = colnames(df())[3])
    })
    output$echY <- renderUI({
      req(input$nameY)
     
      df <- df ()
      if (is.numeric(df[,colnames(df)==input$nameY]))
      {
        Y <- input$nameY
        variableY <- as.character(df[,colnames(df)== Y])
        sliderInput("echelleY",
                    paste("Scale :"),
                    min = min(as.numeric(variableY)),
                    max =  max(as.numeric(variableY)),
                    step = step(Ydata()),
                    value = c(min(as.numeric(variableY)),max(as.numeric(variableY))))
      }
    })
    output$dotplot <- renderPlot({
      # generate bins based on input$bins from ui.R
    dotPlot()
   })
    output$NewNameX <- renderUI({
      textInput(inputId = 'newnameX', label = 'Label :', value = input$nameX)
    })
    output$NewNameY <- renderUI({
      textInput(inputId = 'newnameY', label = 'Label :', value = input$nameY)
    })
    Ydata <- function(){ 
      my_data <- read.table(file=input$file2$datapath,header = input$header2, sep = input$sep2)
      if (is.null(input$nameY))
      {
        return(my_data[,2])
      }
      else
      {
        return(my_data[,colnames(my_data)==input$nameY])
      }
      
    }
    Xdata <- function(){ 
      my_data <- read.table(file=input$file2$datapath,header = input$header2, sep = input$sep2)
      return(my_data[,colnames(my_data)==input$nameX])

 
    }
    output$typeDot <- renderUI({ 
      req(input$file2)
      selectInput('formPoint',label = 'Dot form',choices = list('Normal dot' = 19 ,'Small dot' = 20,'Round dot' = 1,'Star dot'=8,'cross point' = 4,'square point' =0,'triangle point' = 6),selected = 'Normal dot')
      })
    output$selection <- renderUI({
      req(input$file2)
      if (FALSE %in% is.na(is.numeric(Ydata())) && input$typeColor == 'Color by selection')
      {
        if (is.null(input$nameY))
        {
          nameY <- toString(colnames(df())[3])
        }
        else if (!is.null(input$nameY))
        {
          nameY <-input$nameY
        }
        sliderInput("slider1", paste("Select intervall  for : '",nameY,"'"),step = step(Ydata()),min = min(as.numeric(Ydata())) , max = max(as.numeric(Ydata())), value = c(min(as.numeric(Ydata())), max(as.numeric(Ydata()))))
      }
      else if (input$typeColor == 'Color by selection')
      {
        shinyalert(title = 'Error!',text = "The Y variable must be numeric for the 'Color by select' option", type = 'error')
      }
      
    })
    output$selectColor <- renderUI({
      if (FALSE %in% is.na(is.numeric(Ydata())) && input$typeColor == 'Color by selection')
      {
        selectInput("selectC",label = "Select color :",choices = list('white','black','red','green','blue','orange','yellow','purple'),selected = 'red')
      }
    })
    output$unselectColor <- renderUI({
      if (FALSE %in% is.na(is.numeric(Ydata())) && input$typeColor == 'Color by selection')
      {
        selectInput("unselectC",label = "Unselect color :",choices = list('white','black','red','green','blue','orange','yellow','purple'),selected = 'black')
      }
    })
    output$line <- renderUI({
      if (FALSE %in% is.na(as.numeric(Ydata())) &&  input$typeColor == 'Color by selection')
      {
        checkboxGroupInput('line',label=NULL,choices = c('Add min line'='min', 'Add max line'='max'), selected = NULL, inline = T)
      }
    })
    output$downloadPlot <- downloadHandler(
     filename = function() { paste(input$dataset, '.png', sep='') },
      content = function(file) {
          ggsave(file,dotPlot())
        }
    )
####################BarPlot##################
    data_summary <- function(data, varname, groupnames){
      require(plyr)
      summary_func <- function(x, col){
        c(mean = mean(x[[col]], na.rm=TRUE),
          sd = sd(x[[col]], na.rm=TRUE))
      }
      data_sum<-ddply(data, groupnames, .fun=summary_func,
                      varname)
      data_sum <- rename(data_sum, c("mean" = varname))
      return(data_sum)
    }
    barPlot <- reactive({
      req(input$file2)
      df <- read.csv(input$file2$datapath,header = input$header2,sep = input$sep2)
      df <- data.frame(rownames(df),df)
      colnames(df) <- c('Id',colnames(df)[2:ncol(df)])
      X <- df[,colnames(df)==input$nameX1]
      

      if (!is.null(input$nameY1))
      {

        Y <- df[,colnames(df) == input$nameY1]
      }
      else if (is.null(input$nameY1))
      {
        Y <- df[,3]
      }
      if(input$histvar == 'Distribution of variable x')
      {
        if (!is.numeric(df[,colnames(df)==input$nameX1]))
        {
          shinyalert(title ="Error!", text =  "You can't look the distribution of a discontinuous variable", type = "error")
          p <- 'Error'
        }
        else 
        {
          p <- ggplot(df,aes(X))
          p <- p + labs(x = input$nameX1)
          p <- p + labs(y = NULL)
          if (input$Col1 == 'None')
          {
            p <- p + geom_histogram(fill = input$fillCol )
          }
          else
          {
            p <- p + geom_histogram(fill = input$fillCol, col = input$Col1 )
          }
          
        }
      }
      else if (input$histvar == 'Count variable x')
      {
        if (!is.null(input$groupOpt) && input$typeColor1 =='Color by group')
        {
          if (!is.null(input$groupbox))
          {
            group = as.character(df[,colnames(df)==input$groupbox])
            if (length(unique(df[,colnames(df)==input$groupbox]))>8)
            {
              shinyalert(title ="Warning", text =  "You can't use the palette option because they have too many group.", type = "warning")
              paletteC = theme_minimal()
              paletteF =  theme_minimal()
            }
            else 
            {
              paletteC = scale_color_brewer(palette=input$paletteLine)
              paletteF = scale_fill_brewer(palette=input$paletteFill)
            }
            if (input$typeB == 'Multi')
            {
              opt <-position_dodge(0.9)
            }
            else if (input$typeB == 'One')
            {
              opt <-"stack"
            }
          }
          if (input$groupOpt == 'Fill')
          {
            if (input$Col1 == 'None')
            {
              p<-ggplot(data=df, aes(x=X, fill =group )) +paletteF+
                geom_histogram(stat="count",position=opt) +labs(fill=input$groupbox) + theme_minimal() + theme(legend.position=input$PosLegende)
            }
            else 
            {
              p<-ggplot(data=df, aes(x=X, fill =group )) +paletteF+
                geom_histogram(stat="count",  col = input$Col1 ,position=opt) +labs(fill=input$groupbox) + theme_minimal() + theme(legend.position=input$PosLegende)
            }
            
          }
          else if (input$groupOpt == 'Line')
          {
            p<-ggplot(data=df, aes(x=X, color = group)) +
              geom_histogram(stat="count",  fill = input$fillCol ,position=opt)+ paletteC+ theme(legend.position=input$PosLegende)+labs(color=input$groupbox)
          }
          else if (input$groupOpt == 'Fill & Line')
          {
            p<-ggplot(data=df, aes(x=X, fill =group, color = group)) + paletteF + paletteC +labs(fill=input$groupbox)+labs(color=input$groupbox)+
              geom_histogram(stat="count",position=opt)+ theme(legend.position=input$PosLegende)
          }
          if (input$value2 == T)
          {
            p <- p  + geom_text(stat = 'count',aes(label=..count..), position= opt, vjust=1.6, 
                                color="white", size=5)
          }
          
        }
        
        else
        {
          if (input$Col1 == 'None')
          {
            p<-ggplot(data=df, aes(x=X)) +
              geom_histogram(stat="count", fill = input$fillCol)
          }
          else
          {
            p<-ggplot(data=df, aes(x=X)) +
              geom_histogram(stat="count", fill = input$fillCol, col = input$Col1 )
          }
          if (input$value1 == T)
          {
            p <- p  + geom_text(stat = 'count',aes(label=..count..), vjust=1.6, 
                                color="white", size=5)
          }

        }
      
        p <- p + labs(x = input$newnameX1)
        p <- p + labs(y = 'count')
       
      }
      else if (input$histvar == 'x vs y')
      {
        if (is.null(input$nameY1))
        {
          varname <- colnames(df)[3]
        }
        else 
        {
          varname <- input$nameY1
        }
        ######Message error#######
        if(varname == input$nameX1)
        {
          shinyalert(title ="Error", text =  "You can't attribute the same variable for X and Y axis", type = "error")
        }
        if(!is.numeric(df[,colnames(df)==varname]))
        {
          shinyalert(title ="Error", text =  "You can't choose a non numeric variable for Y axis", type = "error")
        }
        if(input$groupbox == varname && !is.null(input$groupbox))
        {
          shinyalert(title ="Error", text =  "You can't attribute the same variable for Y axis and Group variable", type = "error")
        }
        ##########################
        if (!is.null(input$groupbox) && input$typeColor1 =='Color by group' )
        {
          X <- df[,colnames(df)==input$nameX1]
          df1 <- data_summary(df,varname,c(input$nameX1,input$groupbox))
          X <- df1[,colnames(df1)==input$nameX1]
          Y <- df1[,colnames(df1)==varname]
          group = as.character(df1[,colnames(df1)==input$groupbox])
          if (length(unique(df1[,colnames(df1)==input$groupbox]))>8)
          {
            shinyalert(title ="Warning", text =  "You can't use the palette option because they have too many group.", type = "warning")
            paletteC = theme_minimal()
            paletteF =  theme_minimal()
          }
          else 
          {
            paletteC = scale_color_brewer(palette=input$paletteLine)
            paletteF = scale_fill_brewer(palette=input$paletteFill)
          }
          if (input$typeB == 'Multi')
          {
            opt <-position_dodge(0.9)
          }
          else if (input$typeB == 'One')
          {
            opt <-"stack"
          }
          if (input$groupOpt == 'Fill')
          {
            if (input$Col1 == 'None')
            {
              p<-ggplot(data=df1, aes(x=X, y=Y,fill =group)) + paletteF  +labs(fill=input$groupbox)+ 
                geom_bar(stat="identity",position=opt) + theme(legend.position=input$PosLegende)
            }
            else 
            {
              p<-ggplot(data=df1, aes(x=X, y=Y,color = group)) + paletteF + paletteC +
                geom_bar(stat="identity",position=opt,col = input$Col1) + theme(legend.position=input$PosLegende)
            }
            
          }
          else if (input$groupOpt == 'Line')
          {
            p<-ggplot(data=df1, aes(x=X, y=Y,color = group)) +  paletteC +labs(color=input$groupbox) +
              geom_bar(stat="identity",position=opt,  fill = input$fillCol) + theme(legend.position=input$PosLegende)
          }
          else if (input$groupOpt == 'Fill & Line')
          {
            p<-ggplot(data=df1, aes(x=X, y=Y,color = group,fill =group)) +  paletteC + paletteF +labs(color=input$groupbox) + labs(fill=input$groupbox)+ 
              geom_bar(stat="identity",position=opt) + theme(legend.position=input$PosLegende)
          }
          if (input$errorBar == T && !is.null(input$errorBar) &&  input$typeB == 'Multi' )
          {
            p <- p + geom_errorbar(aes(ymin=df1[,colnames(df1)==varname]-df1[,colnames(df1)=='sd'], ymax=df1[,colnames(df1)==varname]+df1[,colnames(df1)=='sd']), width=.2,
                                   position=position_dodge(.9))
          }
          if (input$value2 == T)
          {
            p <- p  + geom_text(data=df1,aes(label=df1[,colnames(df1)==varname]), position= opt, vjust=1.6, 
                                color="white", size=5)
          }
 

          }

        else
        {
          df1 <- data_summary(df,varname,c(input$nameX1))
          X <- df1[,colnames(df1)==input$nameX1]
          Y <- df1[,colnames(df1)==varname]
          if (input$Col1 == 'None')
          {
            p<-ggplot(data=df1, aes(x=X, y=Y)) +
              geom_bar(stat="identity", fill = input$fillCol) 
          }
          else
          {
            p<-ggplot(data=df1, aes(x=X, y=Y)) +
              geom_bar(stat="identity", fill = input$fillCol, col = input$Col1 )
          }
          if (input$value1 == T)
          {
            p <- p  + geom_text(data=df1,aes(label=df1[,colnames(df1)==varname]), vjust=1.6, 
                                color="white", size=5)
          }
          if (is.null(input$nameY1))
          {
            p <- p + ylab(colnames(df)[3]) 
          }
          else 
          {
            p <- p + labs(y = input$newnameY1) 
          }
        }

      }

      p <- p + labs(x = input$newnameX1)
      p <- p + ggtitle(input$title1)
      p <- p + theme(plot.title = element_text(colour=input$colorTitle1, size=14, face=input$formTitle1))
      return(p)
    })
    
    output$errorBarOpt <- renderUI({
      if (input$histvar == 'x vs y' && input$typeColor1 =='Color by group' &&  input$typeB == 'Multi' )
      {
        checkboxInput('errorBar',label = 'Show error bar')
      }
    })
    output$value1 <- renderUI({
      if (input$typeColor1 =='Simple color'  )
      {
        checkboxInput('value1',label = 'Show value')
      }
    })
    output$value2 <- renderUI({
      if (input$typeColor1 =='Color by group' )
      {
        checkboxInput('value2',label = 'Show value')
      }
    })
    output$X1 <- renderUI({
      selectInput(inputId = 'nameX1', label = 'Variable',choices = colnames(df()),selected = colnames(df())[2])
    })
    output$Y1 <- renderUI({
      selectInput(inputId = 'nameY1', label = 'Variable',choices = colnames(df()),selected = colnames(df())[3])
    })
    output$NewNameX1 <- renderUI({
      textInput(inputId = 'newnameX1', label = 'Label :', value = input$nameX1)
    })
    output$NewNameY1 <- renderUI({
      textInput(inputId = 'newnameY1', label = 'Label :', value = input$nameY1)
    })
    Ydata1 <- function(){ 
      my_data <- read.table(file=input$file2$datapath,header = input$header2, sep = input$sep2)
      if (is.null(input$nameY1))
      {
        return(my_data[,2])
      }
      else
      {
        return(my_data[,colnames(my_data)==input$nameY1])
      }
      
    }
    Xdata1 <- function(){ 
      my_data <- read.table(file=input$file2$datapath,header = input$header2, sep = input$sep2)
      return(my_data[,colnames(my_data)==input$nameX1])
      
      
    }
    output$colFIll <- renderUI({
      if (input$groupOpt == 'Fill' && !is.null(input$groupOpt) && input$typeColor1 == 'Color by group' |  input$groupOpt == 'Fill & Line' && !is.null(input$groupOpt)&& input$typeColor1 == 'Color by group')
      {
        selectInput('paletteFill',label = 'Choose a palette', choices = c('Classic'='Paired',
                                                                          'Shade of red'='Reds',
                                                                          'Shade of purple' = 'Purples',
                                                                          'Shade of orange' = 'Oranges',
                                                                          'Shade of grey' = 'Greys',
                                                                          'Shade of green' = 'Greens',
                                                                          'Shade of blue' = 'Blues',
                                                                          'Yellow To Red'='YlOrRd',
                                                                          'Yellow To blue'='YlGnBu',
                                                                          'Yellow To Green'='YlGn'), selected = 'Classic')
      }
      else 
      {      
        selectInput('fillCol',label = 'Choose a fill color', choices = c('Black' = 'black','White' = 'white','Grey' = 'grey','Blue'='steelblue4','Red'='red3','Green'='springgreen4','Purple'='purple4','Yellow' = 'goldenrod3'), selected = 'grey')
      }
      
    })
    output$col <- renderUI({
      if (input$groupOpt == 'Line' && !is.null(input$groupOpt)  |  input$groupOpt == 'Fill & Line' && !is.null(input$groupOpt )&& input$typeColor1 == 'Color by group')
      {
        selectInput('paletteLine',label = 'Choose a palette', choices = c('Classic'='Paired',
                                                                          'Shade of red'='Reds',
                                                                          'Shade of purple' = 'Purples',
                                                                          'Shade of orange' = 'Oranges',
                                                                          'Shade of grey' = 'Greys',
                                                                          'Shade of green' = 'Greens',
                                                                          'Shade of blue' = 'Blues',
                                                                          'Yellow To Red'='YlOrRd',
                                                                          'Yellow To blue'='YlGnBu',
                                                                          'Yellow To Green'='YlGn'), selected = 'Classic')      }
      else 
      {
        selectInput('Col1',label = 'Choose a line color', choices = c('None','Black' = 'black','White' = 'white','Grey' = 'grey','Blue'='steelblue4','Red'='red3','Green'='springgreen4','Purple'='purple4','Yellow' = 'goldenrod3'), selected = 'None')
      }
      
    })
    output$typeb <- renderUI({
      if (input$typeColor1 == 'Color by group' )
      {
        selectInput('typeB',label = 'choose visualization',choices = c('One Bar by value' = 'One','Multi Bar by value' = 'Multi'))
      }
    })
    output$typeColo <- renderUI({
      if (input$histvar != 'Distribution of variable x')
      {
        selectInput('typeColor1',label = 'Color type',choices = c('Simple color','Color by group'),selected = 'Simple color')
      }
      else 
      {
        selectInput('typeColor1',label = 'Color type',choices = c('Simple color'),selected = 'Simple color')
      }
    })
    output$PositionLegende <- renderUI({
      if (input$typeColor1 == 'Color by group' )
      {
        selectInput('PosLegende',label = 'Legend :', choices = c('None'='none','At left'='left','At right'= 'right','At bottom'= 'bottom','At top' = 'top'),selected = 'right')
      }
    })
    output$GroupOption <- renderUI({
      if (input$typeColor1 == 'Color by group' )
      {
        selectInput('groupOpt', label = 'Choose the type of color', choices = c('Fill','Line','Fill & Line'))
      }
    })
    output$groupBox <- renderUI({
      req(input$file2)
      if   (input$typeColor1 == 'Color by group')
      {
        selectInput(inputId = 'groupbox', label = 'Color by group',choices = c('Id',colnames(df())),selected = colnames(df())[2])
      }
    })
    output$barplot <- renderPlot({
      # generate bins based on input$bins from ui.R
      barPlot()
    })
    output$downloadPlot1 <- downloadHandler(
      filename = function() { paste(input$dataset, '.png', sep='') },
      content = function(file) {
        ggsave(file,barPlot())
      })
################## Box plot #############################
    boxPlot <- reactive({
      req(input$file2)
      df <- read.csv(input$file2$datapath,header = input$header2,sep = input$sep2)
      df <- data.frame(rownames(df),df)
      colnames(df) <- c('Id',colnames(df)[2:ncol(df)])
      X <- as.vector(as.character(df[,colnames(df)==input$nameX2]))
      if (!is.null(input$nameY2))
      {
        Y <- df[,colnames(df) == input$nameY2]
        nameY <- input$nameY2
        newnameY <- input$newnameY2
      }
      else if (is.null(input$nameY2))
      {
        Y <- df[,3]
        nameY <-  colnames(df)[3]
        newnameY <- colnames(df)[3]
        
      }
      p <- ggplot(df,aes(x=X,y=Y)) 
      if (!is.null(input$groupbox1) && input$typeColor2 =='Color by group' )
      {
        group = as.character(df[,colnames(df)==input$groupbox1])
        if (length(unique(df[,colnames(df)==input$groupbox1]))>8)
        {
          shinyalert(title ="Warning", text =  "You can't use the palette option because they have too many group.", type = "warning")
          paletteC = theme_minimal()
          paletteF =  theme_minimal()
        }
        else 
        {
          paletteC = scale_color_brewer(palette=input$paletteLine1)
          paletteF = scale_fill_brewer(palette=input$paletteFill1)
        }
        opt <-position_dodge(0.9)
        if (input$groupOpt1 == 'Fill')
        {
          p<-ggplot(data=df, aes(x=X, y=Y,fill = group)) + paletteF + paletteC +labs(fill=input$groupbox1)+ 
          geom_boxplot(position=opt,col = input$Col2) + theme(legend.position=input$PosLegende1)
  
        }
        else if (input$groupOpt1 == 'Line')
        {
          p<-ggplot(data=df, aes(x=X, y=Y,color = group)) +  paletteC +labs(color=input$groupbox1) +
            geom_boxplot(position=opt,  fill = input$fillCol1) + theme(legend.position=input$PosLegende1)
        }
        p <- p + labs(y = newnameY)
      }
      
      else
        {
          p<-ggplot(data=df, aes(x=X, y=Y)) +
          geom_boxplot( fill = input$fillCol1, col = input$Col2 )

          if (is.null(input$nameY2))
          {
            p <- p + ylab(colnames(df)[3]) 
          }
          else 
          {
            p <- p + labs(y = input$newnameY2) 
          }
        }
        
      
      p <- p + labs(x = input$newnameX2)
      p <- p + ggtitle(input$title2)
      p <- p + theme(plot.title = element_text(colour=input$colorTitle2, size=14, face=input$formTitle2))
      return(p)
    })
    output$X2 <- renderUI({
      selectInput(inputId = 'nameX2', label = 'Variable',choices = colnames(df()),selected = colnames(df())[2])
    })
    output$Y2 <- renderUI({
      selectInput(inputId = 'nameY2', label = 'Variable',choices = colnames(df()),selected = colnames(df())[3])
    })
    output$NewNameX2 <- renderUI({
      textInput(inputId = 'newnameX2', label = 'Label :', value = input$nameX2)
    })
    output$NewNameY2 <- renderUI({
      textInput(inputId = 'newnameY2', label = 'Label :', value = input$nameY2)
    })
    Ydata2 <- function(){ 
      my_data <- read.table(file=input$file2$datapath,header = input$header2, sep = input$sep2)
      if (is.null(input$nameY2))
      {
        return(my_data[,2])
      }
      else
      {
        return(my_data[,colnames(my_data)==input$nameY2])
      }
      
    }
    Xdata2 <- function(){ 
      my_data <- read.table(file=input$file2$datapath,header = input$header2, sep = input$sep2)
      return(my_data[,colnames(my_data)==input$nameX2])
      output$barplot <- renderPlot({
        # generate bins based on input$bins from ui.R
        barPlot()
      })
      
    }
    output$colFIll1 <- renderUI({
      if (input$groupOpt1 == 'Fill' && !is.null(input$groupOpt1) && input$typeColor2 == 'Color by group' |  input$groupOpt1 == 'Fill & Line' && !is.null(input$groupOpt1)&& input$typeColor2 == 'Color by group')
      {
        selectInput('paletteFill1',label = 'Choose a palette', choices = c('Classic'='Paired',
                                                                          'Shade of red'='Reds',
                                                                          'Shade of purple' = 'Purples',
                                                                          'Shade of orange' = 'Oranges',
                                                                          'Shade of grey' = 'Greys',
                                                                          'Shade of green' = 'Greens',
                                                                          'Shade of blue' = 'Blues',
                                                                          'Yellow To Red'='YlOrRd',
                                                                          'Yellow To blue'='YlGnBu',
                                                                          'Yellow To Green'='YlGn'), selected = 'Classic')
      }
      else 
      {      
        selectInput('fillCol1',label = 'Choose a fill color', choices = c('Black' = 'black','White' = 'white','Grey' = 'grey','Blue'='steelblue4','Red'='red3','Green'='springgreen4','Purple'='purple4','Yellow' = 'goldenrod3'), selected = 'white')
      }
      
    })
    output$col1 <- renderUI({
      if (input$groupOpt1 == 'Line' && !is.null(input$groupOpt1)  && input$typeColor2 == 'Color by group' |  input$groupOpt1 == 'Fill & Line' && !is.null(input$groupOpt1 ) && input$typeColor2 == 'Color by group')
      {
        selectInput('paletteLine1',label = 'Choose a palette', choices = c('Classic'='Paired',
                                                                          'Shade of red'='Reds',
                                                                          'Shade of purple' = 'Purples',
                                                                          'Shade of orange' = 'Oranges',
                                                                          'Shade of grey' = 'Greys',
                                                                          'Shade of green' = 'Greens',
                                                                          'Shade of blue' = 'Blues',
                                                                          'Yellow To Red'='YlOrRd',
                                                                          'Yellow To blue'='YlGnBu',
                                                                          'Yellow To Green'='YlGn'), selected = 'Classic')      }
      else 
      {
        selectInput('Col2',label = 'Choose a line color', choices = c('Black' = 'black','White' = 'white','Grey' = 'grey','Blue'='steelblue4','Red'='red3','Green'='springgreen4','Purple'='purple4','Yellow' = 'goldenrod3'), selected = 'black')
      }
      
    })
    output$typeColo1 <- renderUI({
      if (input$histvar != 'Distribution of variable x')
      {
        selectInput('typeColor2',label = 'Color type',choices = c('Simple color','Color by group'),selected = 'Simple color')
      }
      else 
      {
        selectInput('typeColor2',label = 'Color type',choices = c('Simple color'),selected = 'Simple color')
      }
    })
    output$PositionLegende1 <- renderUI({
      if (input$typeColor2 == 'Color by group' )
      {
        selectInput('PosLegende1',label = 'Legend :', choices = c('None'='none','At left'='left','At right'= 'right','At bottom'= 'bottom','At top' = 'top'),selected = 'right')
      }
    })
    output$GroupOption1 <- renderUI({
      if (input$typeColor2 == 'Color by group' )
      {
        selectInput('groupOpt1', label = 'Choose the type of color', choices = c('Fill','Line'))
      }
    })
    output$groupBox1 <- renderUI({
      req(input$file2)
      if   (input$typeColor2 == 'Color by group')
      {
        selectInput(inputId = 'groupbox1', label = 'Color by group',choices = c('Id',colnames(df())),selected = colnames(df())[2])
      }
    })
    output$boxplot <- renderPlot({
      # generate bins based on input$bins from ui.R
      boxPlot()
    })
    output$downloadPlot2 <- downloadHandler(
      filename = function() { paste(input$dataset, '.png', sep='') },
      content = function(file) {
        ggsave(file,boxPlot())
      })
}

# Run the application 
shinyApp(ui, server)

#setwd("~/Documents/Plot")
#rsconnect::setAccountInfo(name='floriancha', token='90EF7F3A41696AC9A4B86137CB52321D', secret='tysnnaJTPGvcjc7/ei5/1UCy3k7MRUuYzDv0YvxJ')
#rsconnect::deployApp(appName='Open_File_with_R_v2')
