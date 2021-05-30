library(shiny)
library(ggplot2)
library(plyr)
library(magrittr) 
library(dplyr)    
library(corrplot)
library(GGally)
library(shinythemes)
library(shinyjs)

Data <- read.csv('D:/Pau/BIG DATA/Logiciel Statistiques/R/earthq.csv')
NomColonnes <- c("Annee","Pays","Magnitude","Profondeur","Deces")
names(Data) <-   NomColonnes
Data[is.na(Data)] <- 0

# Define UI for random distribution app ----
ui <-fluidPage(useShinyjs(),
                 theme = shinytheme("cerulean"),
                # App title ----
               
                navbarPage(
                'Earthquake Analysis 2000-2020',
                
                tabPanel('Accueil',
                sidebarPanel(
                    checkboxGroupInput('attrnames', 'Colonnes de table:', names(Data),
                                     selected = names(Data)),
                   h4('Voici les colonnes de notre jeu de donnees, vous pouvez selectionner les colonnes que nous voulons afficher.'),
                   h4('Vous pouvez les donnees selon le Pays que vous voulez')
                ),
                mainPanel(
                  tabsetPanel(
                    tabPanel('Donnee',uiOutput("countryList"),hr(),tableOutput("generalData")),
                    tabPanel("Sommaire", verbatimTextOutput("summary"))))
                ),
                  
                tabPanel("Unvariee", 
                         mainPanel(
                           uiOutput('earthqAttr'),
                           hr(),
                           tabsetPanel(
                                                  
                             tabPanel('Histogram',plotOutput("histStatPlot")),
                             tabPanel('Boxplot',plotOutput("boxStatPlot"),h3("- Chaque boxPlot presente 2 annees")),
                             tabPanel('Graph',plotOutput("graphStatPlot"))
                           ))),
                
                tabPanel("Bivariee",
                         mainPanel(
                           uiOutput('earthqAttrCheckBox'),
                           hr(),
                           tabsetPanel(
                              tabPanel('Nuages de Points',plotOutput("corrStatPlot")),
                              tabPanel('Corralation', uiOutput("countryList1"),hr(), hidden(p(id="alertp", "")), plotOutput("corrStatPlot2"))
                              )
                 )
                )
)
)           
# Define server logic for random distribution app ----
server <- function(input, output){
    output$countryList <- renderUI({
      list(span(style = "color:blue;", "Choisissez votre Pays "),
           selectInput(inputId = "countryattrs", label="", choices = countryattrList()))
      
    })
    output$countryList1 <- renderUI({
      list(span(style = "color:blue;", "Choisissez votre Pays "),
           selectInput(inputId = "countryattrs1", label="", choices = countryattrList()))
      
    })
    
    output$earthqAttr = renderUI({
      list(span(style = "color:blue;", "Choissisez votre variables."),
           selectInput(inputId = "statisticattrs", label="", choices = statisticattrList()))
      
    })
    output$earthqAttrCheckBox = renderUI({
      list(span(style = "color:blue;", "Choisissez votre variables "),
           checkboxGroupInput(inline = TRUE,inputId = "statisticattrscheck", selected = statisticattrList(),label="", choices = statisticattrList()))
      
    })
    
    statisticattrList <- reactive({
      return(c('Magnitude','Profondeur','Deces'))
    })
    countryattrList <- reactive({
      if(is.null(Data)){
        lst <- c()
      } else {
        lst <- Data %>% select(Pays) %>% distinct(Pays) %>% arrange(Pays) 
      }
      return(lst)
    })
    
    # customize the length drop-down menu; display 5 rows per page by default
    output$generalData = renderTable({
      req(input$countryattrs)
      req(input$attrnames)
      selectedCountry <- input$countryattrs
      selectedAttr <- input$attrnames
      Data %>% filter(Pays == selectedCountry) %>%  select(selectedAttr)
    })
    output$summary <- renderPrint({
      summary(Data)
    })
    output$results1 = renderDataTable({
      Data1
    })
    output$distPlot2 = renderDataTable({
      summary(Data)
    })
    
    ###  Unvarie hist
    output$histStatPlot <- renderPlot({
      req(input$statisticattrs)
      selectedStatAttr <- input$statisticattrs
      hist(Data[,selectedStatAttr],probability = TRUE,xlab = selectedStatAttr,ylab = "Densite de freq",main=paste("Densite de frequence pour",selectedStatAttr))
      dens <- density(Data[,selectedStatAttr])
      lines(dens, col= 'red',lwd=2)
  
})
    ###  Unvarie boxplot
      output$boxStatPlot <- renderPlot({
      req(input$statisticattrs)
      selectedStatAttr <- input$statisticattrs
      Data[,c(selectedStatAttr,"Annee")] %>% mutate(anneeAppariee = (Annee %/% 2) * 2) %>% with(boxplot(eval(as.symbol(selectedStatAttr))
~anneeAppariee ,ylab = selectedStatAttr, xlab="anneeAppariee",main=paste("Boxplot de",selectedStatAttr,"selon anneeAppariee")))
      
    })

    #### Graph
    output$graphStatPlot <- renderPlot({
    req(input$statisticattrs)
    selectedStatAttr <- input$statisticattrs
    if(selectedStatAttr == "Deces"){
      Data %>% group_by(Annee) %>% summarise(TotalDeces=sum(Deces)) %>% with(plot(Annee,TotalDeces, type="l", col="black", lwd=1, xlab="Annee", ylab="TotalDeces", main="Nombre de deces par an"))
    }else{
      Data %>% group_by(Annee) %>% summarise(avgStat=mean(eval(as.symbol(selectedStatAttr)))) %>% with(plot(Annee,avgStat, type="l", col="black", lwd=1, xlab="anneeAppariee", ylab=paste("Moyenne",selectedStatAttr), main=paste("Moyenne de",selectedStatAttr)))
    }
    
    })
    
    ## Analyse bivariee
    output$corrStatPlot <- renderPlot({
        req(input$statisticattrscheck)
        checkedStatAttr <- input$statisticattrscheck
        ggpairs(Data, columns=checkedStatAttr,diag=list(continuous="densityDiag",discrete="barDiag"), axisLabels="show",mapping = ggplot2::aes(colour='Deces'))
      
     })
    output$corrStatPlot2 <- renderPlot({
      req(input$statisticattrscheck)
      req(input$countryattrs1)
      shinyjs::hide("alertp")
      checkedStatAttr <- input$statisticattrscheck
      selectedCountry <- input$countryattrs1
      
      if(length(checkedStatAttr)<2){
        shinyjs::show("alertp")
        html("alertp","<h3 style='color:red;'> !!! Vous devez selectionner au moins 2 variables.</h3>")
      }else{
        indx <- which(Data$Pays== selectedCountry)
        mat.cor <- cor(x = Data[indx,checkedStatAttr])
        if(!is.na(all(abs(mat.cor) <= 1)) ){
          corrplot(mat.cor, method = "color", addCoef.col="grey", order = "AOE",number.cex=0.75, title = selectedCountry,mar=c(0,0,1,0))  
        }  else {
          html("alertp", paste("<h4 style='color:red;'>!!! Pas assez de donnees pour calculer la  correlation pour</h4>",selectedCountry))
          
          shinyjs::show("alertp")
        }
      }
    })
}
# Create Shiny app ----
shinyApp(ui, server)