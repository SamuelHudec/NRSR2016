
library(dplyr)
library(shiny)
library(ggplot2)
library(DT)

volby <- read.table("/University/doktorantske/apps/volby16.txt", header=TRUE, quote="\"")


# Define UI ----
ui <- fluidPage(
  # main title
  titlePanel("Prehlad volebnych dat do NRSR 2016"),
  
  sidebarLayout(
    sidebarPanel(
      
      helpText("Vysvetlovana premenna je v nasom pripade zvolena politicka strana vyjadrena v percentach na volebny okrsok"),

      # Input: response
      selectInput(inputId = "res",
                  label = "Vysvetlovana premenna:",
                  choices = c("OlanoNova"="OlanoNova_percent", 
                    "SmeRodina" = "SmeRodina_percent", 
                    "Most"="Most_percent",
                    "SNS"="SNS_percent",
                    "SMER"="SMER_percent",
                    "KDH"="KDH_percent",
                    "LSNS"="LSNS_percent",
                    "Siet"="Siet_percent",
                    "SMK"="SMK_percent",
                    "SaS"="SaS_percent"),
                  selected = "SMER_percent"),
      
      
      helpText("Vo vztahu k vysvetlujucej premennej mozeme za pomoci interaktivnej vyzulalizacie lepsie pochopit zmyslanie volicov"),
      
      # Input: variable
      selectInput(inputId = "var",
                  label = "Vysvetlujuca premenna:",
                  choices = c("Mladi"="mladi",
                    "Zakladne vzdelanie" ="zakladne_vzdelanie_.",
                    "Stredoskolske vzdelanie" = "zaklad.stred",
                    "Vysoko skolske vzdelanie" = "vs_vzdelanie_mgr",
                    "Bez vzdelania" ="bez_vzdelania_.",
                    "Pocet obyvatelov"="pocet_obyvatelov_2014",
                    "Prirastok obyvatelov"="perce_rozdiel_obyv_2014_ku2004",
                    "Vek vyssi ako 65" ="vek65v",
                    "Madarska narodnost" = "madarska_narodnost",
                    "Rimsko katolicka cirkev" ="rim_kat",
                    "Vek do 14" ="vek0_14",
                    "Romska mensina" ="perc_romov",
                    "per_uoz"),
                  selected = "mladi"),
      
      hr(), # little break for better visual 
      
      checkboxInput(inputId = "check", 
                  label = "Ofarbit podla kraja",
                  value = F),

      sliderInput(inputId= "al",
        label = "Jas:", min=0, max=1,
        value = 0.5)
      ),
    
    
    # Main panel for displaying outputs
    mainPanel(
      # Output main plot 
      plotOutput(outputId = "scatterplot", brush="plot_brush"), hr(),
      helpText("Vyznacenim bodov v obrazku vyplnite tabulku"),
      dataTableOutput(outputId = "partytable")
      )
    
  )
)

# Define server logic
server <- function(input, output) {
  output$scatterplot <- renderPlot({
    if(input$check){
      ggplot(data=volby, aes_string(y=input$res,x=input$var,col=volby$kod_kraja)) + 
      geom_point(alpha=input$al) + labs(y="Vysvetlovana premenna", x="Vysvetlujuca premenna") +
        theme(legend.position="none")
    }else{
      ggplot(data=volby, aes_string(y=input$res,x=input$var)) + 
      geom_point(alpha=input$al) + labs(y="Vysvetlovana premenna", x="Vysvetlujuca premenna")
    }
    
  })
  
  output$partytable <- DT::renderDataTable({
    brushedPoints(volby, input$plot_brush) %>% select(kod_kraja, obvod ,obec)
  })
  
  
  
}

# Run the app
shinyApp(ui = ui, server = server)



