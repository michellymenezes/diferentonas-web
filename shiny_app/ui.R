library(shiny)
library(plotly)
library(DT)
library(readr)

ginicities = read_csv("data/ginicities_5.csv")

# Define UI for application that draws a histogram
shinyUI(fluidPage(theme = "bootstrap.css",
  
  # Application title
  titlePanel("Convênios X Fornecedores"),
  
  sidebarLayout(
    sidebarPanel(
      
      selectInput("selectEstado", "Estado:", choices = unique(ginicities %>%
                                                              select(UF))),
      selectInput("selectCidade", "Cidade:", choices = ginicities %>%
                                                      filter(UF == "PB") %>%
                                                      select(cidade))
      ),
    
    # Show the generated plot
    mainPanel(
      tabsetPanel(
        
        tabPanel("Ginis", plotlyOutput("plotCidades"),
                          DT::dataTableOutput("cidadesSemelhantes")),
        tabPanel("Convênios", plotlyOutput("plotConvenios1"),
                               plotlyOutput("plotConvenios2"),
                               plotlyOutput("plotConvenios3")),
        tabPanel("Fornecedores",  plotlyOutput("plotFornecedores1"),
                                   plotlyOutput("plotFornecedores2"),
                                   plotlyOutput("plotFornecedores3")),
        tabPanel("Tabela", DT::dataTableOutput("plotTabela"))
        )
    )
  )
))
