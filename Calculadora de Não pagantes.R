library(shiny)
library(dplyr)

ui <- fluidPage(
  titlePanel("Calculadora de Almoço por Peso"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Quantidade por cargo"),
      numericInput("n_ass2", "Assessor 2", 1, min = 0),
      numericInput("n_ass1", "Assessor 1", 1, min = 0),
      numericInput("n_rt", "Responsável Técnico", 1, min = 0),
      numericInput("n_sup", "Supervisor", 1, min = 0),
      numericInput("n_ges", "Gestor", 1, min = 0),
      
      hr(),
      
      h4("Peso por cargo"),
      numericInput("p_ass2", "Peso Assessor 2", 1.0, min = 0),
      numericInput("p_ass1", "Peso Assessor 1", 1.2, min = 0),
      numericInput("p_rt", "Peso RT", 1.5, min = 0),
      numericInput("p_sup", "Peso Supervisor", 2.0, min = 0),
      numericInput("p_ges", "Peso Gestor", 3.0, min = 0),
      
      hr(),
      
      numericInput("valor_total", "Valor da conta (R$)", 400, min = 0)
    ),
    
    mainPanel(
      h4("Resultado"),
      tableOutput("tabela"),
      h4(textOutput("valor_cota"))
    )
  )
)

server <- function(input, output) {
  
  dados <- reactive({
    tibble(
      Cargo = c("Assessor 2", "Assessor 1", "RT", "Supervisor", "Gestor"),
      Quantidade = c(
        input$n_ass2,
        input$n_ass1,
        input$n_rt,
        input$n_sup,
        input$n_ges
      ),
      Peso = c(
        input$p_ass2,
        input$p_ass1,
        input$p_rt,
        input$p_sup,
        input$p_ges
      )
    ) %>%
      mutate(
        Cotas = Quantidade * Peso
      )
  })
  
  valor_cota <- reactive({
    total_cotas <- sum(dados()$Cotas)
    if (total_cotas == 0) return(0)
    input$valor_total / total_cotas
  })
  
  output$tabela <- renderTable({
    dados() %>%
      mutate(
        `Valor por pessoa (R$)` = round(valor_cota() * Peso, 2),
        `Total por cargo (R$)` = round(`Valor por pessoa (R$)` * Quantidade, 2)
      )
  })
  
  output$valor_cota <- renderText({
    paste0("Valor da cota: R$ ", round(valor_cota(), 2))
  })
}

shinyApp(ui, server)
