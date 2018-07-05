library(shiny)
library(shinydashboard)
library(ggplot2)

ui <- dashboardPage(
  skin="red",
  dashboardHeader(title = "Cálculo Numérico"),
  dashboardSidebar(
    
    ## Sidebar content
    dashboardSidebar(
      sidebarMenu(
        
        menuItem("Trapézio", tabName = "trapezio", icon = icon("area-chart")),
        menuItem("Bisseção", tabName = "bissecao", icon = icon("line-chart"))
      )
    )
    
  ),
  dashboardBody(
    
    tabItems(
      # First tab content
      tabItem(tabName = "trapezio",
              
              fluidRow(
                infoBoxOutput("tp1Box"),
                infoBoxOutput("tp2Box"),
                infoBoxOutput("tp3Box")
              ),
              
              fluidRow(
                box(
                  status="success",
                  width=12,
                  plotOutput("funcao1"),br()
                ),
                
                box(
                  title = "Intervalo de integração", status="success",
                  sliderInput(inputId = 'inter', label='', 
                              min = 0, max=50, value=c(10,20), step=2)
                ),
                
                box(
                  title = "Divisões do intervalo", background = "blue",
                  sliderInput(inputId = 'div', label='', 
                              min = 2, max=250, value=c(2), step=1)
                )
              )
      ),
      
      tabItem(tabName = "bissecao",
              fluidRow(
                box(
                  title = "Intervalo de busca", status="success", width = 4,
                  sliderInput(inputId = 'interb', label='',
                              min = -5, max=5, value=c(0,1), step=0.2)
                ),                
                box(
                  title = "Erro tolerado", status="warning", width = 3,
                  textInput("errotol", "Insira um critério de parada para o erro", value=0.0001)
                ),
                
                box(
                  title = "Interações. Estimadas", status="warning", width = 3,
                  textOutput("estimativa")
                ),
                #tags$style("#b1Box {width:200px}"),
                
                box(
                  status="success",
                  width=12,
                  plotOutput("bissecao"),br()
                )
                
              )
              
              
      )
      
    )
  )
)

server <- function(input, output) { 
  
  trapezio <- function(divisoes, ponto_inicial, ponto_final, funcao) {
    h <- (ponto_final-ponto_inicial)/divisoes
    cortes <- ponto_inicial  + seq(1, divisoes-1, by=1) * h
    x <- cortes[-length(cortes)]
    y <- funcao(x)
    xfim <- cortes[-1]
    yfim <- funcao(xfim)
    yfim2 <- -funcao(xfim)*100
    auxiliar <- data.frame(x,y,xfim,yfim,yfim2)
    pontos <- data.frame(x=cortes, y=funcao(cortes))
    grafico <- ggplot(data.frame(x = c(0, 50)), aes(x = x)) +
      stat_function(fun = funcao) +
      geom_vline(xintercept = 0, col='red4', alpha=0.25) +
      geom_hline(yintercept = 0, col='red4', alpha=0.25) +
      geom_vline(xintercept = c(ponto_inicial,ponto_final), col='dodgerblue4') +
      geom_segment(aes(x = ponto_inicial, y = funcao(ponto_inicial), xend = cortes[1], yend = funcao(cortes[1])), col='red') +
      geom_segment(data = auxiliar, aes(x = x, y = y, xend = xfim, yend = yfim), col='red') +
      geom_segment(aes(x = cortes[length(cortes)], y = funcao(cortes[length(cortes)]), xend = ponto_final, yend = funcao(ponto_final)), col='red') +
      geom_point(data=pontos, aes(x, y)) +
      geom_segment(data = pontos, aes(x = x, y = y, xend = x, yend = 0), col='red', alpha=0.5) +
      labs(title=paste0('Aproximação da integral: \nIntervalos divididos em ', divisoes,' partes iguais.'), y='f(x)') +
      theme_minimal() + ylim(c(-50,50))
    exato <- as.numeric(integrate(funcao, ponto_inicial, ponto_final)[1])
    aproximado <- numeric(divisoes-1)
    i0 <- c((h/2) * (funcao(ponto_inicial) + funcao(ponto_inicial+h)))
    for(i in seq(1, divisoes-1, by=1)){
      aproximado[i] <- (h/2) * (funcao(ponto_inicial+h*i) + funcao(ponto_inicial+h*(i+1)))
    }
    valores <- c(i0, aproximado)
    aproximado <- sum(valores)
    return(list('a1' = grafico, 'a2'=aproximado, 'a3'= exato))}
  
  
  output$funcao1 <- renderPlot({trapezio(input$div, input$inter[1], input$inter[2], function(x) x*cos(x))$a1})
  
  
  # CAIXAS _,.-'~'-.,__,.-'~'-.,__,.-'~'-.,__,.-'~'-.,__,.-'~'-.,_
  
  output$tp1Box <- renderInfoBox({
    infoBox(
      "Valor aproximado", paste(as.integer(trapezio(input$div, input$inter[1], input$inter[2], function(x) x*cos(x))$a2)), icon = icon("sliders"),
      color = "purple", fill = TRUE
    )
  })
  
  output$tp2Box <- renderInfoBox({
    infoBox(
      "Valor exato", paste(as.integer(trapezio(input$div, input$inter[1], input$inter[2], function(x) x*cos(x))$a3)), icon = icon("thumbs-up", lib = "glyphicon"),
      color = "green", fill = TRUE
    )
  })
  
  output$tp3Box <- renderInfoBox({
    infoBox(
      "Erro absoluto", paste(abs(as.integer(trapezio(input$div, input$inter[1], input$inter[2], function(x) x*cos(x))$a2) - as.integer(trapezio(input$div, input$inter[1], input$inter[2], function(x) x*cos(x))$a3))), icon = icon("thumbs-down", lib = "glyphicon"),
      color = "red", fill = TRUE
    )
  })
  
  
  # CAIXAS BISSEÇÃO_,.-'~'-.,__,.-'~'-.,__,.-'~'-.,__,.-'~'-.,__,.-'~'-.,_
  
  estimar <- function(a,b,e){
    return(round(log(abs(b-a)/e)/log(2) - 1))
  }
  
  output$estimativa <- renderText({paste0("Estima-se um número maior igual á ", estimar(input$interb[1], input$interb[2], as.numeric(input$errotol)), " interações.")})
  
  raiz <- function(a,b,erro,funcao){
    auxiliar1 <- data.frame(x=c(0,1.570801,-4.712402,4.712402,-1.570801),y=c(0,0,0,0,0))
    i <- 1
    aa <- a
    bb <- b
    anteriores <- c()
    while(abs(a-b) > erro){
      c <- (a+b)/2
      anteriores <- c(anteriores,c)
      i <- i + 1
      if(i > 10000) {
        return(print("Máximo de interações atingido."))
      }
      
      if(funcao(c) == 0){
        return(list(Aviso="A divisão do intervalo coincidiu com a raíz.", Valor=0, Interacoes=i))
      }
      
      if(funcao(a)*funcao(c) > 0){
        a <- c
      } else {
        b <- c
      }
      
    }
    plot(ggplot(data.frame(x = c(-5, 5)), aes(x = x)) +
           stat_function(fun = funcao) +
           geom_point(data=auxiliar1, aes(x,y)) +
           geom_vline(xintercept = 0, col='red4', alpha=0.25) +
           geom_hline(yintercept = 0, col='red4', alpha=0.25) +
           geom_vline(xintercept = c(aa,bb), col=c('dodgerblue4','red4')) +
           geom_vline(xintercept = c(anteriores), alpha=0.4) +
           geom_vline(xintercept = c(a), col=c('forestgreen')) +
           labs(title=paste0('Aproximação da raíz: \nInteração ', i,'.'), y='f(x)') +
           annotate("label", label=paste0('Intervalo: [',round(a,3),' ; ',round(b,3),']'), x=3, y=5) + theme_minimal() +
           annotate("label", label='Azul & Vermelho | Pontos Iniciais', x=3, y=4) +
           annotate("label", label='Cinza | Pontos Percorridos', x=3, y=3) +
           annotate("label", label='Verde | Ponto final', x=3, y=2) +
            ylim(c(-5,5)))
    
    return(list(Intervalo=c(a,b), Interacoes=i))
  }

  output$bissecao <- renderPlot(raiz(input$interb[1], input$interb[2], as.numeric(input$errotol), function(x) x*cos(x)))
  
}

shinyApp(ui, server)
