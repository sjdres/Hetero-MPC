#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

# Load packages used by the app. Install when needed:
library(shiny)
library(bslib)
library(ggplot2)
library(gitlink)

ui <- page_navbar(
  
  theme = bs_theme(version = 5),
  
  title = "Spending Multipliers (Dressler and Reed, 2025)",
  
  nav_panel(
    title = 'Government Spending',
    page_sidebar(
      sidebar = sidebar(
        tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar, .js-irs-0 .irs-handle {background: gray}")),
        tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar, .js-irs-1 .irs-handle {background: blue}")),
        tags$style(HTML(".js-irs-2 .irs-single, .js-irs-2 .irs-bar-edge, .js-irs-2 .irs-bar, .js-irs-2 .irs-handle {background: red}")),
        tags$style(HTML(".js-irs-3 .irs-single, .js-irs-3 .irs-bar-edge, .js-irs-3 .irs-bar, .js-irs-3 .irs-handle {background: green}")),
        
        tags$h6(HTML("Enter Information for Simulation:")),
        sliderInput("DELTG", label = HTML("Choose &Delta;G (millions):"), min = -1, max = 1, value = 0, step = 0.5),
        sliderInput("MPC1", label = HTML("Choose MPC<sub>1</sub>:"), min = 0, max = 0.99, value = 0.50, step = 0.01),
        sliderInput("MPC2", label = HTML("Choose MPC<sub>2</sub>:"), min = 0, max = 0.99, value = 0.50, step = 0.01),
        sliderInput("P2", "% of Type 2 Agents:", min = 0, max = 100, value = 50, step = 5)
      ), # end sidebar
      
      # main window cards
      layout_column_wrap(
        value_box( 
          title = "Type 1 Agent Multiplier:",
          p(HTML("Equation: <sup>1</sup>&frasl;<sub>(1 - MPC<sub>1</sub>)</sub>")),
          showcase = icon("sack-dollar"),
          value = uiOutput("GMULT1", inline = TRUE),
          theme = "bg-gradient-blue-red"
        ), 
        value_box( 
          title = "Type 2 Agent Multiplier:", 
          p(HTML("Equation: <sup>1</sup>&frasl;<sub>(1 - MPC<sub>2</sub>)</sub>")),
          showcase = icon("sack-dollar"), 
          value = uiOutput("GMULT2", inline = TRUE),
          theme = "bg-gradient-red-blue"
        ), 
        fill = FALSE,
        width = '300px',
        min_height = '100px'
      ),
      
      navset_card_underline(
        title = "Simulation Results:",
        nav_panel("Agent Types Individually", plotOutput("GmultPlot")),
        nav_panel("Agent Types Combined", plotOutput("GaggPlot")),
        full_screen = TRUE
      ),
      
    ) # end page_sidebar
  ), # end nav_panel (Government Spending)
  
  nav_panel(
    title = 'Taxation'
    
  ),
  
  nav_panel(
    title = 'Both'
  ),
  
  nav_panel(
    title = 'Instructions'
  )
  
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Government Spending Results:
  output$GMULT1 <- renderText({ 
    glue::glue("{round(1/(1 - input$MPC1),2)}")
  })
  
  output$GMULT2 <- renderText({ 
    glue::glue("{round(1/(1 - input$MPC2),2)}") 
  })
  
  output$GmultPlot <- renderPlot({
    
    MULT1 <- 1 / (1 - input$MPC1)
    G1 = input$DELTG
    G1sum = input$DELTG
    
    MULT2 <- 1 / (1 - input$MPC2)
    G2 = input$DELTG
    G2sum = input$DELTG
    
    ROUNDS = 20
    RVEC = 1:ROUNDS
    
    for (i in 2:ROUNDS){
      G1 <- append(G1,G1[i-1] * input$MPC1)
      G1sum <- append(G1sum,sum(G1))
      G2 <- append(G2,G2[i-1] * input$MPC2)
      G2sum <- append(G2sum,sum(G2))
    }
    
    DF <- data.frame(RVEC,G1,G1sum,G2,G2sum)
    
    p1 <- ggplot(DF,aes(x=RVEC)) +
      geom_segment(x = 1, y = 0, xend = 1, yend = input$DELTG, color = "black") +
      geom_line(aes(y=G1sum, color = "Type 1 Running Total"),linewidth = 2) +
      geom_point(aes(y=G1sum, color = "Type 1 Running Total"),size = 4) +
      geom_point(aes(y=G1, color = "Type 1 Round Amount"), size = 4) +
      geom_line(aes(y=G1, color = "gray"),linetype = 2) +
      geom_hline(yintercept=MULT1*input$DELTG,linetype="dashed",color="black") +
      geom_line(aes(y=G2sum, color = "Type 2 Running Total"),linewidth = 2) +
      geom_point(aes(y=G2sum, color = "Type 2 Running Total"),size = 4) +
      geom_point(aes(y=G2, color = "Type 2 Round Amount"), size = 4) +
      geom_line(aes(y=G2, color = "gray"),linetype = 2) +
      geom_hline(yintercept=MULT2*input$DELTG,linetype="dashed",color="black") +
      labs(title = "Total and Individual Expenditures",
           x = "Expenditure Rounds", y = "Dollars (Millions)"
      ) + 
      scale_color_manual(breaks=c('Type 1 Running Total',
                                  'Type 1 Round Amount',
                                  'Type 2 Running Total',
                                  'Type 2 Round Amount'),
                         values=c('Type 1 Running Total' = "blue",
                                  'Type 1 Round Amount' = "blue",
                                  'Type 2 Running Total' = "red",
                                  'Type 2 Round Amount' = "red")
      ) + labs(color = "Expenditures") +
      scale_x_continuous(breaks = seq(0, 20, by = 1))
    
    p1 + theme_gray(base_size = 18)
    
  }) # end Gmultplot
  
  output$GaggPlot <- renderPlot({
    
    MPC1 <- input$MPC1
    MPC2 <- input$MPC2
    P2   <- input$P2
    ROUNDS = 20
    RVEC = 1:ROUNDS
    
    SIMS = 1000
    RESIND = NULL
    RESSUM = NULL
    MPCZ = rep(c(MPC1,MPC2),c(100-P2,P2))
    for (j in 1:SIMS){
      G1 = input$DELTG
      G1sum = input$DELTG
      for (i in 2:ROUNDS){
        G1 <- append(G1,G1[i-1] * MPCZ[[sample(1:length(MPCZ),1)]])
        G1sum <- append(G1sum,sum(G1))
      }
      RESIND = rbind(RESIND,G1)
      RESSUM = rbind(RESSUM,G1sum)
    }
    
    Meanpath <- colMeans(RESSUM)
    Maxpath <- apply(RESSUM, 2, function(x) max(x, na.rm = TRUE))
    U95path <- apply(RESSUM, 2, function(x) quantile(x, 0.95, na.rm = TRUE))
    Minpath <- apply(RESSUM, 2, function(x) min(x, na.rm = TRUE))
    L5path <- apply(RESSUM, 2, function(x) quantile(x, 0.05, na.rm = TRUE))
    
    DF <- data.frame(RVEC,Meanpath,U95path,L5path)
    p2 <- ggplot(DF,aes(x=RVEC)) + 
      geom_segment(x = 1, y = 0, xend = 1, yend = input$DELTG, color = "black") +
      geom_ribbon(aes(ymin = L5path,ymax = U95path,color = "90% of Simulations"),
                  fill="green", alpha=0.75) +
      geom_line(aes(y=Meanpath, color = "Average Result"), linewidth = 2) +
      geom_point(aes(y=Meanpath, color = "Average Result"),size = 4) +
      labs(title = "Total Expenditures (from 1000 simulations)",
           x = "Expenditure Rounds", y = "Dollars (Millions)"
      ) +
      scale_color_manual(breaks=c('Average Result',
                                  '90% of Simulations'),
                         values=c('Average Result' = 'black',
                                  '90% of Simulations' = 'green')
      )
    
    p2 + theme_gray(base_size = 18) + theme(legend.title = element_blank()) +
      scale_x_continuous(breaks = seq(0, 20, by = 1))
    
    
  }) # end GaggPlot
  
}

# Run the application 
shinyApp(ui = ui, server = server)
