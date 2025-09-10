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
  
  title = "Spending Multipliers",
  
  nav_panel(
    title = 'Government Spending',
    page_sidebar(
      sidebar = sidebar(
        tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar, .js-irs-0 .irs-handle {background: gray} .js-irs-0 .irs-handle {background: gray !important;} .js-irs-0 .irs-handle:active {background: gray !important;}")),
        tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar, .js-irs-1 .irs-handle {background: blue} .js-irs-1 .irs-handle {background: blue !important;} .js-irs-1 .irs-handle:active {background: blue !important;}")),
        tags$style(HTML(".js-irs-2 .irs-single, .js-irs-2 .irs-bar-edge, .js-irs-2 .irs-bar, .js-irs-2 .irs-handle {background: red} .js-irs-2 .irs-handle {background: red !important;} .js-irs-2 .irs-handle:active {background: red !important;}")),
        tags$style(HTML(".js-irs-3 .irs-single, .js-irs-3 .irs-bar-edge, .js-irs-3 .irs-bar, .js-irs-3 .irs-handle {background: green} .js-irs-3 .irs-handle {background: green !important;} .js-irs-3 .irs-handle:active {background: green !important;}")),
        
        tags$h6(HTML("Choose Simulation Criteria:")),
        sliderInput("DELTG", label = HTML("&Delta;G (Millions):"), min = -1, max = 1, value = 0, step = 0.5),
        sliderInput("MPC1", label = HTML("MPC<sub>1</sub>:"), min = 0, max = 0.99, value = 0.50, step = 0.01),
        sliderInput("MPC2", label = HTML("MPC<sub>2</sub>:"), min = 0, max = 0.99, value = 0.50, step = 0.01),
        sliderInput("P2", label = HTML("% of Population with MPC<sub>2</sub>:"), min = 0, max = 100, value = 50, step = 5),
        p("(Dressler and Reed, 2025)")
      ), # end sidebar
      
      # main window cards
      layout_column_wrap(
        value_box( 
          tags$h1(HTML("Government Spending Multiplier with MPC<sub>1</sub>:")),
          tags$h6(HTML("Equation: <sup>1</sup>&frasl;<sub>(1 - MPC<sub>1</sub>)</sub>")),
          showcase = icon("sack-dollar"),
          value = uiOutput("GMULT1", inline = TRUE),
          theme = "bg-gradient-blue-red"
        ), 
        value_box( 
          tags$h1(HTML("Government Spending Multiplier with MPC<sub>2</sub>:")), 
          tags$h6(HTML("Equation: <sup>1</sup>&frasl;<sub>(1 - MPC<sub>2</sub>)</sub>")),
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
        nav_panel("MPC Values Individually", plotOutput("GmultPlot")),
        nav_panel("Interacting MPC Values", plotOutput("GaggPlot")),
        full_screen = TRUE
      ),
      
    ) # end page_sidebar
  ), # end nav_panel (Government Spending)
  
  nav_panel(
    title = 'Taxation',
    page_sidebar(
      sidebar = sidebar(
        tags$style(HTML(".js-irs-4 .irs-single, .js-irs-4 .irs-bar-edge, .js-irs-4 .irs-bar, .js-irs-4 .irs-handle {background: gray} .js-irs-4 .irs-handle {background: gray !important;} .js-irs-4 .irs-handle:active {background: gray !important;}")),
        tags$style(HTML(".js-irs-5 .irs-single, .js-irs-5 .irs-bar-edge, .js-irs-5 .irs-bar, .js-irs-5 .irs-handle {background: blue} .js-irs-5 .irs-handle {background: blue !important;} .js-irs-5 .irs-handle:active {background: blue !important;}")),
        tags$style(HTML(".js-irs-6 .irs-single, .js-irs-6 .irs-bar-edge, .js-irs-6 .irs-bar, .js-irs-6 .irs-handle {background: red} .js-irs-6 .irs-handle {background: red !important;} .js-irs-6 .irs-handle:active {background: red !important;}")),
        tags$style(HTML(".js-irs-7 .irs-single, .js-irs-7 .irs-bar-edge, .js-irs-7 .irs-bar, .js-irs-7 .irs-handle {background: green} .js-irs-7 .irs-handle {background: green !important;} .js-irs-7 .irs-handle:active {background: green !important;}")),
        
        tags$h6(HTML("Choose Simulation Criteria:")),
        sliderInput("DELTT", label = HTML("&Delta;T (millions):"), min = -1, max = 1, value = 0, step = 0.5),
        sliderInput("TMPC1", label = HTML("MPC<sub>1</sub>:"), min = 0, max = 0.99, value = 0.50, step = 0.01),
        sliderInput("TMPC2", label = HTML("MPC<sub>2</sub>:"), min = 0, max = 0.99, value = 0.50, step = 0.01),
        sliderInput("TP2", label = HTML("% of Population with MPC<sub>2</sub>:"), min = 0, max = 100, value = 50, step = 5),
        p("(Dressler and Reed, 2025)")
      ), # end sidebar
      
      # main window cards
      layout_column_wrap(
        value_box( 
          tags$h1(HTML("Tax Multiplier with MPC<sub>1</sub>:")),
          tags$h6(HTML("Equation: <sup>-MPC<sub>1</sub></sup>&frasl;<sub>(1 - MPC<sub>1</sub>)</sub>")),
          showcase = icon("sack-dollar"),
          value = uiOutput("TMULT1", inline = TRUE),
          theme = "bg-gradient-blue-red"
        ), 
        value_box( 
          tags$h1(HTML("Tax Multiplier with MPC<sub>2</sub>:")), 
          tags$h6(HTML("Equation: <sup>-MPC<sub>2</sub></sup>&frasl;<sub>(1 - MPC<sub>2</sub>)</sub>")),
          showcase = icon("sack-dollar"), 
          value = uiOutput("TMULT2", inline = TRUE),
          theme = "bg-gradient-red-blue"
        ), 
        fill = FALSE,
        width = '300px',
        min_height = '100px'
      ),
      
      navset_card_underline(
        title = "Simulation Results:",
        nav_panel("MPC Values Individually", plotOutput("TmultPlot")),
        nav_panel("Interacting MPC Values", plotOutput("TaggPlot")),
        full_screen = TRUE
      ),
      
    ) # end page_sidebar
  ),
  
  nav_panel(
    title = 'Both',
    page_sidebar(
      sidebar = sidebar(
        tags$style(HTML(".js-irs-8 .irs-single, .js-irs-8 .irs-bar-edge, .js-irs-8 .irs-bar, .js-irs-8 .irs-handle {background: gray} .js-irs-8 .irs-handle {background: gray !important;} .js-irs-8 .irs-handle:active {background: gray !important;}")),
        tags$style(HTML(".js-irs-9 .irs-single, .js-irs-9 .irs-bar-edge, .js-irs-9 .irs-bar, .js-irs-9 .irs-handle {background: gray} .js-irs-9 .irs-handle {background: gray !important;} .js-irs-9 .irs-handle:active {background: gray !important;}")),
        tags$style(HTML(".js-irs-10 .irs-single, .js-irs-10 .irs-bar-edge, .js-irs-10 .irs-bar, .js-irs-10 .irs-handle {background: blue} .js-irs-10 .irs-handle {background: blue !important;} .js-irs-10 .irs-handle:active {background: blue !important;}")),
        tags$style(HTML(".js-irs-11 .irs-single, .js-irs-11 .irs-bar-edge, .js-irs-11 .irs-bar, .js-irs-11 .irs-handle {background: red} .js-irs-11 .irs-handle {background: red !important;} .js-irs-11 .irs-handle:active {background: red !important;}")),
        tags$style(HTML(".js-irs-12 .irs-single, .js-irs-12 .irs-bar-edge, .js-irs-12 .irs-bar, .js-irs-12 .irs-handle {background: green} .js-irs-12 .irs-handle {background: green !important;} .js-irs-12 .irs-handle:active {background: green !important;}")),
        
        tags$h6(HTML("Choose Simulation Criteria:")),
        sliderInput("BDELTG", label = HTML("&Delta;G (millions):"), min = -1, max = 1, value = 0, step = 0.5),
        sliderInput("BDELTT", label = HTML("&Delta;T (millions):"), min = -1, max = 1, value = 0, step = 0.5),
        sliderInput("BMPC1", label = HTML("MPC<sub>1</sub>:"), min = 0, max = 0.99, value = 0.50, step = 0.01),
        sliderInput("BMPC2", label = HTML("MPC<sub>2</sub>:"), min = 0, max = 0.99, value = 0.50, step = 0.01),
        sliderInput("BP2", label = HTML("% of Population with MPC<sub>2</sub>:"), min = 0, max = 100, value = 50, step = 5),
        p("(Dressler and Reed, 2025)")
      ), # end sidebar
      
      # main window cards
      layout_column_wrap(
        value_box( 
          tags$h1(HTML("GS Multiplier with MPC<sub>1</sub>:")),
          tags$h6(HTML("Equation: <sup>1</sup>&frasl;<sub>(1 - MPC<sub>1</sub>)</sub>")),
          value = uiOutput("BGMULT1", inline = TRUE),
          theme = "bg-gradient-blue-red"
        ), 
        value_box( 
          tags$h1(HTML("Tax Multiplier with MPC<sub>1</sub>:")),
          tags$h6(HTML("Equation: <sup>-MPC<sub>1</sub></sup>&frasl;<sub>(1 - MPC<sub>1</sub>)</sub>")),
          value = uiOutput("BTMULT1", inline = TRUE),
          theme = "bg-gradient-blue-red"
        ), 
        value_box( 
          tags$h1(HTML("GS Multiplier with MPC<sub>2</sub>:")), 
          tags$h6(HTML("Equation: <sup>1</sup>&frasl;<sub>(1 - MPC<sub>2</sub>)</sub>")),
          value = uiOutput("BGMULT2", inline = TRUE),
          theme = "bg-gradient-red-blue"
        ),
        value_box( 
          tags$h1(HTML("Tax Multiplier with MPC<sub>2</sub>:")), 
          tags$h6(HTML("Equation: <sup>-MPC<sub>2</sub></sup>&frasl;<sub>(1 - MPC<sub>2</sub>)</sub>")),
          value = uiOutput("BTMULT2", inline = TRUE),
          theme = "bg-gradient-red-blue"
        ),
        fill = FALSE,
        width = 1/4,
        min_height = '100px'
      ),
      
      navset_card_underline(
        title = "Simulation Results:",
        nav_panel("MPC Values Individually", plotOutput("BmultPlot")),
        nav_panel("Interacting MPC Values", plotOutput("BaggPlot")),
        full_screen = TRUE
      ),
      
    ) # end page_sidebar
  ),
  
  nav_panel(
    title = 'Instructions',
    card(
      card_header(class = "bg-dark","Instructions"),
      card_body("This app illustrates the impact of spending multipliers when an economy is populated with different marginal propensities to consume (MPCs).",
                tags$ol(
                  tags$li("Select the change in fiscal policy in the top panel: Government Spending, Taxation, or Both (at the same time)."),
                  tags$li(HTML("Select the size of the change (or changes): &Delta;G, &Delta;T, or &Delta;G and &Delta;T.")),
                  tags$li(HTML("Select the size of the marginal propensities to consume (MPC<sub>1</sub> and MPC<sub>2</sub>).")),
                  tags$li(HTML("Select the percentage of population that have MPC<sub>2</sub> (the remainder having MPC<sub>1</sub>)."))
                  ),
               p(em("Value Boxes"),HTML(": report the size of the multiplier for each marginal propensity to consume given (MPC<sub>1</sub> or MPC<sub>2</sub>)" )),
               p("The ", em("MPC Values Individually"), " panel illustrates spending for each MPC (detailed in step 3) as well as the accumulated spending observed over several spending rounds.")), 
               p("The ", em("Interacting MPC Values"),HTML( " panel illustrates total spending for each round when the change in income is received by a person in the economy with either MPC<sub>2</sub> (with probability given in step 4) or MPC<sub>1</sub> (otherwise). 
                    The simulation is run 1000 times. The black line illustrates the average spending across all simulations, while the green band illustrates the range of spending paths from 90% of all observed simulation outcomes.")
              ),
      card_footer("Dressler and Reed (2025)"),
      fill = FALSE
    )
  )
  
  
)

# Define server 
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
      geom_line(aes(y=G1sum, color = "Cumulative Spending with MPC1"),linewidth = 2) +
      geom_point(aes(y=G1sum, color = "Cumulative Spending with MPC1"),size = 4) +
      geom_point(aes(y=G1, color = "Round Spending with MPC1"), size = 4) +
      geom_line(aes(y=G1, color = "gray"),linetype = 2) +
      geom_hline(yintercept=MULT1*input$DELTG,linetype="dashed",color="black") +
      geom_line(aes(y=G2sum, color = "Cumulative Spending with MPC2"),linewidth = 2) +
      geom_point(aes(y=G2sum, color = "Cumulative Spending with MPC2"),size = 4) +
      geom_point(aes(y=G2, color = "Round Spending with MPC2"), size = 4) +
      geom_line(aes(y=G2, color = "gray"),linetype = 2) +
      geom_hline(yintercept=MULT2*input$DELTG,linetype="dashed",color="black") +
      labs(title = "Total and Individual Spending",
           x = "Spending Rounds", y = "Dollars (Millions)"
      ) + 
      scale_color_manual(breaks=c('Cumulative Spending with MPC1',
                                  'Round Spending with MPC1',
                                  'Cumulative Spending with MPC2',
                                  'Round Spending with MPC2'),
                         labels = c(bquote("Cumulative Spending with"~MPC[1]),
                                    bquote("Round Spending with"~MPC[1]),
                                    bquote("Cumulative Spending with"~MPC[2]),
                                    bquote("Round Spending with"~MPC[2])),
                         values=c('Cumulative Spending with MPC1' = "blue",
                                  'Round Spending with MPC1' = "darkblue",
                                  'Cumulative Spending with MPC2' = "red",
                                  'Round Spending with MPC2' = "darkred")
      ) + labs(color = "Spending") +
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
      labs(title = "Total Spending (from 1000 simulations)",
           x = "Spending Rounds", y = "Dollars (Millions)"
      ) +
      scale_color_manual(breaks=c('Average Result',
                                  '90% of Simulations'),
                         values=c('Average Result' = 'black',
                                  '90% of Simulations' = 'green')
      )
    
    p2 + theme_gray(base_size = 18) + theme(legend.title = element_blank()) +
      scale_x_continuous(breaks = seq(0, 20, by = 1))
    
    
  }) # end GaggPlot
  
  # Taxation Results:
  output$TMULT1 <- renderText({ 
    glue::glue("{round(-input$TMPC1/(1 - input$TMPC1),2)}")
  })
  
  output$TMULT2 <- renderText({ 
    glue::glue("{round(-input$TMPC2/(1 - input$TMPC2),2)}") 
  })
  
  output$TmultPlot <- renderPlot({
    
    TMULT1 <- -input$TMPC1 / (1 - input$TMPC1)
    T1 = -input$DELTT * input$TMPC1
    T1sum = -input$DELTT * input$TMPC1
    
    TMULT2 <- -input$TMPC2 / (1 - input$TMPC2)
    T2 = -input$DELTT * input$TMPC2
    T2sum = -input$DELTT * input$TMPC2
    
    ROUNDS = 20
    RVEC = 1:ROUNDS
    
    for (i in 2:ROUNDS){
      T1 <- append(T1,T1[i-1] * input$TMPC1)
      T1sum <- append(T1sum,sum(T1))
      T2 <- append(T2,T2[i-1] * input$TMPC2)
      T2sum <- append(T2sum,sum(T2))
    }
    
    DF <- data.frame(RVEC,T1,T1sum,T2,T2sum)
    
    p1 <- ggplot(DF,aes(x=RVEC)) +
      geom_segment(x = 1, y = 0, xend = 1, yend = max(abs(T1[1]),abs(T2[1]))*sign(T1[1]), color = "black") +
      geom_line(aes(y=T1sum, color = "Cumulative Spending with MPC1"),linewidth = 2) +
      geom_point(aes(y=T1sum, color = "Cumulative Spending with MPC1"),size = 4) +
      geom_point(aes(y=T1, color = "Round Spending with MPC1"), size = 4) +
      geom_line(aes(y=T1, color = "gray"),linetype = 2) +
      geom_hline(yintercept=TMULT1*input$DELTT,linetype="dashed",color="black") +
      geom_line(aes(y=T2sum, color = "Cumulative Spending with MPC2"),linewidth = 2) +
      geom_point(aes(y=T2sum, color = "Cumulative Spending with MPC2"),size = 4) +
      geom_point(aes(y=T2, color = "Round Spending with MPC2"), size = 4) +
      geom_line(aes(y=T2, color = "gray"),linetype = 2) +
      geom_hline(yintercept=TMULT2*input$DELTT,linetype="dashed",color="black") +
      labs(title = "Total and Individual Spending",
           x = "Spending Rounds", y = "Dollars (Millions)"
      ) + 
      scale_color_manual(breaks=c('Cumulative Spending with MPC1',
                                  'Round Spending with MPC1',
                                  'Cumulative Spending with MPC2',
                                  'Round Spending with MPC2'),
                         labels = c(bquote("Cumulative Spending with"~MPC[1]),
                                    bquote("Round Spending with"~MPC[1]),
                                    bquote("Cumulative Spending with"~MPC[2]),
                                    bquote("Round Spending with"~MPC[2])),
                         values=c('Cumulative Spending with MPC1' = "blue",
                                  'Round Spending with MPC1' = "darkblue",
                                  'Cumulative Spending with MPC2' = "red",
                                  'Round Spending with MPC2' = "darkred")
      ) + labs(color = "Spending") +
      scale_x_continuous(breaks = seq(0, 20, by = 1))
    
    p1 + theme_gray(base_size = 18)
    
  }) # end Tmultplot
  
  output$TaggPlot <- renderPlot({
    
    TMPC1 <- input$TMPC1
    TMPC2 <- input$TMPC2
    TP2   <- input$TP2
    ROUNDS = 20
    RVEC = 1:ROUNDS
    
    SIMS = 1000
    RESIND = NULL
    RESSUM = NULL
    TMPCZ = rep(c(TMPC1,TMPC2),c(100-TP2,TP2))
    for (j in 1:SIMS){
      T1 = (1-TP2/100) * -input$DELTT * input$TMPC1 + TP2/100 * -input$DELTT * input$TMPC2
      T1sum = (1-TP2/100) * -input$DELTT * input$TMPC1 + TP2/100 * -input$DELTT * input$TMPC2
      for (i in 2:ROUNDS){
        T1 <- append(T1,T1[i-1] * TMPCZ[[sample(1:length(TMPCZ),1)]])
        T1sum <- append(T1sum,sum(T1))
      }
      RESIND = rbind(RESIND,T1)
      RESSUM = rbind(RESSUM,T1sum)
    }
    
    Meanpath <- colMeans(RESSUM)
    Maxpath <- apply(RESSUM, 2, function(x) max(x, na.rm = TRUE))
    U95path <- apply(RESSUM, 2, function(x) quantile(x, 0.95, na.rm = TRUE))
    Minpath <- apply(RESSUM, 2, function(x) min(x, na.rm = TRUE))
    L5path <- apply(RESSUM, 2, function(x) quantile(x, 0.05, na.rm = TRUE))
    
    DF <- data.frame(RVEC,Meanpath,U95path,L5path)
    p2 <- ggplot(DF,aes(x=RVEC)) + 
      geom_segment(x = 1, y = 0, xend = 1, yend = T1[1], color = "black") +
      geom_ribbon(aes(ymin = L5path,ymax = U95path,color = "90% of Simulations"),
                  fill="green", alpha=0.75) +
      geom_line(aes(y=Meanpath, color = "Average Result"), linewidth = 2) +
      geom_point(aes(y=Meanpath, color = "Average Result"),size = 4) +
      labs(title = "Total Spending (from 1000 simulations)",
           x = "Spending Rounds", y = "Dollars (Millions)"
      ) +
      scale_color_manual(breaks=c('Average Result',
                                  '90% of Simulations'),
                         values=c('Average Result' = 'black',
                                  '90% of Simulations' = 'green')
      )
    
    p2 + theme_gray(base_size = 18) + theme(legend.title = element_blank()) +
      scale_x_continuous(breaks = seq(0, 20, by = 1))
    
    
  }) # end TaggPlot

  # Both Results
  output$BGMULT1 <- renderText({ 
    glue::glue("{round(1/(1 - input$BMPC1),2)}")
  })
  
  output$BTMULT1 <- renderText({ 
    glue::glue("{round(-input$BMPC1/(1 - input$BMPC1),2)}")
  })
  
  output$BGMULT2 <- renderText({ 
    glue::glue("{round(1/(1 - input$BMPC2),2)}") 
  })
  
  output$BTMULT2 <- renderText({ 
    glue::glue("{round(-input$BMPC2/(1 - input$BMPC2),2)}") 
  })
  
  output$BmultPlot <- renderPlot({
    
    BTOT1 <- 1 / (1 - input$BMPC1) * input$BDELTG - input$BMPC1 / (1 - input$BMPC1) * input$BDELTT
    B1 = input$BDELTG - input$BMPC1 * input$BDELTT
    B1sum = B1
    
    BTOT2 <- 1 / (1 - input$BMPC2) * input$BDELTG - input$BMPC2 / (1 - input$BMPC2) * input$BDELTT
    B2 = input$BDELTG - input$BMPC2 * input$BDELTT
    B2sum = B2
    
    ROUNDS = 20
    RVEC = 1:ROUNDS
    
    for (i in 2:ROUNDS){
      B1 <- append(B1,B1[i-1] * input$BMPC1)
      B1sum <- append(B1sum,sum(B1))
      B2 <- append(B2,B2[i-1] * input$BMPC2)
      B2sum <- append(B2sum,sum(B2))
      }
    
    DF <- data.frame(RVEC,B1,B1sum,B2,B2sum)
    
    p1 <- ggplot(DF,aes(x=RVEC)) +
      geom_segment(x = 1, y = 0, xend = 1, yend = max(abs(B1[1]),abs(B2[1]))*sign(B1[1]), color = "black") +
      geom_line(aes(y=B1sum, color = "Cumulative Spending with MPC1"),linewidth = 2) +
      geom_point(aes(y=B1sum, color = "Cumulative Spending with MPC1"),size = 4) +
      geom_point(aes(y=B1, color = "Round Spending with MPC1"), size = 4) +
      geom_line(aes(y=B1, color = "gray"),linetype = 2) +
      geom_hline(yintercept=BTOT1,linetype="dashed",color="black") +
      geom_line(aes(y=B2sum, color = "Cumulative Spending with MPC2"),linewidth = 2) +
      geom_point(aes(y=B2sum, color = "Cumulative Spending with MPC2"),size = 4) +
      geom_point(aes(y=B2, color = "Round Spending with MPC2"), size = 4) +
      geom_line(aes(y=B2, color = "gray"),linetype = 2) +
      geom_hline(yintercept=BTOT2,linetype="dashed",color="black") +
      labs(title = "Total and Individual Spending",
           x = "Spending Rounds", y = "Dollars (Millions)"
      ) + 
      scale_color_manual(breaks=c('Cumulative Spending with MPC1',
                                  'Round Spending with MPC1',
                                  'Cumulative Spending with MPC2',
                                  'Round Spending with MPC2'),
                         labels = c(bquote("Cumulative Spending with"~MPC[1]),
                                    bquote("Round Spending with"~MPC[1]),
                                    bquote("Cumulative Spending with"~MPC[2]),
                                    bquote("Round Spending with"~MPC[2])),
                         values=c('Cumulative Spending with MPC1' = "blue",
                                  'Round Spending with MPC1' = "darkblue",
                                  'Cumulative Spending with MPC2' = "red",
                                  'Round Spending with MPC2' = "darkred")
      ) + labs(color = "Spending") +
      scale_x_continuous(breaks = seq(0, 20, by = 1))
    
    p1 + theme_gray(base_size = 18)
    
  }) # end Bmultplot
  
  output$BaggPlot <- renderPlot({
    
    BMPC1 <- input$BMPC1
    BMPC2 <- input$BMPC2
    BP2   <- input$BP2
    ROUNDS = 20
    RVEC = 1:ROUNDS
    
    SIMS = 1000
    RESIND = NULL
    RESSUM = NULL
    BMPCZ = rep(c(BMPC1,BMPC2),c(100-BP2,BP2))
    for (j in 1:SIMS){
      B1 = input$BDELTG + (1-BP2/100) * -input$BDELTT * input$BMPC1 + BP2/100 * -input$BDELTT * input$BMPC2
      B1sum = B1
      for (i in 2:ROUNDS){
        B1 <- append(B1,B1[i-1] * BMPCZ[[sample(1:length(BMPCZ),1)]])
        B1sum <- append(B1sum,sum(B1))
      }
      RESIND = rbind(RESIND,B1)
      RESSUM = rbind(RESSUM,B1sum)
    }
    
    Meanpath <- colMeans(RESSUM)
    Maxpath <- apply(RESSUM, 2, function(x) max(x, na.rm = TRUE))
    U95path <- apply(RESSUM, 2, function(x) quantile(x, 0.95, na.rm = TRUE))
    Minpath <- apply(RESSUM, 2, function(x) min(x, na.rm = TRUE))
    L5path <- apply(RESSUM, 2, function(x) quantile(x, 0.05, na.rm = TRUE))
    
    DF <- data.frame(RVEC,Meanpath,U95path,L5path)
    p2 <- ggplot(DF,aes(x=RVEC)) + 
      geom_segment(x = 1, y = 0, xend = 1, yend = B1[1], color = "black") +
      geom_ribbon(aes(ymin = L5path,ymax = U95path,color = "90% of Simulations"),
                  fill="green", alpha=0.75) +
      geom_line(aes(y=Meanpath, color = "Average Result"), linewidth = 2) +
      geom_point(aes(y=Meanpath, color = "Average Result"),size = 4) +
      labs(title = "Total Spending (from 1000 simulations)",
           x = "Spending Rounds", y = "Dollars (Millions)"
      ) +
      scale_color_manual(breaks=c('Average Result',
                                  '90% of Simulations'),
                         values=c('Average Result' = 'black',
                                  '90% of Simulations' = 'green')
      )
    
    p2 + theme_gray(base_size = 18) + theme(legend.title = element_blank()) +
      scale_x_continuous(breaks = seq(0, 20, by = 1))
    
    
  }) # end BaggPlot
  
}

# Run the application 
shinyApp(ui = ui, server = server)
