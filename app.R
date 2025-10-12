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
library(plotly)
library(gitlink)
library(DT)

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
        sliderInput("DELTG", label = HTML("&Delta;G (Millions):"), min = -1, max = 1, value = 1, step = 1),
        sliderInput("MPC1", label = HTML("MPC<sub>1</sub>:"), min = 0, max = 0.99, value = 0.50, step = 0.01),
        sliderInput("MPC2", label = HTML("MPC<sub>2</sub>:"), min = 0, max = 0.99, value = 0, step = 0.01),
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
        nav_panel("MPC Values Individually (Figure)", plotlyOutput("GPlot")),
        nav_panel("MPC Values Individually (Table)", DTOutput("GmultTable")),
        nav_panel("Interacting MPC Values", plotlyOutput("GAPlot")),
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
        sliderInput("DELTT", label = HTML("&Delta;T (millions):"), min = -1, max = 1, value = -1, step = 1),
        sliderInput("TMPC1", label = HTML("MPC<sub>1</sub>:"), min = 0, max = 0.99, value = 0.50, step = 0.01),
        sliderInput("TMPC2", label = HTML("MPC<sub>2</sub>:"), min = 0, max = 0.99, value = 0, step = 0.01),
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
        nav_panel("MPC Values Individually (Figure)", plotlyOutput("TPlot")),
        nav_panel("MPC Values Individually (Table)", DTOutput("TmultTable")),
        nav_panel("Interacting MPC Values", plotlyOutput("TAPlot")),
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
        sliderInput("BMPC2", label = HTML("MPC<sub>2</sub>:"), min = 0, max = 0.99, value = 0, step = 0.01),
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
        nav_panel("MPC Values Individually (Figure)", plotlyOutput("BmultPlot")),
        nav_panel("MPC Values Individually (Table)", DTOutput("BmultTable")),
        nav_panel("Interacting MPC Values", plotlyOutput("BaggPlot")),
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
  
##########################################################
  # Government Spending Results:
  reactive_MPC1  <- reactive({ input$MPC1 })
  reactive_MPC2  <- reactive({ input$MPC2 })
  reactive_DELTG <- reactive({ input$DELTG })
  reactive_P2    <- reactive({ input$P2 })
  
  output$GMULT1 <- renderText({
    glue::glue("{round(1/(1 - reactive_MPC1()),4)}")
  })
  
  output$GMULT2 <- renderText({ 
    glue::glue("{round(1/(1 - reactive_MPC2()),4)}") 
  })
  
  output$GPlot <- renderPlotly({
    
    MPC1 <- reactive_MPC1()
    MPC2 <- reactive_MPC2()
    DELTG <- reactive_DELTG()
    
    MULT1 <- 1 / (1 - MPC1)
    MULT2 <- 1 / (1 - MPC2)
    
    ROUNDS = 20
    RVEC = 1:ROUNDS
    
    G1 <- DELTG * MPC1^(0:(ROUNDS-1))
    G1sum <- cumsum(G1)
    G2 <- DELTG * MPC2^(0:(ROUNDS-1))
    G2sum <- cumsum(G2)
    
    DF <- data.frame(RVEC,G1,G1sum,G2,G2sum)
    
    fig <- plot_ly(DF, x = ~RVEC, y = ~G1sum, type = 'scatter', mode = 'lines+markers',
                   name = 'Cumulative Spending with MPC<sub>1</sub>',
                   line = list(color = 'blue', width = 2, dash = 'solid'),
                   marker = list(color = 'blue', size = 10),
                   hovertemplate = '<b>Cumulative Spending with MPC<sub>1</sub>:</b> %{y:$,.4f} million<extra></extra>')
    fig <- fig %>% add_trace(y = ~G1, mode = 'lines+markers',
                             name = 'Round Spending with MPC<sub>1</sub>',
                             line = list(color = 'grey', width = 1, dash = 'dash'),
                             marker = list(color = 'darkblue', size = 10),
                             hovertemplate = '<b>Round Spending with MPC<sub>1</sub>:</b> %{y:$,.4f} million<extra></extra>')
       
    if (MPC2 != 0) {
      fig <- fig %>% add_trace(y = ~G2sum, mode = 'lines+markers',
                               name = 'Cumulative Spending with MPC<sub>2</sub>',
                               line = list(color = 'red', width = 2, dash = 'solid'),
                               marker = list(color = 'red', size = 10),
                               hovertemplate = '<b>Cumulative Spending with MPC<sub>2</sub>:</b> %{y:$,.4f} million<extra></extra>')
      fig <- fig %>% add_trace(y = ~G2,  mode = 'lines+markers',
                               name = 'Round Spending with MPC<sub>1</sub>',
                               line = list(color = 'grey', width = 1, dash = 'dash'),
                               marker = list(color = 'darkred', size = 10),
                               hovertemplate = '<b>Round Spending with MPC<sub>2</sub>:</b> %{y:$,.4f} million<extra></extra>')
      fig <- fig %>%
        layout(shapes = list(
          list(
            type = "line",
            x0 = 0,
            x1 = 21,
            y0 = MULT2*DELTG,
            y1 = MULT2*DELTG,
            line = list(color = 'black', width = 1, dash = "dash")),
          list(
            type = "line",
            x0 = 0,
            x1 = 21,
            y0 = MULT1*DELTG,
            y1 = MULT1*DELTG,
            line = list(color = 'black', width = 1, dash = "dash"))
          ))
    }
    
    fig <- fig %>%
      layout(hovermode = 'x unified',
             paper_bgcolor='white', plot_bgcolor='lightgrey',
             title = list(text = "Cumulative and Individual Spending",
                          x = 0, xanchor = "left", xref = "paper"),
             xaxis = list(title = "Spending Round",
                          gridcolor = 'white',
                          showgrid = TRUE,
                          showline = FALSE,
                          showticklabels = TRUE,
                          tickcolor = 'lightgrey',
                          ticks = 'outside',
                          zeroline = FALSE,
                          tickvals = seq(1,20,1)),
             yaxis = list(title = "Dollars (millions)",
                          gridcolor = 'white',
                          showgrid = TRUE,
                          showline = FALSE,
                          showticklabels = TRUE,
                          tickcolor = 'lightgrey',
                          ticks = 'outside',
                          zeroline = TRUE),
             shapes = list(
                      list(type = "line",
                           x0 = 0, x1 = 21,
                           y0 = MULT1*DELTG,
                           y1 = MULT1*DELTG,
                           line = list(
                             color = 'black', width = 1, dash = "dash"))))
    
    fig <- fig %>% 
      config(modeBarButtonsToRemove = c('zoom2d', 'pan2d', 'resetScale2d', 'lasso2d','autoScale','select2d','zoomIn2d','zoomOut2d'))
    
  }) # end GPlot
  
  output$GmultTable <- renderDT({
  
    MPC1 <- reactive_MPC1()
    MPC2 <- reactive_MPC2()
    DELTG <- reactive_DELTG()
    
    MULT1 <- 1 / (1 - MPC1)
    MULT2 <- 1 / (1 - MPC2)
    
    ROUNDS = 20
    RVEC = 1:ROUNDS
    
    G1 <- DELTG * MPC1^(0:(ROUNDS-1))
    G1sum <- cumsum(G1)
    G2 <- DELTG * MPC2^(0:(ROUNDS-1))
    G2sum <- cumsum(G2)
    
  datatable(data.frame(round(G1,4),round(G1sum,4),round(G2,4),round(G2sum,4)),
            colnames = c("Round Spending with MPC\U2081", "Cumulated Spending with MPC\U2081", "Round Spending with MPC\U2082", "Cumulated Spending with MPC\U2082"))
  })
  
  output$GAPlot <- renderPlotly({
    
    MPC1 <- reactive_MPC1()
    MPC2 <- reactive_MPC2()
    P2 <-   reactive_P2()
    DELTG <- reactive_DELTG()
    
    ROUNDS = 20
    RVEC = 1:ROUNDS
    SIMS = 1000
    RESIND = NULL
    RESSUM = NULL
    
    for (j in 1:SIMS){
      
      G1 = DELTG
      G1sum = DELTG
      MPCZ = sample(c(MPC1,MPC2), ROUNDS-1, replace = TRUE, prob = c(100-P2,P2))
      
      for (i in 2:ROUNDS){
        G1 <- append(G1,G1[i-1] * MPCZ[i-1])
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
    
    fig <- plot_ly(DF, x = ~RVEC, y = ~U95path, type = 'scatter', mode = 'lines',
                   line = list(color = 'green'),
                   showlegend = FALSE, name = 'High 2014',
                   hovertemplate = '<b>90% of Outcomes, Upper Value:</b> %{y:$,.4f} million<extra></extra>')
    fig <- fig %>% add_trace(y = ~L5path, type = 'scatter', mode = 'lines',
                             fill = 'tonexty', fillcolor='rgba(0,255,0,0.4)', line = list(color = 'green'),
                             showlegend = FALSE, name = 'Low 2014',
                             hovertemplate = '<b>90% of Outcomes, Lower Value:</b> %{y:$,.4f} million<extra></extra>')
    fig <- fig %>% add_trace(y = ~Meanpath, mode = 'lines+markers',
                             line = list(color = 'black', width = 2, dash = 'solid'),
                             marker = list(color = 'black', size = 10),
                             hovertemplate = '<b>Average Outcome of Simulations:</b> %{y:$,.4f} million<extra></extra>')
    fig <- fig %>%
      layout(hovermode = 'x unified',
             paper_bgcolor='white', plot_bgcolor='lightgrey',
             title = list(text = "Cumulative Spending (from 1000 Simulations)",
                          x = 0, xanchor = "left", xref = "paper"),
             xaxis = list(title = "Spending Round",
                          gridcolor = 'white',
                          showgrid = TRUE,
                          showline = FALSE,
                          showticklabels = TRUE,
                          tickcolor = 'lightgrey',
                          ticks = 'outside',
                          zeroline = FALSE,
                          tickvals = seq(1,20,1)),
             yaxis = list(title = "Dollars (millions)",
                          gridcolor = 'white',
                          showgrid = TRUE,
                          showline = FALSE,
                          showticklabels = TRUE,
                          tickcolor = 'lightgrey',
                          ticks = 'outside',
                          zeroline = TRUE))
    
    fig <- fig %>% 
      config(modeBarButtonsToRemove = c('zoom2d', 'pan2d', 'resetScale2d', 'lasso2d','autoScale','select2d','zoomIn2d','zoomOut2d'))
    
  }) # end GAPlot
  
  
######################################################
  # Taxation Results:
  reactive_TMPC1  <- reactive({ input$TMPC1 })
  reactive_TMPC2  <- reactive({ input$TMPC2 })
  reactive_DELTT  <- reactive({ input$DELTT })
  reactive_TP2    <- reactive({ input$TP2 })
  
  output$TMULT1 <- renderText({ 
    glue::glue("{round(-reactive_TMPC1()/(1 - reactive_TMPC1()),4)}")
  })
  
  output$TMULT2 <- renderText({ 
    glue::glue("{round(-reactive_TMPC2()/(1 - reactive_TMPC2()),4)}") 
  })
  
  output$TPlot <- renderPlotly({
    
    TMPC1 <- reactive_TMPC1()
    TMPC2 <- reactive_TMPC2()
    DELTT <- reactive_DELTT()
    
    TMULT1 <- -TMPC1 / (1 - TMPC1)
    TMULT2 <- -TMPC2 / (1 - TMPC2)
    
    ROUNDS = 20
    RVEC = 1:ROUNDS
    
    T1 <- -DELTT * TMPC1^(1:(ROUNDS))
    T1sum <- cumsum(T1)
    T2 <- -DELTT * TMPC2^(1:(ROUNDS))
    T2sum <- cumsum(T2)
    
    DF <- data.frame(RVEC,T1,T1sum,T2,T2sum)
    
    fig <- plot_ly(DF, x = ~RVEC, y = ~T1sum, type = 'scatter', mode = 'lines+markers',
                   name = 'Cumulative Spending with MPC<sub>1</sub>',
                   line = list(color = 'blue', width = 2, dash = 'solid'),
                   marker = list(color = 'blue', size = 10),
                   hovertemplate = '<b>Cumulative Spending with MPC<sub>1</sub>:</b> %{y:$,.4f} million<extra></extra>')
    fig <- fig %>% add_trace(y = ~T1, mode = 'lines+markers',
                             name = 'Round Spending with MPC<sub>1</sub>',
                             line = list(color = 'grey', width = 1, dash = 'dash'),
                             marker = list(color = 'darkblue', size = 10),
                             hovertemplate = '<b>Round Spending with MPC<sub>1</sub>:</b> %{y:$,.4f} million<extra></extra>')
    
    if (TMPC2 != 0) {
      fig <- fig %>% add_trace(y = ~T2sum, mode = 'lines+markers',
                               name = 'Cumulative Spending with MPC<sub>2</sub>',
                               line = list(color = 'red', width = 2, dash = 'solid'),
                               marker = list(color = 'red', size = 10),
                               hovertemplate = '<b>Cumulative Spending with MPC<sub>2</sub>:</b> %{y:$,.4f} million<extra></extra>')
      fig <- fig %>% add_trace(y = ~T2,  mode = 'lines+markers',
                               name = 'Round Spending with MPC<sub>1</sub>',
                               line = list(color = 'grey', width = 1, dash = 'dash'),
                               marker = list(color = 'darkred', size = 10),
                               hovertemplate = '<b>Round Spending with MPC<sub>2</sub>:</b> %{y:$,.4f} million<extra></extra>')
      fig <- fig %>%
        layout(shapes = list(
          list(
            type = "line",
            x0 = 0,
            x1 = 21,
            y0 = TMULT2*DELTT,
            y1 = TMULT2*DELTT,
            line = list(color = 'black', width = 1, dash = "dash")),
          list(
            type = "line",
            x0 = 0,
            x1 = 21,
            y0 = TMULT1*DELTT,
            y1 = TMULT1*DELTT,
            line = list(color = 'black', width = 1, dash = "dash"))
        ))
    }
    
    fig <- fig %>%
      layout(hovermode = 'x unified',
             paper_bgcolor='white', plot_bgcolor='lightgrey',
             title = list(text = "Cumulative and Individual Spending",
                          x = 0, xanchor = "left", xref = "paper"),
             xaxis = list(title = "Spending Round",
                          gridcolor = 'white',
                          showgrid = TRUE,
                          showline = FALSE,
                          showticklabels = TRUE,
                          tickcolor = 'lightgrey',
                          ticks = 'outside',
                          zeroline = FALSE,
                          tickvals = seq(1,20,1)),
             yaxis = list(title = "Dollars (millions)",
                          gridcolor = 'white',
                          showgrid = TRUE,
                          showline = FALSE,
                          showticklabels = TRUE,
                          tickcolor = 'lightgrey',
                          ticks = 'outside',
                          zeroline = TRUE),
             shapes = list(
               list(type = "line",
                    x0 = 0, x1 = 21,
                    y0 = TMULT1*DELTT,
                    y1 = TMULT1*DELTT,
                    line = list(
                      color = 'black', width = 1, dash = "dash"))))
    
    fig <- fig %>% 
      config(modeBarButtonsToRemove = c('zoom2d', 'pan2d', 'resetScale2d', 'lasso2d','autoScale','select2d','zoomIn2d','zoomOut2d'))
    
    
  }) # end Tplot
  
  output$TmultTable <- renderDT({
    
    TMPC1 <- reactive_TMPC1()
    TMPC2 <- reactive_TMPC2()
    DELTT <- reactive_DELTT()
    
    TMULT1 <- -TMPC1 / (1 - TMPC1)
    TMULT2 <- -TMPC2 / (1 - TMPC2)
    
    ROUNDS = 100
    RVEC = 1:ROUNDS
    
    T1 <- -DELTT * TMPC1^(1:(ROUNDS))
    T1sum <- cumsum(T1)
    T2 <- -DELTT * TMPC2^(1:(ROUNDS))
    T2sum <- cumsum(T2)
    
    datatable(data.frame(round(T1,4),round(T1sum,4),round(T2,4),round(T2sum,4)),
              colnames = c("Round Spending with MPC\U2081", "Cumulated Spending with MPC\U2081", "Round Spending with MPC\U2082", "Cumulated Spending with MPC\U2082"))
    
  })
  
  output$TAPlot <- renderPlotly({
    
    TMPC1 <- reactive_TMPC1()
    TMPC2 <- reactive_TMPC2()
    TP2 <-   reactive_TP2()
    DELTT <- reactive_DELTT()
    
    ROUNDS = 20
    RVEC = 1:ROUNDS
    SIMS = 1000
    RESIND = NULL
    RESSUM = NULL
    
    for (j in 1:SIMS){
      
      TMPCZ = sample(c(TMPC1,TMPC2), ROUNDS, replace = TRUE, prob = c(100-TP2,TP2))
      T1 = -DELTT * TMPCZ[1]
      T1sum = T1
      
      for (i in 2:ROUNDS){
        
        T1 <- append(T1,T1[i-1] * TMPCZ[i])
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
    
    fig <- plot_ly(DF, x = ~RVEC, y = ~U95path, type = 'scatter', mode = 'lines',
                   line = list(color = 'green'),
                   showlegend = FALSE, name = 'High 2014',
                   hovertemplate = '<b>90% of Outcomes, Upper Value:</b> %{y:$,.4f} million<extra></extra>')
    fig <- fig %>% add_trace(y = ~L5path, type = 'scatter', mode = 'lines',
                             fill = 'tonexty', fillcolor='rgba(0,255,0,0.4)', line = list(color = 'green'),
                             showlegend = FALSE, name = 'Low 2014',
                             hovertemplate = '<b>90% of Outcomes, Lower Value:</b> %{y:$,.4f} million<extra></extra>')
    fig <- fig %>% add_trace(y = ~Meanpath, mode = 'lines+markers',
                             line = list(color = 'black', width = 2, dash = 'solid'),
                             marker = list(color = 'black', size = 10),
                             hovertemplate = '<b>Average Outcome of Simulations:</b> %{y:$,.4f} million<extra></extra>')
    fig <- fig %>%
      layout(hovermode = 'x unified',
             paper_bgcolor='white', plot_bgcolor='lightgrey',
             title = list(text = "Cumulative Spending (from 1000 Simulations)",
                          x = 0, xanchor = "left", xref = "paper"),
             xaxis = list(title = "Spending Round",
                          gridcolor = 'white',
                          showgrid = TRUE,
                          showline = FALSE,
                          showticklabels = TRUE,
                          tickcolor = 'lightgrey',
                          ticks = 'outside',
                          zeroline = FALSE,
                          tickvals = seq(1,20,1)),
             yaxis = list(title = "Dollars (millions)",
                          gridcolor = 'white',
                          showgrid = TRUE,
                          showline = FALSE,
                          showticklabels = TRUE,
                          tickcolor = 'lightgrey',
                          ticks = 'outside',
                          zeroline = TRUE))
    
    fig <- fig %>% 
      config(modeBarButtonsToRemove = c('zoom2d', 'pan2d', 'resetScale2d', 'lasso2d','autoScale','select2d','zoomIn2d','zoomOut2d'))
    
    
  }) # end TAPlot

####################################################
  # Both Results
  reactive_BMPC1  <- reactive({ input$BMPC1 })
  reactive_BMPC2  <- reactive({ input$BMPC2 })
  reactive_BDELTG  <- reactive({ input$BDELTG })
  reactive_BDELTT  <- reactive({ input$BDELTT })
  reactive_BP2    <- reactive({ input$BP2 })
  
  output$BGMULT1 <- renderText({ 
    glue::glue("{round(1/(1 - reactive_BMPC1()),4)}")
  })
  
  output$BTMULT1 <- renderText({ 
    glue::glue("{round(-reactive_BMPC1()/(1 - reactive_BMPC1()),4)}")
  })
  
  output$BGMULT2 <- renderText({ 
    glue::glue("{round(1/(1 - reactive_BMPC2()),2)}") 
  })
  
  output$BTMULT2 <- renderText({ 
    glue::glue("{round(-reactive_BMPC2()/(1 - reactive_BMPC2()),2)}") 
  })
  
  output$BmultPlot <- renderPlotly({
    
    BMPC1 <- reactive_BMPC1()
    BMPC2 <- reactive_BMPC2()
    BDELTG <- reactive_BDELTG()
    BDELTT <- reactive_BDELTT()
    
    BTOT1 <- 1 / (1 - BMPC1) * BDELTG - BMPC1 / (1 - BMPC1) * BDELTT
    B1i = BDELTG - BMPC1 * BDELTT
    
    BTOT2 <- 1 / (1 - BMPC2) * BDELTG - BMPC2 / (1 - BMPC2) * BDELTT
    B2i = BDELTG - BMPC2 * BDELTT
    
    ROUNDS = 20
    RVEC = 1:ROUNDS
    
    B1 <- B1i * BMPC1^(0:(ROUNDS-1))
    B1sum <- cumsum(B1)
    B2 <- B2i * BMPC2^(0:(ROUNDS-1))
    B2sum <- cumsum(B2)
    
    DF <- data.frame(RVEC,B1,B1sum,B2,B2sum)
    
    fig <- plot_ly(DF, x = ~RVEC, y = ~B1sum, type = 'scatter', mode = 'lines+markers',
                   name = 'Cumulative Spending with MPC<sub>1</sub>',
                   line = list(color = 'blue', width = 2, dash = 'solid'),
                   marker = list(color = 'blue', size = 10),
                   hovertemplate = '<b>Cumulative Spending with MPC<sub>1</sub>:</b> %{y:$,.4f} million<extra></extra>')
    fig <- fig %>% add_trace(y = ~B1, mode = 'lines+markers',
                             name = 'Round Spending with MPC<sub>1</sub>',
                             line = list(color = 'grey', width = 1, dash = 'dash'),
                             marker = list(color = 'darkblue', size = 10),
                             hovertemplate = '<b>Round Spending with MPC<sub>1</sub>:</b> %{y:$,.4f} million<extra></extra>')
    
    if (BMPC2 != 0) {
      fig <- fig %>% add_trace(y = ~B2sum, mode = 'lines+markers',
                               name = 'Cumulative Spending with MPC<sub>2</sub>',
                               line = list(color = 'red', width = 2, dash = 'solid'),
                               marker = list(color = 'red', size = 10),
                               hovertemplate = '<b>Cumulative Spending with MPC<sub>2</sub>:</b> %{y:$,.4f} million<extra></extra>')
      fig <- fig %>% add_trace(y = ~B2,  mode = 'lines+markers',
                               name = 'Round Spending with MPC<sub>1</sub>',
                               line = list(color = 'grey', width = 1, dash = 'dash'),
                               marker = list(color = 'darkred', size = 10),
                               hovertemplate = '<b>Round Spending with MPC<sub>2</sub>:</b> %{y:$,.4f} million<extra></extra>')
      fig <- fig %>%
        layout(shapes = list(
          list(
            type = "line",
            x0 = 0,
            x1 = 21,
            y0 = BTOT2,
            y1 = BTOT2,
            line = list(color = 'black', width = 1, dash = "dash")),
          list(
            type = "line",
            x0 = 0,
            x1 = 21,
            y0 = BTOT1,
            y1 = BTOT1,
            line = list(color = 'black', width = 1, dash = "dash"))
        ))
    }
    
    fig <- fig %>%
      layout(hovermode = 'x unified',
             paper_bgcolor='white', plot_bgcolor='lightgrey',
             title = list(text = "Cumulative and Individual Spending",
                          x = 0, xanchor = "left", xref = "paper"),
             xaxis = list(title = "Spending Round",
                          gridcolor = 'white',
                          showgrid = TRUE,
                          showline = FALSE,
                          showticklabels = TRUE,
                          tickcolor = 'lightgrey',
                          ticks = 'outside',
                          zeroline = FALSE,
                          tickvals = seq(1,20,1)),
             yaxis = list(title = "Dollars (millions)",
                          gridcolor = 'white',
                          showgrid = TRUE,
                          showline = FALSE,
                          showticklabels = TRUE,
                          tickcolor = 'lightgrey',
                          ticks = 'outside',
                          zeroline = TRUE),
             shapes = list(
               list(type = "line",
                    x0 = 0, x1 = 21,
                    y0 = BTOT1,
                    y1 = BTOT1,
                    line = list(
                      color = 'black', width = 1, dash = "dash"))))
    
    fig <- fig %>% 
      config(modeBarButtonsToRemove = c('zoom2d', 'pan2d', 'resetScale2d', 'lasso2d','autoScale','select2d','zoomIn2d','zoomOut2d'))
    
  }) # end Bmultplot
  
  output$BmultTable <- renderDT({
    
    BMPC1 <- reactive_BMPC1()
    BMPC2 <- reactive_BMPC2()
    BDELTG <- reactive_BDELTG()
    BDELTT <- reactive_BDELTT()
    
    BTOT1 <- 1 / (1 - BMPC1) * BDELTG - BMPC1 / (1 - BMPC1) * BDELTT
    B1i = BDELTG - BMPC1 * BDELTT
    
    BTOT2 <- 1 / (1 - BMPC2) * BDELTG - BMPC2 / (1 - BMPC2) * BDELTT
    B2i = BDELTG - BMPC2 * BDELTT
    
    ROUNDS = 20
    RVEC = 1:ROUNDS
    
    B1 <- B1i * BMPC1^(0:(ROUNDS-1))
    B1sum <- cumsum(B1)
    B2 <- B2i * BMPC2^(0:(ROUNDS-1))
    B2sum <- cumsum(B2)
    
    datatable(data.frame(round(B1,4),round(B1sum,4),round(B2,4),round(B2sum,4)),
              colnames = c("Round Spending with MPC\U2081", "Cumulated Spending with MPC\U2081", "Round Spending with MPC\U2082", "Cumulated Spending with MPC\U2082"))
    
  })
  
  output$BaggPlot <- renderPlotly({
    
    BMPC1 <- reactive_BMPC1()
    BMPC2 <- reactive_BMPC2()
    BP2 <-   reactive_BP2()
    
    BDELTG <- reactive_BDELTG()
    BDELTT <- reactive_BDELTT()
    
    ROUNDS = 20
    RVEC = 1:ROUNDS
    
    SIMS = 1000
    RESIND = NULL
    RESSUM = NULL
    
    for (j in 1:SIMS){
      
      BMPCZ = sample(c(BMPC1,BMPC2), ROUNDS, replace = TRUE, prob = c(100-BP2,BP2))
      B1 = BDELTG - BMPCZ[1] * BDELTT
      B1sum = B1
      
      for (i in 2:ROUNDS){
        
        B1 <- append(B1,B1[i-1] * BMPCZ[i])
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
    fig <- plot_ly(DF, x = ~RVEC, y = ~U95path, type = 'scatter', mode = 'lines',
                   line = list(color = 'green'),
                   showlegend = FALSE, name = 'High 2014',
                   hovertemplate = '<b>90% of Outcomes, Upper Value:</b> %{y:$,.4f} million<extra></extra>')
    fig <- fig %>% add_trace(y = ~L5path, type = 'scatter', mode = 'lines',
                             fill = 'tonexty', fillcolor='rgba(0,255,0,0.4)', line = list(color = 'green'),
                             showlegend = FALSE, name = 'Low 2014',
                             hovertemplate = '<b>90% of Outcomes, Lower Value:</b> %{y:$,.4f} million<extra></extra>')
    fig <- fig %>% add_trace(y = ~Meanpath, mode = 'lines+markers',
                             line = list(color = 'black', width = 2, dash = 'solid'),
                             marker = list(color = 'black', size = 10),
                             hovertemplate = '<b>Average Outcome of Simulations:</b> %{y:$,.4f} million<extra></extra>')
    fig <- fig %>%
      layout(hovermode = 'x unified',
             paper_bgcolor='white', plot_bgcolor='lightgrey',
             title = list(text = "Cumulative Spending (from 1000 Simulations)",
                          x = 0, xanchor = "left", xref = "paper"),
             xaxis = list(title = "Spending Round",
                          gridcolor = 'white',
                          showgrid = TRUE,
                          showline = FALSE,
                          showticklabels = TRUE,
                          tickcolor = 'lightgrey',
                          ticks = 'outside',
                          zeroline = FALSE,
                          tickvals = seq(1,20,1)),
             yaxis = list(title = "Dollars (millions)",
                          gridcolor = 'white',
                          showgrid = TRUE,
                          showline = FALSE,
                          showticklabels = TRUE,
                          tickcolor = 'lightgrey',
                          ticks = 'outside',
                          zeroline = TRUE))
    
    fig <- fig %>% 
      config(modeBarButtonsToRemove = c('zoom2d', 'pan2d', 'resetScale2d', 'lasso2d','autoScale','select2d','zoomIn2d','zoomOut2d'))
    
    
  }) # end BaggPlot
  
}

# Run the application 
shinyApp(ui = ui, server = server)
