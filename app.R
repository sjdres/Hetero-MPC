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
  
  theme = bs_theme(version = 5, bootswatch = "yeti"),
  
  title = "The Multiplier Effect: An Application",
  
  header = tags$style("
    .bslib-sidebar-layout .sidebar-content { 
      scrollbar-width: none;     /* Firefox */
      -ms-overflow-style: none;  /* IE and Edge */
    }
    .bslib-sidebar-layout .sidebar-content::-webkit-scrollbar { 
      display: none;             /* Chrome, Safari, Opera */
    }
  "),
  
  #########################################
  # Common Sidebar
  sidebar = sidebar(
    
    tags$h4(HTML("Simulation Criteria:")),
    layout_columns(
      column_width = c("50%", "50%"),
    numericInput("DELTG", label = HTML("&Delta;G (Billions):"), min = -1000, max = 1000, value = 1, step = 1),
    numericInput("DELTT", label = HTML("&Delta;T (Billions):"), min = -1000, max = 1000, value = 0, step = 1),
    ),
    
    sliderInput("MTR", label = HTML("MTR (%):"), min = 0, max = 100, value = 0, step = 1, width = 300),
    hr(),
    
    tags$h5(HTML("Type 1 Households:")),
    sliderInput("MPS1", label = HTML("MPS<sub>1</sub>:"), min = 0, max = 1, value = 0.50, step = 0.01, ticks = FALSE, width = 300),
    sliderInput("MPI1", label = HTML("MPI<sub>1</sub>:"), min = 0, max = 1, value = 0, step = 0.01, ticks = FALSE, width = 300),
    tags$h6(HTML("MPC<sub>1</sub>: 1 - MTR - MPS<sub>1</sub> - MPI<sub>1</sub> = "), uiOutput("MPC1t", inline = TRUE)),
    hr(),
    
    tags$h5(HTML("Type 2 Households (Optional):")),
    sliderInput("P2", label = HTML("% of Type-2 Population:"), min = 0, max = 100, value = 0, step = 5, width = 300),
    sliderInput("MPS2", label = HTML("MPS<sub>2</sub>:"), min = 0, max = 1, value = 0.50, step = 0.01, ticks = FALSE, width = 300),
    sliderInput("MPI2", label = HTML("MPI<sub>2</sub>:"), min = 0, max = 1, value = 0, step = 0.01, ticks = FALSE, width = 300),
    tags$h6(HTML("MPC<sub>2</sub>: 1 - MTR - MPS<sub>2</sub> - MPI<sub>2</sub> = "), uiOutput("MPC2t", inline = TRUE)),
    
    open = "always",
    resizeable = FALSE,
    width = 300
  ), # end sidebar
  
  
  ######################################################################
  # Instructions:
  nav_panel(
    title = 'Instructions',
    card(
      height = 700,
      card_header(class = "bg-dark","Instructions"),
      card_body(p("This app illustrates the broad range of potential impacts of spending and tax / transfer multipliers in an aggregate economy."),
                tags$h4(HTML("Step 1: Select the type(s) of Fiscal Policy Change(s) from the above menu:")),
                tags$ol(
                  tags$li(HTML("A change in Government Purchases (&Delta;G)")),
                  tags$li(HTML("A change in Taxation or Transfer Payments (&Delta;T)")),
                  tags$li(HTML("A change in both Government Purchases and Taxation or Transfer Payments (&Delta;G and &Delta;T)"))
                ),
                tags$h4(HTML("Step 2: Select the following Simulation Criteria:")),
                p("The default is that there is only 1 household type in the economy (Type 1)"),
                tags$ol(
                  tags$li(HTML("The size of the policy change (in millions of dollars)")),
                  tags$li(HTML("The marginal income tax rate: MTR")),
                  tags$li(HTML("The marginal propensity to save: MPS<sub>1</sub>")),
                  tags$li(HTML("The marginal propensity to import: MPI<sub>1</sub>")),
                  tags$li(HTML("The marginal propensity to consume (domestic goods & services) is given by: MPC<sub>1</sub> = 1 - MTR - MPS<sub>1</sub> - MPI<sub>1</sub>")),
                ),
                tags$h4(HTML("Step 3: (Optional) Allow for a second household type that can differ in their marginal propensities:")),
                tags$ol(
                  tags$li(HTML("Select the percentage of the population for the Type 2 households")),
                  tags$li(HTML("Select marginal propensities for the Type 2 household (assuming the same MTR across households)"))
                  ),
                tags$h4(HTML("Step 4: Explore the results of the simulation:")),
                tags$ol(
                  tags$li("The ", em("MPC Values Individually (Figure)"), " panel illustrates spending for each household type as well as the accumulated spending observed over several spending rounds."), 
                  tags$li("The ", em("MPC Values Individually (Table)"), " panel illustrates the same information as in the previous panel in table form."), 
                  tags$li(em("Value Boxes"),HTML(": report the size of the individual multipliers for each household type provided" )),
                  tags$li("The ", em("Interacting MPC Values"),HTML( " panel illustrates total spending for each round when the change in income is received by either a Type 2 household (with probability equal to the percentage of population) or a Type 1 household (otherwise). 
                    The simulation is run 1000 times. The black line illustrates the average spending across all simulations, while the purple band illustrates the range of spending paths from 90% of all observed simulation outcomes.")),
                ),
      ),
      card_footer("XXX and XXX (XXXX) [Link to Paper]"),
      
    )
  ), # end nav_panel (Instructions)
  ####################################################################
  # Government Spending:
  nav_panel(
    title = 'Government Spending',
    
    # main window cards
      
      navset_card_underline(
        full_screen = TRUE,
        
        title = tags$h4("Simulation Results"),
        nav_panel("MPC Values Individually (Figure)", plotlyOutput("GPlot")),
        nav_panel("MPC Values Individually (Table)", DTOutput("GmultTable")),
        nav_panel("Interacting MPC Values", plotlyOutput("GAPlot")),
        
        
       ),
      
      layout_column_wrap(
        value_box( 
          tags$h1(HTML("Multiplier with MPC<sub>1</sub>: <sup>1</sup>&frasl;<sub>(1 - MPC<sub>1</sub>)</sub>")),
          showcase = icon("sack-dollar"),
          value = uiOutput("GMULT1", inline = TRUE),
          theme = "bg-gradient-blue-red"
        ), 
        value_box( 
          tags$h1(HTML("Multiplier with MPC<sub>2</sub>: <sup>1</sup>&frasl;<sub>(1 - MPC<sub>2</sub>)</sub>")), 
          showcase = icon("sack-dollar"), 
          value = uiOutput("GMULT2", inline = TRUE),
          theme = "bg-gradient-red-blue"
        ),
        fill = FALSE,
        width = "300px",
        height = "100px"
      ),
      
  ), # end nav_panel (Government Spending)
  
  #nav_panel(
  #  title = 'Taxation',
  #  ), # end nav_panel (Taxation)
  
  #nav_panel(
  #  title = 'Both',
  #  ), # end nav_panel (Both)
  
  
  

) # end page_navbar

# Define server 
server <- function(input, output, session) {
  
##########################################################
  # Government Spending Results:
  
  # update sliders to ensure that MPC1 + MPS1 + MTR + MPI1 = 1
  # Observe a change in MTR...
  observeEvent(input$MTR, {
    MPC1tmp <- 1 - sum(c(input$MTR/100, input$MPI1, input$MPS1))
    new_max <- 1 - input$MTR/100
    MPS1 <- input$MPS1
    if (MPC1tmp < 0) {
      MPS1 <- min(MPS1, new_max)
      updateSliderInput(session, "MPS1", value = MPS1, max = new_max)
      updateSliderInput(session, "MPI1", value = new_max-MPS1, max = new_max)
      output$MPC1t <- renderUI({0})
    } else {
      updateSliderInput(session, "MPS1", max = new_max)
      updateSliderInput(session, "MPI1", max = new_max)
      output$MPC1t <- renderUI({round(1-sum(c(input$MTR/100, input$MPI1, input$MPS1)),2)})
    }
    MPC2tmp <- 1 - sum(c(input$MTR/100, input$MPI2, input$MPS2))
    new_max <- 1 - input$MTR/100
    MPS2 <- input$MPS2
    if (MPC2tmp < 0) {
      MPS2 <- min(MPS2, new_max)
      updateSliderInput(session, "MPS2", value = MPS2, max = new_max)
      updateSliderInput(session, "MPI2", value = new_max-MPS2, max = new_max)
      output$MPC2t <- renderUI({0})
    } else {
      updateSliderInput(session, "MPS2", max = new_max)
      updateSliderInput(session, "MPI2", max = new_max)
      output$MPC2t <- renderUI({round(1-sum(c(input$MTR/100, input$MPI2, input$MPS2)),2)})
    }
  })
  # Observe a change in MPS
  observeEvent(input$MPS1, {
    MPC1tmp <- 1 - sum(c(input$MTR/100, input$MPI1, input$MPS1))
    if (MPC1tmp < 0) {
      MPI1 <- 1 - input$MTR/100 - input$MPS1
      updateSliderInput(session, "MPI1", value = MPI1)
      output$MPC1t <- renderUI({0})
    } else {
      output$MPC1t <- renderUI({round(1-sum(c(input$MTR/100, input$MPI1, input$MPS1)),2)})
    }
  })
  observeEvent(input$MPS2, {
    MPC2tmp <- 1 - sum(c(input$MTR/100, input$MPI2, input$MPS2))
    if (MPC2tmp < 0) {
      MPI2 <- 1 - input$MTR/100 - input$MPS2
      updateSliderInput(session, "MPI2", value = MPI2)
      output$MPC2t <- renderUI({0})
    } else {
      output$MPC2t <- renderUI({round(1-sum(c(input$MTR/100, input$MPI2, input$MPS2)),2)})
    }
  })
  # Observe a change in MPI
  observeEvent(input$MPI1, {
    MPC1tmp <- 1 - sum(c(input$MTR/100, input$MPI1, input$MPS1))
    if (MPC1tmp < 0) {
      MPS1 <- 1 - input$MTR/100 - input$MPI1
      updateSliderInput(session, "MPS1", value = MPS1)
      output$MPC1t <- renderUI({0})
    } else {
      output$MPC1t <- renderUI({round(1-sum(c(input$MTR/100, input$MPI1, input$MPS1)),2)})
    }
  })
  observeEvent(input$MPI2, {
    MPC2tmp <- 1 - sum(c(input$MTR/100, input$MPI2, input$MPS2))
    if (MPC2tmp < 0) {
      MPS2 <- 1 - input$MTR/100 - input$MPI2
      updateSliderInput(session, "MPS2", value = MPS2)
      output$MPC2t <- renderUI({0})
    } else {
      output$MPC2t <- renderUI({round(1-sum(c(input$MTR/100, input$MPI2, input$MPS2)),2)})
    }
  })
  
  reactive_MPC1  <- reactive({ 1-sum(c(input$MTR/100, input$MPI1, input$MPS1)) })
  reactive_MPC2  <- reactive({ 1-sum(c(input$MTR/100, input$MPI2, input$MPS2)) })
  reactive_DELTG <- reactive({ input$DELTG })
  reactive_P2    <- reactive({ input$P2 })
  
  output$GMULT1 <- renderText({
    glue::glue("{round(1/(1 - reactive_MPC1()),2)}")
  })
  
  output$GMULT2 <- renderText({ 
    glue::glue("{round(1/(1 - reactive_MPC2()),2)}") 
  })
  
  output$GPlot <- renderPlotly({
    
    MPC1 <- reactive_MPC1()
      MPC1 <- max(MPC1,0)
    MPC2 <- reactive_MPC2()
      MPC2 <- max(MPC2,0)
    DELTG <- reactive_DELTG()
    P2 <- reactive_P2()
    
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
                   hovertemplate = '<b>Cumulative Spending with MPC<sub>1</sub>:</b> %{y:$,.2f} million<extra></extra>')
    fig <- fig %>% add_trace(y = ~G1, mode = 'lines+markers',
                             name = 'Round Spending with MPC<sub>1</sub>',
                             line = list(color = 'grey', width = 1, dash = 'dash'),
                             marker = list(color = 'darkblue', size = 10),
                             hovertemplate = '<b>Round Spending with MPC<sub>1</sub>:</b> %{y:$,.2f} million<extra></extra>')
       
    if (P2 != 0) {
      fig <- fig %>% add_trace(y = ~G2sum, mode = 'lines+markers',
                               name = 'Cumulative Spending with MPC<sub>2</sub>',
                               line = list(color = 'red', width = 2, dash = 'solid'),
                               marker = list(color = 'red', size = 10),
                               hovertemplate = '<b>Cumulative Spending with MPC<sub>2</sub>:</b> %{y:$,.2f} million<extra></extra>')
      fig <- fig %>% add_trace(y = ~G2,  mode = 'lines+markers',
                               name = 'Round Spending with MPC<sub>1</sub>',
                               line = list(color = 'grey', width = 1, dash = 'dash'),
                               marker = list(color = 'darkred', size = 10),
                               hovertemplate = '<b>Round Spending with MPC<sub>2</sub>:</b> %{y:$,.2f} million<extra></extra>')
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
    
    ROUNDS = 100
    RVEC = 1:ROUNDS
    
    G1 <- DELTG * MPC1^(0:(ROUNDS-1))
    G1sum <- cumsum(G1)
    G2 <- DELTG * MPC2^(0:(ROUNDS-1))
    G2sum <- cumsum(G2)
    
  datatable(data.frame(round(G1,2),round(G1sum,2),round(G2,2),round(G2sum,2)),
            colnames = c("Round Spending with MPC\U2081", "Cumulated Spending with MPC\U2081", "Round Spending with MPC\U2082", "Cumulated Spending with MPC\U2082"))
  })
  
  output$GAPlot <- renderPlotly({
    
    MPC1 <- reactive_MPC1()
      MPC1 <- max(MPC1,0)
    MPC2 <- reactive_MPC2()
      MPC2 <- max(MPC2,0)
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
                   line = list(color = 'purple'),
                   showlegend = FALSE, name = 'High 2014',
                   hovertemplate = '<b>90% of Outcomes, Upper Value:</b> %{y:$,.4f} million<extra></extra>')
    fig <- fig %>% add_trace(y = ~L5path, type = 'scatter', mode = 'lines',
                             fill = 'tonexty', fillcolor='rgba(232,59,212,0.8)', line = list(color = 'purple'),
                             showlegend = FALSE, name = 'Low 2014',
                             hovertemplate = '<b>90% of Outcomes, Lower Value:</b> %{y:$,.2f} million<extra></extra>')
    fig <- fig %>% add_trace(y = ~Meanpath, mode = 'lines+markers',
                             line = list(color = 'black', width = 2, dash = 'solid'),
                             marker = list(color = 'black', size = 10),
                             hovertemplate = '<b>Average Outcome of Simulations:</b> %{y:$,.2f} million<extra></extra>')
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
    
    ROUNDS = 100
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
