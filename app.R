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

    accordion(
    accordion_panel(
    title = "Type 1 Households (Default):",
    sliderInput("MPS1", label = HTML("MPS<sub>1</sub>:"), min = 0, max = 1, value = 0.50, step = 0.01, ticks = FALSE, width = 300),
    sliderInput("MPI1", label = HTML("MPI<sub>1</sub>:"), min = 0, max = 1, value = 0, step = 0.01, ticks = FALSE, width = 300),
    tags$h6(HTML("MPC<sub>1</sub>: 1 - MTR - MPS<sub>1</sub> - MPI<sub>1</sub> = "), uiOutput("MPC1t", inline = TRUE)),
    ),
    hr(),
    accordion_panel(
    title = "Type 2 Households (Optional):",

    sliderInput("P2", label = HTML("% of Type-2 Population:"), min = 0, max = 100, value = 0, step = 5, width = 300),
    sliderInput("MPS2", label = HTML("MPS<sub>2</sub>:"), min = 0, max = 1, value = 0.50, step = 0.01, ticks = FALSE, width = 300),
    sliderInput("MPI2", label = HTML("MPI<sub>2</sub>:"), min = 0, max = 1, value = 0, step = 0.01, ticks = FALSE, width = 300),
    tags$h6(HTML("MPC<sub>2</sub>: 1 - MTR - MPS<sub>2</sub> - MPI<sub>2</sub> = "), uiOutput("MPC2t", inline = TRUE)),
    ),
    ),
    
    open = "always",
    resizeable = FALSE,
    width = 320

  ), # end sidebar
  
  
  ######################################################################
  # Instructions:
  nav_panel(
    title = 'Instructions',
    card(
      #height = 700,
      card_header(class = "bg-dark","Instructions"),
      card_body(p("This app illustrates the impacts of government spending and lump-sum tax / transfer payment multipliers in an aggregate economy."),
                p("The assumptions regarding the economic environment are as follows."),
                tags$ol(
                  tags$li(HTML("All prices are assumed to be constant.")),
                  tags$li(HTML("There are no supply-side constraints, so any change in quantity demanded can be satisfied by a change in production.")),
                  tags$li(HTML("There are no changes in the interest rate, so there are no crowding-out effects on private investment and no monetary-policy responses to fiscal policy changes.")),
                  tags$li(HTML("While the application allows for the purchase of imported goods and services, it is assumed that there are no exports (for simplicity).")),
                ),
                  p("To begin, select the following parameters using the menu on the left."),
                tags$h4(HTML("Step 1: Select the type(s) of Fiscal Policy:")),
                tags$ol(
                  tags$li(HTML("A change in Government Purchases of goods and services (&Delta;G)")),
                  tags$li(HTML("A change in Lump-Sum Taxation or Transfer Payments (&Delta;T)*")),
                  tags$li(HTML("The Marginal Income Tax Rate (MTR)")),
                  p(HTML("* Note that &Delta;T > 0 is interpretable as either a lump-sum tax <em>decrease</em> or a transfer payment <em>increase</em>.")),
                ),
                tags$h4(HTML("Step 2: Select the Type-1 Household Characteristics:")),
                p("The default is that there is only 1 household type in the economy (Type 1 Households)"),
                tags$ol(
                  tags$li(HTML("The marginal propensity to save: MPS<sub>1</sub>*")),
                  tags$li(HTML("The marginal propensity to import: MPI<sub>1</sub>*")),
                  tags$li(HTML("The marginal propensity to consume (only domestic goods & services) is given by: MPC<sub>1</sub> = 1 - MTR - MPS<sub>1</sub> - MPI<sub>1</sub>")),
                  p(HTML("* Note that the sliders can be moved by clicking on the button and either dragging with your mouse or using the arrow keys on the keyboard.")),
                  ),
                tags$h4(HTML("Step 3: Allow for a Second Household Type (Optional):")),
                tags$ol(
                  tags$li(HTML("Select the percentage of the population for the Type 2 households*")),
                  tags$li(HTML("The marginal propensity to save: MPS<sub>2</sub>*")),
                  tags$li(HTML("The marginal propensity to import: MPI<sub>2</sub>*")),
                  tags$li(HTML("The marginal propensity to consume (only domestic goods & services) is given by: MPC<sub>2</sub> = 1 - MTR - MPS<sub>2</sub> - MPI<sub>2</sub>")),
                  p(HTML("* Note that the sliders can be moved by clicking on the button and either dragging with your mouse or using the arrow keys on the keyboard.")),
                ),
                tags$h4(HTML("Step 4: Explore the Simulation Results:")),
                tags$ol(
                  tags$li("The ", em("Value Boxes")," at the bottom of the page report the size of the individual multipliers for each household type." ),
                  tags$li("The ", em("MPC Values Individually (Figure)"), " panel illustrates the cumulative and round spending for each household type. This is useful for comparing the results of two distinctly different economies."), 
                  tags$li("The ", em("MPC Values Individually (Table)"), " panel presents a table of the same information illustrated in the previous figure."), 
                  tags$li("The ", em("Interacting MPC Values"),HTML( " panel illustrates total spending for each round when the change in income is randomly received by either a Type 2 household (with probability equal to the percentage of population) or a Type 1 household (otherwise). 
                    The simulation is run 1000 times. The black line illustrates the average spending across all simulations, while the purple band illustrates the range of spending paths from 90% of all observed simulation outcomes.")),
                ),
                tags$h4(HTML("See the paper (linked below) for details on the derivation of the multipliers and detailed exercises.")),
      ),
      card_footer("XXX and XXX (XXXX) [Link to Paper]"),
      
    )
  ), # end nav_panel (Instructions)
  #######################################################################
  # Government Spending:
  nav_panel(
    title = 'Simulation Results',
    
    # main window cards
      
      navset_card_underline(
        full_screen = TRUE,
        
        title = tags$h4("Simulation Results  "),
        nav_panel("MPC Values Individually (Figure)", plotlyOutput("GPlot")),
        nav_panel("MPC Values Individually (Table)", DTOutput("GmultTable")),
        nav_panel("Interacting MPC Values", plotlyOutput("GAPlot")),
        
        
       ),
      
      layout_column_wrap(
        value_box( 
          tags$h1(HTML("Type 1 &Delta;G Multiplier:")),
          p(HTML("Equation: <sup>1</sup>&frasl;<sub>(1 - MPC<sub>1</sub>)</sub>")),
          value = uiOutput("GMULT1", inline = TRUE),
          theme = "bg-gradient-blue-red"
        ),
        value_box( 
          tags$h1(HTML("Type 1 &Delta;T Multiplier:")),
          p(HTML("Equation: <sup>MPC<sub>1</sub></sup>&frasl;<sub>(1 - MPC<sub>1</sub>)</sub>")),
          value = uiOutput("TMULT1F", inline = TRUE),
          theme = "bg-gradient-blue-red"
        ),
        value_box( 
          tags$h1(HTML("Type 2 &Delta;G Multiplier:")), 
          p(HTML("Equation: <sup>1</sup>&frasl;<sub>(1 - MPC<sub>2</sub>)</sub>")),
          value = uiOutput("GMULT2", inline = TRUE),
          theme = "bg-gradient-red-blue"
        ),
        value_box( 
          tags$h1(HTML("Type 2 &Delta;T Multiplier:")), 
          p(HTML("Equation: <sup>MPC<sub>2</sub></sup>&frasl;<sub>(1 - MPC<sub>2</sub>)</sub>")),
          value = uiOutput("TMULT2F", inline = TRUE),
          theme = "bg-gradient-red-blue"
        ),
        fill = FALSE,
        width = "150px",
        height = "100px"
      ),
      
  ), # end nav_panel

) # end page_navbar

# Define server 
server <- function(input, output, session) {
  
##########################################################
  # Simulation Spending Results:
  
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
  reactive_DELTT <- reactive({ input$DELTT })
  reactive_P2    <- reactive({ input$P2 })
  
  output$GMULT1 <- renderText({
    glue::glue("{round(1/(1 - reactive_MPC1()),2)}")
  })
  
  output$TMULT1F <- renderText({
    glue::glue("{round(reactive_MPC1()/(1 - reactive_MPC1()),2)}")
  })
  
  output$GMULT2 <- renderText({ 
    glue::glue("{round(1/(1 - reactive_MPC2()),2)}") 
  })
  
  output$TMULT2F <- renderText({
    glue::glue("{round(reactive_MPC2()/(1 - reactive_MPC2()),2)}")
  })
  
  output$GPlot <- renderPlotly({
    
    MPC1 <- reactive_MPC1()
      MPC1 <- max(MPC1,0)
    MPC2 <- reactive_MPC2()
      MPC2 <- max(MPC2,0)
    DELTG <- reactive_DELTG()
    DELTT <- reactive_DELTT()
    P2 <- reactive_P2()
    
    GMULT1 <- 1 / (1 - MPC1)
    GMULT2 <- 1 / (1 - MPC2)
    
    TMULT1 <- MPC1 / (1 - MPC1)
    TMULT2 <- MPC2 / (1 - MPC2)
    
    ROUNDS = 20
    RVEC = 1:ROUNDS
    
    G1 <- DELTG * MPC1^(0:(ROUNDS-1)) + DELTT *  MPC1^(1:ROUNDS)
    G1sum <- cumsum(G1)
    G2 <- DELTG * MPC2^(0:(ROUNDS-1)) + DELTT *  MPC2^(1:ROUNDS)
    G2sum <- cumsum(G2)
    
    BTOT1F <- DELTG * GMULT1 + DELTT * TMULT1
    BTOT2F <- DELTG * GMULT2 + DELTT * TMULT2
    
    DF <- data.frame(RVEC,G1,G1sum,G2,G2sum)
    
    fig <- plot_ly(DF, x = ~RVEC, y = ~G1sum, type = 'scatter', mode = 'lines+markers',
                   name = 'Cumulative Spending (Type 1)',
                   line = list(color = 'blue', width = 2, dash = 'solid'),
                   marker = list(color = 'blue', size = 10),
                   hovertemplate = '<b>Cumulative Spending (Type 1):</b> %{y:$,.2f} Billion<extra></extra>')
    fig <- fig %>% add_trace(y = ~G1, mode = 'lines+markers',
                             name = 'Round Spending (Type 1)',
                             line = list(color = 'grey', width = 1, dash = 'dash'),
                             marker = list(color = 'darkblue', size = 10),
                             hovertemplate = '<b>Round Spending (Type 1):</b> %{y:$,.2f} Billion<extra></extra>')
       
    if (P2 != 0) {
      fig <- fig %>% add_trace(y = ~G2sum, mode = 'lines+markers',
                               name = 'Cumulative Spending (Type 2)',
                               line = list(color = 'red', width = 2, dash = 'solid'),
                               marker = list(color = 'red', size = 10),
                               hovertemplate = '<b>Cumulative Spending (Type 2):</b> %{y:$,.2f} Billion<extra></extra>')
      fig <- fig %>% add_trace(y = ~G2,  mode = 'lines+markers',
                               name = 'Round Spending (Type 2)',
                               line = list(color = 'grey', width = 1, dash = 'dash'),
                               marker = list(color = 'darkred', size = 10),
                               hovertemplate = '<b>Round Spending (Type 2):</b> %{y:$,.2f} Billion<extra></extra>')
      fig <- fig %>%
        layout(shapes = list(
          list(
            type = "line",
            x0 = 0,
            x1 = 21,
            y0 = BTOT2F,
            y1 = BTOT2F,
            line = list(color = 'black', width = 1, dash = "dash")),
          list(
            type = "line",
            x0 = 0,
            x1 = 21,
            y0 = BTOT1F,
            y1 = BTOT1F,
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
             yaxis = list(title = "Dollars (Billions)",
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
                           y0 = BTOT1F,
                           y1 = BTOT1F,
                           line = list(
                             color = 'black', width = 1, dash = "dash"))))
    
    fig <- fig %>% 
      config(modeBarButtonsToRemove = c('zoom2d', 'pan2d', 'resetScale2d', 'lasso2d','autoScale','select2d','zoomIn2d','zoomOut2d'))
    
  }) # end GPlot
  
  output$GmultTable <- renderDT({
  
    MPC1 <- reactive_MPC1()
    MPC2 <- reactive_MPC2()
    DELTG <- reactive_DELTG()
    DELTT <- reactive_DELTT()
    
    GMULT1 <- 1 / (1 - MPC1)
    GMULT2 <- 1 / (1 - MPC2)
    TMULT1 <- MPC1 / (1 - MPC1)
    TMULT2 <- MPC2 / (1 - MPC2)
    
    ROUNDS = 100
    RVEC = 1:ROUNDS
    
    G1 <- DELTG * MPC1^(0:(ROUNDS-1)) + DELTT *  MPC1^(1:ROUNDS)
    G1sum <- cumsum(G1)
    G2 <- DELTG * MPC2^(0:(ROUNDS-1)) + DELTT *  MPC2^(1:ROUNDS)
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
    DELTT <- reactive_DELTT()
    
    ROUNDS = 20
    RVEC = 1:ROUNDS
    SIMS = 1000
    RESIND = NULL
    RESSUM = NULL
    
    for (j in 1:SIMS){
      
      MPCZ = sample(c(MPC1,MPC2), ROUNDS, replace = TRUE, prob = c(100-P2,P2))
      G1 = DELTG + MPCZ[1] * DELTT
      G1sum = G1
      
      for (i in 2:ROUNDS){
        G1 <- append(G1,G1[i-1] * MPCZ[i])
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
                   hovertemplate = '<b>90% of Outcomes, Upper Value:</b> %{y:$,.2f} Billion<extra></extra>')
    fig <- fig %>% add_trace(y = ~L5path, type = 'scatter', mode = 'lines',
                             fill = 'tonexty', fillcolor='rgba(232,59,212,0.8)', line = list(color = 'purple'),
                             showlegend = FALSE, name = 'Low 2014',
                             hovertemplate = '<b>90% of Outcomes, Lower Value:</b> %{y:$,.2f} Billion<extra></extra>')
    fig <- fig %>% add_trace(y = ~Meanpath, mode = 'lines+markers',
                             line = list(color = 'black', width = 2, dash = 'solid'),
                             marker = list(color = 'black', size = 10),
                             hovertemplate = '<b>Average Outcome of Simulations:</b> %{y:$,.2f} Billion<extra></extra>')
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
             yaxis = list(title = "Dollars (Billions)",
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
  
}

# Run the application 
shinyApp(ui = ui, server = server)
