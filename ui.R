
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinyjs)

ui <- fluidPage(
  
  titlePanel("Visualization & Data transformation "),
  helpText("Use the following options to transform & visualize your Flow Cytometry Data"),
  sidebarPanel(
    textInput("title", "Graph title label", "Biaxial plot"),
    textInput("x_label", "X-axis label", "PE :: 585_29[561]"),
    textInput("y_label", "Y-axis label", "APC :: 670_30[640]"),
    textInput("legend", "Legend title label", ""),
    sliderInput("plot_width", "Plot width (px)", 200, 2000, 500),
    sliderInput("plot_height", "Plot height (px)", 200, 2000, 500)
  ),
  mainPanel(
    tabsetPanel(
      id = "tabs",
      tabPanel(
        title = "Bi-axial plot",
        plotOutput(outputId = "biaxial"), width=7
      ),
      tabPanel(
        title = "x axis transformation",
        plotOutput(outputId = "distribution_x"),
        textInput(inputId = "breaks_x", 
                  label = "x-axis breaks", 
                  value = "-10, 0, 1e1, 1e2, 1e3, 1e4"),
        selectInput(inputId = "x_trans_type", 
                    label = "Transformation:",
                    choices = c("Linear" = "linear", 
                                "Logarithmic" = "log10",
                                "Biexponential" = "biexponential", 
                                "Logicle" = "logicle"), 
                    selected = "Linear"),
        sliderInput(inputId = "width_basis_x", 
                    label = "Width Basis", 
                    min = -2000, 
                    max = 0, 
                    value = -12.60),
        sliderInput(inputId = "neg_decades_x", 
                    label = "Extra negative decades",
                    min = 0, 
                    max = 10, 
                    value = 0, 
                    step = 0.01),
        sliderInput(inputId = "pos_decades_x", 
                    label = "Positive decades", 
                    min = 0,
                    max = 50,
                    value = 4.5,
                    step = 0.01),
      ),
      tabPanel(
        title = "y axis transformation",
        plotOutput(outputId = "distribution_y"),
        textInput(inputId = "breaks_y", 
                  label = "y-axis breaks", 
                  value = "-10, 0, 1e1, 1e2, 1e3, 1e4"),
        selectInput(inputId = "y_trans_type", 
                    label = "Transformation:",
                    choices = c("Linear" = "linear", 
                                "Logarithmic" = "log10",
                                "Biexponential" = "biexponential", 
                                "Logicle" = "logicle"), 
                    selected = "Linear"),
        sliderInput(inputId = "width_basis_y", 
                    label = "Width Basis", 
                    min = -2000, 
                    max = 0, 
                    value = -12.60),
        sliderInput(inputId = "neg_decades_y", 
                    label = "Extra negative decades",
                    min = 0, 
                    max = 10, 
                    value = 0, 
                    step = 0.01),
        sliderInput(inputId = "pos_decades_y", 
                    label = "Positive decades", 
                    min = 0,
                    max = 50,
                    value = 4.6,
                    step = 0.01),
      ),
      tabPanel(
        "Misc",
        checkboxInput("labs", "Apply labels", 0),
        checkboxInput("wrap", "Wrap panel grid", 0),
        checkboxInput("fixed", "Axes equal for panels", 0),
        checkboxInput("space", "Equal FCS space", 0),
      )
    )
  )
)


shinyUI(fluidPage(
  shinyjs::useShinyjs(),

  # Application title
  titlePanel("Tercen"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      shinyjs::hidden(sliderInput("bins",
                         "Number of bins:",
                         min = 1,
                         max = 50,
                         value = 1)),
      actionButton("saveSettingsBtn", "Save settings", disabled=FALSE)
    ),

    # Show a plot of the generated distribution
    mainPanel(
      h3(textOutput("mode")),
      plotOutput("distPlot"),
      actionButton("runBtn", "Run", disabled=TRUE),
      h5(textOutput("msg"))
    )
  )
))
