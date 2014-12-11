library(shiny)

# Define UI for application that plots random distributions 

shinyUI(pageWithSidebar(
  
  
  # Application title
  headerPanel("Forest Fire and Air Quality Visualization"),
  
  # Sidebar with a slider input for number of observations
  sidebarPanel(
    sliderInput("obs", 
                "Animation:", (2001+1/12),2012,(2001+1/12),step=(1/12),
                animate=animationOptions(interval=300,loop=F),format="####")
  ,
  selectInput("county", "Air Quality Timeseries for:",
              list("Basin" = "basinmavg", 
                   "San Joaquin" = "X6077", 
                   "Stanislaus" = "X6099",
                   "Merced"="X6047",
                   "Madera"="X6039",
                   "Fresno"="X6019",
                   "Kings"="X6029",
                   "Tulare"="X6107",
                   "Kern"="X6029"))
  ),
  
  mainPanel(
    plotOutput("timePlot"),
    plotOutput("countyPlot")
  )
))
