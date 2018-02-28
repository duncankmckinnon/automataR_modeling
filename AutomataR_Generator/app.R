#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Cellular Automata Rule Generator"),
   sidebarLayout(
      sidebarPanel(
         numericInput(inputId = "rulenum",
                      label = "Rule Number",
                      value = 30,
                      min = 1,
                      max = 256),
         sliderInput(inputId = "generations",
                     label = "Number of Generations:",
                     min = 2,
                     max = 2001,
                     value = 51),
         actionButton(inputId = "runbutton", 
                      label = "Run")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("CA.Plot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$CA.Plot <- renderPlot({
      input$runbutton
      x <- isolate(cellular_automata(rule_num = input$rulenum, CA.cross_size = input$generations, show = F))
      image(1 - t(x), col = grey(seq(0, 1, length = 256)))
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

