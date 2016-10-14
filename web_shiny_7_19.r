#Code and slides example: bit.ly/shiny-quickstart-2
library(shiny)
ui <- fluidPage(
	sliderInput(inputId = "num",
		label = "Choose a number",
		value = 25, min = 1, max = 100),
	plotOutput("hist_1")
)
server <- function(input, output){
	output$hist_1 <- renderPlot({ 
		title <-"random normal values"
		hist(rnorm(input$num), main = title)
	})
}
shinyApp(ui = ui, server = server)

####----------------- input method ----------------####
inputId = "字串"
label = "字串"

##----------- 1. slider ------------##
sliderInput(inputId = "num",
	label = "Choose a number",
	value = 25, min = 1, max = 100)

####----------------- output method ----------------####
same as above

##-------------------- 1. plotOutput ------------------------##
plotOutput("hist")

####-------------------- server --------------------####
server <- function(input, output){
	output$hist <- renderPlot({ 
		title <-"100 random normal values"
		hist(rnorm(100), main = title)
	})
}

####------------------- connection ----------------####
#Tell R studio to connect to the particular page
#The one below is incorrect we have to go to https://www.shinyapps.io/admin/#/tokens
#press show + show secret and copy to clip board
rsconnect::setAccountInfo(name='pm25testweb', #web name
                           token='DE5E3D4179A1C03BAC1E99445CFC02CA',
                           secret='<SECRET>') 
#then we can publish the web we designed on net

#actionButton increase by 1 when we click it