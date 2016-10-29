#rm(list= ls())
library(changepoint)
library(ggplot2)
library(dplyr)
#source("C:/Users/anthony/Desktop/my_project/census-app/helpers.R")
library(shiny)

air_data <- readRDS("C:/Users/anthony/Desktop/my_project/census-app/data/Air_box.rds")
sensor_ID <- unique(air_data$ID)
select_id <- as.character(sensor_ID)
names(select_id) = as.character(sensor_ID)

ui <- fluidPage(
	#input
	fluidRow(
		column(4,
			selectInput(inputId = "cur_sen_box",
					  label = "Sensor_ID",
					  choices = select_id
					  )
		  ),
		column(4,
			   textInput(inputId = "cur_sen_tex",
						 label = "Sensor_ID",
						 value = select_id[1]
						 )
		  
		),
		column(4,
			dateRangeInput(inputId = "date_range",
						  label = "Choose the range of datein",
						  start = "2016-05-01",
						  end = "2016-05-01",
						  min = "2016-05-01",
						  max = "2016-05-31",
						  format = "mm/dd/yy",
						  separator = " to "
						  )
		)
	),
	fluidRow(
		  column(4,
			    radioButtons(inputId = "ch_method",
				    label = "Method",
				    choices = c( "AMOC" = "AMOC", "PELT" = "PELT", "SegNeigh" = "SegNeigh", "BinSeg" = "BinSeg")
			    )
		  ),
		  column(4,
		        numericInput(inputId = "Bin_t",
		          label = "Change Times",
		          value = 1,
		          min = 1,
		          max = 31
		        ),
		        actionButton(inputId = "submit",
		                     label = "Cpt Detect"
		                     )
		  )
	),

	plotOutput("PM25_Graph"),
	verbatimTextOutput("act_days")
)

server <- function(input, output) {
  
	#------------------ reactive -----------------#
	PM25_of_sensor <- reactive({
		subset(air_data, ID == input$cur_sen_box)
    })
	plot_fig <- reactive({
	  
	  PM25_of_sensor()[
	    which((as.numeric(as.Date(PM25_of_sensor()$f_time, tz = "Etc/GMT+8")) >= as.numeric(input$date_range[1])
	          & as.numeric(as.Date(PM25_of_sensor()$f_time, tz = "Etc/GMT+8")) <= as.numeric(input$date_range[2])))
	    ,]
	})
	#----------------event reactive--------------------#
	change_time <- eventReactive( input$submit,{
	  switch(input$ch_method,
	         AMOC = {ifelse(length( cpts( cpt.meanvar(plot_fig()$PM25, method = "AMOC") ) ) == 0, NA, cpts( cpt.meanvar(plot_fig()$PM25, method = "AMOC") ) )},
	         PELT = {ifelse(length( cpts( cpt.meanvar(plot_fig()$PM25, method = "PELT") ) ) == 0, NA, cpts( cpt.meanvar(plot_fig()$PM25, method = "PELT") ) )},
	         SegNeigh = {
				ifelse(length( cpt.meanvar(PM25_of_sensor$PM25,penalty="Asymptotic",pen.value=0.01,method="SegNeigh",Q = input$Bin_t ,class=FALSE) ) == 0, NA, 
				cpt.meanvar(PM25_of_sensor$PM25,penalty="Asymptotic",pen.value=0.01,method="SegNeigh",Q = input$Bin_t ,class=FALSE) )},
	         BinSeg = {ifelse(length( cpt.meanvar(PM25_of_sensor$PM25,penalty="Manual",pen.value="4*log(n)",method="BinSeg",Q = input$Bin_t,class=FALSE) ) == 0, NA, 
				cpt.meanvar(PM25_of_sensor$PM25,penalty="Manual",pen.value="4*log(n)",method="BinSeg",Q = input$Bin_t,class=FALSE) )}
	  )
	})
	#----------------- output -------------------#
	output$act_days <- renderPrint({
	  if(input$submit){
	    list(
	      total_data_count = length(PM25_of_sensor()$PM25),
	      covered_Days  = unique(as.Date(PM25_of_sensor()$f_time, tz = "Etc/GMT+8")),
	      chtime = plot_fig()$f_time[ change_time() ]
	    )
	  }else{
	    list(
	      total_data_count = length(PM25_of_sensor()$PM25),
	      covered_Days  = unique(as.Date(PM25_of_sensor()$f_time, tz = "Etc/GMT+8"))
	    )
	    
	  }
	})
	output$PM25_Graph <- renderPlot({
	  p <- ggplot(plot_fig(),
		       aes(x = as.POSIXct(f_time, tz = "Etc/GMT+8"), y = PM25)) + 
	          geom_line()
	  if(input$submit){
	          p <- p + geom_vline(xintercept = as.numeric(plot_fig()$f_time[change_time()]), colour = "red")
	    }
	  print(p)
	  
	 #plot(as.POSIXct(plot_fig()$f_time, tz = "Etc/GMT+8"), plot_fig()$PM25,type = "l")
		           
	})
}

shinyApp(ui = ui, server = server)