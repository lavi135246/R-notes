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
					  choices = select_id,
					  selected = "28C2DDDD41C0"
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
				    choices = c( "AMOC" = "AMOC", "PELT" = "PELT", "SegNeigh" = "SegNeigh", "BinSeg" = "BinSeg", "Best" = "Best"),
				    selected = "Best"
			    )
		  ),
		  column(4,
		        numericInput(inputId = "Bin_t",
		          label = "Change times (only for SegNeigh and Best)",
		          value = 7,
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
	         PELT = {ifelse(length( cpts( cpt.meanvar(plot_fig()$PM25, method = "PELT") ) ) == 0, NA, list( cpts( cpt.meanvar(plot_fig()$PM25, method = "PELT") ) ))},
	         SegNeigh = {
				      temp <- cpt.meanvar(plot_fig()$PM25,penalty="Asymptotic",pen.value=0.01,method="SegNeigh",Q = input$Bin_t ,class=FALSE)
				      ifelse(length(temp) == 0, NA, list(temp))},
	         BinSeg = {
				      temp <- cpt.meanvar(plot_fig()$PM25,penalty="Manual",pen.value="4*log(n)",method="BinSeg",Q = input$Bin_t,class=FALSE)
				      ifelse(length(temp) == 0, NA, list(temp))},
    			 Best = {
    			   temp <- cpt.meanvar(plot_fig()$PM25, pen.value = c(4,1500), penalty = "CROPS", method = "PELT")
    			   crop_matrix <- cpts.full(temp)
    			   crop_index <-crop_matrix[ nrow(crop_matrix) - input$Bin_t,]
    			   list(crop_index <- crop_index[!is.na(crop_index)])
    			 }
	  )
	})
	#----------------- output -------------------#
	output$act_days <- renderPrint({
	  if(input$submit){
  	    if(input$ch_method == "AMOC"){
  	        list(
      	        total_data_count = length(PM25_of_sensor()$PM25),
      	        covered_Days  = unique(as.Date(PM25_of_sensor()$f_time, tz = "Etc/GMT+8")),
      	        chtime = plot_fig()$f_time[ change_time() ]
  	        )
  	    }else if(input$ch_method == "PELT" || input$ch_method == "SegNeigh" || input$ch_method == "BinSeg" || input$ch_method == "Best"){
  	      list(
  	        total_data_count = length(PM25_of_sensor()$PM25),
  	        covered_Days  = unique(as.Date(PM25_of_sensor()$f_time, tz = "Etc/GMT+8")),
  	        chtime = plot_fig()$f_time[ change_time()[[1]] ]
  	      )
  	    }
  	}else{
	    list(
	      total_data_count = length(PM25_of_sensor()$PM25),
	      covered_Days  = unique(as.Date(PM25_of_sensor()$f_time, tz = "Etc/GMT+8"))
	    )
	    
	  }
	})
	#----------------------- Output Graph -------------------------#
	output$PM25_Graph <- renderPlot({
	  p <- ggplot(plot_fig(),
		       aes(x = as.POSIXct(f_time, tz = "Etc/GMT+8"), y = PM25)) + 
	          geom_line()
	  if(input$submit){
	    if(input$ch_method == "AMOC"){
	          p <- p + geom_vline(xintercept = as.numeric(plot_fig()$f_time[change_time()]), colour = "red")
	    }else if(input$ch_method == "PELT" || input$ch_method == "BinSeg" || input$ch_method == "SegNeigh"){
	       p <- p + geom_vline(xintercept = as.numeric(plot_fig()$f_time[ change_time()[[1]] ]), colour = "red")
	    }else if(input$ch_method == "Best" ){
	      j <- 1
	      cpt_vec <- vector(mode="numeric", length=0)
	      cpt_vec_fill <-  vector(mode="numeric", length=0)
	      today_time <- as.POSIXct(plot_fig()$f_time, tz = "Etc/GMT+8")
	      
	      for(i in change_time()[[1]]){
	        cpt_vec[j:i-1] <- mean(plot_fig()$PM25[j:i-1])
	        cpt_vec[i-1] <- NA
	        
	        j <- i+1
	        #print(cpt_vec)
	      }
	      i <- length( plot_fig()$PM25 )
	      cpt_vec[j:i] <- mean(plot_fig()$PM25[j:i])
	      p <- p + geom_line(aes(today_time, cpt_vec,na.rm = TRUE), colour = "red")
	      #p <- p + geom_vline(xintercept = as.numeric(plot_fig()$f_time[ change_time()[[1]] ]), colour = "blue")
	    }
	 }
	  print(p)
	  
	 #plot(as.POSIXct(plot_fig()$f_time, tz = "Etc/GMT+8"), plot_fig()$PM25,type = "l")
		           
	})
}

shinyApp(ui = ui, server = server)