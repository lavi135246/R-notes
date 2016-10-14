library(changepoint)


#check_tab <- readLines("E:\\airbox-May.csv")
air_data <- read.table("E:\\airbox-May.csv", header = FALSE)
#names(air_data) <- c("date-time", "ID", "PM25", "T", "Humid", "Coor_x", "Coor_y")
colnames(air_data) <- c("date_time", "ID", "PM25", "T", "Humid", "Coor_x", "Coor_y")

air_data$date_d <- sapply(air_data$date_time, substr, start = 1, stop = 10)
air_data$time_d <- sapply(air_data$date_time, substr, start = 12, stop = 19)

sensor_ID <- unique(air_data$ID)
#data_of_sensor <- which(air_data$ID %in% sensor_ID[1])
for(i in 1:length(sensor_ID)){
	PM25_of_sensor <- subset(air_data, ID == sensor_ID[i], select = PM25)
	if( length(PM25_of_sensor$PM25) < 10 ) next
	#is.data.frame(PM25_of_sensor)
	#PM25_of_sensor <- as.numeric(unlist(PM25_of_sensor)) 可以把他變成一個普通的vector
	#data.matrix(PM25_of_sensor)
	ansmeanvar=cpt.meanvar(PM25_of_sensor$PM25)
	plot(ansmeanvar)
	pic_name <- paste0("E:\\myGraph\\p", i,".png")
	dev.copy(png,pic_name)
	dev.off()
	cat ("Press [enter] to continue")
	line <- readline()
}