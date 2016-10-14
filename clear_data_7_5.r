library(changepoint)

air_data <- read.table("E:\\airbox-May.csv", header = FALSE)
colnames(air_data) <- c("date_time", "ID", "PM25", "T", "Humid", "Coor_x", "Coor_y")
air_data$f_time <- as.POSIXct(strptime(air_data$date_time, "%Y-%m-%dT%H:%M:%SZ"))

#找到對應的sensor
sensor_ID <- unique(air_data$ID)
PM25_of_sensor <- subset(air_data, ID == sensor_ID[1])
ansmeanvar = cpt.meanvar(PM25_of_sensor$PM25)
ch_id <- cpts(ansmeanvar)

#找某sensor的每一天
Day_ID <- unique(as.Date(PM25_of_sensor$f_time))
for(i in 1:length(Day_ID)){
	sensor_of_day <- PM25_of_sensor[ which( as.Date(PM25_of_sensor$f_time) == Day_ID[i] ), ]
	print(Day_ID[i])
	print(length(sensor_of_day$PM25))
	cat ("Press [enter] to continue")
	line <- readline()
	if( length(sensor_of_day$PM25) < 4 ) next
	ansmeanvar=cpt.meanvar(sensor_of_day$PM25)
	ch_id <- cpts(ansmeanvar) #回傳編號
	#plot(sensor_of_day$PM25, sensor_of_day$date_d)
	zz_b = 1:ch_id
	xx_b = sensor_of_day$f_time[zz_b]
	yy_b = rep(mean(sensor_of_day$PM25[zz_b]), times = length(zz_b))
	zz_f = (ch_id+1):length(sensor_of_day$PM25)
	xx_f = sensor_of_day$f_time[zz_f]
	yy_f = rep(mean(sensor_of_day$PM25[zz_f]), times = length(zz_f))
	plot(sensor_of_day$f_time, sensor_of_day$PM25,type = "o", xlab = "PM2.5", ylab = "Time")
	lines(xx_b,yy_b,col="red")
	lines(xx_f,yy_f,col="red")
	
	#curve(x=sensor_of_day$PM25[ch_id],as.numeric(sensor_of_day$f_time[1]), as.numeric(sensor_of_day$f_time[ch_id]))
	pic_name <- paste0("E:\\myGraph\\p", i,".png")
	dev.copy(png,pic_name)
	dev.off()
	#time_dif <- diff(time_of_d)
	#diff_in_dat <- as.numeric(as.Date(time_of_d[2]) - as.Date(time_of_d[1]))

	
}