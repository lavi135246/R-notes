library(changepoint)
library(jsonlite)
library("rjson")


file_names = list.files(path = "E:\\project\\20160517_testfile_device_id", pattern = NULL, all.files = FALSE, full.names = FALSE, recursive = FALSE, ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)

i = 1
id = vector()
device_cpt = vector()
gps_lat = vector()
gps_lon = vector()
for (json_file in file_names){
	#-----------------$device_id $gps_lat $gps_lon $s_d0(pm2.5值) $time------------------#
	my_path = paste0("E:\\project\\20160517_testfile_device_id\\", json_file)
	json_data <- fromJSON(file=my_path)
	if(length(json_data$s_d0) < 30) next
	#-------------change in mean and variance--------------#
	ansmeanvar=cpt.meanvar(json_data$s_d0)

	ch_id <- cpts(ansmeanvar) #回傳編號
	ch_time <- json_data$time[ch_id]

	#print(ch_time)
	
	id[i] = json_data$device_id
	gps_lat[i] = json_data$gps_lat
	gps_lon[i] = json_data$gps_lon
	device_cpt[i] = ch_time
	i = i+1
	
	#pic
	if(is.na(ch_id)) {
			print(i)
			next
		}
	zz_b = 1:ch_id
	xx_b = json_data$time[zz_b]
	yy_b = rep(mean(json_data$s_d0[zz_b]), times = length(zz_b))
	zz_f = (ch_id+1):length(json_data$s_d0)
	xx_f = json_data$time[zz_f]
	yy_f = rep(mean(json_data$s_d0[zz_f]), times = length(zz_f))
	plot(as.POSIXct(strptime(json_data$time, "%H:%M:%S")), json_data$s_d0 ,type = "l", xlab = "PM2.5", ylab = "Time")
	lines(xx_b,yy_b,col="red")
	lines(xx_f,yy_f,col="red")
	
	#curve(x=sensor_of_day$PM25[ch_id],as.numeric(sensor_of_day$f_time[1]), as.numeric(sensor_of_day$f_time[ch_id]))
		
	pic_name <- paste0("E:\\LASS_Graph\\", i,".png")
	dev.copy(png,pic_name)
	dev.off()
}

device_order <- as.integer( as.POSIXct(strptime(device_cpt, "%H:%M:%S")) )
data_end <- data.frame(id, gps_lat, gps_lon, device_cpt, device_order)
write.table(data_end, file = "E:\\20160517_testfile_device_id\\test.CSV", sep = ",")