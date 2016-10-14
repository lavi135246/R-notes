library(changepoint)
library(dplyr)


air_data <- read.table("E:\\airbox-May.csv", header = FALSE)
colnames(air_data) <- c("date_time", "ID", "PM25", "T", "Humid", "Coor_x", "Coor_y")
air_data$f_time <- as.POSIXct(strptime(air_data$date_time, "%Y-%m-%dT%H:%M:%SZ",tz = "Etc/GMT+8"))


sensor_ID <- unique(air_data$ID)
for(j in 1:101){ #應該要到length(sensor_ID)
  if( j == 6 || j == 7 || j == 8 || j == 11 || j == 15 || j == 19 || j == 24 ||
        j == 25 || j == 27 || j == 28 || j == 29 || j == 32 || j == 43 || j == 51 ||
        j == 52 || j == 53 || j == 55 || j == 57 || j == 62 || j == 66 || j == 70 ||
        j == 71 || j == 76 || j == 80 || j == 84 || j == 86 || j == 92 || j == 94 ||
        j == 97 || j == 99
      ) next
  if( j == 82 ) next
	PM25_of_sensor <- subset(air_data, ID == sensor_ID[j]) 

	del_index_arr = group_by(PM25_of_sensor, as.Date(f_time, tz = "Etc/GMT+8")) %>%
	summarise( length(PM25) > 5 )

	Day_ID <- unique(as.Date(PM25_of_sensor$f_time, tz = "Etc/GMT+8"))
	del_index = which(as.data.frame(del_index_arr[,2])[,1] %in% 0)

	if(length(del_index) != 0) PM25_of_sensor <- PM25_of_sensor[ -which( Day_ID[del_index] == as.Date(PM25_of_sensor$f_time, tz = "Etc/GMT+8") ),]
	#get Day_ID after deleting the ones with length shorter than 4
	Day_ID <- unique(as.Date(PM25_of_sensor$f_time, tz = "Etc/GMT+8"))
	
	ch_id_group = group_by(PM25_of_sensor, as.Date(f_time, tz = "Etc/GMT+8")) %>%
	summarise(cpts(cpt.meanvar(PM25)))

	ch_id = as.data.frame(ch_id_group[,2])[,1]
	
	if( length(ch_id) == 0 ) next
	for(i in 1:length(ch_id)){
	    #if( j == 5 && i == 11 ) break 
		if(is.na(ch_id[i])) {
			print(i)
			next
		}
		sensor_of_day <- PM25_of_sensor[ which( as.Date(PM25_of_sensor$f_time, tz = "Etc/GMT+8") == Day_ID[i] ), ]
		#plot(sensor_of_day$PM25, sensor_of_day$date_d)
		zz_b = 1:ch_id[i]
		xx_b = sensor_of_day$f_time[zz_b]
		yy_b = rep(mean(sensor_of_day$PM25[zz_b]), times = length(zz_b))
		zz_f = (ch_id[i]+1):length(sensor_of_day$PM25)
		xx_f = sensor_of_day$f_time[zz_f]
		yy_f = rep(mean(sensor_of_day$PM25[zz_f]), times = length(zz_f))
		
		plot(as.POSIXct(sensor_of_day$f_time), sensor_of_day$PM25,type = "o", xlab = "PM2.5", ylab = "Time")
		lines(xx_b,yy_b,col="red")
		lines(xx_f,yy_f,col="red")
		
		#curve(x=sensor_of_day$PM25[ch_id],as.numeric(sensor_of_day$f_time[1]), as.numeric(sensor_of_day$f_time[ch_id]))
			
		pic_name <- paste0("E:\\myGraph\\Sensor",j, "_p", i,".png")
		dev.copy(png,pic_name)
		dev.off()
		#time_dif <- diff(time_of_d)
		#diff_in_dat <- as.numeric(as.Date(time_of_d[2]) - as.Date(time_of_d[1]))
	}
}