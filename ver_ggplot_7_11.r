library(changepoint)
library(dplyr)
library(ggplot2)
library(gcookbook)

air_data <- read.table("C:\\Users\\anthony\\Desktop\\my_project\\2016_7_20_webtest\\data\\airbox-May.csv", header = FALSE)
colnames(air_data) <- c("date_time", "ID", "PM25", "T", "Humid", "Coor_x", "Coor_y")
air_data$f_time <- as.POSIXct(strptime(air_data$date_time, "%Y-%m-%dT%H:%M:%SZ",tz = "Etc/GMT+8"))


sensor_ID <- unique(air_data$ID)
for(j in 127:length(sensor_ID)){ #length(sensor_ID)
  if( j == 82 || j == 126) next #error message Data must have at least 4 observations to fit a change point model.

	PM25_of_sensor <- subset(air_data, ID == sensor_ID[j]) 

	del_index_arr = group_by(PM25_of_sensor, as.Date(f_time, tz = "Etc/GMT+8")) %>%
	summarise( length(PM25) > 4 ) #change point detection is not available when length is shorter than 4

	Day_ID <- unique(as.Date(PM25_of_sensor$f_time, tz = "Etc/GMT+8"))
	del_index = which(as.data.frame(del_index_arr[,2])[,1] %in% 0)

	if(length(del_index) != 0) PM25_of_sensor <- PM25_of_sensor[ -which( Day_ID[del_index] == as.Date(PM25_of_sensor$f_time, tz = "Etc/GMT+8") ),]
	#get Day_ID after deleting the ones with length shorter than 4
	Day_ID <- unique(as.Date(PM25_of_sensor$f_time, tz = "Etc/GMT+8"))
  
	ch_id_group = group_by(PM25_of_sensor, as.Date(f_time, tz = "Etc/GMT+8")) %>%
	summarise(ifelse(length( cpts( cpt.meanvar(PM25) ) ) == 0, NA, cpts( cpt.meanvar(PM25) ) ))

	ch_id = as.data.frame(ch_id_group[,2])[,1]
	
	if( length(ch_id) == 0 ) next
	for( i in 1:length(ch_id) ){#length(ch_id)
	    #if( j == 5 && i == 11 ) break 
		if(is.na(ch_id[i])) {
			print(c(j,i))
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
		
		xx_b = c(xx_b, xx_f)
		yy_b = c(yy_b, yy_f)
		p <- ggplot(sensor_of_day,
			aes(as.POSIXct(sensor_of_day$f_time, tz = "Etc/GMT+8"), sensor_of_day$PM25)) +
			geom_line() + xlab("PM2.5") + ylab("Time") + ggtitle(Day_ID[i]) +
			#annotate("segment", x = sensor_of_day$f_time[1], xend = sensor_of_day$f_time[ch_id[i]], y = mean(sensor_of_day$PM25[zz_b]), yend = mean(sensor_of_day$PM25[zz_b]), colour = "red")
			geom_line(aes(as.POSIXct(xx_b),yy_b), colour = "red") +
			annotate(geom = "text", x = as.POSIXct(Day_ID[i], tz = "Etc/GMT+8"), y = max(sensor_of_day$PM25)+5, 
				label = as.POSIXct(sensor_of_day$f_time[ch_id[i]], tz = "Etc/GMT+8"))
			
		#curve(x=sensor_of_day$PM25[ch_id],as.numeric(sensor_of_day$f_time[1]), as.numeric(sensor_of_day$f_time[ch_id]))
		pic_name <- paste0("E:\\GGraph\\Sensor",j, "_p", i,".png")
		print(p)
		ggsave(pic_name, plot = p)
		graphics.off()
		
	}
}