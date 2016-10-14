library(changepoint)
library(dplyr)

air_data <- readRDS("Air_box.rds")

sensor_ID <- unique(air_data$ID)

data_f_ID = character(0)
data_f_CP = numeric(0)
data_f_lat = numeric(0)
data_f_lon = numeric(0)
up_down = numeric (0)

for(j in 1:length(sensor_ID)){ #length(sensor_ID)
  if( j == 127 ) next #error message Data must have at least 4 observations to fit a change point model.

	PM25_of_sensor <- subset(air_data, ID == sensor_ID[j]) 

	del_index_arr = group_by(PM25_of_sensor, as.Date(f_time, tz = "Etc/GMT+8")) %>%
	summarise( length(PM25) > 4 ) #change point detection is not available when length is shorter than 4

	Day_ID <- unique(as.Date(PM25_of_sensor$f_time, tz = "Etc/GMT+8"))
	del_index = which(as.data.frame(del_index_arr[,2])[,1] %in% 0)

	if(length(del_index) != 0) PM25_of_sensor <- PM25_of_sensor[ -which( Day_ID[del_index] == as.Date(PM25_of_sensor$f_time, tz = "Etc/GMT+8") ),]
	#get Day_ID after deleting the ones with length shorter than 4
	Day_ID <- unique(as.Date(PM25_of_sensor$f_time, tz = "Etc/GMT+8"))


	ch_id_group = group_by(PM25_of_sensor, as.Date(f_time, tz = "Etc/GMT+8")) %>%
	summarise(
	  ifelse(length(cpt.meanvar(PM25,penalty="Manual",pen.value="4*log(n)",method="BinSeg",Q=2,class=FALSE)) == 0, NA, list(cpt.meanvar(PM25,penalty="Manual",pen.value="4*log(n)",method="BinSeg",Q=2,class=FALSE))) 
	  )

	# numeric_id_in_each_day ch_id_group[[2]][[DAY_i]]
	# each_day ch_id_group[[1]][[DAY_i]]
	# sen_of_day <- PM25_of_sensor[which(as.Date(PM25_of_sensor$f_time, tz = "Etc/GMT+8") == Day_ID[DAY_i]),]
	# sen_of_day$f_time[ch_id_group[[2]][[DAY_i]]]
	if(length(Day_ID)==0) next
	my_vec = numeric (0)
	
	
	
	ID_of_full_length = 0
	for(Day_i in 1:length(Day_ID)){
	  The_ID = ch_id_group[[2]][[Day_i]]
	  sen_of_day <- PM25_of_sensor[which(as.Date(PM25_of_sensor$f_time, tz = "Etc/GMT+8") == Day_ID[Day_i]),]
	  
	  for(k in 1:length(The_ID)){
	    if(is.na( PM25_of_sensor$PM25[ ID_of_full_length + The_ID[k] + 1 ] )){
	      print(ID_of_full_length + The_ID[k] + 1)
	      up_down <- append(up_down, -1) }
		  else if( (PM25_of_sensor$PM25[ ID_of_full_length + The_ID[k] ]) > (PM25_of_sensor$PM25[ ID_of_full_length + The_ID[k] + 1 ]) ) {up_down <- append(up_down, 0)}
		  else{up_down <- append(up_down, 1)}
	  }
	  my_vec <- append(my_vec, sen_of_day$f_time[The_ID])
	  
	  ID_of_full_length <- ID_of_full_length + length(sen_of_day$PM25)
	}
	# make it as dataframe
	x <- rep(sensor_ID[j], length(my_vec))
	lat <- rep(PM25_of_sensor$Coor_x[1], length(my_vec))
	lon <- rep(PM25_of_sensor$Coor_y[1], length(my_vec))
	
	data_f_CP <- append(data_f_CP, my_vec)
	data_f_ID <- append(data_f_ID, as.character(x))
	data_f_lat <- append(data_f_lat, lat)
	data_f_lon <- append(data_f_lon, lon)
}
df <- data.frame(data_f_ID, data_f_CP, data_f_lat, data_f_lon, up_down)
saveRDS(df, file = "C:\\Users\\anthony\\Desktop\\Changepoint.rds")