#using pelt CROPS
library(changepoint)
library(dplyr)
library(ggplot2)

air_data <- readRDS("C:/Users/anthony/Desktop/my_project/census-app/data/Air_box.rds")

PM25_of_sensor <- subset(air_data, ID == sensor_ID[13])
del_index_arr = group_by(PM25_of_sensor, as.Date(f_time, tz = "Etc/GMT+8")) %>%
summarise( length(PM25) > 4 ) #change point detection is not available when length is shorter than 4

Day_ID <- unique(as.Date(PM25_of_sensor$f_time, tz = "Etc/GMT+8"))
del_index = which(as.data.frame(del_index_arr[,2])[,1] %in% 0)

if(length(del_index) != 0) PM25_of_sensor <- PM25_of_sensor[ -which( Day_ID[del_index] == as.Date(PM25_of_sensor$f_time, tz = "Etc/GMT+8") ),]
#get Day_ID after deleting the ones with length shorter than 4
Day_ID <- unique(as.Date(PM25_of_sensor$f_time, tz = "Etc/GMT+8"))

sen_of_day <- PM25_of_sensor[which(as.Date(PM25_of_sensor$f_time, tz = "Etc/GMT+8") == Day_ID[1]),]

pelt_1 <- cpt.meanvar(sen_of_day$PM25, pen.value = c(4,1500), penalty = "CROPS", method = "PELT") #call by cpts(pelt)
possible_ncpts <- unique(apply(pelt_1@cpts.full, 2, function(x) length(which(!is.na(x)))))

crop_matrix <- cpts.full(pelt_1)
crop_index <-crop_matrix[ nrow(crop_matrix) - 7,]

# j <- 1
# cpt_vec <- vector(mode="numeric", length=0)
# today_time <- as.POSIXct(sen_of_day$f_time, tz = "Etc/GMT+8")
# for(i in crop_index[!is.na(crop_index)]){
#   
# }
# 
# i <- length( sen_of_day$PM25 )
# cpt_vec[j:i] <- mean(sen_of_day$PM25[j:i])
# p <- ggplot(sen_of_day,
#        aes(today_time, PM25)) +
#   geom_line() + xlab("PM2.5") + ylab("Time")
#   geom_line(aes(today_time, cpt_vec,na.rm = TRUE), colour = "red")
# 

# cpt_vec <- vector(mode="numeric", length=0)
# j <- 1
# 
# for(i in crop_index[!is.na(crop_index)]){
#   p <- p + geom_line(
#         aes(x = as.POSIXct(sen_of_day$f_time[i], tz = "Etc/GMT+8"), y = mean(sen_of_day$PM25[j:i]),
#         xend = as.POSIXct(sen_of_day$f_time[j], tz = "Etc/GMT+8"), yend = mean(sen_of_day$PM25[j:i]),
#         colour = "segment")
#         )
#   print(j:i)
#   j <- i+1
# }
# 
# print(p)
