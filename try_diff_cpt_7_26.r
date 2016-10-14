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
#ch_id_group = group_by(PM25_of_sensor, as.Date(f_time, tz = "Etc/GMT+8")) %>%
#	summarise(ifelse(length( cpts( cpt.meanvar(PM25) ) ) == 0, NA, cpts( cpt.meanvar(PM25) ) ))


amoc <- cpt.meanvar(PM25_of_sensor$PM25, method = "AMOC") #call by cpts(amoc)
pelt <- cpt.meanvar(PM25_of_sensor$PM25, method = "PELT") #call by cpts(pelt)
segneigh <- cpt.meanvar(PM25_of_sensor$PM25,penalty="Asymptotic",pen.value=0.01,method="SegNeigh",Q=5,class=FALSE)
binseg <- cpt.meanvar(PM25_of_sensor$PM25,penalty="Manual",pen.value="4*log(n)",method="BinSeg",Q=5,class=FALSE)


#other try
pelt_1 <- cpt.meanvar(sen_of_day$PM25, pen.value = c(4,1500), penalty = "CROPS", method = "PELT") #call by cpts(pelt)
cpts.full(pelt_1)
cpt.out(pelt_1)
plot(pelt_1,ncpts = 5)

apply(pelt_1@cpts.full, 2, function(x) length(which(!is.na(x))))

#print pelt
p <- ggplot(PM25_of_sensor,
 		       aes(x = as.POSIXct(f_time, tz = "Etc/GMT+8"), y = PM25)) + 
 	          geom_line() + geom_vline(xintercept = as.numeric(PM25_of_sensor$f_time[ cpts(pelt_1) ]), colour = "red")
print(p)
#print SegNeigh
p <- ggplot(PM25_of_sensor,
 		       aes(x = as.POSIXct(f_time, tz = "Etc/GMT+8"), y = PM25)) + 
 	          geom_line() + geom_vline(xintercept = as.numeric(PM25_of_sensor$f_time[ segneigh ]), colour = "red")
print(p)

#print BinSeg			  
p <- ggplot(PM25_of_sensor,
 		       aes(x = as.POSIXct(f_time, tz = "Etc/GMT+8"), y = PM25)) + 
 	          geom_line() + geom_vline(xintercept = as.numeric(PM25_of_sensor$f_time[ cpts(pelt_1) ]), colour = "red")
print(p)

