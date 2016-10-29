file_names = list.files(path = "C:\\Users\\anthony\\Desktop\\my_project\\2016_10_29\\history_data", pattern = NULL, all.files = FALSE, full.names = FALSE, recursive = FALSE, ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)


for(rds_file in file_names){
  path = paste0("C:\\Users\\anthony\\Desktop\\my_project\\2016_10_29\\history_data\\", rds_file)
  air_data = readRDS(path)
  u_id = unique(air_data$Device_id)

  air_data$PM_mean <- rep(0, length(air_data$Device_id))
  air_data$Line_color <- rep(0, length(air_data$Device_id))

  #the index of change point will be 
  for(cur_id in u_id){
  
    indice_of_cur_id = which(air_data$Device_id == cur_id)
    PM25_of_sensor <- subset(air_data, Device_id == cur_id) 
  
    if(length(PM25_of_sensor$PM2.5) < 150) next
  
    temp <- cpt.meanvar(PM25_of_sensor$PM2.5, pen.value = c(4,1500), penalty = "CROPS", method = "PELT")
    crop_matrix <- cpts.full(temp)
    crop_index <-crop_matrix[ nrow(crop_matrix) - 3,]
    crop_index <- crop_index[!is.na(crop_index)]
    j <- 1
    cpt_vec <- vector(mode="numeric", length=0)
    today_time <- as.POSIXct(PM25_of_sensor$f_time, tz = "Etc/GMT+8")
    for(i in crop_index){
      cpt_vec[j:i-1] <- mean(PM25_of_sensor$PM2.5[j:i-1])
      cpt_vec[i-1] <- NA
      j <- i+1
      #print(cpt_vec)
    }
  
    i <- length( PM25_of_sensor$PM2.5 )
    cpt_vec[j:i] <- mean(PM25_of_sensor$PM2.5[j:i])
    #p <- p + geom_line(aes(today_time, cpt_vec,na.rm = TRUE), colour = "red")
    air_data$PM_mean[indice_of_cur_id] <- cpt_vec
  }

#color

  air_data$Line_color[which(air_data$PM_mean >= 71)] = 10 #"#9900FF" #Purple

  air_data$Line_color[which(air_data$PM_mean <  71)] = 9 #"#990000" #Dark Red
  air_data$Line_color[which(air_data$PM_mean <  65)] = 8 #"#FF0000" #Red
  air_data$Line_color[which(air_data$PM_mean <  59)] = 7 #"#FF9999" #Light Red

  air_data$Line_color[which(air_data$PM_mean <  54)] = 6 #"#FF6600" #Orange
  air_data$Line_color[which(air_data$PM_mean <  48)] = 5 #"#FFCC00" #Tangerine
  air_data$Line_color[which(air_data$PM_mean <  42)] = 4 #"#FFFF00" #Yellow

  air_data$Line_color[which(air_data$PM_mean <  36)] = 3 #"#336600" #Dark Green
  air_data$Line_color[which(air_data$PM_mean <  24)] = 2 #"#00FF00" #Green
  air_data$Line_color[which(air_data$PM_mean <  12)] = 1 #"#CCFF99" #Light Green

  new_path = "C:\\Users\\anthony\\Desktop\\my_project\\2016_10_29\\modified_data\\2016-08-13.rds"
  saveRDS(air_data, file = new_path)
}
write.table(air_data, file = "C:\\Users\\anthony\\Desktop\\my_project\\2016_10_29\\modified_data\\2016-08-13.csv",sep = "\t", row.names = FALSE)

