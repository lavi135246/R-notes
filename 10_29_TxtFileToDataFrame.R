file_names = list.files(path = "C:\\Users\\anthony\\Desktop\\history_data", pattern = NULL, all.files = FALSE, full.names = FALSE, recursive = FALSE, ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)

for(txt_file in file_names){
  
  my_path = paste0("C:\\Users\\anthony\\Desktop\\history_data\\",txt_file)
  air_data = read.csv(my_path)
  colnames(air_data) <- c("Device_id", "GPS_latitude", "GPS_longtitude", "DateTime", "PM2.5", "Humidity", "Temperature")
  air_data$f_time <- as.POSIXct(strptime(air_data$DateTime, "%Y-%m-%dT%H:%M:%SZ"))
  new_path = paste0("C:\\Users\\anthony\\Desktop\\my_project\\2016_10_29\\history_data\\",substr(txt_file,1,10), ".rds")
  saveRDS(air_data, file = new_path)
}
