air_data <- readRDS("C:\\Users\\anthony\\Desktop\\my_project\\2016_10_29\\modified_data\\2016-08-06.rds")
eight_direct_dist_taipei<- readRDS("C:\\Users\\anthony\\Desktop\\my_project\\2016_12_12\\Table_of_Eight_Dir\\2016-08-06.rds")

air_data<-subset(air_data,air_data$Line_color!=0)

air_data<-subset(air_data, air_data$GPS_latitude<25.17)
air_data<-subset(air_data, air_data$GPS_latitude>24.95)
air_data<-subset(air_data, air_data$GPS_longtitude<121.62)
air_data<-subset(air_data, air_data$GPS_longtitude>121.42)

u_device<-unique(air_data[c("Device_id","GPS_longtitude","GPS_latitude")])

Device_id<-u_device$Device_id

eight_dir_prob<-as.data.frame(Device_id)
eight_dir_prob$prob1<-numeric (length(u_device$Device_id))
eight_dir_prob$prob2<-numeric (length(u_device$Device_id))
eight_dir_prob$prob3<-numeric (length(u_device$Device_id))
eight_dir_prob$prob4<-numeric (length(u_device$Device_id))

eight_dir_prob$prob5<-numeric (length(u_device$Device_id))
eight_dir_prob$prob6<-numeric (length(u_device$Device_id))
eight_dir_prob$prob7<-numeric (length(u_device$Device_id))
eight_dir_prob$prob8<-numeric (length(u_device$Device_id))

eight_dir_prob$count<-numeric (length(u_device$Device_id)) #weight

i=1
for(cur_id in eight_direct_dist_taipei$Device_id){
  cur_color<-air_data$Line_color[which(air_data$Device_id==cur_id)]
  dir1 <-air_data$Line_color[which(air_data$Device_id==eight_direct_dist_taipei$dist1[i])]
  dir2 <-air_data$Line_color[which(air_data$Device_id==eight_direct_dist_taipei$dist2[i])]
  dir3 <-air_data$Line_color[which(air_data$Device_id==eight_direct_dist_taipei$dist3[i])]
  dir4 <-air_data$Line_color[which(air_data$Device_id==eight_direct_dist_taipei$dist4[i])]
  
  dir5 <-air_data$Line_color[which(air_data$Device_id==eight_direct_dist_taipei$dist5[i])]
  dir6 <-air_data$Line_color[which(air_data$Device_id==eight_direct_dist_taipei$dist6[i])]
  dir7 <-air_data$Line_color[which(air_data$Device_id==eight_direct_dist_taipei$dist7[i])]
  dir8 <-air_data$Line_color[which(air_data$Device_id==eight_direct_dist_taipei$dist8[i])]
  
  dev_time <-air_data$f_time[which(diff(cur_color)>0)]
  
  dir1_time<-air_data$f_time[which(diff(dir1)>0)]
  dir2_time<-air_data$f_time[which(diff(dir2)>0)]
  dir3_time<-air_data$f_time[which(diff(dir3)>0)]
  dir4_time<-air_data$f_time[which(diff(dir4)>0)]
  
  dir5_time<-air_data$f_time[which(diff(dir5)>0)]
  dir6_time<-air_data$f_time[which(diff(dir6)>0)]
  dir7_time<-air_data$f_time[which(diff(dir7)>0)]
  dir8_time<-air_data$f_time[which(diff(dir8)>0)]
  
  eight_dir_prob$count[i]<-length(dev_time)
  
  for(changpoint in dev_time){
	  if(mean((findInterval(dir1_time, c(changpoint,changpoint+600))==1L)+0)>0 && !is.na(mean((findInterval(dir1_time, c(changpoint,changpoint+600))==1L)+0)>0)) 
		eight_dir_prob$prob1[i] <- (eight_dir_prob$prob1[i]+1/length(dev_time))
	  if(mean((findInterval(dir2_time, c(changpoint,changpoint+600))==1L)+0)>0 && !is.na(mean((findInterval(dir2_time, c(changpoint,changpoint+600))==1L)+0)>0)) 
		eight_dir_prob$prob2[i] <- (eight_dir_prob$prob2[i]+1/length(dev_time))
	  if(mean((findInterval(dir3_time, c(changpoint,changpoint+600))==1L)+0)>0 && !is.na(mean((findInterval(dir3_time, c(changpoint,changpoint+600))==1L)+0)>0)) 
		eight_dir_prob$prob3[i] <- (eight_dir_prob$prob3[i]+1/length(dev_time))
	  if(mean((findInterval(dir4_time, c(changpoint,changpoint+600))==1L)+0)>0 && !is.na(mean((findInterval(dir4_time, c(changpoint,changpoint+600))==1L)+0)>0)) 
		eight_dir_prob$prob4[i] <- (eight_dir_prob$prob4[i]+1/length(dev_time))
	  
	  if(mean((findInterval(dir5_time, c(changpoint,changpoint+600))==1L)+0)>0 && !is.na(mean((findInterval(dir5_time, c(changpoint,changpoint+600))==1L)+0)>0)) 
		eight_dir_prob$prob5[i] <- (eight_dir_prob$prob5[i]+1/length(dev_time))
	  if(mean((findInterval(dir6_time, c(changpoint,changpoint+600))==1L)+0)>0 && !is.na(mean((findInterval(dir6_time, c(changpoint,changpoint+600))==1L)+0)>0)) 
		eight_dir_prob$prob6[i] <- (eight_dir_prob$prob6[i]+1/length(dev_time))
	  if(mean((findInterval(dir7_time, c(changpoint,changpoint+600))==1L)+0)>0 && !is.na(mean((findInterval(dir7_time, c(changpoint,changpoint+600))==1L)+0)>0)) 
		eight_dir_prob$prob7[i] <- (eight_dir_prob$prob7[i]+1/length(dev_time))
	  if(mean((findInterval(dir8_time, c(changpoint,changpoint+600))==1L)+0)>0 && !is.na(mean((findInterval(dir8_time, c(changpoint,changpoint+600))==1L)+0)>0)) 
		eight_dir_prob$prob8[i] <- (eight_dir_prob$prob8[i]+1/length(dev_time))
	  
  } 
  i=i+1
}

