air_data <- readRDS("C:\\Users\\anthony\\Desktop\\my_project\\2016_10_29\\modified_data\\2016-09-22.rds")

air_data<-subset(air_data, air_data$GPS_latitude<25.17)
air_data<-subset(air_data, air_data$GPS_latitude>24.95)
air_data<-subset(air_data, air_data$GPS_longtitude<121.62)
air_data<-subset(air_data, air_data$GPS_longtitude>121.42)

u_device<-unique(air_data[c("Device_id","GPS_longtitude","GPS_latitude")])

Device_id<-u_device$Device_id
eight_direct_dist_taipei<-as.data.frame(Device_id)
eight_direct_dist_taipei$dist1<-character (length(u_device$Device_id))
eight_direct_dist_taipei$dist2<-character (length(u_device$Device_id))
eight_direct_dist_taipei$dist3<-character (length(u_device$Device_id))
eight_direct_dist_taipei$dist4<-character (length(u_device$Device_id))

eight_direct_dist_taipei$dist5<-character (length(u_device$Device_id))
eight_direct_dist_taipei$dist6<-character (length(u_device$Device_id))
eight_direct_dist_taipei$dist7<-character (length(u_device$Device_id))
eight_direct_dist_taipei$dist8<-character (length(u_device$Device_id))

# i=1

# for(i_device in u_device){
#   cur_table<-as.data.frame(Device_id)
#   temp_long<-i_device$GPS_longtitude[i]
#   temp_lat<-i_device$GPS_latitude[i]
#   
#   cur_long<-u_device$GPS_longtitude-temp_long
#   cur_lat<-u_device$GPS_latitude-temp_lat
#   cur_table$cur_dist<-(cur_long^2+cur_lat^2)^(1/2)
#   
#   cur_table<-cur_table[order(cur_table$cur_dist),]
#   
#   
#   
#   i=i+1
# }

X <- matrix(c(u_device$GPS_longtitude, u_device$GPS_latitude),length(Device_id),2)
L <- dim(X)[1]
squa_X <- matrix(rep(rowSums(X*X),L),L,L)
M <- X %*% t(X)
dist_d <- sqrt(-2*M + squa_X+t(squa_X))

for(i in 1:length(Device_id)){
  
  device_order<-u_device[sort(dist_d[i,], index.return=TRUE)$ix,]
  temp_long<-u_device$GPS_longtitude[i]
  temp_lat<-u_device$GPS_latitude[i]
  device_order$GPS_longtitude<-device_order$GPS_longtitude-temp_long
  device_order$GPS_latitude<-device_order$GPS_latitude-temp_lat
  
  lat_div_long<-device_order$GPS_longtitude/device_order$GPS_latitude
  #------ For Debug ------#
  #device_order$lat_div_long<-device_order$GPS_longtitude/device_order$GPS_latitude
  device_order$dir<-rep(0,length(Device_id))
  
  device_order$dir[which(device_order$GPS_longtitude>=0)]<-1
  device_order$dir[intersect(which(device_order$dir>=1),which(lat_div_long<1))]<-2
  device_order$dir[intersect(which(device_order$dir>=1),which(lat_div_long<0))]<-7
  device_order$dir[intersect(which(device_order$dir>=1),which(lat_div_long< -1))]<-8
  
  device_order$dir[which(device_order$GPS_longtitude<0)]<-(-5)
  device_order$dir[intersect(which(device_order$dir<0),which(lat_div_long<1))]<-(-6)
  device_order$dir[intersect(which(device_order$dir<0),which(lat_div_long<0))]<-(-3)
  device_order$dir[intersect(which(device_order$dir<0),which(lat_div_long< -1))]<-(-4)
  
  device_order$dir<-abs(device_order$dir)
  
  device_order$dir[is.nan(lat_div_long)]<-0
  
  # if NA -> no nearby in that direction
  eight_direct_dist_taipei$dist1[i]<-as.character(device_order$Device_id[min(which(device_order$dir==1))])
  eight_direct_dist_taipei$dist2[i]<-as.character(device_order$Device_id[min(which(device_order$dir==2))])
  eight_direct_dist_taipei$dist3[i]<-as.character(device_order$Device_id[min(which(device_order$dir==3))])
  eight_direct_dist_taipei$dist4[i]<-as.character(device_order$Device_id[min(which(device_order$dir==4))])
  
  eight_direct_dist_taipei$dist5[i]<-as.character(device_order$Device_id[min(which(device_order$dir==5))])
  eight_direct_dist_taipei$dist6[i]<-as.character(device_order$Device_id[min(which(device_order$dir==6))])
  eight_direct_dist_taipei$dist7[i]<-as.character(device_order$Device_id[min(which(device_order$dir==7))])
  eight_direct_dist_taipei$dist8[i]<-as.character(device_order$Device_id[min(which(device_order$dir==8))])
  
}



