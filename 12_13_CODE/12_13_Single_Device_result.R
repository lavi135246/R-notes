file_names = list.files(path = "C:\\Users\\anthony\\Desktop\\my_project\\2016_12_12\\Table_of_ChangHua_Prob", pattern = NULL, all.files = FALSE, full.names = FALSE, recursive = FALSE, ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)

dir1_prob <- 0
dir2_prob <- 0
dir3_prob <- 0
dir4_prob <- 0

dir5_prob <- 0
dir6_prob <- 0
dir7_prob <- 0
dir8_prob <- 0

count <- 0
total_weight <-0

i=1
for(rds_file in file_names){
	my_path = paste0("C:\\Users\\anthony\\Desktop\\my_project\\2016_12_12\\Table_of_ChangHua_Prob\\",substr(rds_file,1,10), ".rds")
	eight_dir_prob <- readRDS(my_path)
	
	
	select_row <- which(eight_dir_prob$Device_id=="74DA3895C2B4")
	
	if(length(select_row)==0){
		count[i]<-0
	
		dir1_prob[i] <- 0
		dir2_prob[i] <- 0
		dir3_prob[i] <- 0
		dir4_prob[i] <- 0

		dir5_prob[i] <- 0
		dir6_prob[i] <- 0
		dir7_prob[i] <- 0
		dir8_prob[i] <- 0
		next
	}
	count[i]<-eight_dir_prob$count[select_row]
	
	dir1_prob[i] <- eight_dir_prob$prob1[select_row]
	dir2_prob[i] <- eight_dir_prob$prob2[select_row]
	dir3_prob[i] <- eight_dir_prob$prob3[select_row]
	dir4_prob[i] <- eight_dir_prob$prob4[select_row]

	dir5_prob[i] <- eight_dir_prob$prob5[select_row]
	dir6_prob[i] <- eight_dir_prob$prob6[select_row]
	dir7_prob[i] <- eight_dir_prob$prob7[select_row]
	dir8_prob[i] <- eight_dir_prob$prob8[select_row]
	
	
	# dir1_prob <- dir1_prob + eight_dir_prob$prob1[select_row]*count
	# dir2_prob <- dir2_prob + eight_dir_prob$prob2[select_row]*count
	# dir3_prob <- dir3_prob + eight_dir_prob$prob3[select_row]*count
	# dir4_prob <- dir4_prob + eight_dir_prob$prob4[select_row]*count
	# 
	# dir5_prob <- dir5_prob + eight_dir_prob$prob5[select_row]*count
	# dir6_prob <- dir6_prob + eight_dir_prob$prob6[select_row]*count
	# dir7_prob <- dir7_prob + eight_dir_prob$prob7[select_row]*count
	# dir8_prob <- dir8_prob + eight_dir_prob$prob8[select_row]*count

	total_weight<- total_weight+count[i]
	i=i+1
}