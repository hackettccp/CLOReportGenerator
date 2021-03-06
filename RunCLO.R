source("CLOProcessor.R")
#source("CLOTableProcessor.R")
source("CourseProcessor.R")
#clos <- c("1", "2", "3", "4", "5", "6", "7")
clos <- c("3")
lapply(clos, processor, "CIS205.csv", "CIS205")
#lapply(clos, table_processor, "CSCI111.csv", "CSCI111")
#course_processor("CSCI111.csv", "CSCI111")



