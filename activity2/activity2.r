#Read data from csv file 
data <- read.csv("y:\\Students\\hkropp\\a02\\2011124.csv")

#Question1
nrow(data) #Number of rows
ncol(data) #Number of cols

#Specify a col with proper date format 
data$dateF <- as.Date(data$DATE,"%Y-%m-%d")


#create a date column by reformatting the date to only include years
#and indicating that it should be treated as numeric data
data$year <- as.numeric(format(data$dateF,"%Y"))

#Question2
ch <- c("a","b","c","d","e") #character vector
num <- c(1,2.5,0.5,-3,2) #numeric vector 
int <- c(1,2,3,4,5) #integer vector 
#represents rankings from 1 to 5
fact <- c(rep("1",20),rep("2",30),rep("3",50),rep("4",20),rep("5",10)) #factor vector


mean(data$TMAX[data$NAME == "ABERDEEN, WA US" ], na.rm = TRUE)
mean(data$TMAX[data$NAME == "ABERDEEN, WA US" ])

data$avg_temp <- data$TMIN + ((data$TMAX-data$TMIN)/2)
average_temp <- aggregate(data$avg_temp, by = list(data$NAME), FUN = 'mean', na.rm = TRUE)
colnames(average_temp)<-c("Name","MAAT")

    
