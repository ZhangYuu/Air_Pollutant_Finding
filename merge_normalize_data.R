
#This code is not write in sequence, it include all the code we use to merge and normalize raw data

# Read in csv
data = read.csv("/Users/Memo/Downloads/test/beijing_all_20160101.csv", header = TRUE)

#open the working space
setwd("/Users/Memo/Downloads/")

#input all the file name in test into a
a = list.files("beijing_20160101-20161231")

#change the working dictionart
dir = paste("./beijing_20160101-20161231/",a,sep="")                                      

#read the number of documents in test
n = length(dir)                                                                

merge.data = read.csv(file = dir[1],header=T,sep=",")   

for (i in 2:n){
  new.data = read.csv(file = dir[i], header=T, sep=",")
  merge.data = rbind(merge.data,new.data)
}

#output the data
write.csv(data,file = "./test/merge_extra2.csv",row.names=F)  

## Read in csv
data = read.csv("/Users/Memo/Downloads/test/merge_extra.csv", header = TRUE)

# Remove other measurements (simplify)
data = data[data$type != "PM2.5_24h", ]
data = data[data$type != "AQI", ]
data = data[data$type != "PM10_24h", ]

data = data[data$type != "SO2_24h", ]
data = data[data$type != "NO2_24h", ]
data = data[data$type != "O3_24h", ]
data = data[data$type != "CO_24h", ]

data2<-data

# Air quality index: https://airnow.gov/index.cfm?action=aqibasics.aqi
# 200+ is unhealthy
# Number of times a value greater than or equal to 200 appears.
sum(data2 >= 200, na.rm = T)

#read and merge data
data_all= read.csv("/Users/Memo/Desktop/merge_all3.csv", header = TRUE)
data_pm25 = data_all[data_all$type != "PM10",]
data_pm10 = data_all[data_all$type !="PM2.5",]

data_extra= read.csv("/Users/Memo/Desktop/merge_extra3.csv", header = TRUE)
data_SO2 = data_extra[data_extra$type != "NO2",]
data_SO2 = data_SO2[data_SO2$type != "O3",]
data_SO2 = data_SO2[data_SO2$type != "CO",]

data_NO2 = data_extra[data_extra$type != "SO2",]
data_NO2 = data_NO2[data_NO2$type != "O3",]
data_NO2 = data_NO2[data_NO2$type != "CO",]

data_O3 = data_extra[data_extra$type != "SO2",]
data_O3 = data_O3[data_O3$type != "NO2",]
data_O3 = data_O3[data_O3$type != "CO",]

data_CO = data_extra[data_extra$type != "SO2",]
data_CO = data_CO[data_CO$type != "NO2",]
data_CO = data_CO[data_CO$type != "O3",]

#merge
x<-merge(data_pm25,data_pm10, by=c("date","hour"))
x<-merge(x,data_CO, by=c("date","hour"))
x<-merge(x,data_NO2, by=c("date","hour"))
x<-merge(x,data_SO2, by=c("date","hour"))
x<-merge(x,data_O3, by=c("date","hour"))

#remove NA data
data[!complete.cases(data),];
nrow(data[!complete.cases(data),]);
data<-na.omit(data);

#write csv
write.csv(x,file = "/Users/Memo/Desktop/final.csv",row.names=F)  
write.csv(data,file = "/Users/Memo/Desktop/final3.csv",row.names=F)  

#normalization data
data <- read.csv("/Users/Memo/Desktop/final.csv")

# Function of normalization
mmnorm<-function(x,minx,maxx)
{z<-((x-minx)/(maxx-minx))
return(z)}

temp <- data[,"PM2.5"]
data[,"PM2.5"] <- mmnorm(temp,min(temp),max(temp))
temp <- data[,"PM10"]
data[,"PM10"] <- mmnorm(temp,min(temp),max(temp))
temp <- data[,"CO"]
data[,"CO"] <- mmnorm(temp,min(temp),max(temp))
temp <- data[,"NO2"]
data[,"NO2"] <- mmnorm(temp,min(temp),max(temp))
temp <- data[,"SO2"]
data[,"SO2"] <- mmnorm(temp,min(temp),max(temp))
temp <- data[,"O3"]
data[,"O3"] <- mmnorm(temp,min(temp),max(temp))

#deal with pm2.5
data_pm25_new = data$PM2.5

for (i in 1:8180){
  if (data_pm25_new[i]>200){
    data_pm25_new[i]=1
    print(1)
  }
  else{
    data_pm25_new[i]=0
    print(0)
  }
}
data$PM2.5=data_pm25_new

datadate=data$date
datahour=data$hour
for (i in 1:8180){
  datadate[i]=datadate[i]*100+datahour[i]
}
data$date=datadate
data=data[,-2]
#export data
write.csv(data,file = "/Users/Memo/Desktop/final4.csv",row.names=F)  