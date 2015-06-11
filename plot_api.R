library(ggplot2)
library(ggmap)

qmap("University of Southern California", zoom=14)

qmap("white house",zoom=14,maptype="satellite")

qmap("white house",zoom=14,maptype="hybrid")

qmap("white house",zoom=14,maptype="toner")

?crime

summary(crime)

qmap("Houston")
attach(crime)
summary(offense)

violent_crimes= crime[offense %in% c("murder","rape","aggravated assault","theft"),]

violent_crime=subset(crime, offense=="murder"| offense =="rape" | offense=="aggravated assault" | offense=="theft")
  
HoustonMap<- qmap("Houston",zoom=12, col="bw") 

gglocator(2) 

#1 -95.27683 29.68053
#2 -95.38837 29.79515

violent_crime=subset(violent_crime,lon<=-95.27683 & lon>=-95.38837 
                     & lat<=29.79515 & lat>=29.68053)


HoustonMap+
  stat_bin2d(aes(x=lon, y=lat, colour=offense, fill=offense, alpha=0.2),data=violent_crime)

geocode("4814 Austin St")
  