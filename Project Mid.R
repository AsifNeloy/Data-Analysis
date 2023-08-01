install.packages(pkgs = "xlsx")
require(xlsx)
mydata <- read.xlsx("G:/PRACTICE/Data Science/Dataset_midterm.xlsx", sheetIndex = 1)
mydata

sum(is.na(mydata$id))
sum(is.na(mydata$age))
sum(is.na(mydata$weight.kg.))
sum(is.na(mydata$Delivery_number))
sum(is.na(mydata$Delivery_time))
sum(is.na(mydata$Blood))
sum(is.na(mydata$Heart))
sum(is.na(mydata$Caesarian))

mydata$weight.kg.[is.na(mydata$weight.kg.)]<-mean(mydata$weight.kg.,na.rm=TRUE)

mydata$Delivery_number[is.na(mydata$Delivery_number)]<-mean(mydata$Delivery_number,na.rm=TRUE)
mydata$Delivery_time[is.na(mydata$Delivery_time)]<-mean(mydata$Delivery_time,na.rm=TRUE)
mydata



mydata<-na.omit(mydata)
mydata

mydata$weight.kg. = as.numeric(format(round(mydata$weight.kg.,0)))
mydata$Delivery_number = as.numeric(format(round(mydata$Delivery_number,0)))
mydata$Delivery_time = as.numeric(format(round(mydata$Delivery_time,0)))
mydata


mydata1<-mydata
install.packages(pkgs = "dplyr")
library(dplyr)

mydata1<- mydata1%>%
  mutate(Blood_num = case_when(
    Blood == "high" ~ 3,
    Blood == "normal" ~ 2,
    Blood == "low" ~ 1
  )
)

mydata1


mydata1 <- mydata1 %>% 
  mutate(Emergency = case_when(
    Blood_num = 3 & Delivery_number >= 2 ~ "Risk",
    Blood_num = 3 & Delivery_number == 1 ~ "Safe",
    Blood_num = 2 & Delivery_number >= 1 ~ "Safe",
    Blood_num = 1 & Delivery_number > 1 ~ "Risk",
    Blood_num = 1 & Delivery_number == 1 ~ "Safe"
  )
  
)

mydata1 <- mydata1 %>% 
  mutate(Emergency_num = case_when(
    Emergency == "Risk" ~ 1,
    Emergency == "Safe" ~ 0
    
  )
  
  )

mydata1

mean(mydata1$weight.kg.)
mean(mydata1$Blood_num)
mean(mydata1$Age)
mean(mydata1$Delivery_number)

median(mydata1$Age)
median(mydata1$weight.kg.)
median(mydata1$Caesarian)

find_mode <- function(x) {
  u <- unique(x)
  tab <- tabulate(match(x, u))
  u[tab == max(tab)]
}
find_mode(mydata1$Age)
find_mode(mydata1$weight.kg.)
find_mode(mydata1$Delivery_number)
find_mode(mydata1$Blood_num)
find_mode(mydata1$Emergency_num)

na.omit(mydata1)

