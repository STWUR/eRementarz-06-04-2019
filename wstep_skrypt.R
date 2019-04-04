data <- read.csv("dane/german_credit_data.csv", 
                 encoding = "UTF-8")

# wymiary tabeli
dim(data)

# pierwsze wiersze z tabeli
data[1:4,1:11]
head(data)
sapply(data, class)

# podsumowanie tabeli
summary(data)

head(is.na(data))
data<-na.omit(data)


library(sqldf)
zapytanie1<-"select Age, Sex, Risk from data order by Age desc limit 5" 


zapytanie0<-read.csv.sql("dane/german_credit_data.csv",
                         sql = "select Age, Sex, Risk from file order by Age desc limit 5")

library(dplyr)
print(zapytanie0)
sqldf(zapytanie1)