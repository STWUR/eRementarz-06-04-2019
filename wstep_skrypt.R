#Skrypt do æwiczeñ - Erementarz - 6.04.18

data <- read.csv("dane/german_credit_data.csv", 
                 encoding = "UTF-8")

# wymiary tabeli
dim(data)

# pierwsze wiersze z tabeli
data[1:4,1:11]
head(data)
sapply(data, class)

data2<-data
data2$Job <- factor(x = as.character(data$Job), 
                    labels = 
                      c('unskilled and non-resident', 
                        'unskilled and resident',
                        'skilled','highly skilled'))
head(data2$Job)

data2$Risk<-as.numeric(x=data$Risk,labels=c(1,2))
head(data2$Risk)#1-bad, 2-good

# podsumowanie tabeli
summary(data)

head(is.na(data))
data2<-na.omit(data)


library(sqldf)
zapytanie1<-"select Age, Sex, Risk from data2 order by Age desc limit 5" 
sqldf(zapytanie1)


zapytanie0<-read.csv.sql("dane/german_credit_data.csv",
                         sql = "select Age, Sex, Risk from file order by Age desc limit 5")
sqldf(zapytanie0)

zapytanie2 = "select avg(Age) as sredni_wiek from data2 where Sex='male' and Risk=1" 
sqldf(zapytanie2)


zapytanie4 = 'select Sex,Risk, avg(Age) as [Wiek], avg("Credit.amount") as [Kwota kredytu] from data2 group by Sex,Risk'
sqldf(zapytanie4)



library(dplyr)
select(data2,Age,Sex,Risk)

##Konstrukcja chain
  data2 %>%
  select(Age,Sex,Risk) %>%
  arrange(Age) %>%
  top_n(5)

data2 %>%
  filter (Sex=='male', Risk==1) %>%
  summarise(sredni_wiek=mean(Age)) 