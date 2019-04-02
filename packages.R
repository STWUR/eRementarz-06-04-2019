install.packages(c("tidyverse", "mlr", "rpart", "rpart.plot", "randomForest", "funModeling", "sqldf"), 
                 repos = "https://cloud.r-project.org/")

df  <- read.csv('https://raw.githubusercontent.com/STWUR/eRementarz-06-04-2019/master/dane/german_credit_data.csv')
