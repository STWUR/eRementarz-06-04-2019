

#######  BIBLIOTEKI  ###############################################################################

library(rpart)
library(rpart.plot)
library(mlr)
library(dplyr)
library(funModeling)

#######  DANE  #####################################################################################

credits <- read.csv('data/german_credit_data.csv')

#######  ESKLPORACYJNA ANALIZA DANYCH  #############################################################

# Sprawdź rozmiar ramki danych (ilość obserwacji)
dim(credits)

# wyświetl podsumowanie dla ramki (wybrane statystyki opisowe)
summary(credits)





