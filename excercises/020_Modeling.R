

#######  BIBLIOTEKI  ###############################################################################

library(rpart)
library(rpart.plot)
library(mlr)
library(dplyr)
library(funModeling)

#######  DANE  #####################################################################################

credits <- read.csv('dane/german_credit_data.csv')

#######  ESKLPORACYJNA ANALIZA DANYCH  #############################################################

### Wyjaśnienie znaczenia zmiennnych ###


# Sprawdź rozmiar ramki danych (ilość obserwacji)

dim(credits)

# sprawdź typy danych w naszej ramce używając funkcji bazowej i funkcji z pakietu dplyr

str(credits)
glimpse(credits)

# wyświetl podsumowanie ramki, użyj: summary (wybrane statystyki opisowe) i funModeling::df_status

summary(credits)
df_status(credits)

# czy któraś ze zmiennych 
# 






