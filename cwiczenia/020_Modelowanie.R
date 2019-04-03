

#######  BIBLIOTEKI  ###############################################################################

library(rpart)
library(rpart.plot)
library(mlr)
library(tidyverse)
library(funModeling)

#######  DANE  #####################################################################################

# wczytaj dane za pomocą fukncji read.csv()
credits <- read.csv('dane/german_credit_data.csv')

#######  ESKLPORACYJNA ANALIZA DANYCH  #############################################################

### Wyjaśnienie znaczenia zmiennnych ###

#   1.  X - Client's ID
#   2.  Age (numeric)
#   3.  Sex (text: male, female)
#   4.  Job (numeric: 0 - unskilled and non-resident,
#               1 - unskilled and resident,
#               2 - skilled, 
#               3 - highly skilled)
#   5.  Housing (text: own, rent, or free)
#   6.  Saving.accounts (text - little, moderate, quite rich, rich)
#   7.  Checking.account (numeric, in DM - Deutsch Mark)
#   8.  Credit.amount (numeric, in DM)
#   9.  Duration (numeric, in month)
#   10. Purpose (text: car, furniture/equipment, radio/TV, domestic appliances.. and so on...)
#   --- Cecha modelowana ----
#   11. Risk (text: good/bad): it's our Target Variable, describes if client paid or didn't pay loan

# Sprawdź rozmiar ramki danych (ilość obserwacji)



# sprawdź typy danych w naszej ramce używając funkcji bazowej i funkcji z pakietu dplyr



# wyświetl podsumowanie ramki, użyj: summary (wybrane statystyki opisowe) i funModeling::df_status


#######  PREPROCESSING DANYCH  #####################################################################

# zapropononuj modyfikacje, utworzenie cech lub usuniecie cech w ramce danych

# przekodowanie numeric na factor i dodanie poziomow

credits$Job <- factor(x = as.character(credits$Job), labels = c('unskilled and non-resident',
                                                                'unskilled and resident',
                                                                'skilled',
                                                                'highly skilled')
)

# dodanie nowej zmiennej bazującej na istniejących: miesięczna rata



# rpart domyślnie obsluguje NA's (wykona model i predykcje) jednak bedzie to mieć wływ na wynik
# dlatego tworzymy alternatywną ramkę danych z uzupełnionymi NA aby porównać wyniki modelowania

credits_wna <- credits %>% mutate_at(c("Saving.accounts", "Checking.account"), fct_explicit_na)


 






