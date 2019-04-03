
#######  BIBLIOTEKI  ###############################################################################

library(rpart)
library(rpart.plot)
library(mlr)
library(tidyverse)
library(funModeling)

#######  DANE  #####################################################################################

credits <- read.csv('dane/german_credit_data.csv')

#######  ESKLPORACYJNA ANALIZA DANYCH  #############################################################

### Wyjaśnienie znaczenia zmiennnych ###

#   
#   1.  Age (numeric)
#   2.  Sex (text: male, female)
#   3.  Job (numeric: 0 - unskilled and non-resident,
#               1 - unskilled and resident,
#               2 - skilled, 
#               3 - highly skilled)
#   4.  Housing (text: own, rent, or free)
#   5.  Saving.accounts (text - little, moderate, quite rich, rich)
#   5.  Checking.account (numeric, in DM - Deutsch Mark)
#   6.  Credit.amount (numeric, in DM)
#   7.  Duration (numeric, in month)
#   8.  Purpose (text: car, furniture/equipment, radio/TV, domestic appliances.. and so on...)
#   9.  Risk (text: good/bad): it's our Target Variable, describes if client paid or didn't pay loan

# Sprawdź rozmiar ramki danych (ilość obserwacji)

dim(credits)

# sprawdź typy danych w naszej ramce używając funkcji bazowej i funkcji glimpse z pakietu dplyr

str(credits)
glimpse(credits)

# wyświetl podsumowanie ramki, użyj: summary (wybrane statystyki opisowe) i funModeling::df_status

summary(credits)
df_status(credits)

#######  PREPROCESSING DANYCH  #####################################################################

# zapropononuj modyfikacje, utworzenie cech lub usuniecie cech w ramce danych

# usuniecie Id klienta - nie jest to cecha, na podstawie ktorej mozna wnioskowac

credits <- credits %>% column_to_rownames("X")
colnames(credits)

# przekodowanie numeric na factor i dodanie poziomów

credits$Job <- factor(x = as.character(credits$Job), labels = c('unskilled and non-resident',
                                                                'unskilled and resident',
                                                                'skilled',
                                                                'highly skilled')
)

# dodanie nowej zmiennej bazującej na istniejących: miesięczna rata

credits$Installment <- credits$Credit.amount/credits$Duration

# rpart domyślnie obsluguje NA's (wykona model i predykcje) jednak bedzie to mieć wływ na wynik
# dlatego tworzymy alternatywną ramkę danych z uzupełnionymi NA aby porównać wyniki modelowania

credits_wna <- credits %>% mutate_at(c("Saving.accounts", "Checking.account"), fct_explicit_na)

####### MODELOWANIE ################################################################################

###### SPOSÓB 1 #### modolowanie 'manualne' z poziomu 'rpart' ######################################

# dzzielenie zbioru na uczący i testowy w relacji 30/70

set.seed(1234)
ix_train <- sample(1:nrow(credits), size = 0.7 * nrow(credits))

# model z domyślnymi parametrami bez zastąpionych braków danych

# wyjaśnienie: 'formula', 'rpart.control'

m1
