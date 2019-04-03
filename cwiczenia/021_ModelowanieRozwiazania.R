
#######  BIBLIOTEKI  ###############################################################################

library(rpart)
library(rpart.plot)
library(mlr)
library(tidyverse)
library(funModeling)
library(MLmetrics)

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

# dzzielenie zbioru na uczący i testowy w relacji 80/20

set.seed(1234)
ix_train <- sample(1:nrow(credits), size = 0.8 * nrow(credits))

# model z domyślnymi parametrami bez zastąpionych braków danych

# wyjaśnienie: 'formula', 'rpart.control'

m1 <- rpart(formula = Risk ~., 
            data = credits,
            subset = ix_train
)

rpart.plot(m1)


# model 2 z domyślnymi parametrami i zastąpionymi brakami danych

m2 <- rpart(formula = Risk ~.,
            data = credits_wna,
            subset = ix_train
)

rpart.plot(m2)

# model 3 ze zmienionym parametrem cp = 0.00001 bez zastąpienia braków danych

m3 <- rpart(formula = Risk ~.,
            data = credits,
            subset = ix_train,
            control = rpart.control(
              cp = 0.00001
            )
)

rpart.plot(m3)


# model 4 z zastąonionymi barakmi danych i zmienionymi wybranymi parametrem  cp = 0.00001
# i zmienonymi dowolnie przez Ciebie wybranym(i) z nastpujmących: maxdepth, minbucket, minsplit

m4 <- rpart(formula = Risk ~.,
            data = credits_wna,
            subset = ix_train,
            control = rpart.control(
              cp = 0.00001,
              maxdepth = 5
            )
)

rpart.plot(m4)

# predykcja modeli na zbiorach uczących i testowych, porównanie wyników

y_tr <- credits[ix_train, "Risk"]
y_tst <- credits[-ix_train, "Risk"]

m1_pr_tr <- predict(object = m1, newdata = credits[ix_train,], type = "class")
m1_pr_tst <- predict(object = m1,  newdata = credits[-ix_train,], type = "class")

m2_pr_tr <- predict(object = m2, newdata = credits_wna[ix_train,], type = "class")
m2_pr_tst <- predict(object = m2, newdata = credits_wna[-ix_train,], type = "class")

m3_pr_tr <- predict(object = m3, newdata = credits[ix_train,], type = "class")
m3_pr_tst <- predict(object = m3, newdata = credits[-ix_train,], type = "class")

m4_pr_tr <- predict(object = m4, newdata = credits_wna[ix_train,], type = "class")
m4_pr_tst <- predict(object = m4, newdata = credits_wna[-ix_train,], type = "class")

# macierz pomyłek i trafność 

# tworzymy pustą ramkę danych do zapisu wyników modelowania



# obliczamy trafność każdego modelu na zbiorze treningowym i testowym i zapisujemy do ramki danych

# M1
# zbiór treningowy
ConfusionMatrix(y_pred = m1_pr_tr, y_true = y_tr)
acc_tr_m1 <- Accuracy(y_pred = m1_pr_tr, y_true = y_tr)

# zbiór testowy
ConfusionMatrix(y_pred = m1_pr_tst, y_true = y_tst)
acc_tst_m1 <- Accuracy(y_pred = m1_pr_tst, y_true = y_tst)

df_performance <- data.frame(model = "m1",
                             acc_train = acc_tr_m1,
                             acc_test = acc_tst_m1,
                             stringsAsFactors = FALSE)
#M2
# zbiór treningowy
ConfusionMatrix(y_pred = m2_pr_tr, y_true = y_tr)
acc_tr_m2 <- Accuracy(y_pred = m2_pr_tr, y_true = y_tr)

# zbiór testowy
ConfusionMatrix(y_pred = m2_pr_tst, y_true = y_tst)
acc_tst_m2 <- Accuracy(y_pred = m2_pr_tst, y_true = y_tst)

df_performance <- rbind.data.frame(df_performance,list('m2', acc_tr_m2, acc_tst_m2))

#M3
# zbiór treningowy
ConfusionMatrix(y_pred = m3_pr_tr, y_true = y_tr)
acc_tr_m3 <- Accuracy(y_pred = m3_pr_tr, y_true = y_tr)

# zbiór testowy
ConfusionMatrix(y_pred = m3_pr_tst, y_true = y_tst)
acc_tst_m3 <- Accuracy(y_pred = m3_pr_tst, y_true = y_tst)

df_performance <- rbind.data.frame(df_performance,list('m3', acc_tr_m3, acc_tst_m3))

#M4
# zbiór treningowy
ConfusionMatrix(y_pred = m4_pr_tr, y_true = y_tr)
acc_tr_m4 <- Accuracy(y_pred = m4_pr_tr, y_true = y_tr)

# zbiór testowy
ConfusionMatrix(y_pred = m4_pr_tst, y_true = y_tst)
acc_tst_m4 <- Accuracy(y_pred = m4_pr_tst, y_true = y_tst)

df_performance <- rbind.data.frame(df_performance,list('m4', acc_tr_m4, acc_tst_m4))




