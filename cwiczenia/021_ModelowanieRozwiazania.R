
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

####### MODELOWANIE z rpart ########################################################################

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

####### PREDYKCJA i OCENA ##########################################################################

# predykcja modeli na zbiorach uczących i testowych, porównanie wyników

y_tr <- credits[ix_train, "Risk"]
y_tst <- credits[-ix_train, "Risk"]

# tworzymy model 'bazowy' czyli symulujemy sytuacje jak wak wygladalaby sutuacja, gdybysmy dzialali bez modelu

m0_pr <- factor(rep("good", times = 1000), levels = c('good', 'bad'))
ConfusionMatrix(y_pred =m0_pr, y_true = credits[,"Risk"])
Accuracy(m0_pr, y_true = credits[,"Risk"])

# okazuje się, że przy takim stanie, klasyfikując wszystkich jako dobrych uzyskujemy 70% trafności

m1_pr_tr <- predict(object = m1, newdata = credits[ix_train,], type = "class")
m1_pr_tst <- predict(object = m1,  newdata = credits[-ix_train,], type = "class")

m2_pr_tr <- predict(object = m2, newdata = credits_wna[ix_train,], type = "class")
m2_pr_tst <- predict(object = m2, newdata = credits_wna[-ix_train,], type = "class")

m3_pr_tr <- predict(object = m3, newdata = credits[ix_train,], type = "class")
m3_pr_tst <- predict(object = m3, newdata = credits[-ix_train,], type = "class")

m4_pr_tr <- predict(object = m4, newdata = credits_wna[ix_train,], type = "class")
m4_pr_tst <- predict(object = m4, newdata = credits_wna[-ix_train,], type = "class")

# macierz pomyłek i trafność 

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
df_performance

# zauważmy, że niektóre modele uzyskują gorzą jakość mierzoną 'accuracy' niż w przypadku 'modelu bazowego'
# czy to znaczy, że faktycznie są gorsze?
# NIE ! - accoracy nie jest dobrą miarą, zauważmy, że koszt udzielenia kredytu 'złemu' klientowi jest
# większy (strata kapitału) niż nie udzielenia 'dobremu' - brak zysku z marży
# wybór najlepszego modelu pod kątem biznesowym omówimy w modelowaniu przy użyciu 'mlr'

####### MODELOWANIE z mlr ##########################################################################

###### SPOSÓB 2 #### modolowanie 'manualne' i 'automatyczne' #######################################

task <- makeClassifTask(data = credits, target = 'Risk', positive = 'bad')
task_wna <- makeClassifTask(data = credits_wna, target = 'Risk', positive = 'bad')

tree <-makeLearner('classif.rpart', predict.type = 'prob')

m1_mlr <-train(learner = tree, task = task, subset = ix_train)    # trenowanie modelu

##### PREDYKCJA I OCENA ############################################################################

m1_pr_tr_mlr <- predict(m1_mlr, newdata = credits[ix_train,])     #predykcja na zbiorze treningowym
m1_pr_tst_mlr <- predict(m1_mlr, newdata = credits[-ix_train,])   #predyckja na zbiorze testowym

performance(m1_pr_tr_mlr, acc)                                    # trafność na zbiorze treningowym
performance(m1_pr_tst_mlr, acc)                                   # trafność na zbiorze testowym

df_performance                                                    # wyniki identyczne jak w spo

# pobieramy obiekt modelu z mlr do obektu klasy 'rpart' w cely jego wizualicaji

m1_mlr <- getLearnerModel(m1_mlr)

# porównujemy drzewa graficznie. Wywołaj 'rpart.plot()' na obiekcie 'm1' i 'm1_mlr'

rpart.plot(m1)
rpart.plot(m1_mlr)

##### MODELOWANIE Z mlr. STROJENIE MODELI ##########################################################

# określenie strategii resamplingu. 5 krotna cross-walidacja

tree_desc <- makeResampleDesc('CV', iters = 5, stratify = TRUE, predict = 'both')

# ewalucja modelu m1

tree_res <- resample(learner = tree,
                     task = task,
                     resampling = tree_desc,
                     measures = list(acc, acc.train = setAggregation(acc, train.mean))
)

# ewaluacja modelu z różnnymi parametrami


# wybiramy parametry, które chcemy zmieniać

ps <- makeParamSet(makeIntegerParam('maxdepth', lower = 2, upper = 7),
                   makeIntegerParam('minbucket', lower = 20, upper = 100),
                   makeDiscreteParam('cp',values = 0.00001))

# definiujemy liczbę iteracli i sposób przeszukiwania przestrzeni parametrów

ctrl <- makeTuneControlRandom(maxit = 50)


# let's tune! 

set.seed(1234)

res <- tuneParams(learner = tree,
                  task = task,
                  resampling = tree_desc,
                  par.set = ps,
                  control = ctrl,
                  measures = list(auc, 
                                  auc.train = setAggregation(auc, train.mean),
                                  auc.test.sd = setAggregation(auc, test.sd),
                                  acc)
)

res_wna <- tuneParams(learner = tree,
                      task = task_wna,
                      resampling = tree_desc,
                      par.set = ps,
                      control = ctrl,
                      measures = list(auc, 
                                      auc.train = setAggregation(auc, train.mean),
                                      auc.test.sd = setAggregation(auc, test.sd),
                                      acc)
)

# and the winner is !!!!

res
res_wna

# wyniki tuningu

df_tuned <- generateHyperParsEffectData(res, partial.dep = TRUE)$data

# sprawdź wyniki tuningu dla modelu z uzupełnionymi NA i zapisz do zmiennej 'df_tuned_wna'

df_tuned_wna <- generateHyperParsEffectData(res_wna, partial.dep = TRUE)$data

# decyzja którą kombinacje parametrów wybrać należy do analityka i zawsze oprócz performance
# powinna także brać pod uwagę inne czynniki takie jest zrozumiałość biznesowa modelu, jego stabilność etc...

# rangowanie wyników tuningu
df_tuned <- df_tuned %>%
  mutate(auc_diff = auc.train.mean - auc.test.mean) %>%
  arrange(-auc.test.mean, auc_diff) %>%
  select(maxdepth, minbucket, auc.train.mean, auc.test.mean, auc_diff, auc.test.sd, acc.test.mean) %>%
  head(10)

# wykonaj analogiczny ranking dla modeli z uzupełnionymi NA i zapisz do zmiennej 'df_tuned_wna'
df_tuned_wna <- df_tuned_wna %>%
  mutate(auc_diff = auc.train.mean - auc.test.mean) %>%
  arrange(-auc.test.mean, auc_diff) %>%
  select(maxdepth, minbucket, auc.train.mean, auc.test.mean, auc_diff, auc.test.sd, acc.test.mean) %>%
  head(10)
<<<<<<< HEAD

# który model rekomendujesz do wdrożenia???


# rekomenduję model bazujący na danych z uzupełnionymi NA o parametrach maxdepth = 5, minbucket = 38
# ze względu na relatywnie wysokie auc małą różnicę pomiędzy auc na zbiorze treningowym i testowym i
# niskim odchyleniem standardowym auc

#### OSTATECZNY MODEL ##############################################################################

tree_final <- makeLearner('classif.rpart',
                          predict.type = 'prob',
                          par.vals = list(cp = 0.00001, maxdepth = 5, minbucket = 38)
                          )

m_final <-train(learner = tree_final,task = task_wna, subset = ix_train)
m_final <- getLearnerModel(m_final)
rpart.plot(m_final, roundint = FALSE)

m_final_prd_tst <- predict(m_final, newdata = credits_wna[-ix_train,], type = 'class')

#### SYMULACJA WYNIKU BIZNESOWEGO ##################################################################

# Założenia
# średni kwota udzielonego kredytu 5000 PLN
# marża na spłaconym kredycie 15% 
# strata nie niespłaconym kredycie 50%

# symulujemy na zbiorze testowym

# M0 sytuacja bez modelu - udzielamy wszytskim

ConfusionMatrix(y_pred =m0_pr[-ix_train], y_true = credits[-ix_train,"Risk"])

(68* -(5000 * 0.5)) + (132 * 5000 * 0.2)  # bez modelu tracimy na 200 kredytach 71 tys PLN

# M1 model bez uzupełnionych braków danych z domyślnymi parametrami rpart

ConfusionMatrix(y_pred = m1_pr_tst, y_true = y_tst)

(58 * -(5000*0.5)) + (122*5000*0.2)      # z tym modelem tracimy na 200 kredytach 53 tys PLN

# M_finak model z uzupełnionymi brakami danych, nową cechą i po tuningu w mlr

ConfusionMatrix(y_pred = m_final_prd_tst , y_true = y_tst)

(36 * -(5000*0.5)) + (111*5000*0.2)      # zaczynamy zarabiać :)






