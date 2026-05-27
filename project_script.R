# Progetto di Inferenza Statistica ########################################################
## Lavoro di Matteo Coccia, Cecilia Cotti, Pietro Ferrario, Ian Alexander Fontana Rava ####

## 0. Importazione delle librerie #########################################################
library(car)
library(rgl)
library(MASS)
library(leaps)
library(GGally)
library(ellipse)
library(faraway)
# library(qpcR)

## 1. Importazione dati ###################################################################
dataset = read.csv("OnlineNewsPopularity.csv")
View(dataset)
dim(dataset)

### 1.a Summary, NA, duplicati, tipi e modifiche ##########################################
head(dataset)
summary(dataset)
sum(is.na(dataset))
sum(duplicated(dataset))
print(sapply(dataset, typeof))
str(dataset)

# Togliamo colonne inutili, come l'url
dataset$url = NULL

# Notiamo che i nomi delle colonne hanno uno spazio iniziale, quindi lo sistemiamo
names(dataset) = trimws(names(dataset))

### 1.b Separazione in Training Set e Test Set (Scopo Predittivo) ########################

# Impostiamo un seme per rendere la separazione riproducibile ad ogni esecuzione
set.seed(123)

# Definiamo la proporzione: 70% per l'addestramento/inferenza, 30% per la predizione
percentuale_train = 0.70
index =  sample(1:nrow(dataset), size = floor(percentuale_train * nrow(dataset)))

# Creazione dei due dataset separati
train_set = dataset[index, ]
test_set  = dataset[-index, ]

# Verifica delle dimensioni per la relazione
cat("Dimensioni Train Set:", dim(train_set)[1], "osservazioni\n")
cat("Dimensioni Test Set:", dim(test_set)[1], "osservazioni\n")
