# Script 1: Raccolta dati e pulizia.

##### 0. Pacchetti richiesti

library(car)
library(rgl)
library(MASS)
library(leaps)
library(GGally)
library(ellipse)
library(faraway)
# library(qpcR)

##### 1.Importazione dati
dataset = read.csv("sleep_health_dataset.csv")
View(dataset)

# Il dataset ha 32 colonne e 100000 righe.
dim(dataset)

# Visualizziamo le prime righe e ricaviamone un summary. Notiamo che non sono presenti NA.
head(dataset)
summary(dataset)
sum(is.na(dataset))

# Controlliamo i tipi di dati presenti
print(sapply(dataset, typeof))

# Sono presenti alcune variabili categoriche, quindi dobbiamo trasformarle in variabili numeriche con factor.
