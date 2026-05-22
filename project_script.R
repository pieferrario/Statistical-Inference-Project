# Script 1: Raccolta dati e pulizia.

library(car)
library(rgl)
library(MASS)
library(leaps)
library(GGally)
library(ellipse)
library(faraway)
# library(qpcR)

#### 1. Importazione dati
dataset = read.csv("ScreenTime vs MentalWellness.csv")
View(dataset)
dim(dataset)

# Visualizziamo le prime righe e ricaviamone un summary. Notiamo che non sono presenti NA.
head(dataset)
summary(dataset)
sum(is.na(dataset))

print(sapply(dataset, typeof))

sum(duplicated(dataset))
