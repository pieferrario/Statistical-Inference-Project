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

#### 1. Importazione dati
dataset = read.csv("Sleep_Health_and_Lifestyle_Dataset.csv")
View(dataset)
dim(dataset)

# Visualizziamo le prime righe e ricaviamone un summary. Notiamo che non sono presenti NA.
head(dataset)
summary(dataset)
sum(is.na(dataset))

# Controlliamo i tipi di dati presenti
print(sapply(dataset, typeof))

## Pulizia dei dati

# Togliamo colonne inutili, come person id
dataset$Person.ID = NULL
sum(duplicated(dataset))
# Correggiamo typo, come "Normal weight" e "Normal"
dataset$BMI.Category[dataset$BMI.Category == "Normal Weight"] = "Normal"

# La pressione sanguigna risulta come stringa, quindi la dividiamo nei due valori, sistolica e diastolica e poi calcoliamo la PAM
bp_split = strsplit(as.character(dataset$Blood.Pressure), "/")
dataset$Systolic_BP  = as.numeric(sapply(bp_split, "[", 1))
dataset$Diastolic_BP = as.numeric(sapply(bp_split, "[", 2))
dataset$Blood.Pressure = NULL
dataset$PAM = dataset$Diastolic_BP + ((dataset$Systolic_BP - dataset$Diastolic_BP)/3)
dataset$Diastolic_BP = NULL
dataset$Systolic_BP = NULL

# Visualizziamo le professioni
table(dataset$Occupation)

# Notiamo che abbiamo pochi dati per quanto riguarda le professioni "Manager", "Sales Representative", 
# "Scientist", "Software Engineer", notiamo che abbiamo meno di 4 individui in tutte queste categorie,
# quindi le accorpiamo in un'unico gruppo "Others"
rare_occupations <- c("Manager", "Sales Representative", "Scientist", "Software Engineer")
dataset$Occupation[dataset$Occupation %in% rare_occupations] <- "Other"

# Convertiamo in fattori le stringhe
dataset$Gender         = as.factor(dataset$Gender)
dataset$BMI.Category <- factor(dataset$BMI.Category, 
                               levels = c("Normal", "Overweight", "Obese"))
dataset$Sleep.Disorder = as.factor(dataset$Sleep.Disorder)
dataset$Occupation     = as.factor(dataset$Occupation)

dataset$Sleep.Disorder = relevel(dataset$Sleep.Disorder, ref = "None")

# Verifica dei livelli (il primo livello che compare è la baseline)
levels(dataset$Sleep.Disorder) # Mostrerà: "None", "Insomnia", "Sleep Apnea"
levels(dataset$BMI.Category)   # Mostrerà: "Normal", "Obese", "Overweight"


# Controlliamo che le modifiche siano avvenute correttamente
head(dataset)
summary(dataset)


#### 2. Visualizziamo i dati

# Impostiamo la griglia grafica 2x2 per vedere i 4 fattori insieme
par(mfrow = c(2, 2))

# 1. Impatto del Genere
boxplot(Quality.of.Sleep ~ Gender, data = dataset,
        main = "Sonno vs Genere", col = c("pink", "lightblue"),
        xlab = "Genere", ylab = "Qualità del Sonno")

# 2. Impatto della Categoria BMI
boxplot(Quality.of.Sleep ~ BMI.Category, data = dataset,
        main = "Sonno vs BMI", col = c("lightgreen", "orange", "red"),
        xlab = "BMI", ylab = "Qualità del Sonno")

# 3. Impatto dei Disturbi del Sonno
boxplot(Quality.of.Sleep ~ Sleep.Disorder, data = dataset,
        main = "Sonno vs Disturbi del Sonno", col = c("lightgray", "purple", "darkblue"),
        xlab = "Disturbo", ylab = "Qualità del Sonno")

# 4. Impatto dell'Occupazione
# Usiamo le professioni già pulite (con la categoria "Other")
boxplot(Quality.of.Sleep ~ Occupation, data = dataset,
        main = "Sonno vs Professione", col = rainbow(length(levels(dataset$Occupation))),
        xlab = "Professione", ylab = "Qualità del Sonno", las = 2) # las = 2 ruota le etichette se sono lunghe

# Ripristiniamo la finestra grafica singola
par(mfrow = c(1, 1))