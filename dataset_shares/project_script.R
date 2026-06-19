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
library(tidyverse)
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

# Raggruppiamo i channel in un unico fattore
dataset$data_channel <- case_when(
  dataset$data_channel_is_lifestyle == 1     ~ "Lifestyle",
  dataset$data_channel_is_entertainment == 1 ~ "Entertainment",
  dataset$data_channel_is_bus == 1           ~ "Business",
  dataset$data_channel_is_socmed == 1        ~ "SocialMedia",
  dataset$data_channel_is_tech == 1          ~ "Tech",
  dataset$data_channel_is_world == 1         ~ "World",
  TRUE                                       ~ "Other"
)
dataset$data_channel = as.factor(dataset$data_channel)

# Raggruppiamo i giorni della settimana in un unico fattore ordinato
dataset$weekday <- case_when(
  dataset$weekday_is_monday == 1    ~ "Monday",
  dataset$weekday_is_tuesday == 1   ~ "Tuesday",
  dataset$weekday_is_wednesday == 1 ~ "Wednesday",
  dataset$weekday_is_thursday == 1  ~ "Thursday",
  dataset$weekday_is_friday == 1    ~ "Friday",
  dataset$weekday_is_saturday == 1  ~ "Saturday",
  dataset$weekday_is_sunday == 1    ~ "Sunday"
)
dataset$weekday <- factor(dataset$weekday, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

# Trasformiamo anche is_weekend in fattore
dataset$is_weekend <- as.factor(dataset$is_weekend)


# Rimuoviamo le vecchie colonne dummy singole per evitare che vengano inserite per errore nel modello
# creando collinearità perfetta con i nuovi fattori
colonne_da_rimuovere <- c("data_channel_is_lifestyle", "data_channel_is_entertainment", 
                          "data_channel_is_bus", "data_channel_is_socmed", 
                          "data_channel_is_tech", "data_channel_is_world",
                          "weekday_is_monday", "weekday_is_tuesday", "weekday_is_wednesday", 
                          "weekday_is_thursday", "weekday_is_friday", "weekday_is_saturday", "weekday_is_sunday")
dataset <- dataset[, !(names(dataset) %in% colonne_da_rimuovere)]

# Creiamo ora una colonna con i log degli shares, che ci sarà utile dopo
dataset$logshares = log(dataset$shares)

# Controlliamo che tutto sia stato eseguito correttamente
dim(dataset)
head(dataset)
summary(dataset)


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

# Visualizziamo i quantili della variabile shares. Notiamo che la distribuzione è profondamente asimmetrica
print(quantile(train_set$shares, probs = c(0.25, 0.50, 0.75, 0.90, 0.95, 0.98, 0.99)))

# Facciamo un istogramma della variabile shares. Siccome il quantile di ordine 0.98 di shares è 20600, 
# trascuriamo momentaneamente i valori di share che superano 20000 nel grafico (visto che saranno molti pochi)

hist(train_set$shares, breaks = 1500, xlim = c(0, 20000), 
     main = "Istogramma: Forma di Shares",
     xlab = "Shares (tagliato a 20k)", col = "steelblue", border = "white")

# Facciamo un box plot della variabile shares per verificare gli outlier
dev.new()
boxplot(train_set$shares, 
        main = "Boxplot: Outlier di Shares",
        ylab = "Shares (Scala Assoluta)", col = "lightcoral")
boxplot(train_set$shares, 
        outline = FALSE, # Rimuove visivamente gli outlier che stirano l'asse Y
        main = "Boxplot di Shares (Zoom sul 95% dei Dati)",
        ylab = "Numero di Condivisioni (Fino al baffo sup.)", 
        col = "lightcoral")

### 1.c Applicazione del logaritmo alla variabile shares ########################
# Vista la forte asimmetria del dataset, proviamo a renderla simmetrica applicando una trasformazione logaritmo ai dati.
# Andremo quindi a studiare non tanto la relazione tra share e le covariate ma il log(shares) e covariate.

dataset_logs   = dataset[, names(dataset) != "shares"]
train_set_logs = train_set[, names(train_set) != "shares"]
test_set_logs  = test_set[, names(test_set) != "shares"]

# Tramite questa trasformazione il dataset è più simmetrico. Infatti,
# Visualizziamo i quantili della variabile logshares
print(quantile(train_set_logs$logshares, probs = c(0.25, 0.50, 0.75, 0.90, 0.95, 0.98, 0.99)))

# Istogramma del logaritmo per verificare la forma campanulare
dev.new()
hist(train_set_logs$logshares, breaks = 50,
     main = "Istogramma: Forma di Log(Shares)",
     xlab = "Log(Shares)", col = "darkgreen", border = "white")

# Boxplot del logaritmo COMPLETO (mostra gli outlier residui nella nuova scala)
dev.new()
boxplot(train_set_logs$logshares, 
        main = "Boxplot: Outlier di Log(Shares)",
        ylab = "Log(Shares)", col = "pink")

# Boxplot del logaritmo SENZA OUTLIER (zoom sul corpo centrale trasformato)
dev.new()
boxplot(train_set_logs$logshares, 
        outline = FALSE, 
        main = "Boxplot di Log(Shares) (Senza Outlier)",
        ylab = "Log(Shares)", col = "orange")

### 1.d Verifichiamo la necessità di una trasformazione box-cox
modello_lineare_base <- lm(shares ~ . - logshares - is_weekend, data = train_set)

# Creiamo il grafico della trasformazione di Box-Cox
# Cerchiamo il lambda ottimale in un intervallo standard [-2, 2]
dev.new()
bc <- boxcox(modello_lineare_base, lambda = seq(-2, 2, by = 0.1))

# Estraiamo il valore esatto di lambda che massimizza la log-verosimiglianza
lambda_ottimo <- bc$x[which.max(bc$y)]
cat("Il lambda ottimale suggerito da Box-Cox è:", lambda_ottimo, "\n")

# Siccome lambda ottimale è \lambda = 0, scegliamo come trasformazione il logaritmo.

## 2. Creazione del modello di regressione #####################################
modello_completo = lm(logshares ~ ., data = train_set_logs)

### 2.a Visualizzazione della sintesi inferenziale (Coefficienti, R2, t-test, F-test) #####
summary(modello_completo)

## 3. Selezione delle covariate ################################################ 
# Notiamo che weekdaySunday risulta NA perché c'è forte collinearità tra il factor "weekday" e "is_weekend".
# Facciamo quindi un confronto tra i due sottomodelli in cui consideriamo
# a. Solo is_weekend (1 è weekend, 0 è feriale)
# b. Solo weekday (trascuriamo la distinzione in weekend)
modello_is_weekend <- lm(logshares ~ . - weekday, data = train_set_logs)
summary(modello_is_weekend)

modello_weekday    <- lm(logshares ~ . - is_weekend, data = train_set_logs)
summary(modello_weekday)

Delta_AIC = AIC(modello_is_weekend) - AIC(modello_weekday)
# Notiamo che l'R2 adjusted è leggermente migliore nel secondo caso, quindi lo teniamo.
# Una conclusione analoga può essere ottenuta usando AIC. La differenza tra i due è 
# positiva, quindi modello_weekday ha l'AIC più basso.

train_set_logs$is_weekend <- NULL
test_set_logs$is_weekend  <- NULL


# Ora verifichiamo il VIF
vif_model = vif(modello_weekday)
print(vif_model)

# Identifichiamo le variabili problematiche in modo robusto
if (!is.null(dim(vif_model))) {
  # CASO MATRICE: il modello contiene factor con gradi di libertà (Df) > 1.
  # car restituisce 3 colonne: GVIF, Df, GVIF^(1/(2*Df)).
  # Per paragonare l'ultima colonna a un VIF classico di 5, usiamo la radice quadrata di 5.
  soglia <- sqrt(5) 
  variabili_collineari = rownames(vif_model)[vif_model[, 3] > soglia]
} else {
  # CASO VETTORE: non ci sono factor complessi, restituisce i VIF standard.
  soglia <- 5
  variabili_collineari = names(vif_model)[vif_model > soglia]
}

cat("Variabili con forte collinearità (soglia equivalente a VIF > 5):\n")
print(variabili_collineari)

# Abbiamo identificato diverse colonne collineari e procediamo ad escluderle

variabili_filtro_minimo <- c("LDA_04", "n_non_stop_words")

train_set_logs_semi_pulito <- train_set_logs[, !(names(train_set_logs) %in% variabili_filtro_minimo)]

# Ricreiamo il modello base escludendo anche is_weekend
modello_semi_pulito <- lm(logshares ~ ., data = train_set_logs_semi_pulito)

# Lanciamo lo stepAIC su questo set molto più grande
cat("\nAvvio selezione stepwise (versione estesa) in corso... ci vorrà di più.\n")
modello_ottimizzato_esteso <- stepAIC(modello_semi_pulito, direction = "both", trace = FALSE)

# Valutiamo i risultati
summary(modello_ottimizzato_esteso)