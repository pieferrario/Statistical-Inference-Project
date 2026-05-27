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
dataset$data_channel <- as.factor(dataset$data_channel)

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


## 2. Creazione del modello di regressione #####################################
modello_completo = lm(logshares ~ ., data = train_set_logs)

# Visualizzazione della sintesi inferenziale (Coefficienti, R2, t-test, F-test)
summary(modello_completo)
