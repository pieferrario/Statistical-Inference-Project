## ============================================================
##  PROGETTO DI INFERENZA STATISTICA
##  Dataset: Online News Popularity (Mashable)
##  Target:  log(shares)
## ============================================================


## 0. Importazione delle librerie =====================================================
library(car)
library(rgl)
library(MASS)
library(leaps)
library(GGally)
library(ellipse)
library(faraway)
library(tidyverse)


## 1. Importazione e pre-processing dei dati ==========================================

dataset <- read.csv("OnlineNewsPopularity.csv")

### 1.a  Pulizia iniziale --------------------------------------------------------------

# Rimuoviamo colonne non predittive per definizione
dataset$url       <- NULL
dataset$timedelta <- NULL   # ← RIMOSSO: misura giorni fino alla raccolta dati (8-gen-2015),
#   non è una caratteristica dell'articolo ma un artefatto
#   temporale del campionamento. Includerlo sarebbe leakage.

# Sistemiamo eventuali spazi nei nomi delle colonne
names(dataset) <- trimws(names(dataset))

### 1.b  Encoding dei fattori ----------------------------------------------------------

# Raggruppiamo i channel dummy in un unico fattore
dataset$data_channel <- case_when(
  dataset$data_channel_is_lifestyle    == 1 ~ "Lifestyle",
  dataset$data_channel_is_entertainment== 1 ~ "Entertainment",
  dataset$data_channel_is_bus          == 1 ~ "Business",
  dataset$data_channel_is_socmed       == 1 ~ "SocialMedia",
  dataset$data_channel_is_tech         == 1 ~ "Tech",
  dataset$data_channel_is_world        == 1 ~ "World",
  TRUE                                      ~ "Other"
)
dataset$data_channel <- as.factor(dataset$data_channel)

# Raggruppiamo i giorni della settimana in un unico fattore ordinato
dataset$weekday <- case_when(
  dataset$weekday_is_monday    == 1 ~ "Monday",
  dataset$weekday_is_tuesday   == 1 ~ "Tuesday",
  dataset$weekday_is_wednesday == 1 ~ "Wednesday",
  dataset$weekday_is_thursday  == 1 ~ "Thursday",
  dataset$weekday_is_friday    == 1 ~ "Friday",
  dataset$weekday_is_saturday  == 1 ~ "Saturday",
  dataset$weekday_is_sunday    == 1 ~ "Sunday"
)
dataset$weekday <- factor(dataset$weekday,
                          levels = c("Monday","Tuesday","Wednesday",
                                     "Thursday","Friday","Saturday","Sunday"))

# is_weekend come fattore (lo confronteremo con weekday, poi elimineremo il peggiore)
dataset$is_weekend <- as.factor(dataset$is_weekend)

# Rimuoviamo le dummy originali per evitare collinearità perfetta con i nuovi fattori
colonne_dummy <- c(
  "data_channel_is_lifestyle","data_channel_is_entertainment",
  "data_channel_is_bus","data_channel_is_socmed",
  "data_channel_is_tech","data_channel_is_world",
  "weekday_is_monday","weekday_is_tuesday","weekday_is_wednesday",
  "weekday_is_thursday","weekday_is_friday","weekday_is_saturday","weekday_is_sunday"
)
dataset <- dataset[, !(names(dataset) %in% colonne_dummy)]

### 1.c  Variabile risposta: log(shares) -----------------------------------------------

# La distribuzione di shares è fortemente asimmetrica a destra.
# Applichiamo il logaritmo naturale: la trasformazione è giustificata sia
# empiricamente (istogramma più simmetrico) sia formalmente (Box-Cox suggerisce λ ≈ 0).

dataset$logshares <- log(dataset$shares)

### 1.d  Esplorazione della variabile risposta -----------------------------------------

# Quantili di shares
cat("--- Quantili di shares ---\n")
print(quantile(dataset$shares, probs = c(0.25, 0.50, 0.75, 0.90, 0.95, 0.98, 0.99)))

# Istogramma shares (tagliato a 20k per leggibilità)
hist(dataset$shares,
     breaks = 1500, xlim = c(0, 20000),
     main = "Distribuzione di Shares (taglio a 20k)",
     xlab = "Shares", col = "steelblue", border = "white")

# Istogramma log(shares) — deve apparire campanuliforme
dev.new()
hist(dataset$logshares,
     breaks = 80,
     main = "Distribuzione di log(Shares)",
     xlab = "log(Shares)", col = "steelblue", border = "white")

# Boxplot shares — evidenzia la presenza massiccia di outlier
dev.new()
par(mfrow = c(1, 2))
boxplot(dataset$shares,
        main = "Boxplot Shares (scala assoluta)",
        ylab = "Shares", col = "lightcoral")
boxplot(dataset$shares, outline = FALSE,
        main = "Boxplot Shares (zoom, no outlier)",
        ylab = "Shares", col = "lightcoral")
par(mfrow = c(1, 1))

### 1.e  Conferma formale: Box-Cox -------------------------------------------------------

# Fittiamo un modello lineare temporaneo su shares (non log) per stimare λ ottimale
modello_per_boxcox <- lm(shares ~ . - logshares - is_weekend, data = dataset)

dev.new()
bc          <- boxcox(modello_per_boxcox, lambda = seq(-0.5, 0.5, by = 0.05))
lambda_ott  <- bc$x[which.max(bc$y)]
cat("\nλ ottimale Box-Cox:", lambda_ott,
    "→ conferma la trasformazione logaritmica (λ ≈ 0)\n")

rm(modello_per_boxcox)  # pulizia: non ci serve più


## 2. Separazione Train / Test =========================================================

set.seed(123)
perc_train  <- 0.70
idx         <- sample(1:nrow(dataset), size = floor(perc_train * nrow(dataset)))
train_set   <- dataset[ idx, ]
test_set    <- dataset[-idx, ]

cat("\nDimensioni Train Set:", nrow(train_set), "osservazioni\n")
cat("Dimensioni Test Set: ", nrow(test_set),  "osservazioni\n")

# Dataset "log-only": escludiamo la colonna shares originale da qui in poi
train_logs  <- train_set[, names(train_set) != "shares"]
test_logs   <- test_set[,  names(test_set)  != "shares"]


## 3. Selezione della variabile temporale: is_weekend vs weekday ======================

# weekday e is_weekend sono collineari (Sunday/Saturday ↔ is_weekend = 1).
# Confrontiamo i due sottomodelli alternativi.

modello_is_weekend <- lm(logshares ~ . - weekday,    data = train_logs)
modello_weekday    <- lm(logshares ~ . - is_weekend,  data = train_logs)

cat("\n--- Confronto is_weekend vs weekday ---\n")
cat("R² adj (is_weekend):", summary(modello_is_weekend)$adj.r.squared, "\n")
cat("R² adj (weekday):   ", summary(modello_weekday)$adj.r.squared,    "\n")
cat("ΔAIC (is_weekend − weekday):", AIC(modello_is_weekend) - AIC(modello_weekday), "\n")
cat("→ ΔAIC > 0 significa che weekday ha AIC più basso: preferito.\n")

# Scegliamo weekday: R² adj leggermente migliore e AIC più basso
train_logs$is_weekend <- NULL
test_logs$is_weekend  <- NULL


## 4. Analisi della collinearità (VIF) e pulizia ======================================

modello_base <- lm(logshares ~ ., data = train_logs)

vif_res <- vif(modello_base)
cat("\n--- VIF / GVIF ---\n")
print(vif_res)

# Soglia: per fattori con Df > 1 usiamo GVIF^(1/2Df), confrontato con sqrt(5) ≈ 2.24
# Per variabili continue usiamo VIF > 5
if (!is.null(dim(vif_res))) {
  soglia              <- sqrt(5)
  variabili_collineari <- rownames(vif_res)[vif_res[, 3] > soglia]
} else {
  soglia              <- 5
  variabili_collineari <- names(vif_res)[vif_res > soglia]
}
cat("\nVariabili con collinearità problematica (VIF equiv. > 5):\n")
print(variabili_collineari)

### ---- Motivazione delle rimozioni ----
# LDA_04:                  somma dei 5 topic LDA = 1 → collinearità perfetta con LDA_00..03
# n_non_stop_words:        costante ≈ 1 in quasi tutte le righe, nessuna variabilità utile
# n_non_stop_unique_tokens: ridondante con n_unique_tokens (correlazione > 0.95)
# rate_negative_words:     ≈ 1 − rate_positive_words → quasi collinearità perfetta
# self_reference_min_shares, self_reference_max_shares:
#                          ridondanti con self_reference_avg_sharess

variabili_da_rimuovere <- c(
  "LDA_04",
  "n_non_stop_words",
  "n_non_stop_unique_tokens",
  "rate_negative_words",
  "self_reference_min_shares",
  "self_reference_max_shares"
)

train_pulito <- train_logs[, !(names(train_logs) %in% variabili_da_rimuovere)]
test_pulito  <- test_logs[,  !(names(test_logs)  %in% variabili_da_rimuovere)]

# Verifica: il VIF è ora sotto controllo?
modello_pulito <- lm(logshares ~ ., data = train_pulito)
cat("\n--- VIF dopo pulizia ---\n")
print(vif(modello_pulito))


## 5. Selezione automatica delle variabili (stepwise BIC) ============================

# Usiamo BIC (k = log(n)) invece di AIC (k = 2):
# con n ≈ 27.000, AIC penalizza troppo poco e tende a includere quasi tutto.
# BIC è più parsimonioso e più adatto all'inferenza.

n_train <- nrow(train_pulito)
cat("\nAvvio selezione stepwise BIC (direction = both)... attendere.\n")

modello_bic <- stepAIC(modello_pulito,
                       direction = "both",
                       k        = log(n_train),   # ← BIC
                       trace    = FALSE)

cat("\n--- Riepilogo modello selezionato (BIC) ---\n")
summary(modello_bic)

cat("\nVariabili selezionate:", length(coef(modello_bic)) - 1, "\n")
cat("R² adj:", summary(modello_bic)$adj.r.squared, "\n")
cat("AIC:   ", AIC(modello_bic), "\n")
cat("BIC:   ", BIC(modello_bic), "\n")


## 6. [BOZZA] Diagnostica dei residui =================================================
#
# OBIETTIVO: verificare le ipotesi del modello lineare —
#   (a) linearità, (b) omoschedasticità, (c) normalità dei residui,
#   (d) assenza di osservazioni eccessivamente influenti.

# --- 6.1  Grafici diagnostici standard ---
dev.new()
par(mfrow = c(2, 2))
plot(modello_bic,
     which = 1:4,   # Residuals vs Fitted | Q-Q | Scale-Location | Cook's D
     col   = "steelblue", pch = 20, cex = 0.4)
par(mfrow = c(1, 1))

# --- 6.2  Test formale di normalità dei residui (Shapiro-Wilk su campione) ---
# Con n grande Shapiro-Wilk è sempre significativo; usiamo un campione casuale.
set.seed(42)
sw_sample <- sample(residuals(modello_bic), size = 5000)
cat("\n--- Shapiro-Wilk sui residui (campione n=5000) ---\n")
print(shapiro.test(sw_sample))
# Nota: con questa numerosità rifiutare H0 è quasi certo; guardiamo il Q-Q plot.

# --- 6.3  Test di omoschedasticità (Breusch-Pagan) ---
# library(lmtest)  # aggiungere all'inizio se non già presente
# cat("\n--- Breusch-Pagan test (omoschedasticità) ---\n")
# print(bptest(modello_bic))

# --- 6.4  Identificazione osservazioni influenti (Cook's Distance) ---
soglia_cook    <- 4 / n_train
idx_influenti  <- which(cooks.distance(modello_bic) > soglia_cook)
cat("\nOsservazioni influenti (Cook's D > 4/n):", length(idx_influenti), "\n")

# Confronto R² con e senza le osservazioni più influenti
modello_no_influenti <- lm(formula(modello_bic),
                           data = train_pulito[-idx_influenti, ])
cat("R² adj CON    osservazioni influenti:", summary(modello_bic)$adj.r.squared,        "\n")
cat("R² adj SENZA  osservazioni influenti:", summary(modello_no_influenti)$adj.r.squared, "\n")
# Se la differenza è grande, vale la pena discuterne nella relazione.

# --- 6.5  Leverage (hat values) ---
hat_vals      <- hatvalues(modello_bic)
p             <- length(coef(modello_bic))
soglia_lev    <- 2 * p / n_train
idx_leverage  <- which(hat_vals > soglia_lev)
cat("Osservazioni ad alto leverage (h > 2p/n):", length(idx_leverage), "\n")


## 7. [BOZZA] Inferenza sul modello finale ============================================
#
# OBIETTIVO: interpretare i coefficienti e costruire intervalli di confidenza.

# --- 7.1  Tabella coefficienti con IC al 95% ---
cat("\n--- Coefficienti con Intervalli di Confidenza (95%) ---\n")
print(cbind(coef(modello_bic), confint(modello_bic)))

# --- 7.2  Test F parziali per gruppi tematici di variabili ---
# Es.: le variabili "keyword" aggiungono potere esplicativo oltre al resto?
# Costruiamo un modello ristretto senza quel gruppo, poi usiamo anova().

vars_keyword  <- grep("^kw_", names(train_pulito), value = TRUE)
vars_sentiment <- c("global_subjectivity","global_sentiment_polarity",
                    "global_rate_positive_words","rate_positive_words",
                    "avg_positive_polarity","min_positive_polarity","max_positive_polarity",
                    "avg_negative_polarity","min_negative_polarity","max_negative_polarity",
                    "title_subjectivity","title_sentiment_polarity",
                    "abs_title_subjectivity","abs_title_sentiment_polarity")

# Modello senza keyword features (solo se presenti nel modello BIC)
vars_in_bic     <- names(coef(modello_bic))[-1]
kw_in_bic       <- intersect(vars_keyword, vars_in_bic)

if (length(kw_in_bic) > 0) {
  formula_no_kw   <- as.formula(
    paste("logshares ~", paste(setdiff(vars_in_bic, kw_in_bic), collapse = " + "))
  )
  modello_no_kw   <- lm(formula_no_kw, data = train_pulito)
  cat("\n--- F-test parziale: contributo variabili keyword ---\n")
  print(anova(modello_no_kw, modello_bic))
}

# --- 7.3  Significatività del fattore data_channel e weekday ---
# Usiamo drop1() con test F per valutare ciascun gruppo di dummy insieme
cat("\n--- Test F per rimozione di ciascun predittore (drop1) ---\n")
print(drop1(modello_bic, test = "F"))


## 8. [BOZZA] Validazione predittiva sul Test Set =====================================
#
# OBIETTIVO: verificare che il modello generalizzi (R² train ≈ R² test).

# --- 8.1  Predizioni sul test set ---
pred_log   <- predict(modello_bic, newdata = test_pulito)
res_log    <- test_pulito$logshares - pred_log

# --- 8.2  Metriche su scala logaritmica ---
rmse_log   <- sqrt(mean(res_log^2))
mae_log    <- mean(abs(res_log))
ss_res     <- sum(res_log^2)
ss_tot     <- sum((test_pulito$logshares - mean(test_pulito$logshares))^2)
r2_test    <- 1 - ss_res / ss_tot

cat("\n--- Performance sul Test Set (scala log) ---\n")
cat("RMSE  :", round(rmse_log, 4), "\n")
cat("MAE   :", round(mae_log,  4), "\n")
cat("R²    :", round(r2_test,  4), "\n")
cat("R² adj (train):", round(summary(modello_bic)$adj.r.squared, 4), "\n")
cat("→ Se R² test ≈ R² train, il modello non è in overfitting.\n")

# --- 8.3  Metriche su scala originale (back-transform exp) ---
pred_shares  <- exp(pred_log)
true_shares  <- exp(test_pulito$logshares)   # equivale a test_set$shares

rmse_orig    <- sqrt(mean((true_shares - pred_shares)^2))
mae_orig     <- mean(abs(true_shares - pred_shares))
mape         <- mean(abs((true_shares - pred_shares) / true_shares)) * 100

cat("\n--- Performance sul Test Set (scala shares originale) ---\n")
cat("RMSE :", round(rmse_orig, 1), "shares\n")
cat("MAE  :", round(mae_orig,  1), "shares\n")
cat("MAPE :", round(mape,      2), "%\n")
cat("Nota: RMSE e MAE in scala assoluta sono molto sensibili agli outlier estremi.\n")
cat("      Il MAPE e le metriche in scala log sono più rappresentative.\n")

# --- 8.4  Grafico valori osservati vs predetti (test set) ---
dev.new()
plot(test_pulito$logshares, pred_log,
     pch = 20, cex = 0.3, col = rgb(0.2, 0.4, 0.8, 0.4),
     main = "Osservati vs Predetti — Test Set (scala log)",
     xlab = "log(shares) osservato",
     ylab = "log(shares) predetto")
abline(0, 1, col = "red", lwd = 2)  # linea di perfetta predizione

# --- 8.5  Grafico residui test set ---
dev.new()
plot(pred_log, res_log,
     pch = 20, cex = 0.3, col = rgb(0.2, 0.4, 0.8, 0.4),
     main = "Residui sul Test Set",
     xlab = "Predetti (log)", ylab = "Residui")
abline(h = 0, col = "red", lwd = 2)