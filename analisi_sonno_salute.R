# ==============================================================================
# Progetto di Inferenza Statistica
# Sonno (PSQI), Regolazione Emotiva (TMMS) e Stato di Salute Percepito
# ------------------------------------------------------------------------------
# Adattato dallo script originale (OnlineNewsPopularity) allo stesso flusso di
# lavoro, con le modifiche necessarie segnalate nei commenti con [MODIFICA].
# ==============================================================================

## 0. Importazione delle librerie ---------------------------------------------
library(readxl)   # [MODIFICA] lettura file .xlsx (l'originale usava read.csv)
library(car)      # vif()
library(MASS)     # polr(), stepAIC()
library(GGally)   # ggpairs()
library(tidyverse)  # dplyr, tidyr, ggplot2

# [MODIFICA] Librerie dell'originale non usate nel codice visibile (rgl, leaps,
# faraway, ellipse) sono state omesse. Aggiungetele pure se vi servono altrove.


## 1. Importazione dati ---------------------------------------------------------
dataset <- read_excel("sharing_dataset.xlsx", sheet = "Sheet1")
dataset <- as.data.frame(dataset)

dim(dataset)
head(dataset)
summary(dataset)

### 1.a Controlli di qualita': NA, duplicati, tipi -------------------------------
sum(is.na(dataset))
sum(duplicated(dataset))
str(dataset)

# Pulizia difensiva dei nomi colonna (come nell'originale, qui non risultano
# spazi spuri ma manteniamo il controllo per sicurezza)
names(dataset) <- trimws(names(dataset))


### 1.b Gestione dei factor -------------------------------------------------------
# gender e majors sono nominali; perceived_healthstatus e' la risposta ORDINALE
dataset$gender <- factor(dataset$gender, levels = c(0, 1),
                          labels = c("Male", "Female"))

dataset$majors <- factor(dataset$majors, levels = c(0, 1, 2),
                          labels = c("MedicalLifeScience", "SocialScience", "Technology"))

dataset$perceived_healthstatus <- factor(dataset$perceived_healthstatus,
                                          levels = c(0, 1, 2),
                                          labels = c("Good", "Fair", "Poor"),
                                          ordered = TRUE)

dim(dataset)
str(dataset)


### 1.c Collinearita' "strutturale" esplicita -------------------------------------
# [MODIFICA] Qui non c'e' un solo caso come weekday/is_weekend, ma due:
# a) tmms_totalscore e' la somma ESATTA delle 3 subscale TMMS
# b) psqiglobalscore e' la somma ESATTA dei 7 componenti PSQI
# Includere totale + componenti nello stesso modello produrrebbe collinearita'
# PERFETTA (rank deficiency, coefficienti NA, come accadeva con weekdaySunday
# nello script originale). Verifichiamolo esplicitamente:

check_tmms <- all(dataset$tmms_repair + dataset$tmms_attention + dataset$tmms_clarity
                   == dataset$tmms_totalscore)
cat("tmms_totalscore = somma delle subscale per tutte le righe?", check_tmms, "\n")

psqi_componenti <- c("Subjectivesleepquality", "Sleeplatency", "Sleepduration",
                      "Sleepefficiency", "Sleepdisturbance", "Useofsleepmedication",
                      "Daytimedysfunction")

check_psqi <- all(rowSums(dataset[, psqi_componenti]) == dataset$psqiglobalscore)
cat("psqiglobalscore = somma dei componenti per tutte le righe?", check_psqi, "\n")

# Procederemo quindi confrontando le due specificazioni alternative (Sez. 4)
# tramite AIC, esattamente come fatto nello script originale per weekday/is_weekend.


## 2. Split train/test -----------------------------------------------------------
set.seed(123)
percentuale_train <- 0.70
index <- sample(1:nrow(dataset), size = floor(percentuale_train * nrow(dataset)))

train_set <- dataset[index, ]
test_set  <- dataset[-index, ]

cat("Dimensioni Train Set:", nrow(train_set), "osservazioni\n")
cat("Dimensioni Test Set:", nrow(test_set), "osservazioni\n")


## 3. Analisi esplorativa (EDA) ---------------------------------------------------

### 3.a Bilanciamento della variabile risposta ------------------------------------
# [MODIFICA] Al posto di istogramma/Box-Cox su una risposta continua,
# verifichiamo il bilanciamento delle classi (rilevante per un outcome categoriale)
print(table(train_set$perceived_healthstatus))
print(prop.table(table(train_set$perceived_healthstatus)))

ggplot(train_set, aes(x = perceived_healthstatus, fill = perceived_healthstatus)) +
  geom_bar() +
  labs(title = "Distribuzione di perceived_healthstatus (Train Set)",
       x = "Stato di salute percepito", y = "Conteggio") +
  theme_minimal() + theme(legend.position = "none")

### 3.b Predittori continui per livello della risposta ----------------------------
vars_continue <- c("Age", "tmms_repair", "tmms_attention", "tmms_clarity",
                    psqi_componenti, "psqiglobalscore")

train_long <- train_set %>%
  pivot_longer(cols = all_of(vars_continue), names_to = "variabile", values_to = "valore")

ggplot(train_long, aes(x = perceived_healthstatus, y = valore, fill = perceived_healthstatus)) +
  geom_boxplot() +
  facet_wrap(~ variabile, scales = "free_y") +
  labs(title = "Predittori continui per livello di salute percepita") +
  theme_minimal() + theme(legend.position = "none")

### 3.c Collinearita' visiva nei due blocchi (TMMS e PSQI) ------------------------
ggpairs(train_set[, c("tmms_repair", "tmms_attention", "tmms_clarity", "tmms_totalscore")],
        title = "Collinearita' interna al blocco TMMS")

ggpairs(train_set[, c(psqi_componenti, "psqiglobalscore")],
        title = "Collinearita' interna al blocco PSQI")


## 4. Confronto sottomodelli: scelta della granularita' (potere informativo) -----
# [MODIFICA] Stessa logica del confronto weekday/is_weekend dell'originale,
# applicata qui due volte (TMMS e PSQI) perche' i casi di collinearita' sono due.

### 4.a TMMS: subscale vs punteggio totale (PSQI fissato a "componenti") ---------
modello_tmms_subscale <- polr(
  perceived_healthstatus ~ Age + gender + majors +
    tmms_repair + tmms_attention + tmms_clarity +
    Subjectivesleepquality + Sleeplatency + Sleepduration +
    Sleepefficiency + Sleepdisturbance + Useofsleepmedication + Daytimedysfunction,
  data = train_set, Hess = TRUE
)

modello_tmms_totale <- polr(
  perceived_healthstatus ~ Age + gender + majors +
    tmms_totalscore +
    Subjectivesleepquality + Sleeplatency + Sleepduration +
    Sleepefficiency + Sleepdisturbance + Useofsleepmedication + Daytimedysfunction,
  data = train_set, Hess = TRUE
)

cat("\n--- Confronto TMMS: subscale vs totale ---\n")
print(AIC(modello_tmms_subscale, modello_tmms_totale))

ggplot(data.frame(modello = c("TMMS subscale", "TMMS totale"),
                   AIC = c(AIC(modello_tmms_subscale), AIC(modello_tmms_totale))),
       aes(x = modello, y = AIC, fill = modello)) +
  geom_col() + geom_text(aes(label = round(AIC, 1)), vjust = -0.3) +
  labs(title = "AIC: subscale TMMS vs punteggio totale TMMS") +
  theme_minimal() + theme(legend.position = "none")

# Manteniamo la specificazione con AIC piu' basso. Se nel vostro run risultasse
# "totale" preferibile, invertite di conseguenza la formula in Sez. 5.

### 4.b PSQI: componenti vs punteggio totale (TMMS fissato secondo 4.a) ----------
modello_psqi_componenti <- modello_tmms_subscale  # stessa specificazione del 4.a

modello_psqi_totale <- polr(
  perceived_healthstatus ~ Age + gender + majors +
    tmms_repair + tmms_attention + tmms_clarity +
    psqiglobalscore,
  data = train_set, Hess = TRUE
)

cat("\n--- Confronto PSQI: componenti vs totale ---\n")
print(AIC(modello_psqi_componenti, modello_psqi_totale))

ggplot(data.frame(modello = c("PSQI componenti", "PSQI totale"),
                   AIC = c(AIC(modello_psqi_componenti), AIC(modello_psqi_totale))),
       aes(x = modello, y = AIC, fill = modello)) +
  geom_col() + geom_text(aes(label = round(AIC, 1)), vjust = -0.3) +
  labs(title = "AIC: componenti PSQI vs punteggio totale PSQI") +
  theme_minimal() + theme(legend.position = "none")


## 5. Modello completo (specificazione vincente del punto 4) ---------------------
# Specificazione di default sotto = quella granulare (subscale + componenti),
# coerente con l'esito atteso del confronto AIC. Modificate qui se nel vostro
# run l'esito fosse diverso.

modello_completo <- polr(
  perceived_healthstatus ~ Age + gender + majors +
    tmms_repair + tmms_attention + tmms_clarity +
    Subjectivesleepquality + Sleeplatency + Sleepduration +
    Sleepefficiency + Sleepdisturbance + Useofsleepmedication + Daytimedysfunction,
  data = train_set, Hess = TRUE
)
summary(modello_completo)

# [MODIFICA] polr() non restituisce i p-value di default: li calcoliamo a mano
aggiungi_pvalue <- function(modello) {
  ct <- coef(summary(modello))
  p  <- pnorm(abs(ct[, "t value"]), lower.tail = FALSE) * 2
  cbind(ct, "p value" = p)
}
print(aggiungi_pvalue(modello_completo))


## 6. Controllo collinearita' tramite VIF -----------------------------------------
vif_model <- vif(modello_completo)
print(vif_model)

if (!is.null(dim(vif_model))) {
  # CASO MATRICE: ci sono factor con Df > 1 (es. majors a 3 livelli).
  # car restituisce GVIF, Df, GVIF^(1/(2*Df)); confrontiamo l'ultima colonna a sqrt(5)
  soglia <- sqrt(5)
  variabili_collineari <- rownames(vif_model)[vif_model[, 3] > soglia]
  vif_plot_data <- data.frame(variabile = rownames(vif_model), VIF = vif_model[, 3])
} else {
  soglia <- 5
  variabili_collineari <- names(vif_model)[vif_model > soglia]
  vif_plot_data <- data.frame(variabile = names(vif_model), VIF = vif_model)
}

cat("Variabili con forte collinearita' (soglia equivalente a VIF > 5):\n")
print(variabili_collineari)

ggplot(vif_plot_data, aes(x = reorder(variabile, VIF), y = VIF)) +
  geom_col(fill = "steelblue") +
  geom_hline(yintercept = soglia, color = "red", linetype = "dashed") +
  coord_flip() +
  labs(title = "VIF per covariata (modello completo)",
       x = "", y = "VIF / GVIF^(1/2Df)") +
  theme_minimal()

# Se variabili_collineari non e' vuoto, rimuovetele qui prima di proseguire:
# train_set_pulito <- train_set[, !(names(train_set) %in% variabili_collineari)]
# e ricostruite modello_completo su train_set_pulito.


## 7. Selezione del modello: stepwise AIC -----------------------------------------
modello_step <- stepAIC(modello_completo, direction = "both", trace = FALSE)
summary(modello_step)
print(aggiungi_pvalue(modello_step))

cat("\n--- Confronto modello completo vs modello stepwise ---\n")
print(AIC(modello_completo, modello_step))
print(anova(modello_completo, modello_step))  # test rapporto di verosimiglianza (modelli annidati)

modello_definitivo <- modello_step

### 7.a Visualizzazione degli Odds Ratio del modello finale -----------------------
or_table <- exp(coef(modello_definitivo))
or_df <- data.frame(variabile = names(or_table), OR = or_table)

ggplot(or_df, aes(x = reorder(variabile, OR), y = OR)) +
  geom_point(size = 3, color = "darkred") +
  geom_hline(yintercept = 1, linetype = "dashed") +
  coord_flip() +
  labs(title = "Odds Ratio del modello finale (proportional-odds)",
       subtitle = "OR > 1: maggiore probabilita' di stato di salute peggiore",
       x = "", y = "Odds Ratio (exp(coefficiente))") +
  theme_minimal()


## 8. Validazione predittiva sul test set ------------------------------------------
# [MODIFICA] Sezione assente nello script originale: aggiunta come richiesto.

pred_class <- predict(modello_definitivo, newdata = test_set, type = "class")

matrice_confusione <- table(Osservato = test_set$perceived_healthstatus, Predetto = pred_class)
print(matrice_confusione)

accuracy_test <- sum(diag(matrice_confusione)) / sum(matrice_confusione)
cat("Accuracy sul test set:", round(accuracy_test, 3), "\n")

baseline_accuracy <- max(table(test_set$perceived_healthstatus)) / nrow(test_set)
cat("Accuracy baseline (predire sempre la classe maggioritaria):", round(baseline_accuracy, 3), "\n")

# Kappa pesato (quadratico): adatto a outcome ORDINALI, penalizza meno gli errori
# "vicini" (es. Good->Fair) rispetto a errori "lontani" (es. Good->Poor)
kappa_pesato <- function(obs, pred, livelli) {
  obs_num  <- as.numeric(factor(obs,  levels = livelli))
  pred_num <- as.numeric(factor(pred, levels = livelli))
  n <- length(livelli)
  w <- outer(1:n, 1:n, function(i, j) ((i - j) / (n - 1))^2)
  O <- table(factor(obs_num, levels = 1:n), factor(pred_num, levels = 1:n))
  O <- O / sum(O)
  r <- rowSums(O); cc <- colSums(O)
  E <- outer(r, cc)
  1 - sum(w * O) / sum(w * E)
}

kw <- kappa_pesato(test_set$perceived_healthstatus, pred_class,
                    levels(test_set$perceived_healthstatus))
cat("Kappa pesato (quadratico) sul test set:", round(kw, 3), "\n")

# Heatmap della matrice di confusione
conf_df <- as.data.frame(matrice_confusione)
ggplot(conf_df, aes(x = Predetto, y = Osservato, fill = Freq)) +
  geom_tile() +
  geom_text(aes(label = Freq), color = "white", size = 5) +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Matrice di Confusione - Test Set") +
  theme_minimal()


## 9. Cross-validation (k-fold) -----------------------------------------------------
# [MODIFICA] Sezione assente nello script originale: aggiunta come richiesto.
# Eseguita SOLO sul train_set, cosi' il test_set resta una verifica realmente
# "fuori campione" del modello selezionato.

set.seed(123)
k <- 10
folds <- sample(rep(1:k, length.out = nrow(train_set)))

formula_finale <- formula(modello_definitivo)

cv_accuracy <- numeric(k)
cv_kappa    <- numeric(k)

for (i in 1:k) {
  train_cv <- train_set[folds != i, ]
  test_cv  <- train_set[folds == i, ]

  modello_cv <- polr(formula_finale, data = train_cv, Hess = TRUE)
  pred_cv    <- predict(modello_cv, newdata = test_cv, type = "class")

  cv_accuracy[i] <- mean(pred_cv == test_cv$perceived_healthstatus)
  cv_kappa[i]    <- kappa_pesato(test_cv$perceived_healthstatus, pred_cv,
                                  levels(test_cv$perceived_healthstatus))
}

cat("Accuracy per ciascun fold:\n"); print(round(cv_accuracy, 3))
cat("Accuracy media (CV):", round(mean(cv_accuracy), 3),
    " - Deviazione standard:", round(sd(cv_accuracy), 3), "\n")
cat("Kappa pesato medio (CV):", round(mean(cv_kappa), 3), "\n")

cv_df <- data.frame(fold = factor(1:k), accuracy = cv_accuracy)
ggplot(cv_df, aes(x = "", y = accuracy)) +
  geom_boxplot(fill = "lightgreen", outlier.shape = NA) +
  geom_jitter(width = 0.05, size = 2) +
  geom_hline(yintercept = baseline_accuracy, color = "red", linetype = "dashed") +
  labs(title = paste0(k, "-fold Cross-Validation: Accuracy (sul Train Set)"),
       subtitle = "Linea rossa = baseline (classe maggioritaria)",
       x = "", y = "Accuracy") +
  theme_minimal()


## 10. Riepilogo finale ---------------------------------------------------------
cat("\n========== RIEPILOGO FINALE ==========\n")
cat("Formula modello definitivo:\n"); print(formula_finale)
cat("\nAccuracy test set:", round(accuracy_test, 3),
    " | baseline:", round(baseline_accuracy, 3), "\n")
cat("Kappa pesato test set:", round(kw, 3), "\n")
cat("Accuracy media CV (", k, "-fold, train set):", round(mean(cv_accuracy), 3), "\n")
cat("Kappa pesato medio CV:", round(mean(cv_kappa), 3), "\n")
