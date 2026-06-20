# ==============================================================================
# Progetto di Inferenza Statistica
# Stato di Salute Percepito (TMMS, PSQI-componenti) → Qualità del Sonno
# ------------------------------------------------------------------------------
# MODIFICHE RISPETTO ALLO SCRIPT ORIGINALE (segnalate con [MOD]):
#
# [MOD-1]  RISPOSTA: psqiglobalscore (intero 0–21) al posto di
#           perceived_healthstatus (ordinale a 3 livelli).
#           Scelta modellistica: psqiglobalscore è trattata come continua →
#           regressione lineare (lm). La sezione 4-BIS offre anche la versione
#           categorizzata (Good/Fair/Poor secondo soglie PSQI standard) con polr,
#           così il confronto AIC è possibile su entrambe le specifiche.
#
# [MOD-2]  PREDITTORI: perceived_healthstatus diventa predittore (nominale),
#           psqiglobalscore e i 7 componenti PSQI escono dal lato predittori
#           (sarebbero strutturalmente collineari con la risposta).
#
# [MOD-3]  COLLINEARITÀ STRUTTURALE: rimane solo il blocco TMMS
#           (subscale vs totale). Il blocco PSQI è ora la risposta, sparisce.
#
# [MOD-4]  SEZIONE 4: confronto AIC su un solo caso (TMMS subscale vs totale),
#           non due come nell'originale.
#
# [MOD-5]  BUG CORRETTO: car::vif() non accetta oggetti polr.
#           Per lm() funziona direttamente. Se si usa polr (sezione 4-BIS)
#           il VIF viene calcolato su un lm() ausiliario con gli stessi predittori.
#
# [MOD-6]  METRICHE DI VALUTAZIONE: accuracy e kappa pesato (metriche per
#           classificazione) sostituite con RMSE e R² (metriche per regressione).
#           La sezione 4-BIS mantiene accuracy + kappa se si sceglie polr.
#
# [MOD-7]  stepAIC() funziona sia su lm che su polr → nessuna modifica necessaria.
# ==============================================================================


## 0. Librerie -----------------------------------------------------------------
library(readxl)
library(car)      # vif()
library(MASS)     # polr(), stepAIC()
library(GGally)   # ggpairs()
library(tidyverse)


## 1. Importazione dati ---------------------------------------------------------
dataset <- read_excel("sharing dataset.xlsx", sheet = "Sheet1")
dataset <- as.data.frame(dataset)

dim(dataset)
head(dataset)
summary(dataset)

### 1.a Qualità: NA, duplicati, tipi
sum(is.na(dataset))
sum(duplicated(dataset))
str(dataset)
names(dataset) <- trimws(names(dataset))


### 1.b Gestione dei factor
# [MOD-2] perceived_healthstatus è ora PREDITTORE nominale (non ordinale):
#         usiamo factor() senza ordered = TRUE, così lm() crea le dummy corrette.
dataset$gender <- factor(dataset$gender, levels = c(0, 1),
                         labels = c("Male", "Female"))

dataset$majors <- factor(dataset$majors, levels = c(0, 1, 2),
                         labels = c("MedicalLifeScience", "SocialScience", "Technology"))

dataset$perceived_healthstatus <- factor(dataset$perceived_healthstatus,
                                         levels = c(0, 1, 2),
                                         labels = c("Good", "Fair", "Poor"))
# "Good" è la categoria di riferimento (livello base) nelle dummy.


### 1.c Collinearità strutturale
# [MOD-3] Rimane solo la verifica TMMS; il blocco PSQI è la risposta.
check_tmms <- all(dataset$tmms_repair + dataset$tmms_attention + dataset$tmms_clarity
                  == dataset$tmms_totalscore)
cat("tmms_totalscore = somma delle subscale per tutte le righe?", check_tmms, "\n")
# → TRUE: includere subscale + totale nello stesso modello produrrebbe rank deficiency.


## 2. Split train/test -----------------------------------------------------------
set.seed(123)
index <- sample(1:nrow(dataset), size = floor(0.70 * nrow(dataset)))
train_set <- dataset[index, ]
test_set  <- dataset[-index, ]

cat("Train:", nrow(train_set), "| Test:", nrow(test_set), "\n")


## 3. Analisi esplorativa (EDA) --------------------------------------------------

### 3.a Distribuzione della risposta (continua)
# [MOD-1] Istogramma di psqiglobalscore (0 = ottimo, 21 = pessimo).
ggplot(train_set, aes(x = psqiglobalscore)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
  labs(title = "Distribuzione di psqiglobalscore (Train Set)",
       x = "PSQI Global Score (0–21)", y = "Conteggio") +
  theme_minimal()

# Q-Q plot: verifica approssimativa di normalità (rilevante per i residui di lm)
qqnorm(train_set$psqiglobalscore, main = "Q-Q plot: psqiglobalscore")
qqline(train_set$psqiglobalscore, col = "red")


### 3.b Risposta per livello di salute percepita
ggplot(train_set, aes(x = perceived_healthstatus, y = psqiglobalscore,
                      fill = perceived_healthstatus)) +
  geom_boxplot() +
  labs(title = "PSQI Global Score per stato di salute percepita",
       x = "Salute percepita", y = "PSQI Global Score") +
  theme_minimal() + theme(legend.position = "none")


### 3.c Collinearità visiva nel blocco TMMS
ggpairs(train_set[, c("tmms_repair", "tmms_attention", "tmms_clarity", "tmms_totalscore")],
        title = "Collinearità interna al blocco TMMS")


### 3.d Scatter: predittori continui vs psqiglobalscore
vars_cont <- c("Age", "tmms_repair", "tmms_attention", "tmms_clarity")

train_long <- train_set %>%
  pivot_longer(cols = all_of(vars_cont), names_to = "variabile", values_to = "valore")

ggplot(train_long, aes(x = valore, y = psqiglobalscore)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  facet_wrap(~ variabile, scales = "free_x") +
  labs(title = "Predittori continui vs PSQI Global Score") +
  theme_minimal()


## 4. Confronto sottomodelli: subscale TMMS vs punteggio totale ------------------
# [MOD-4] Un solo confronto (TMMS); la risposta è psqiglobalscore (continua → lm).

modello_tmms_subscale <- lm(
  psqiglobalscore ~ Age + gender + majors + perceived_healthstatus +
    tmms_repair + tmms_attention + tmms_clarity,
  data = train_set
)

modello_tmms_totale <- lm(
  psqiglobalscore ~ Age + gender + majors + perceived_healthstatus +
    tmms_totalscore,
  data = train_set
)

cat("\n--- Confronto TMMS: subscale vs totale (AIC) ---\n")
print(AIC(modello_tmms_subscale, modello_tmms_totale))

ggplot(data.frame(modello = c("TMMS subscale", "TMMS totale"),
                  AIC = c(AIC(modello_tmms_subscale), AIC(modello_tmms_totale))),
       aes(x = modello, y = AIC, fill = modello)) +
  geom_col() +
  geom_text(aes(label = round(AIC, 1)), vjust = -0.3) +
  labs(title = "AIC: subscale TMMS vs punteggio totale TMMS") +
  theme_minimal() + theme(legend.position = "none")

# Manteniamo la specificazione con AIC più basso.
# Per default usiamo "subscale"; se nel vostro run vince "totale", cambiate qui.


## 5. Modello completo -----------------------------------------------------------
modello_completo <- modello_tmms_subscale   # adeguate se vince tmms_totale in Sez.4
summary(modello_completo)


## 6. Controllo collinearità tramite VIF -----------------------------------------
# [MOD-5] lm() è accettato direttamente da car::vif(); nessun workaround necessario.
vif_vals <- vif(modello_completo)
print(vif_vals)

# car restituisce una matrice quando ci sono factor con Df > 1 (GVIF, Df, GVIF^(1/(2Df)))
if (!is.null(dim(vif_vals))) {
  soglia <- sqrt(5)
  variabili_collineari <- rownames(vif_vals)[vif_vals[, 3] > soglia]
  vif_plot_data <- data.frame(variabile = rownames(vif_vals), VIF = vif_vals[, 3])
} else {
  soglia <- 5
  variabili_collineari <- names(vif_vals)[vif_vals > soglia]
  vif_plot_data <- data.frame(variabile = names(vif_vals), VIF = vif_vals)
}

cat("Variabili con forte collinearità (soglia VIF equivalente a 5):\n")
print(variabili_collineari)

ggplot(vif_plot_data, aes(x = reorder(variabile, VIF), y = VIF)) +
  geom_col(fill = "steelblue") +
  geom_hline(yintercept = soglia, color = "red", linetype = "dashed") +
  coord_flip() +
  labs(title = "VIF per covariata (modello completo)",
       x = "", y = "VIF / GVIF^(1/2Df)") +
  theme_minimal()


## 7. Selezione del modello: stepwise AIC -----------------------------------------
modello_step <- stepAIC(modello_completo, direction = "both", trace = FALSE)
summary(modello_step)

cat("\n--- Confronto modello completo vs modello stepwise ---\n")
print(AIC(modello_completo, modello_step))
# Nota: anova() per il test LR richiede modelli annidati; stepAIC può rimuovere
# predittori non annidabili facilmente. Se i modelli sono annidati:
if (length(coef(modello_step)) < length(coef(modello_completo))) {
  print(anova(modello_step, modello_completo))  # F-test per lm annidati
}

modello_definitivo <- modello_step

### 7.a Coefficienti standardizzati (interpretazione relativa delle variabili)
coef_std <- coef(modello_definitivo)[-1]  # escludi intercetta
coef_df  <- data.frame(variabile = names(coef_std), coefficiente = coef_std)

ggplot(coef_df, aes(x = reorder(variabile, coefficiente), y = coefficiente)) +
  geom_point(size = 3, color = "darkred") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  coord_flip() +
  labs(title = "Coefficienti del modello finale",
       subtitle = "Coeff > 0: peggiore qualità del sonno (PSQI più alto)",
       x = "", y = "Coefficiente") +
  theme_minimal()


## 8. Diagnostica dei residui ---------------------------------------------------
par(mfrow = c(2, 2))
plot(modello_definitivo)
par(mfrow = c(1, 1))


## 9. Validazione predittiva sul test set ----------------------------------------
# [MOD-6] Metriche per regressione: RMSE e R² al posto di accuracy e kappa.

pred_test <- predict(modello_definitivo, newdata = test_set)

rmse_test <- sqrt(mean((test_set$psqiglobalscore - pred_test)^2))
ss_res    <- sum((test_set$psqiglobalscore - pred_test)^2)
ss_tot    <- sum((test_set$psqiglobalscore - mean(test_set$psqiglobalscore))^2)
r2_test   <- 1 - ss_res / ss_tot

cat("RMSE sul test set:", round(rmse_test, 3), "\n")
cat("R² sul test set:  ", round(r2_test,   3), "\n")

# Baseline: modello costante (media del train)
rmse_baseline <- sqrt(mean((test_set$psqiglobalscore - mean(train_set$psqiglobalscore))^2))
cat("RMSE baseline (predire sempre la media del train):", round(rmse_baseline, 3), "\n")

# Scatter osservato vs predetto
ggplot(data.frame(osservato = test_set$psqiglobalscore, predetto = pred_test),
       aes(x = predetto, y = osservato)) +
  geom_point(alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Test Set: osservato vs predetto",
       x = "PSQI predetto", y = "PSQI osservato") +
  theme_minimal()


## 10. Cross-validation k-fold (solo sul train set) ------------------------------
set.seed(123)
k     <- 10
folds <- sample(rep(1:k, length.out = nrow(train_set)))

formula_finale <- formula(modello_definitivo)

cv_rmse <- numeric(k)
cv_r2   <- numeric(k)

for (i in 1:k) {
  train_cv <- train_set[folds != i, ]
  test_cv  <- train_set[folds == i, ]
  
  mod_cv  <- lm(formula_finale, data = train_cv)
  pred_cv <- predict(mod_cv, newdata = test_cv)
  
  cv_rmse[i] <- sqrt(mean((test_cv$psqiglobalscore - pred_cv)^2))
  ss_r  <- sum((test_cv$psqiglobalscore - pred_cv)^2)
  ss_t  <- sum((test_cv$psqiglobalscore - mean(test_cv$psqiglobalscore))^2)
  cv_r2[i]   <- 1 - ss_r / ss_t
}

cat("RMSE per fold:\n"); print(round(cv_rmse, 3))
cat("RMSE medio (CV):", round(mean(cv_rmse), 3),
    " - SD:", round(sd(cv_rmse), 3), "\n")
cat("R² medio (CV):  ", round(mean(cv_r2), 3), "\n")

cv_df <- data.frame(fold = factor(1:k), RMSE = cv_rmse)
ggplot(cv_df, aes(x = "", y = RMSE)) +
  geom_boxplot(fill = "lightgreen", outlier.shape = NA) +
  geom_jitter(width = 0.05, size = 2) +
  geom_hline(yintercept = rmse_baseline, color = "red", linetype = "dashed") +
  labs(title = paste0(k, "-fold Cross-Validation: RMSE (Train Set)"),
       subtitle = "Linea rossa = baseline (predire sempre la media)",
       x = "", y = "RMSE") +
  theme_minimal()


## 11. Riepilogo finale ---------------------------------------------------------
cat("\n========== RIEPILOGO FINALE ==========\n")
cat("Formula modello definitivo:\n"); print(formula_finale)
cat("\nRMSE test set:", round(rmse_test, 3),
    " | baseline:", round(rmse_baseline, 3), "\n")
cat("R² test set:   ", round(r2_test, 3), "\n")
cat("RMSE medio CV (", k, "-fold, train set):", round(mean(cv_rmse), 3), "\n")
cat("R² medio CV:   ", round(mean(cv_r2), 3), "\n")


# ==============================================================================
# APPENDICE – Versione con risposta ORDINALE (polr) usando soglie PSQI standard
# ------------------------------------------------------------------------------
# Se preferite mantenere polr per coerenza con l'impostazione originale,
# categorizzate psqiglobalscore in 3 classi secondo le soglie standard PSQI:
#   0–4  → "Good"  (qualità del sonno buona)
#   5–10 → "Fair"  (qualità discreta, attenzione clinica consigliata)
#   11–21→ "Poor"  (qualità del sonno cattiva)
# e sostituite lm() con polr() ovunque.
# ==============================================================================

dataset$psqi_cat <- cut(dataset$psqiglobalscore,
                        breaks = c(-Inf, 4, 10, Inf),
                        labels = c("Good", "Fair", "Poor"),
                        ordered_result = TRUE)

train_set$psqi_cat <- dataset$psqi_cat[index]
test_set$psqi_cat  <- dataset$psqi_cat[-index]

print(table(train_set$psqi_cat))

modello_polr_subscale <- polr(
  psqi_cat ~ Age + gender + majors + perceived_healthstatus +
    tmms_repair + tmms_attention + tmms_clarity,
  data = train_set, Hess = TRUE
)

modello_polr_totale <- polr(
  psqi_cat ~ Age + gender + majors + perceived_healthstatus +
    tmms_totalscore,
  data = train_set, Hess = TRUE
)

cat("\n--- Confronto TMMS (polr su psqi_cat): subscale vs totale ---\n")
print(AIC(modello_polr_subscale, modello_polr_totale))

# p-value per polr (non forniti di default)
aggiungi_pvalue <- function(modello) {
  ct <- coef(summary(modello))
  p  <- pnorm(abs(ct[, "t value"]), lower.tail = FALSE) * 2
  cbind(ct, "p value" = p)
}

modello_polr_step <- stepAIC(modello_polr_subscale, direction = "both", trace = FALSE)
print(aggiungi_pvalue(modello_polr_step))

# [MOD-5 per polr] VIF su lm ausiliario con stessi predittori
formula_ausiliaria <- update(formula(modello_polr_step), as.numeric(psqi_cat) ~ .)
lm_ausiliario <- lm(formula_ausiliaria, data = train_set)
print(vif(lm_ausiliario))

# Valutazione sul test set (polr)
pred_polr <- predict(modello_polr_step, newdata = test_set, type = "class")
mc <- table(Osservato = test_set$psqi_cat, Predetto = pred_polr)
print(mc)
cat("Accuracy polr test set:", round(sum(diag(mc)) / sum(mc), 3), "\n")