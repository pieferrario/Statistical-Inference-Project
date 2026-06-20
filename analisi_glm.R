# ==============================================================================
# Progetto di Inferenza Statistica
# Salute Percepita (TMMS) → Qualità del Sonno (binaria)
# ------------------------------------------------------------------------------
# Variante GLM di bozza_analisi.R: stessa domanda di ricerca e stessi predittori
# (perceived_healthstatus e majors come fattori NOMINALI a piu' livelli, non
# binarizzati come nel paper), ma outcome BINARIO anziche' continuo:
#
#   bozza_analisi.R      : psqiglobalscore (continuo)        -> lm()
#   QUESTO SCRIPT         : poor_sleeper (PSQI > 5, binario)   -> glm(binomial)
#   bozza_analisi.R APPX  : psqi_cat (3 livelli, soglie clin.) -> polr()
#
# Le tre versioni rispondono alla stessa domanda con gradi di informazione
# decrescenti sulla risposta (continua > 3 livelli > 2 livelli) ma assunzioni
# via via piu' semplici. Utile presentarle insieme come analisi di sensibilita'.
# ==============================================================================


## 0. Librerie --------------------------------------------------------------------
library(readxl)
library(car)      # vif() - su glm funziona in modo diretto e non ambiguo
library(MASS)     # stepAIC()
library(GGally)   # ggpairs()
library(pROC)     # curva ROC/AUC (outcome binario)
library(tidyverse)


## 1. Importazione dati -------------------------------------------------------------
dataset <- as.data.frame(read_excel("sharing dataset.xlsx", sheet = "Sheet1"))

dim(dataset)
sum(is.na(dataset))
sum(duplicated(dataset))
names(dataset) <- trimws(names(dataset))


### 1.a Gestione dei factor ---------------------------------------------------------
dataset$gender <- factor(dataset$gender, levels = c(0, 1), labels = c("Male", "Female"))

dataset$majors <- factor(dataset$majors, levels = c(0, 1, 2),
                          labels = c("MedicalLifeScience", "SocialScience", "Technology"))

# perceived_healthstatus resta PREDITTORE nominale (non ordinato), coerente con
# bozza_analisi.R: lascia che ogni livello (Fair, Poor) abbia un proprio effetto
# libero rispetto a "Good", invece di forzare un incremento costante.
dataset$perceived_healthstatus <- factor(dataset$perceived_healthstatus,
                                          levels = c(0, 1, 2),
                                          labels = c("Good", "Fair", "Poor"))

### 1.b Outcome: poor_sleeper (binario, soglia clinica standard PSQI > 5) -----------
dataset$poor_sleeper <- factor(ifelse(dataset$psqiglobalscore > 5, "Poor", "Good"),
                                levels = c("Good", "Poor"))
print(table(dataset$poor_sleeper))
print(round(prop.table(table(dataset$poor_sleeper)) * 100, 1))


### 1.c Collinearita' strutturale (solo blocco TMMS; PSQI e' ora la risposta) -------
check_tmms <- all(dataset$tmms_repair + dataset$tmms_attention + dataset$tmms_clarity
                   == dataset$tmms_totalscore)
cat("tmms_totalscore = somma delle subscale per tutte le righe?", check_tmms, "\n")


## 2. Split train/test (stesso seed delle altre versioni, per confrontabilita') -----
set.seed(123)
index <- sample(1:nrow(dataset), size = floor(0.70 * nrow(dataset)))
train_set <- dataset[index, ]
test_set  <- dataset[-index, ]
cat("Train:", nrow(train_set), "| Test:", nrow(test_set), "\n")


## 3. Analisi esplorativa (EDA) -----------------------------------------------------

### 3.a Bilanciamento della risposta (binaria, non piu' istogramma/QQ-plot) ---------
ggplot(train_set, aes(x = poor_sleeper, fill = poor_sleeper)) +
  geom_bar() +
  labs(title = "Distribuzione di poor_sleeper (Train Set)",
       x = "Qualita' del sonno", y = "Conteggio") +
  theme_minimal() + theme(legend.position = "none")

### 3.b perceived_healthstatus vs poor_sleeper (proporzioni) ------------------------
ggplot(train_set, aes(x = perceived_healthstatus, fill = poor_sleeper)) +
  geom_bar(position = "fill") +
  labs(title = "Proporzione di poor sleepers per livello di salute percepita",
       x = "Salute percepita", y = "Proporzione") +
  theme_minimal()

### 3.c Predittori continui per livello di poor_sleeper -----------------------------
vars_cont <- c("Age", "tmms_repair", "tmms_attention", "tmms_clarity")
train_long <- train_set %>%
  pivot_longer(cols = all_of(vars_cont), names_to = "variabile", values_to = "valore")

ggplot(train_long, aes(x = poor_sleeper, y = valore, fill = poor_sleeper)) +
  geom_boxplot() +
  facet_wrap(~ variabile, scales = "free_y") +
  labs(title = "Predittori continui per qualita' del sonno") +
  theme_minimal() + theme(legend.position = "none")

### 3.d Collinearita' visiva nel blocco TMMS -----------------------------------------
ggpairs(train_set[, c("tmms_repair", "tmms_attention", "tmms_clarity", "tmms_totalscore")],
        title = "Collinearita' interna al blocco TMMS")


## 4. Confronto sottomodelli: TMMS subscale vs punteggio totale ---------------------
modello_tmms_subscale <- glm(
  poor_sleeper ~ Age + gender + majors + perceived_healthstatus +
    tmms_repair + tmms_attention + tmms_clarity,
  data = train_set, family = binomial
)

modello_tmms_totale <- glm(
  poor_sleeper ~ Age + gender + majors + perceived_healthstatus +
    tmms_totalscore,
  data = train_set, family = binomial
)

cat("\n--- Confronto TMMS: subscale vs totale (AIC) ---\n")
print(AIC(modello_tmms_subscale, modello_tmms_totale))

ggplot(data.frame(modello = c("TMMS subscale", "TMMS totale"),
                   AIC = c(AIC(modello_tmms_subscale), AIC(modello_tmms_totale))),
       aes(x = modello, y = AIC, fill = modello)) +
  geom_col() + geom_text(aes(label = round(AIC, 1)), vjust = -0.3) +
  labs(title = "AIC: subscale TMMS vs punteggio totale TMMS") +
  theme_minimal() + theme(legend.position = "none")

# Manteniamo la specificazione con AIC piu' basso (default: subscale).


## 5. Modello completo ---------------------------------------------------------------
modello_completo <- modello_tmms_subscale   # adeguate se vince tmms_totale in Sez. 4
summary(modello_completo)

# p-value gia' inclusi di default nell'output di glm (a differenza di polr)


## 6. Controllo collinearita' tramite VIF ---------------------------------------------
# car::vif() e' pienamente supportato per oggetti glm, nessun workaround necessario
vif_vals <- vif(modello_completo)
print(vif_vals)

if (!is.null(dim(vif_vals))) {
  soglia <- sqrt(5)
  variabili_collineari <- rownames(vif_vals)[vif_vals[, 3] > soglia]
  vif_plot_data <- data.frame(variabile = rownames(vif_vals), VIF = vif_vals[, 3])
} else {
  soglia <- 5
  variabili_collineari <- names(vif_vals)[vif_vals > soglia]
  vif_plot_data <- data.frame(variabile = names(vif_vals), VIF = vif_vals)
}
cat("Variabili con forte collinearita' (soglia VIF equivalente a 5):\n")
print(variabili_collineari)

ggplot(vif_plot_data, aes(x = reorder(variabile, VIF), y = VIF)) +
  geom_col(fill = "steelblue") +
  geom_hline(yintercept = soglia, color = "red", linetype = "dashed") +
  coord_flip() +
  labs(title = "VIF per covariata (modello completo)", x = "", y = "VIF / GVIF^(1/2Df)") +
  theme_minimal()


## 7. Selezione del modello: stepwise AIC -----------------------------------------------
modello_step <- stepAIC(modello_completo, direction = "both", trace = FALSE)
summary(modello_step)

cat("\n--- Confronto modello completo vs modello stepwise ---\n")
print(AIC(modello_completo, modello_step))
if (length(coef(modello_step)) < length(coef(modello_completo))) {
  print(anova(modello_step, modello_completo, test = "Chisq"))  # LRT per glm annidati
}

modello_definitivo <- modello_step

### 7.a Odds Ratio del modello finale --------------------------------------------------
or_table <- exp(cbind(OR = coef(modello_definitivo), confint(modello_definitivo)))
print(round(or_table, 3))

or_df <- data.frame(variabile = rownames(or_table)[-1],
                     OR = or_table[-1, "OR"],
                     low = or_table[-1, 2], high = or_table[-1, 3])
ggplot(or_df, aes(x = reorder(variabile, OR), y = OR)) +
  geom_point(size = 3, color = "darkred") +
  geom_errorbar(aes(ymin = low, ymax = high), width = 0.2) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  coord_flip() +
  labs(title = "Odds Ratio del modello finale", x = "", y = "Odds Ratio (IC 95%)") +
  theme_minimal()

# McFadden pseudo-R^2 (vedi discussione precedente su summary(glm) e R^2)
mcfadden_r2 <- 1 - modello_definitivo$deviance / modello_definitivo$null.deviance
cat("McFadden pseudo-R^2:", round(mcfadden_r2, 3), "\n")


## 8. Validazione predittiva sul test set -------------------------------------------------
prob_test <- predict(modello_definitivo, newdata = test_set, type = "response")
pred_test <- factor(ifelse(prob_test > 0.5, "Poor", "Good"), levels = c("Good", "Poor"))

matrice_confusione <- table(Osservato = test_set$poor_sleeper, Predetto = pred_test)
print(matrice_confusione)

accuracy_test <- sum(diag(matrice_confusione)) / sum(matrice_confusione)
baseline_accuracy <- max(table(test_set$poor_sleeper)) / nrow(test_set)
cat("Accuracy sul test set:", round(accuracy_test, 3),
    " | baseline:", round(baseline_accuracy, 3), "\n")

# Kappa pesato (qui equivale al Cohen's kappa classico, essendo l'outcome binario)
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
kw <- kappa_pesato(test_set$poor_sleeper, pred_test, levels(test_set$poor_sleeper))
cat("Kappa (Cohen) sul test set:", round(kw, 3), "\n")

# Curva ROC e AUC
roc_obj <- roc(response = test_set$poor_sleeper, predictor = prob_test,
               levels = c("Good", "Poor"), quiet = TRUE)
auc_val <- auc(roc_obj)
cat("AUC sul test set:", round(as.numeric(auc_val), 3), "\n")
plot(roc_obj, main = paste0("Curva ROC (AUC = ", round(as.numeric(auc_val), 3), ")"),
     col = "darkblue", lwd = 2)

conf_df <- as.data.frame(matrice_confusione)
ggplot(conf_df, aes(x = Predetto, y = Osservato, fill = Freq)) +
  geom_tile() + geom_text(aes(label = Freq), color = "white", size = 6) +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Matrice di Confusione - Test Set") +
  theme_minimal()


## 9. Cross-validation (10-fold, sul train set) ---------------------------------------
set.seed(123)
k <- 10
folds <- sample(rep(1:k, length.out = nrow(train_set)))
formula_finale <- formula(modello_definitivo)

cv_accuracy <- numeric(k)
cv_auc      <- numeric(k)

for (i in 1:k) {
  train_cv <- train_set[folds != i, ]
  test_cv  <- train_set[folds == i, ]

  mod_cv  <- glm(formula_finale, data = train_cv, family = binomial)
  prob_cv <- predict(mod_cv, newdata = test_cv, type = "response")
  pred_cv <- factor(ifelse(prob_cv > 0.5, "Poor", "Good"), levels = c("Good", "Poor"))

  cv_accuracy[i] <- mean(pred_cv == test_cv$poor_sleeper)
  cv_auc[i] <- as.numeric(auc(roc(response = test_cv$poor_sleeper, predictor = prob_cv,
                                   levels = c("Good", "Poor"), quiet = TRUE)))
}

cat("Accuracy per fold:\n"); print(round(cv_accuracy, 3))
cat("Accuracy media (CV):", round(mean(cv_accuracy), 3),
    " - SD:", round(sd(cv_accuracy), 3), "\n")
cat("AUC media (CV):", round(mean(cv_auc), 3), "\n")

cv_df <- data.frame(fold = factor(1:k), AUC = cv_auc)
ggplot(cv_df, aes(x = "", y = AUC)) +
  geom_boxplot(fill = "lightgreen", outlier.shape = NA) +
  geom_jitter(width = 0.05, size = 2) +
  labs(title = paste0(k, "-fold Cross-Validation: AUC (Train Set)"), x = "") +
  theme_minimal()


## 10. Riepilogo finale -----------------------------------------------------------------
cat("\n========== RIEPILOGO FINALE ==========\n")
cat("Formula modello definitivo:\n"); print(formula_finale)
cat("\nAccuracy test set:", round(accuracy_test, 3),
    " | baseline:", round(baseline_accuracy, 3), "\n")
cat("AUC test set:", round(as.numeric(auc_val), 3), "\n")
cat("Kappa test set:", round(kw, 3), "\n")
cat("McFadden pseudo-R^2 (su train):", round(mcfadden_r2, 3), "\n")
cat("Accuracy media CV:", round(mean(cv_accuracy), 3),
    " | AUC media CV:", round(mean(cv_auc), 3), "\n")
