# ==============================================================================
# Progetto di Inferenza Statistica
# Autori: Matteo Coccia, Cecilia Cotti, Pietro Ferrario, Ian Alexander Fontana Rava
# ==============================================================================

## 0. Librerie --------------------------------------------------------------------
library(readxl)
library(car)      # vif() - su glm funziona in modo diretto e non ambiguo
library(MASS)     # stepAIC()
library(GGally)   # ggpairs()
library(pROC)     # curva ROC/AUC (outcome binario)
library(tidyverse)

# --- DEFINIZIONE PALETTE COLORI (Midnight & Clinical) ---
pal_midnight <- c("Good" = "blue", "Poor" = "brown3")
col_neutral  <- "lightblue"
col_strutt   <- "blue"
col_alert    <- "brown3"
# --------------------------------------------------------

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
dataset$perceived_healthstatus <- factor(dataset$perceived_healthstatus,
                                         levels = c(0, 1, 2),
                                         labels = c("Good", "Fair", "Poor"))

### 1.b Outcome: poor_sleeper (binario, soglia clinica standard PSQI > 5) -----------
# La soglia PSQI > 5 è nota in letteratura clinica
dataset$poor_sleeper <- factor(ifelse(dataset$psqiglobalscore > 5, "Poor", "Good"),
                               levels = c("Good", "Poor"))
print(table(dataset$poor_sleeper))
print(round(prop.table(table(dataset$poor_sleeper)) * 100, 1))

### 1.c Collinearita' strutturale (solo blocco TMMS; PSQI e' ora la risposta) -------
check_tmms <- all(dataset$tmms_repair + dataset$tmms_attention + dataset$tmms_clarity
                  == dataset$tmms_totalscore)
cat("tmms_totalscore = somma delle subscale per tutte le righe?", check_tmms, "\n")

## 2. Split train/test -----
set.seed(123)
index <- sample(1:nrow(dataset), size = floor(0.70 * nrow(dataset)))
train_set <- dataset[index, ]
test_set  <- dataset[-index, ]
cat("Train:", nrow(train_set), "| Test:", nrow(test_set), "\n")

## 3. Analisi esplorativa (EDA) -----------------------------------------------------

### 3.a Bilanciamento della risposta ---------
ggplot(train_set, aes(x = poor_sleeper, fill = poor_sleeper)) +
  geom_bar() +
  scale_fill_manual(values = pal_midnight) +
  labs(title = "Distribuzione di poor_sleeper (Train Set)",
       x = "Qualità del sonno", y = "Conteggio") +
  theme_minimal() + theme(legend.position = "none")

### 3.b perceived_healthstatus vs poor_sleeper (proporzioni) ------------------------
ggplot(train_set, aes(x = perceived_healthstatus, fill = poor_sleeper)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = pal_midnight) +
  labs(title = "Proporzione di poor sleepers per salute percepita",
       x = "Salute percepita", y = "Proporzione") +
  theme_minimal()

### 3.c Predittori continui per livello di poor_sleeper -----------------------------
vars_cont <- c("Age", "tmms_repair", "tmms_attention", "tmms_clarity")
train_long <- train_set %>%
  pivot_longer(cols = all_of(vars_cont), names_to = "variabile", values_to = "valore")

ggplot(train_long, aes(x = poor_sleeper, y = valore, fill = poor_sleeper)) +
  geom_boxplot() +
  scale_fill_manual(values = pal_midnight) +
  facet_wrap(~ variabile, scales = "free_y") +
  labs(title = "Predittori continui per qualità del sonno") +
  theme_minimal() + theme(legend.position = "none")

### 3.d Collinearita' visiva nel blocco TMMS -----------------------------------------
# ggpairs usa una logica colore leggermente diversa
ggpairs(train_set[, c("tmms_repair", "tmms_attention", "tmms_clarity", "tmms_totalscore")],
        title = "Collinearità interna al blocco TMMS",
        lower = list(continuous = wrap("points", color = col_strutt, alpha = 0.6)))

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
       aes(x = modello, y = AIC)) +
  geom_col(fill = col_neutral) + 
  geom_text(aes(label = round(AIC, 1)), vjust = -0.3, color = col_strutt) +
  labs(title = "AIC: subscale TMMS vs punteggio totale TMMS") +
  theme_minimal()

# Siccome la differenza tra i due valori di AIC è minima (inferiore a 2), i due 
# modelli presentano una capacità informativa equivalente. Scegliamo di mantenere 
# il modello con le subscale poiché offre una granularità interpretativa superiore.

## 5. Modello completo ---------------------------------------------
modello_completo <- modello_tmms_subscale
summary(modello_completo)

## 6. Controllo collinearita' tramite VIF ---------------------------------------------
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

ggplot(vif_plot_data, aes(x = reorder(variabile, VIF), y = VIF)) +
  geom_col(fill = col_neutral) +
  geom_hline(yintercept = soglia, color = col_alert, linetype = "dashed") +
  coord_flip() +
  labs(title = "VIF per covariata (modello completo)", x = "", y = "VIF") +
  theme_minimal()

## 7. Selezione del modello: stepwise AIC -----------------------------------------------
modello_step <- stepAIC(modello_completo, direction = "both", trace = FALSE)
summary(modello_step)

modello_definitivo <- modello_step

### 7.a Odds Ratio del modello finale (Forest Plot Corretto) ---------------------------
or_table <- exp(cbind(OR = coef(modello_definitivo), confint(modello_definitivo)))

or_df <- data.frame(variabile = rownames(or_table)[-1],
                    OR = or_table[-1, "OR"],
                    low = or_table[-1, 2], high = or_table[-1, 3])

# Impostiamo un limite all'asse per evitare che perceived_healthstatusPoor distrugga la scala
limite_superiore <- 10

ggplot(or_df, aes(x = reorder(variabile, OR), y = OR)) +
  geom_point(size = 3, color = col_alert) +
  geom_errorbar(aes(ymin = low, ymax = high), width = 0.2, color = col_strutt) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray50") +
  coord_flip() +
  # Questa riga limita l'asse X e aggiunge una freccia visiva se una barra viene tagliata
  scale_y_continuous(limits = c(0, limite_superiore), oob = scales::squish) +
  labs(title = "Odds Ratio del modello finale", x = "", y = "Odds Ratio (IC 95%)",
       caption = "Nota: L'asse è stato limitato a 10 per leggibilità.") +
  theme_minimal()

## 8. Validazione predittiva sul test set -------------------------------------------------
prob_test <- predict(modello_definitivo, newdata = test_set, type = "response")
pred_test <- factor(ifelse(prob_test > 0.5, "Poor", "Good"), levels = c("Good", "Poor"))

matrice_confusione <- table(Osservato = test_set$poor_sleeper, Predetto = pred_test)

conf_df <- as.data.frame(matrice_confusione)
ggplot(conf_df, aes(x = Predetto, y = Osservato, fill = Freq)) +
  geom_tile() + geom_text(aes(label = Freq), color = "white", size = 6) +
  scale_fill_gradient(low = col_neutral, high = col_strutt) +
  labs(title = "Matrice di Confusione - Test Set") +
  theme_minimal()

accuracy_test <- sum(diag(matrice_confusione)) / sum(matrice_confusione)
baseline_accuracy <- max(table(test_set$poor_sleeper)) / nrow(test_set)

# Curva ROC e AUC
roc_obj <- roc(response = test_set$poor_sleeper, predictor = prob_test,
               levels = c("Good", "Poor"), quiet = TRUE)
auc_val <- auc(roc_obj)

plot(roc_obj, main = paste0("Curva ROC (AUC = ", round(as.numeric(auc_val), 3), ")"),
     col = col_strutt, lwd = 2)

## 9. Cross-validation (5-fold, sul train set) ---------------------------------------
set.seed(123)
k <- 5
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

cv_df <- data.frame(fold = factor(1:k), AUC = cv_auc)
ggplot(cv_df, aes(x = "", y = AUC)) +
  geom_boxplot(fill = col_neutral, color = col_strutt, outlier.shape = NA) +
  geom_jitter(width = 0.05, size = 2, color = col_alert) +
  labs(title = paste0(k, "-fold Cross-Validation: AUC (Train Set)"), x = "") +
  theme_minimal()

## 10. Riepilogo finale -----------------------------------------------------------------
cat("\n========== RIEPILOGO FINALE ==========\n")
cat("Formula modello definitivo:\n"); print(formula_finale)
cat("\nAccuracy test set:", round(accuracy_test, 3),
    " | baseline:", round(baseline_accuracy, 3), "\n")
cat("AUC test set:", round(as.numeric(auc_val), 3), "\n")
cat("Accuracy media CV:", round(mean(cv_accuracy), 3),
    " | AUC media CV:", round(mean(cv_auc), 3), "\n")