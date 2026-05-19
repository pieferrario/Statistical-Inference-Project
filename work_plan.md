# Piano di Lavoro: Roadmap e Ripartizione dei Compiti

La presente pianificazione stabilisce la sequenza operativa che abbiamo adottato per lo sviluppo del progetto.

---

## Fase I: Esplorazione Preliminare dei Dati (EDA) e Pre-processing

### Obiettivi e Sequenza Operativa
* **Step 1:** Importazione del dataset in ambiente R e corretta codifica delle strutture dati (definizione dei fattori per le variabili categoriali `occupation` e `felt_rested`).
* **Step 2:** Analisi statistica descrittiva univariata e bivariata; generazione di funzioni di densità, istogrammi e boxplot per identificare visivamente le relazioni strutturali.
* **Step 3:** Diagnostica sui dati: verifica della presenza di dati mancanti (missing values) e gestione di eventuali anomalie.

### Assegnazione dei Ruoli
* **Sviluppo Codice (Lead):** Cecilia, Pietro
* **Validazione e Revisione (Review):** Ian, Matteoeo

---

## Fase II: Stima dei Modelli e Diagnostica Inferenziale

### Obiettivi e Sequenza Operativa
* **Step 1:** Partizione casuale del dataset tramite algoritmo di split gerarchico (80% Training Set, 20% Testing Set).
* **Step 2 (Coppia A):** Studio teorico dei presupposti dell'ANOVA (omogeneità delle varianze, normalità dei residui) e implementazione della funzione `aov()`. Esecuzione dei test post-hoc di Tukey per i confronti multipli.
* **Step 3 (Coppia B):** Studio teorico del modello logit (funzione di legame Link, massima verosimiglianza) e implementazione tramite `glm(family = "binomial")`. Calcolo degli Odds Ratio mediante trasformazione esponenziale dei coefficienti.
* **Step 4 (Modello Comune):** Stima del modello di regressione lineare multipla `lm()` per la performance cognitiva.
* **Step 5 (Predizioni):** Calcolo delle metriche di errore (Mean Absolute Error) per il modello lineare e della matrice di confusione (accuratezza, specificità) per il modello logistico sul Testing Set.

### Assegnazione dei Ruoli
* **Lead ANOVA e Modello Lineare:** Pietro, Ian
* **Lead Modello Logistico e Predizioni:** Cecilia, Matteo
* **Nota di Coordinamento:** Ciascuna coppia esporrà i propri risultati all'altra in una sessione di revisione congiunta, assicurando l'allineamento di tutto il gruppo sulla sintassi R e sull'output dei test.

---

## Fase III: Interpretazione Economica e Semplificazione del Linguaggio

### Obiettivi e Sequenza Operativa
* **Step 1:** Decodifica dell'output ANOVA: individuazione dei cluster professionali con differenze statisticamente significative nei pattern di sonno e traduzione delle distanze in termini assoluti.
* **Step 2:** Decodifica del modello lineare: isolamento dei coefficienti beta per quantificare l'impatto marginale netto di sonno e stress sulle abitudini cognitive.
* **Step 3:** Decodifica del modello logistico: conversione dei log-odds in variazioni percentuali di probabilità d'evento per renderli assimilabili da un pubblico non statistico.

### Assegnazione dei Ruoli
* **Sviluppo Testi (Lead):** Ian, Matteo
* **Validazione di Comprensibilità (Review):** Pietro, Cecilia

---

## Fase IV: Redazione del Supporto Visivo e Simulazione dell'Esposizione

### Obiettivi e Sequenza Operativa
* **Step 1:** Definizione del layout della presentazione. Struttura: Introduzione $\rightarrow$ ANOVA $\rightarrow$ Reg. Lineare $\rightarrow$ Reg. Logistica $\rightarrow$ Validazione Predittiva.
* **Step 2:** Esclusione di tabelle numeriche complesse in favore di grafici essenziali (plot degli effetti, intervalli di confidenza visivi) e sintesi delle conclusioni operative.
* **Step 3:** Sessioni di simulazione dell'esposizione con cronometro per ripartire in modo equo il tempo tra noi quattro relatori (circa 2 minuti e 30 secondi ciascuno).

### Assegnazione dei Ruoli
* **Struttura e Grafica (Lead):** Pietro, Matteo
* **Sintesi Testi e Ottimizzazione Tempi (Review):** Cecilia, Ian
