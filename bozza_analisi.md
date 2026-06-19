# Bozza di Analisi — Sonno, Regolazione Emotiva e Salute Percepita

*Nota: questa bozza è basata su un test preliminare (Python/statsmodels) eseguito sugli
stessi dati per verificare che la pipeline funzioni. I numeri esatti — soprattutto le
variabili selezionate dallo stepwise — vanno confermati eseguendo `analisi_sonno_salute.R`,
perché R gestisce la selezione a livello di intero *factor* (es. `majors` come blocco),
mentre il test preliminare ha lavorato a livello di singola dummy. Sostituite i valori
tra `[ ]` con quelli ottenuti dal vostro run.*

---

## 1. Il dataset

518 studenti universitari (18-30 anni), nessun missing, nessun duplicato. Outcome:
**stato di salute percepito** a 3 livelli ordinati (Good / Fair / Poor), distribuzione
sbilanciata verso "Good" (~69% nel campione): 355 Good, 118 Fair, 45 Poor.

Questo sbilanciamento è il primo elemento da discutere: un classificatore banale che
prevede sempre "Good" otterrebbe già un'accuracy di base attorno al **65%** — qualunque
modello dovrà essere confrontato con questa soglia, non con lo 0%.

## 2. Perché regressione ordinale e non OLS

A differenza del progetto sulle news (`shares`, variabile continua), qui l'outcome è
categoriale ordinato. Una `lm()` tratterebbe le distanze tra "Good", "Fair" e "Poor"
come numericamente equivalenti e arbitrarie; il modello a *odds proporzionali* (`polr`)
rispetta invece l'ordinamento senza assumere una scala cardinale.

## 3. Collinearità strutturale: cosa ci aspettiamo

Il dataset contiene due ridondanze esatte by design:
- `tmms_totalscore` = `tmms_repair + tmms_attention + tmms_clarity`
- `psqiglobalscore` = somma dei 7 componenti PSQI

Nel test preliminare, le versioni **granulari** (subscale/componenti) hanno mostrato
AIC più basso rispetto alle versioni aggregate (totale), in entrambi i casi — segno che
le componenti portano informazione che si perde aggregando in un unico punteggio. Questo
è analogo a quanto succedeva nello script originale con `weekday` (7 categorie) che
batteva `is_weekend` (binario).

**Atteso:** [confermare] il modello finale userà le subscale TMMS e i componenti PSQI
singolarmente, non i due punteggi totali.

## 4. Cosa è probabile emerga come significativo

Dal test preliminare (da confermare con lo stepwise reale in R):

- **`tmms_repair`** (capacità di riparazione dell'umore) → associata a minore probabilità
  di salute percepita peggiore. Coerente con la letteratura: chi sa regolare meglio le
  proprie emozioni negative tende a percepire la propria salute come migliore.
- **`Subjectivesleepquality`** e **`Sleepdisturbance`** (componenti PSQI) → tra i
  predittori più forti, in linea con l'ipotesi di partenza (qualità del sonno collegata
  alla salute percepita).
- **`Age`**, **`gender`** → probabilmente non significativi nel campione (range di età
  ristretto, 18-30 anni).
- **`majors`** (corso di studi) → effetto debole/borderline; da valutare se mantenerlo per
  significatività teorica anche se non strettamente significativo statisticamente.

## 5. Capacità predittiva: aspettative realistiche

Nel test preliminare, il modello finale ha ottenuto un'accuracy sul test set di circa
**67-70%**, contro un baseline del 65%. Il miglioramento è modesto: il modello discrimina
bene la classe "Good" ma fatica a distinguere "Fair" da "Poor" (poche osservazioni,
sovrapposizione nei predittori). Il **kappa pesato** (quadratico), più informativo
dell'accuracy su outcome ordinali, si è attestato attorno a **0.35-0.40**, indicando un
accordo "moderato" tra previsione e realtà.

**Implicazione per la discussione:** è onesto presentare questo come un modello con
potere predittivo modesto ma reale, utile più a livello inferenziale (capire quali
fattori sono associati alla salute percepita) che predittivo (classificare con
precisione un nuovo individuo) — un punto comune in psicologia/scienze sociali con
campioni di questa dimensione.

## 6. Cross-validation

Aspettatevi una certa variabilità tra i fold (deviazione standard dell'accuracy
relativamente alta, [confermare con sd reale]), dovuta soprattutto alla classe "Poor"
poco numerosa (45 casi su 518): in alcuni fold potrebbero esserci pochissimi casi di
quella classe, rendendo la stima dell'accuracy meno stabile su quei fold specifici.
Vale la pena commentarlo esplicitamente come limite del campione, non del modello.

## 7. Struttura suggerita per la sezione "Risultati" della relazione

1. Descrizione del campione e della variabile risposta (sbilanciamento)
2. Giustificazione della scelta di `polr` rispetto a OLS
3. Confronto sottomodelli (TMMS, PSQI) con grafico AIC → giustifica la scelta delle
   variabili granulari
4. Tabella VIF → conferma assenza di collinearità residua dopo la scelta di sezione 3
5. Tabella coefficienti/Odds Ratio del modello finale, con interpretazione di segno e
   significatività
6. Performance: accuracy/kappa su test set vs baseline, matrice di confusione commentata
   (dove sbaglia il modello e perché, es. confusione Fair/Poor)
7. Cross-validation: stabilità della stima, eventuali limiti legati alla classe minoritaria
8. Limiti: dimensione campionaria, sbilanciamento delle classi, natura cross-sectional e
   autoreferita (self-report) di tutte le misure (PSQI, TMMS, salute percepita)

## 8. Limiti da menzionare esplicitamente

- Tutte le variabili (incluso l'outcome) sono **self-report**: rischio di varianza comune
  di metodo (chi si percepisce in modo più negativo potrebbe riportare punteggi peggiori
  su tutte le scale, gonfiando le associazioni).
- Disegno **cross-sectional**: le associazioni trovate non implicano causalità (es. non
  possiamo dire che dormire meglio *causi* una salute percepita migliore).
- Classe "Poor" piccola (45 casi): le stime relative a questa categoria sono meno precise,
  intervalli di confidenza più ampi.
