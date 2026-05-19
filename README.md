# Statistical-Inference-Project
## 0. Come usare Git con RStudio

Regola d'oro: **Prima scarichi le novità, poi lavori, infine carichi online.** Se escono scritte rosse strane, **non toccare nulla** e chiedi sul gruppo!

### 0.a Clonare la repository sul tuo PC
Fallo **una sola volta** all'inizio per scaricare il progetto sul computer.

1. Vai su GitHub nella pagina del progetto, clicca sul tasto verde **Code** e copia l'indirizzo (finisce con `.git`).
2. Apri RStudio, vai nella scheda **Terminal** (di fianco alla Console).
3. Esegui nel terminale:
```bash
cd [Scegli dove vuoi salvare la cartella e copiane l'indirizzo (`C:\` ecc.)]
git clone [Inserisci qui il link che hai copiato prima]
```
4. Inserisci nel terminale il tuo username di GitHub e premi Invio. Quando ti chiede la password, ricorda due cose importanti:
   - **La password non si vede:** mentre scrivi o incolli, lo schermo resta vuoto. Inseriscila comunque e premi Invio.
   - **Non usare la password normale:** devi creare un "Token". Vai su GitHub -> Clicca sulla tua foto in alto a destra -> Settings -> Developer settings (in fondo a sinistra) -> Personal access tokens -> Tokens (classic). Clicca "Generate new token (classic)", metti la spunta alla prima casella "repo" e clicca "Generate token" in fondo. Copia il codice che appare e incollalo nel terminale come password.
5. Aprendo la cartella che si è creata, dovresti vedere un file `test_file.R`

### 0.b Lavorare sui file
Apri il **Terminal** dentro RStudio ed esegui questi comandi in ordine.

#### Salva in locale i file aggiornati fino all'ultima versione
*Da fare appena apri RStudio, prima di toccare qualsiasi file.*
```bash
git pull
```

#### Modifica i file
Fai le tue modifiche agli script su RStudio e **salva i file** (`Ctrl + S` o `Cmd + S`).

#### Carica il tuo lavoro online
*Esegui questi 3 comandi in sequenza quando hai finito di lavorare.*

```bash
# 1. Seleziona tutti i file che hai modificato
git add .

# 2. Salva le modifiche inserendo un messaggio breve tra virgolette
git commit -m "Scrivi qui cosa hai fatto"

# 3. Spedisci tutto su GitHub (se ti chiede la password, usa sempre il Token)
git push
```
---
# Analisi Statistica della Qualità del Sonno e delle Performance Cognitive

Questo archivio contiene il progetto di inferenza statistica sviluppato da Cecilia Cotti, Matteo Coccia, Pietro Ferrario e Ian Fontana Rava.
L'obiettivo del lavoro consiste nell'applicare tecniche di modellazione statistica mediante il software R su un dataset reale, focalizzando l'attenzione sull'interpretazione e sulla divulgazione dei risultati per un pubblico non specialistico.

## Obiettivo del Progetto
La ricerca mira a investigare le relazioni esistenti tra lo stile di vita, l'attività occupazionale, il riposo notturno e le funzioni cognitive globali. Il problema viene declinato attraverso tre quesiti di ricerca, ciascuno affrontato tramite una specifica metodologia inferenziale.

## Modelli Statistici e Quesiti di Ricerca

### 1. Analisi della Varianza (ANOVA)
* **Ipotesi di ricerca:** L'attività lavorativa svolge un ruolo determinante nella qualità del sonno?
* **Specifiche del modello:** Variabile dipendente metrica `sleep_quality_score` spiegata dalla variabile categoriale `occupation`.
* **Razionale economico-sociale:** Si intende verificare se le discrepanze tra le medie dei punteggi di qualità del sonno tra diverse categorie professionali (es. medici, autisti, avvocati) siano statisticamente significative o riconducibili alla variabilità campionaria.

### 2. Regressione Lineare Multipla
* **Ipotesi di ricerca:** In quale misura la durata del sonno e i livelli di stress giornaliero influenzano le capacità cognitive?
* **Specifiche del modello:** Variabile dipendente continua `cognitive_performance_score` spiegata dai regressori quantitativi `sleep_duration_hrs` e `stress_score`.
* **Razionale economico-sociale:** L'obiettivo è isolare l'effetto marginale di un'ora aggiuntiva di sonno e di un punto incrementale di stress sulle facoltà mentali del giorno successivo, valutando anche eventuali effetti di interazione.

### 3. Regressione Logistica Binaria
* **Ipotesi di ricerca:** Quali fattori predicono la probabilità di svegliarsi riposati al mattino?
* **Specifiche del modello:** Variabile dipendente dicotomica `felt_rested` (0/1) spiegata dalle ore di sonno e da comportamenti serali quali `screen_time_before_bed_mins` e `caffeine_mg_before_bed`.
* **Razionale economico-sociale:** Attraverso il calcolo degli Odds Ratio, il modello quantifica la variazione del rapporto di probabilità di un risveglio rigenerato in funzione delle abitudini digitali e alimentari prima del riposo.

## Descrizione del Dataset
Le analisi sono condotte sul file `sleep_health_dataset.csv`, una base dati cross-section contenente 100.000 osservazioni e 32 variabili relative a parametri fisiologici, demografici e comportamentali. Al fine di validare la capacità predittiva e la robustezza dei modelli stimati, la popolazione verrà suddivisa in un sottoinsieme di addestramento (Training Set, 80%) e uno di verifica (Testing Set, 20%).
