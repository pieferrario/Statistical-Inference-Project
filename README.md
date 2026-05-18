# Statistical-Inference-Project
## 1. Come usare Git con RStudio

Regola d'oro: **Prima scarichi le novità, poi lavori, infine carichi online.** Se escono scritte rosse strane, **non toccare nulla** e chiedi sul gruppo!

### 1.a Clonare la repository sul tuo PC
Fallo **una sola volta** all'inizio per scaricare il progetto sul computer.

1. Vai su GitHub nella pagina del progetto, clicca sul tasto verde **Code** e copia l'indirizzo (finisce con `.git`).
2. Apri RStudio, vai nella scheda **Terminal** (di fianco alla Console).
3. Esegui nel terminale:
```bash
cd [Scegli dove vuoi salvare la cartella e copiane l'indirizzo (`C:\` ecc.)]
git clone [Inserisci qui il link che hai copiato prima]
```
4. Inserisci nel terminale username e password di GitHub
5. Aprendo la cartella che si è creata, dovresti vedere un file `test_file.R`

### 1.b Lavorare sui file
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

# 3. Spedisci tutto su GitHub
git push
```
---
