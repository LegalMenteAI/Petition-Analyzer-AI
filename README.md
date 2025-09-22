# Petition Analyzer AI — Simple Guide 

This AI tool reads a scanned **petition PDF**, pulls out each signer’s **name and address**, compares them to a **voter file**, and saves a **CSV** output file with match results and confidence scores.

This is an MVP. You run it with one command. No website. **No API key required.**

---

## What you need

- **R** installed (see “Install R” below).
- Your files:
  - A **petition PDF** (scanned).
  - A **voter file** (CSV or Excel). It should include these columns (case doesn’t matter):  
    `Last Name, First Name, Middle Name, Suffix Name, Primary Address, City, State, Zip, PrimaryZip4, Precinct Name`

The script uses built-in Generative AI OCR/processing. You do **not** need to provide an API key.

---

## Install R

### Windows

1. Go to: <a href="https://cran.r-project.org/" target="_blank">cran.r-project.org</a>
2. Click **Download R for Windows** → **base** → download the latest installer **R-x.y.z-win.exe**.
3. Run the installer and accept the defaults.
4. (Optional if package compilation is ever needed) install RTools: <a href="https://cran.r-project.org/bin/windows/Rtools/" target="_blank">Install here.</a>


### macOS

**Option A: CRAN Installer**

1. Go to: <a href="https://cran.r-project.org/" target="_blank">cran.r-project.org</a>
2. Click **Download R for macOS** and download the latest `.pkg` (Apple Silicon or Intel).
3. Open the `.pkg` and follow the prompts.


**Option B: Homebrew**

```bash
brew install --cask r
```

**If PDF conversion errors appear later on macOS, install Poppler:**

```bash
brew install poppler
```

### Linux (Debian/Ubuntu)
```bash
sudo apt update
sudo apt install -y r-base libpoppler-cpp-dev
```

Fedora/RHEL:
```bash
sudo dnf install -y R poppler-cpp-devel
```

---

## Get the code ready

Place these files together in a folder (any folder is fine):

- `petition_analyzer.R`  *(the main script)*
- `install.R`            *(installs required R packages)*

Open your terminal and change to that folder.

- **Windows PowerShell**

  ```powershell
  cd "C:\Path\to\folder-that-contains-the-scripts"
  ```
- **macOS / Linux**

  ```bash
  cd "/Path/to/folder-that-contains-the-scripts"
  ```

Then install the required R packages (one time):


### macOS / Linux
```bash
Rscript install.R
```

### Windows PowerShell
```powershell
Rscript install.R
```


---

## Run the AI tool

You provide: (1) the petition **PDF**, (2) the **voter file**, (3) where to save the **output CSV** file.

### macOS / Linux

```bash
Rscript petition_analyzer.R \
  --pdf "/Path/to/petition.pdf" \
  --voters "/Path/to/voters.xlsx" \
  --out "/Path/to/results.csv"
```

### Windows PowerShell

```powershell
Rscript petition_analyzer.R `
  --pdf "C:\Path\to\petition.pdf" `
  --voters "C:\Path\to\voters.xlsx" `
  --out "C:\Path\to\results.csv"
```

---

## If PowerShell can’t find `Rscript` (but R is already installed)


PowerShell often doesn’t have R on the PATH by default. Here are simple fixes. Pick one:


### Option 1: Run `Rscript.exe` with its **full path** (quick & reliable)
Replace the version in the path with what you have installed.

```powershell
& "C:\Program Files\R\R-4.4.1\bin\Rscript.exe" `
  "C:\Path\to\petition-analyzer\petition_analyzer.R" `
  --pdf "C:\Path\to\petition.pdf" `
  --voters "C:\Path\to\voters.xlsx" `
  --out "C:\Path\to\results.csv"
```

**Don’t know your exact path?** Let PowerShell find it:

```powershell
$R = Get-ChildItem 'C:\Program Files\R' -Recurse -Filter Rscript.exe -ErrorAction SilentlyContinue |
     Sort-Object LastWriteTime -Desc | Select-Object -First 1 -ExpandProperty FullName
& $R "C:\Path\to\petition-analyzer\petition_analyzer.R" `
     --pdf "C:\Path\to\petition.pdf" `
     --voters "C:\Path\to\voters.xlsx" `
     --out "C:\Path\to\results.csv"
```

### Option 2: Add R to PATH **for this PowerShell window only**
```powershell
$env:Path += ";C:\Program Files\R\R-4.4.1\bin"
Rscript "C:\Path\to\petition-analyzer\petition_analyzer.R" `
  --pdf "C:\Path\to\petition.pdf" `
  --voters "C:\Path\to\voters.xlsx" `
  --out "C:\Path\to\results.csv"
```

### Option 3: Add R to PATH **permanently** (then **restart** PowerShell)
```powershell
setx PATH "$($env:PATH);C:\Program Files\R\R-4.4.1\bin"
```
Close and reopen PowerShell, then you can use `Rscript` normally.


---

## What you’ll see while it runs

Progress messages such as:

- “Load voter file — start/end/duration in seconds”
- “Rasterize PDF — start/end/duration in seconds”
- “Extract signatures [PDF Page X of Y] — start/end/duration in seconds”
- “Match signatures — start/end/duration in seconds”

---

## What you get

A CSV at the `--out` location, including:

- `pdf_extracted_name`, `pdf_extracted_address`, `pdf_extracted_city`, `pdf_extracted_zip`, `pdf_page`, `row`
- `match_confidence_score` (0 – 1.0) 
  + Uses Levenshtein similarity. <a href="https://cran.r-project.org/web/packages/stringdist/refman/stringdist.html#stringdist-metrics" target="_blank">Learn more about Levenshtein distance here</a>
- Matched voter fields (both raw and normalized)
- Original voter columns (`Last Name`, `First Name`, `Middle Name`, `Suffix Name`, `Primary Address`, `City`, `State`, `Zip`, `PrimaryZip4`, `Precinct Name`)


---

## Loom Video Demo

Link to Video Demo of Loom demonstrating MVP working is here:

- Intro video: https://www.loom.com/share/8405f2f89d9846a493faf9c304498ea7?sid=8c0ea49c-c52e-4c24-81de-bdcde977ba9e
- How-to Demo: https://www.loom.com/share/fa316bb2a1b540658c2349c21962d007?sid=08103a52-9c12-475e-9bce-a39caf5a48d2


---

## Common Troubleshooting

- **“Voter file missing required columns”**  
  Ensure your file has the 10 listed columns (case can differ).

- **PDF won’t convert / “Poppler” missing (macOS/Linux)**  
  Install Poppler and re-run:
  - macOS (Homebrew): `brew install poppler`
  - Ubuntu/Debian: `sudo apt install libpoppler-cpp-dev`
  - Fedora/RHEL: `sudo dnf install poppler-cpp-devel`

- **`Rscript` not recognized**  
  Use the *full path* to `Rscript.exe` (Option 1 above), or add R to PATH (Options 2–3).

- **Slow or large files**  
  Big PDFs or voter files take longer. Try a smaller test first to confirm it works.

---

## Updates

If you receive an updated `petition_analyzer.R`, replace the file. Run `Rscript install.R` again only if asked (when new packages are added).




