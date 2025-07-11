<p align="center">

<img src="www/logoDark.png" alt="AMR Visualizer Logo" width="300"/>

</p>

An interactive **R Shiny** application for cleaning, harmonising, interpreting, and visualising antimicrobial resistance (AMR) data derived from antimicrobial susceptibility testing (AST).

Developed by researchers and professionals in the AMR community, this tool streamlines routine AST workflows for users who may lack the time, technical resources, or coding expertise to build analyses from scratch. It leverages internationally recognised guidelines (e.g. **CLSI**, **EUCAST**) and, while originally created for veterinary datasets, is equally applicable to human health data.

-   **Privacy-first:** user-uploaded data are processed **locally** and are **never** stored. All data are deleted when the session ends, though cleaned datasets and figures can be downloaded at any time.

> ⚠️ **Disclaimer**\
> This application is intended for research and exploratory use only. It does **not** provide clinical recommendations, and results must not be used to guide patient treatment. Users are responsible for validating all interpretations and conclusions.

------------------------------------------------------------------------

## Key Features

### 1 · Flexible Data Import

-   **Accepts `.csv` and `.parquet` files**
-   Automatically recognises common AST layouts (edge cases may require minor adjustment)
-   Full guidance in [`Documentation/data-import.md`](Documentation/data-import.md)

### 2 · Automated Data Harmonisation

| Task                      | Example Transformation                        |
|---------------------------|---------------------------------------------|
| **Date standardisation**  | `31-Jan-2024`, `2024/01/31` → `2024-01-31`    |
| **Microorganism names**   | `E. coli` → *Escherichia coli*                |
| **Antimicrobial names**   | `AMC` → Amoxicillin/clavulanic acid           |
| **Drug-class assignment** | Amoxicillin → β-lactams/penicillins           |
| **Geographic mapping**    | Province/state matched to shapefiles for maps |

### 3 · AST Result Management

-   **Normalise existing S/I/R results** to binary, three-letter, or single-letter codes\
-   **OR interpret raw MIC/disk values** using user-selected breakpoints (CLSI, EUCAST, etc.)

### 4 · Interactive Visualisation

Generate publication-ready, interactive outputs tailored to AMR: - Resistance-frequency plots - Cumulative and stratified antibiograms - Time-series trend charts - Geographic heatmaps - Multi-drug-resistance correlation matrices - MIC distribution tables and histograms - Dynamic summary tables

### 5 · Export

-   Download cleaned datasets for further analysis
-   Save any plot or table directly from the interface

> **Continuous Improvement**\
> Real-world AST data are highly variable. We are actively expanding import and cleaning routines---feedback and sample files are welcome!

------------------------------------------------------------------------

## 📂 Required Input

**Minimum columns**

1\. **Microorganism**\
2. **Antimicrobial**\
3. **Antimicrobial results**\
- Raw values (MIC values) *or* pre-interpreted S/I/R codes

**Recommended columns** - Date - Geographic information - Sample source/body site - Animal species or host

See the [Data Formatting Guide](Documentation/data-formatting.md) for detailed examples and naming conventions.

------------------------------------------------------------------------

## 📊 Included Visualisations

| Category                                          | Examples                                                             |
|-------------------------|-----------------------------------------------|
| [**Antibiograms**](Documentation/antibiograms.md) | Summarize the susceptibility of microorganism to various antibiotics |
| [**Trends**](Documentation/trends.md)             | Resistance prevalence over time, time-series plots                   |
| [**Spatial**](Documentation/maps.md)              | Interactive maps by region or sub-region                             |
| [**MDR Correlation**](Documentation/mdr-matrices.md)       | Multi-drug-resistance matrices                                       |
| [**Summaries**](Documentation/data-explore.md)    | Customizable data tables with on-the-fly filtering                   |

Each page includes help text explaining its structure and interpretation.

------------------------------------------------------------------------

## 🛠 How to Launch

### Option 1 · Run from Source (RStudio)

### Option 2 · Download our Docker Image
