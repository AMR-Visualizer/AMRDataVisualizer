<!-- File: quick_start_guide.md -->

# 🧭 AMR Visualizer · Quick Start User Guide
*A comprehensive walkthrough for importing data, configuring columns, and using every analysis tab.*

---

## 📑 Table of Contents
1. [Purpose & Key Features](#1-purpose--key-features)  
2. [System Requirements](#2-system-requirements)  
3. [Data Preparation](#3-data-preparation)  
   - 3.1 [Supported File Types](#31-supported-file-types)  
   - 3.2 [Long vs. Wide Layouts](#32-long-vs-wide-layouts)  
   - 3.3 [Minimum Required Columns](#33-minimum-required-columns)  
4. [Import Workflow](#4-import-workflow)  
5. [Column Configuration](#5-column-configuration)  
   - 5.1 [Mapping Columns](#51-mapping-columns)  
   - 5.2 [MIC‑specific Settings](#52-mic-specific-settings)  
6. [Processing & Validation](#6-processing--validation)  
7. [Navigating Analysis Tabs](#7-navigating-analysis-tabs)  
   - 7.1 [Overview](docs/overview-plots.md)  
   - 7.2 [Antibiogram](docs/antibiograms.md)  
   - 7.3 [MIC Tables](docs/mic-tables.md)  
   - 7.4 [Map](docs/maps.md)  
   - 7.5 [Trends](docs/trends.md)  
   - 7.6 [MDR Matrices](docs/mdr-matrices.md)  
   - 7.7 [Microguide](docs/microguide.md)  
   - 7.8 [Explore](docs/data-explore.md)  
8. [Exporting Results](#8-exporting-results)  
9. [Reproducibility with Parameter Files](#9-reproducibility-with-parameter-files)  
10. [Troubleshooting](#10-troubleshooting)  
11. [Feedback & Contributions](#11-feedback--contributions)

---

## 1  Purpose & Key Features
The **AMR Visualizer** transforms raw antimicrobial‑susceptibility data
into intuitive plots—antibiograms, MIC tables, resistance maps, time‑series
trends, multidrug resistance (MDR) matrices, and more—without requiring
advanced R skills.

**Highlights**

- Automatic detection of **long** and **wide** data layouts  
- Optional **MIC breakpoint interpretation** (CLSI/EUCAST)  
- One‑click generation of publication‑ready figures  
- Downloadable cleaned data and JSON parameter files for reproducibility  
- Pre‑loaded demo dataset (2020 NARMS – canine) for quick exploration

---

## 2  System Requirements
| Component | Version / Notes |
|-----------|-----------------|
| R         | ≥ 4.2           |
| Packages  | `shiny`, `tidyverse`, `plotly`, `AMR`, `bsplus`, `jsonlite`, `DT`, etc. (installed automatically via the app) |
| Browser   | Modern HTML5‑compatible (Chrome, Firefox, Edge, Safari) |

---

## 3  Data Preparation

### 3.1  Supported File Types
- **`.csv`** – UTF‑8, comma‑separated  
- **`.parquet`** – column‑oriented, ideal for large datasets  

### 3.2  Long vs. Wide Layouts
| Layout | Description | Notes |
|--------|-------------|-------|
| **Long** | One row = one drug–isolate result | Native support |
| **Wide** | One row = one isolate; each drug in its own column | Auto‑detected and reshaped |

### 3.3  Minimum Required Columns
| Data Element   | Column Example  | Notes |
|----------------|-----------------|-------|
| Drug name      | `Antimicrobial` | Cleaned via `AMR::as.ab()` |
| Bacteria name  | `Microorganism` | Cleaned via `AMR::as.mo()` |
| Test result    | `Value`         | `S`/`I`/`R` **or** MIC value |

Additional columns (`Species`, `Source`) become **mandatory** if MIC
values are supplied, because breakpoints are host‑ and site‑specific.
Optional columns (Date, Region, etc.) are imported as‑is and enhance
plot filters.

---

## 4  Import Workflow
1. **Open** the **Import** tab  
2. **Browse** for a `.csv`/`.parquet` file *or* choose the **NARMS demo**  
3. Click **Submit** to preview raw vs. extracted data  
4. **Adjust Columns** if the auto‑mapping is incorrect  
5. *(Optional)* enable **MIC Mode** and pick a breakpoint guideline  
6. *(Optional)* add extra columns via **Additional Columns**  
7. Click **Process Data** – the app cleans, standardizes, and stores the dataset  
8. Download **cleaned data** (optional)  
9. New analysis tabs appear in the sidebar

---

## 5  Column Configuration

### 5.1  Mapping Columns
Use the dropdown menus to assign each required variable. Select
**Not Present** if a variable does not exist in your dataset.

### 5.2  MIC‑specific Settings
If you provide MIC values:
1. Specify **Sign** (`<`, `≥`, etc.) and **Value** columns (can be merged).  
2. Choose a guideline (**CLSI** or **EUCAST**).  
3. Confirm host and source fields are mapped—these determine breakpoint
   selection.

---

## 6  Processing & Validation
Upon clicking **Process Data** the app:

- Converts wide → long (if needed)  
- Standardizes dates, organisms, drugs, regions  
- Interprets MICs per selected breakpoints  
- Generates a success screen with **Download Cleaned Data** option

---

## 7  Navigating Analysis Tabs

Each tab offers domain‑specific plots. Full documentation is linked
below:

| Tab          | Description                                     |
|--------------|-------------------------------------------------|
| **Overview** | Sanity‑check plots (tests over time, frequency) |
| **Antibiogram** | Susceptibility matrices · see `docs/antibiograms.md` |
| **MIC Tables**  | MIC distribution tables · see `docs/mic-tables.md` |
| **Map**         | Geographic resistance heatmaps · see `docs/maps.md` |
| **Trends**      | Time‑series plots of susceptibility · see `docs/trends.md` |
| **MDR Matrices**| Pairwise co‑resistance heatmaps · see `docs/mdr-matrices.md` |
| **Microguide**  | Organism‑specific reference data · see `docs/microguide.md` |
| **Explore**     | Custom summary tables · see `docs/data-explore.md` |

---

## 8  Exporting Results
- **Plots** – use the camera icon (`toImage`) to save high‑resolution PNGs  
- **Summary tables** – exported via **Download Table** (`.csv`)  
- **Input parameters** – saved via **Download Inputs** (`.json`) for reproducibility

---

## 9  Reproducibility with Parameter Files
Upload a previously saved `.json` file to **replay** the exact filters and
column mappings on a new dataset.  
> ⚠️ The target dataset must contain the same column names or the
> mapping will fail.

---

## 10  Troubleshooting
| Symptom                         | Likely Cause & Fix                               |
|---------------------------------|--------------------------------------------------|
| Plot shows no data              | Filters too restrictive or missing columns       |
| MIC tab not visible             | No MIC column detected or MIC Mode off           |
| “Unknown column” error          | Column names changed after parameter upload      |
| Slow processing                 | Very large dataset; switch to `.parquet` format  |

---

## 11  Feedback & Contributions
Open an issue or pull request on  
<https://github.com/ksobkowich/AMRDataVisualizer/>

We welcome bug reports, feature requests, and documentation edits!

---

*Last updated · 2025‑07‑16*
