<!-- File: quick_start_guide.md -->

## Quick Start User Guide
*A comprehensive walkthrough for importing data, configuring columns, and using analysis tabs.*

---

### Table of Contents
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

### 1  Purpose & Key Features
The **AMR Visualizer** transforms raw antimicrobial susceptibility (AST) data into a suite of interactive, ready-to-use visualizations - **antibiograms**, **MIC distribution tables**, **resistance maps**, **time-series trends**, **multidrug resistance (MDR) matrices**, and more. Designed with accessibility in mind, it eliminates the need for advanced programming or analytical expertise.

By automatically recognizing data formats, standardizing organism and drug names, and applying breakpoint interpretations where needed, the tool allows users to focus on exploring resistance patterns, generating summaries, and communicating results with minimal preprocessing required.

**Highlights**

- Automatic detection of data layouts  
- Analyze pre-interpretted **SIR** data, or interpret **Raw MIC values** on-the-fly using user-selected breakpoints (CLSI/EUCAST)  
- Provides a suite of common plots for AMR analysis
- One‑click generation of publication‑ready figures  
- Downloadable cleaned data and JSON parameter files for reproducibility  
- Pre‑loaded demo datasets using open-source surveillance data

---

### 2 System Requirements

| Component      | Details |
|----------------|---------|
| **R**          | Version ≥ 4.2 |
| **R Packages** | All dependencies are managed via [`renv`](https://rstudio.github.io/renv/), ensuring version compatibility. Packages will be restored automatically using `renv::restore()`. |
| **Deployment Options** | Users can run the app directly from source code using R, or deploy using the prebuilt Docker image (recommended for reproducibility). |

---

### 3  Data Preparation

#### 3.1  Supported File Types
- **`.csv`** – UTF‑8, comma‑separated  
- **`.parquet`** – column‑oriented, ideal for large datasets  

#### 3.2  Long vs. Wide Layouts
| Layout | Description | Notes |
|--------|-------------|-------|
| **Long** | One row = one drug–isolate result | Native support |
| **Wide** | One row = one isolate; each drug in its own column | Auto‑detected and reshaped |

#### 3.3  Minimum Required Columns
| Data Element   | Column Example  | Notes |
|----------------|-----------------|-------|
| Drug name      | `Antimicrobial` | Cleaned via `AMR::as.ab()` |
| Bacteria name  | `Microorganism` | Cleaned via `AMR::as.mo()` |
| Test result    | `Value`         | `S`/`I`/`R` **or** MIC value |

- **Mandatory when using MIC values**:  
  If your dataset includes MIC values, the following additional columns become **required**:
  
  | Data Element     | Assigned Column | Reason |
  |------------------|------------------|--------|
  | **Host species** | `Species`        | Breakpoints vary by host (e.g., canine vs. feline) |
  | **Sample source**| `Source`         | Breakpoints differ by anatomical site (e.g., UTI vs. non-UTI) |

- **Optional (but recommended)**:  
  Several optional columns will be automatically cleaned and used to unlock additional functionality in the app:

  | Column        | Purpose |
  |---------------|---------|
  | `Date`        | Enables time-series analysis in the **Trends** tab. Accepts either a single `Date` column or separate `Year` and `Month` columns. |
  | `Region` / `Subregion` | Enables choropleth visualizations in the **Maps** tab. These will be matched to internal shapefiles where possible. |
  | `Region` | Enables multi-drug resistance correlation matrices |

- **Custom columns**:  
  Any other column in your dataset (e.g., `Ward`, `Clinic`, `SubmissionType`) can be imported as-is and made available for filtering throughout the app. These filters allow for enhanced subgroup analysis across multiple visualizations, such as customized antibiograms or stratified MIC tables.

---

### 4  Import Workflow
1. **Open** the **Import** tab  
2. **Browse** for a `.csv`/`.parquet` file *or* choose a **demo dataset** from the dropdown menu
3. Click **Submit** to preview raw vs. extracted data  
4. **Adjust Columns** if the auto‑mapping is incorrect  
5. *(Optional)* enable **MIC Mode** and pick a breakpoint guideline  
6. *(Optional)* add extra columns via **Additional Columns**  
7. Click **Process Data** – the app cleans, standardizes, and stores the dataset  
8. Download **cleaned data** (optional)  
9. New analysis tabs appear in the sidebar

---

### 5  Column Configuration

#### 5.1  Mapping Columns
Use the dropdown menus to assign each required variable. Select
**Not Present** if a variable does not exist in your dataset.

#### 5.2 Wide Data
If your data are imported in a wide-format (single row for each sample with multiple test columns), your test columns will be **automatically** detected and pivotted into long format.

#### 5.3 MIC‑specific Settings
If you provide MIC values, additional configuration is required:

1. **Specify Sign and Value Columns**  
   Define which columns contain the MIC result components:
   - **Sign** — Symbols such as `<`, `≥`, `=`, etc.
   - **Value** — The numeric MIC values (e.g., `0.5`, `4`, `64`).
   > 💡 *If your MIC sign and value exist together in a single column (e.g., `">=4"`), assign that column to **either** the **Sign** or **Value** input, and set the other to `Not Present`. The app will parse the components automatically during processing.*

2. **Choose a Breakpoint Guideline**  
   Select either **CLSI** or **EUCAST** from the dropdown. This determines which breakpoints will be applied during interpretation.

3. **Confirm Host and Source Assignments**  
   MIC breakpoints are often host- and site-specific. Ensure the **Species** (host) and **Source** (sample site) fields are correctly mapped.

---

### 6  Processing & Validation
Upon clicking **Process Data** the app:

- Standardizes dates, organisms, drugs, regions  
- Interprets MICs per selected breakpoints  
- Generates a success screen with **Download Cleaned Data** option

---

### 7  Navigating Analysis Tabs

Each tab offers domain‑specific plots. Full documentation is linked below:

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

### 8  Exporting Results
- **Plots** – Use the **Save** button below each plot to export an `.html` file. The saved file includes the plot itself as well as all filters applied, supporting transparency and reproducibility
- **Summary tables** – exported via **Download Table** (`.csv`)  
- **Input Parameters** – In the **Explore** tab, you can save the criteria used to generate your summary tables. These are exported as a `.json` file via the **Download Inputs** button, enabling reproducibility and easy sharing with collaborators who may want to generate equivalent summaries with their own data

---

### 9  Reproducibility with Parameter Files
Upload a previously saved `.json` file to **replay** the exact filters and column mappings on a new dataset.  
> ⚠️ The target dataset must contain the same column names or the mapping will fail.

---

### 10  Troubleshooting
| Symptom                         | Likely Cause & Fix                               |
|---------------------------------|--------------------------------------------------|
| Plot shows no data              | Filters too restrictive or missing columns       |
| MIC Table tab not visible       | No MIC column detected or MIC Mode off           |
| Slow processing                 | Very large dataset; Many MIC interpretations  |
| Map not appearing               | Region information was not provided or could not be matched with a map file |

---

### 11  Feedback & Contributions
Open an issue or pull request on <https://github.com/ksobkowich/AMRDataVisualizer/>

We welcome bug reports, feature requests, and documentation edits!

---

*Last updated · 2025‑07‑16*
