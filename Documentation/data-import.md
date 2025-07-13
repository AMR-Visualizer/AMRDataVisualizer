<!-- File: data-import.md -->
# 🛠️ Data-Import Guide  
*A concise reference for loading and validating your dataset*

## 📑 Table of Contents
1. [Introduction](#1-introduction)  
2. [Supported File Types](#2-supported-file-types)  
3. [Data Layouts](#3-data-layouts)  
4. [Minimum Required Columns](#4-minimum-required-columns)  
   - [Additional Columns Required for MIC Data](#additional-columns-required-only-when-mic-values-are-provided)  
   - [Optional Columns](#optional-columns-cleaned-when-present)  
5. [Sample Data](#5-sample-data)  
   - [Long-format Example](#51-long-format-example)  
   - [Wide-format Example](#52-wide-format-example)  
6. [Import Workflow](#6-import-workflow)
7. [Feedback](#feedback)

---

## Introduction
The **AMR Visualizer** converts raw antimicrobial-susceptibility data
into ready-made plots. It automatically detects wide vs long layouts,
standardizes key fields, and applies breakpoints when MIC values are
present. You may import your own AST dataset or select from one of our
pre-loaded, openly avialable AST datasets.

---

## Supported File Types

| Format        | Notes                                                      |
|---------------|------------------------------------------------------------|
| **`.csv`**    | UTF-8 comma-separated text                                 |
| **`.parquet`**| Column-oriented binary (recommended for large datasets)    |

---

## Data Layouts

| Layout | Description                                   | Support |
|--------|-----------------------------------------------|---------|
| **Long** | One row = one drug–isolate result           | ✅ |
| **Wide** | One row = one isolate, each drug as a column| ✅* |

\*Wide files are **auto-detected** and reshaped to long format during
import. You may review and edit the conversion in the *Configuration*
dialog.

---

## Minimum Required Columns
### Required Columns

| **Data Element**  | **Assigned Column** | **Notes**                                   |
|-------------------|---------------------|---------------------------------------------|
| **Drug name**     | `Antimicrobial`     | Generic or compound name. will be cleaned using `AMR::as.ab()` |
| **Bacteria name** | `Microorganism`     | Full or abbreviated genus + species. Will be cleaned using `AMR::as.mo()` |
| **Test result**   | `Value`             | `S` / `I` / `R` **or** MIC value    |

---

### Additional Columns Required *only* When **MIC Values Are Provided**

| **Data Element**  | **Assigned Column** | **Notes**                                                     |
|-------------------|---------------------|---------------------------------------------------------------|
| **Host species**  | `Species`           | Breakpoints may differ by host (e.g., canine vs. feline)      |
| **Sample source** | `Source`            | Distinct breakpoints for urinary vs. non-urinary specimens    |

---

### Optional Columns (Cleaned When Present)

- **Date** — either a single `Date` column or separate `Year` + `Month`;  
  standardized to `YYYY-MM-DD`.
- **Region / Subregion** — geographic origin of the sample; mapped to
  shapefiles for the **Map** tab, if recognized.

All other columns are imported **as-is** and become available as filters
throughout the app.

---

## Sample Data

### Long-format Example  

| ID  | Date       | Species | Source | Microorganism | Antimicrobial | Value |
|-----|------------|---------|--------|---------------|---------------|-------|
| 001 | 2025-01-15 | Canine  | Urine  | *E. coli*     | Amoxicillin   | R     |
| 001 | 2025-01-15 | Canine  | Urine  | *E. coli*     | Enrofloxacin  | S     |

### Wide-format Example  

| ID  | Date       | Species | Source | Microorganism | Amoxicillin | Enrofloxacin |
|-----|------------|---------|--------|---------------|-------------|--------------|
| 001 | 2025-01-15 | Canine  | Urine  | *E. coli*     | R           | S            |

The app will reshape the wide table into long format automatically.

---

## Import Workflow

1. **Open the *Import* tab**  
2. **Choose an option**  
   - **Browse** to upload a `.csv`/`.parquet`, **or**  
   - Select from the dropdown of built-in demo datasets.  
3. Click **Submit**  
4. Review the **Raw Preview** (top) and **Extracted Preview** (bottom)  
5. If any column is mis-assigned, click **Adjust Columns**  
   - Map your columns or select **Not Present** where applicable  
6. *(If MIC data present)*  
   - Switch to **MIC Mode** at the top of the right-hand column in the **Adjust Columns** menu.  
   - Select your MIC value (i.e., 4), and sign (i.e., <) columns. If MIC value and sign exist in a single column, you can assign it to either 'Sign' or 'Value'.  
   - Choose a breakpoint guideline (**CLSI** or **EUCAST**)  
7. *(Optional)* add extra columns via **Additional Columns** button.  
8. Click **Process Data**  
   - Data will be processed into a standardized format, with harmonized naming conventions. MIC values, if provided, will be interpreted according to the selected breakpoint guidelines.  
   - This step may take a few moments depending on the size of your data.  
9. **Download cleaned data** (optional)  
10. New analysis tabs (Overview, Antibiogram, MIC Tables, etc.) become active on the left-hand sidebar.

---

## 💬 Feedback<a name="feedback"></a>

We are continually making improvements to this app and this display. Please report issues or suggestions via [GitHub Issues](https://github.com/ksobkowich/AMRDataVisualizer/issues).

---