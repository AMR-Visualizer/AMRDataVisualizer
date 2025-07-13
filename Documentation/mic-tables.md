<!-- File: mic-tables.md -->
# 📊 MIC Tables Tab

## Table of Contents
1. [Overview](#overview)
2. [How to Interpret the Figure](#how-to-interpret-the-figure)
3. [Data Filters](#data-filters)
4. [Plot Controls](#plot-controls)
5. [Legend](#legend)
6. [Output](#output)
7. [Feedback](#feedback)

---

## 🧭 Overview<a name="overview"></a>

The **MIC Tables** tab provides a diagnostic summary of your interpreted **Minimum Inhibitory Concentration (MIC)** values.  
**This tab will only be visible if MIC values were included and successfully interpreted during the data import stage.**

MIC tables display all unique MIC values observed in your dataset for a selected **microorganism-antimicrobial** pair. MIC values are shown as individual columns, sorted numerically from lowest to highest. You may select a grouping variable (e.g., year, region, source) to organize rows based on that variable’s distinct values.

Because clinical breakpoints are defined for specific combinations of:
- **Microorganism**
- **Antimicrobial agent**
- **Sample source** (e.g., urinary vs. non-urinary)
- **Host species** (in some guidelines)

you must select a single value for each of these parameters using the filters provided on the right-hand side of the screen.

Each MIC table includes the applicable clinical breakpoints as defined by the guideline selected during import (e.g., CLSI or EUCAST). MIC columns are colour-coded based on the **S**, **I**, or **R** interpretation assigned using these breakpoints.  
If a particular MIC value cannot be interpreted (e.g., it falls outside of defined breakpoints), it will be displayed without colour.

This visualization allows users to:
- View MIC distributions at a glance
- Assess the interpretive landscape for each drug-bug combination
- Identify unusual values or potential issues in data entry

---

## 📖 How to Interpret the Figure<a name="how-to-interpret-the-figure"></a>

Each **column** represents a unique MIC value (e.g., `≤0.25`, `1`, `≥16`), ordered from smallest (left) to largest (right). Each **row** corresponds to a group defined by your selected grouping variable (e.g., year, species, region).

Cells display the **number of isolates** with that specific MIC value and are colour-coded based on interpretation using the selected guideline:

- **Teal** = **S (Susceptible)** — MIC is less than or equal to the susceptible breakpoint.
- **Yellow** = **I (Intermediate)** — MIC falls between the susceptible and resistant breakpoints.
- **Red** = **R (Resistant)** — MIC is greater than or equal to the resistant breakpoint.
- **White with grey border** = **NI (Not Interpretable)** — MIC could not be interpreted with available breakpoints.

> 💡 Colouring is applied only when valid breakpoints exist. MIC values falling outside of interpretive categories will be left uncoloured or marked as **NI** (Not Interpretable).

---

## 🧰 Data Filters<a name="data-filters"></a>

| **Filter**         | **Description**                                                                 |
|--------------------|---------------------------------------------------------------------------------|
| **Microorganism**  | Select a single microorganism to view MIC distribution and apply breakpoints.  |
| **Antimicrobial**  | Choose the drug of interest for MIC interpretation.                            |
| **Sample Source**  | Select whether the sample is from a urinary or non-urinary source.             |
| **Host Species**   | Required if more than one host species is present; filters breakpoints.        |
| **Grouping Variable** | Defines how rows are grouped (e.g., by year, region, species, etc.).        |

---

## 🎛 Plot Controls<a name="plot-controls"></a>

No additional plot controls exist at this time.

---

## 🔹 Legend<a name="legend"></a>

| **Color**                  | **Symbol** | **Meaning**                                                                 |
|----------------------------|------------|------------------------------------------------------------------------------|
| Teal                      | **S**      | Susceptible — MIC is ≤ the susceptible breakpoint.                          |
| Yellow                    | **I**      | Intermediate — MIC is between the susceptible and resistant breakpoints.    |
| Red                       | **R**      | Resistant — MIC is ≥ the resistant breakpoint.                              |
| White with grey border    | **NI**     | Not Interpretable — MIC could not be interpreted with available breakpoints.|

>A legend is also provided below each table.

---

## 📤 Output<a name="output"></a>

- **Save Report** &mdash; download the current plot as an HTML file. The report includes the displayed plot, and all active filters used to generate it. Rendering may take a moment. 

---

## 💬 Feedback<a name="feedback"></a>

We are continually making improvements to this app and this display. Please report issues or suggestions via [GitHub Issues](https://github.com/ksobkowich/AMRDataVisualizer/issues).
