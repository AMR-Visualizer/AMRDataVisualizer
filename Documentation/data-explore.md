<!-- File: data-explore.md -->
# 📊 Explore Tab

## Table of Contents
1. [Overview](#overview)
2. [How to](#how-to)
3. [Points of Caution](#points-of-caution)
4. [Data Filters](#data-filters)
5. [Output](#output)
6. [Feedback](#feedback)

---

## 🧭 Overview<a name="overview"></a>

The Explore tab offers a quick and flexible way to generate summary tables from your AST data. These tables are especially useful for internal reporting, presentations, and external data sharing when access to raw, patient-level data is restricted. The summaries are fully de-identified and free from any personally identifiable information.

You can stratify the summaries by one or more variables (e.g., organism, source, species), allowing you to tailor outputs to specific clinical or epidemiological contexts.

Each row in the summary table provides:
- The total number of AST results
- The number of isolates interpreted as Susceptible, Intermediate, and Resistant
- Optional additional columns depending on stratification (e.g., by year, species, or location)

>These summaries serve as a transparent, high-level overview of resistance patterns across subsets of your data.
---

## 📖 How to<a name="how-to"></a>
1. **Specify Filtering and Stratification Criteria**  
   Use the filter panels on the left-hand side to customize which data are included and how results are grouped. Available filters include:

   - **Microorganism**
   - **Antimicrobial**
   - **Sampling Site**
   - **Species**
   - **Subregion**
   - **Date Range**

   For each of these filters, you have two options:
   - **Select one or more values** to restrict the data to only those entries (e.g., just *E. coli* or just *cephalexin*).
   - **Leave the input blank** to include **all available values** for that variable.  
     > *If no selections are made, the app will include all available data for that filter.*

2. **Aggregate (Group) Filters**  
   Beneath each filter is an **"Aggregate"** checkbox. Use this if you **do not want to stratify** the results by that filter. For example:
   - If you want to include all sampling sites **as one group**, check the *Aggregate sites?* box.
   - If you want to summarize across all species, check *Aggregate species?*

   > 💡 Aggregating reduces stratification and combines groups into single summary rows.

3. **Set a Timeframe**  
   Use the **Date Range** selector to limit the data to a specific period. You can also aggregate by month using the *Aggregate months?* checkbox.

4. **Click “Generate Table”**  
   Once all selections and settings are made, click the **Generate Table** button. This will display the customized summary table on the right-hand side of the screen.

#### 📤 Upload Saved Parameters

To reuse a set of filtering and aggregation settings:

1. Upload a previously downloaded `.json` parameter file using the **Upload parameter file** input below the filters.
2. Click **Generate Table** to create a summary table using those exact settings.

> ⚠️ **Note:** Uploaded parameter files will overwrite any selections you’ve made in the UI.

This functionality is designed to give users full transparency, reproducibility, and flexibility when summarizing AST data.

---

## ⚠️ Points of Caution<a name="points-of-caution"></a>

- **Small Counts May Skew Results**  
  Stratifying by too many variables can lead to very small group sizes. Interpret percentages cautiously when total counts are low.

- **Aggregating Can Hide Patterns**  
  Grouping variables (e.g., species or site) simplifies the table but may obscure important differences between categories.

- **Parameter Files Are Data-Specific**  
  If using a saved parameter file, make sure the filter values still exist in your current dataset. Otherwise, no results may be returned. 

---

## 🧰 Data Filters<a name="data-filters"></a>


| **Input**         | **Description**                                              |
|-------------------|--------------------------------------------------------------|
| **Microorganism** | Bacterial species identified in the AST result.              |
| **Antimicrobial** | Antimicrobial agent tested against the isolate.              |
| **Sampling Site** | Body site or specimen type from which the sample was taken.  |
| **Species**       | Host species (e.g., dog, cat).                               |
| **Subregion**     | Geographic region of sample origin (e.g., county, district). |
| **Date Range**    | Timeframe over which AST data is included in the summary.    |
* **Aggregate** Whether to group all values for that filter into a single summary line.

## 📤 Output<a name="output"></a>

After the table is generated, you will have access to the following download buttons:

- **Download Table**  
  Save the summary table as a `.csv` file for use outside of the app.

- **Download Inputs**  
  Export your current filtering and aggregation selections as a `.json` file. This is useful for:
  - Reproducing the same table later
  - Sharing configuration files with collaborators
  - Running consistent analyses across datasets

---

## 💬 Feedback<a name="feedback"></a>

We are continually making improvements to this app and this display. Please report issues or suggestions via [GitHub Issues](https://github.com/ksobkowich/AMRDataVisualizer/issues).
