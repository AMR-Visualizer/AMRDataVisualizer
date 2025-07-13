<!-- File: overview-plots.md -->
# 📊 Overview Tab

## 📑 Table of Contents
1. [🧭 Overview](#-overview)
2. [📖 How to Interpret](#-how-to-interpret)
3. [⚠️ Points of Caution](#%ef%b8%8f-points-of-caution)
4. [📊 Visual Components](#-visual-components)
5. [📤 Output](#-output)
6. [💬 Feedback](#-feedback)

---

## 🧭 Overview

The **Overview** tab provides a high-level sanity check of the data you
imported. It offers quick visual summaries to confirm that record counts,
dates, and key categories look as expected before you dive into the
analytical tabs.

Specifically, the tab displays:

- **Tests over Time** – an area plot showing the number of AST tests
  performed on each date.
- **Summary Stats** – a numeric table of core dataset metrics
  (e.g., unique organisms, total tests).
- **Antimicrobial Treemap** – a treemap of test counts by antimicrobial
  and drug class.
- **Frequency Plot** – a user-selectable bar chart of the top 10 values
  for common variables (e.g., organism, source, region).

These visuals help you confirm that date ranges, sample sizes, and test
distributions align with expectations.

---

## 📖 How to Interpret

| Component                | What to Look For                                                        |
|--------------------------|-------------------------------------------------------------------------|
| **Tests over Time**      | Check for gaps or unexpected spikes in testing volume.                  |
| **Summary Stats**        | Validate totals (e.g., isolates per sample) and watch for outliers.     |
| **Antimicrobial Treemap**| Identify which drugs (and classes) are most frequently tested.          |
| **Frequency Plot**       | Use the dropdown to view the top 10 counts by antimicrobial, class, organism, region, source, or species. Hover for exact counts. |

> **Tip:**  Hover over points, bars, or treemap tiles to view tooltips
> containing exact counts.

---

## ⚠️ Points of Caution

- **Date Completeness** – If your dataset lacks dates for some records,
  the *Tests over Time* plot may show unexpected gaps or totals lower
  than expected.
- **Panel Bias** – The treemap reflects **test frequency**, not
  resistance; drugs included on high-volume test panels will dominate
  the display.
- **Top-10 Cut-off** – The frequency plot shows only the ten most common
  values. Rare categories will be hidden unless you explore them in
  other tabs.

---

## 📊 Visual Components

| Visual                | Data Source                                                        | Interactivity                                |
|-----------------------|--------------------------------------------------------------------|----------------------------------------------|
| **Area Plot**         | Daily test counts (`nOverTime`)                                    | Hover tooltip                                |
| **Summary Table**     | Calculated metrics (unique counts, means)                          | Static                                       |
| **Treemap**           | Test counts grouped by antimicrobial and class (`abBreakdown`)     | Hover & click, downloadable via Plotly       |
| **Frequency Plot**    | Top-10 counts for selected variable (`freqPlotSelect`)             | Dropdown selector, hover tooltip, downloadable |

---

## 📤 Output

All interactive plots include a **camera icon** in the Plotly toolbar
allowing you to download high-resolution PNG images.  
The *Summary Stats* table can be copied directly or exported using your
browser’s context menu.

---

## 💬 Feedback

If the overview plots reveal missing data or other issues, revisit the
**Import** tab to correct them. For questions or feature requests, open
an issue on our
[GitHub repository](https://github.com/ksobkowich/AMRDataVisualizer/issues).
