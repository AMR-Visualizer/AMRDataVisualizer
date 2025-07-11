<!-- File: mdr-matrices.md -->
# 📊 MDR Tab

## Table of Contents
1. [Overview](#overview)
2. [How to Interpret the Figure](#how-to-interpret-the-figure)
3. [Points of Caution](#points-of-caution)
4. [Data Filters](#data-filters)
5. [Plot Controls](#plot-controls)
6. [Legend](#legend)
7. [Output](#output)
8. [Feedback](#feedback)

---

## 🧭 Overview<a name="overview"></a>

The Multi-Drug Resistance (MDR) Matrices tab presents a heatmap illustrating pairwise correlations in resistance patterns across antimicrobials. This visualization helps uncover patterns of co-resistance—where resistance to one drug tends to be associated with resistance to others.

This can help users:
- Explore resistance clustering by antimicrobial class
- Identify co-resistance patterns that may guide further investigation
- Generate hypotheses about potential shared resistance mechanisms

The correlations are calculated across isolates based on whether they were interpreted as resistant (R) or intermediate (I). Only isolates that were observed to be resistant to **at least 1 agent in 3 or more distinct classes** are included. This follows the widely accepted definition of MDR by [Magiorakos et al., 2011](https://pubmed.ncbi.nlm.nih.gov/21793988/).

---

## 📖 How to Interpret the Figure<a name="how-to-interpret-the-figure"></a>

Each cell in the matrix represents the **Pearson correlation coefficient** between resistance patterns of two antimicrobial drugs.

The color scale ranges from:
- Red (-1): perfect negative correlation (rare in AMR)
- White (0): no correlation
- Teal/Blue (+1): perfect positive correlation (i.e., resistance to one drug often coincides with resistance to another)

Hovering over a cell reveals:
- The drug pair
- The correlation coefficient
- The number of isolates used to compute the value

Only drugs with at least 30 interpretable results are shown, per [CLSI M39](https://clsi.org/shop/standards/m39/) guidelines. Likewise, individual drug pair correlations are only shown if 30 or more isolates were tested for both.

---

## ⚠️ Points of Caution<a name="points-of-caution"></a>

- Correlation ≠ Causation: High correlation does not mean that drugs share a resistance mechanism. Use this tool to explore patterns, not to draw definitive conclusions.
- Sample Size Sensitivity: Cells with fewer than 30 observations are excluded entirely to prevent misleading interpretations.
- Missing Data Bias: Not all isolates are tested against all drugs. Patterns may reflect testing bias more than true biological correlation.
- Visual Density: Large matrices may become hard to interpret. Use filters to reduce clutter.

>Note: Many diagnostic labs use standardized antimicrobial panels, meaning that certain groups of drugs (e.g., β-lactams or fluoroquinolones) are always tested together. This can introduce artificially high correlations within those drug classes—not necessarily because of true biological co-resistance, but due to testing patterns. Interpret these associations with caution and in the context of your lab’s testing practices.
---

## 🧰 Data Filters<a name="data-filters"></a>

Customize the antibiogram using the **Filters** panel (click the *pencil* icon ✏️ in the top right corner of the panel to add/remove fields).

| Default Filter          | Purpose                                                      |
|-------------------------|--------------------------------------------------------------|
| **Microorganism**       | Limit plot to one or more organisms.                         |
| **Sample Source / Site**| Restrict to selected body sites.                             |
| **Species (Host)**      | Select one or more host species.                             |
| **Timeframe**           | Specify a date range (manual or quick-select).               |

*Leaving a filter blank will include all available values in that category.*

Several additional filters are available to refine the data shown. While many are self-explanatory, the following filters warrant further clarification:

| Filter                     | Use Case                                                                                                                    |
|----------------------------|-----------------------------------------------------------------------------------------------------------------------------|
| **Antimicrobial Class**    | Show only selected drug classes.                                                                                            |
| **Resistant to:**  | Display isolates resistant to a chosen drug, useful for cross-resistance analysis.                                          |
| **WHO AWaRe Class**        | Limit drugs to *Access*, *Watch*, or *Reserve* categories per [WHO AWaRe](https://www.who.int/publications/i/item/2021-aware-classification). |

> *Note:* Applying filters will reduce the sample size and may affect stability of estimated susceptiblity.

---

## 🎛 Plot Controls<a name="plot-controls"></a>

Not plot controls are present at this time.

---

## 🔹 Legend<a name="legend"></a>
---

The **MDR Matrix** uses a color-coded heatmap to display **pairwise correlations** in resistance patterns between antimicrobials. Each cell represents the strength and direction of the correlation between resistance to two drugs.

| Correlation Value | Color         | Interpretation                                     |
|-------------------|---------------|----------------------------------------------------|
| **~ 1.0**          | `Teal`       | Strong **positive** correlation – resistance tends to occur together |
| **~ 0.0**          | `Gray`       | No meaningful correlation |
| **~ -1.0**         | `Red`        | Strong **negative** correlation|

---

## 📤 Output<a name="output"></a>

- **Save Report** &mdash; download the current plot as an HTML file. The report includes the displayed plot, and all active filters used to generate it. Rendering may take a moment. 

---

## 💬 Feedback<a name="feedback"></a>

We are continually making improvements to this app and this display. Please report issues or suggestions via [GitHub Issues](https://github.com/ksobkowich/AMRDataVisualizer/issues).
