<!-- File: maps.md -->
# 📊 Map Tab

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

The **Map** tab provides a geographic overview of antimicrobial susceptibility patterns. It displays the **percentage of bacterial isolates interpreted as Susceptible (S)** for a given antimicrobial across mapped regions.

Geographic information is automatically extracted from the `'Region'` and `'Subregion'` columns in your dataset and matched to built-in shapefiles. The resulting maps are interactive, supporting zoom, pan, and click actions. Clicking on a region provides additional details, including the **number of isolates tested**, as well as counts of **Susceptible**, **Intermediate**, and **Resistant** interpretations.

> ⚠️ **Note:** Not all locations are currently supported with map files.

### Supported Map Regions
- **USA**
  - State-level
  - County-level

---

## 📖 How to Interpret the Figure<a name="how-to-interpret-the-figure"></a>

This map displays antimicrobial susceptibility patterns across geographic regions using a choropleth format, where color intensity reflects the proportion of isolates interpreted as Susceptible (S) in each area.

These visualizations are useful for identifying **broad spatial trends**, such as:
- Regional differences in antimicrobial effectiveness
- Potential **clusters** of higher or lower resistance
- Geographic areas that may warrant further investigation or targeted stewardship efforts

Keep in mind that these maps provide a **high-level overview**. Results should be interpreted in the context of sample size, population characteristics, and local testing practices.

---

## ⚠️ Points of Caution<a name="points-of-caution"></a>

- **Sample Size Variability**  
  Regions with very few isolates may display unstable or misleading percentages. Always consider the number of observations behind each value.

- **Data Resolution**  
  Maps are only as specific as the geographic information provided. Broad regions (e.g., states or provinces) may obscure local variation.

- **Visual Imbalance**  
  Densely populated areas (where most observations tend to originate) often occupy the smallest geographic space on the map. As a result, they may appear less visually prominent, despite contributing the majority of the data. Interpret patterns with this spatial imbalance in mind.

- **Non-Clinical Factors**  
  Differences in resistance may be influenced by data collection practices, testing frequency, or sample source distribution, not just true biological variation.

>Use maps as an exploratory tool, not a substitute for clinical judgment or localized investigation.

---

## 🧰 Data Filters<a name="data-filters"></a>

Customize the map using the **Filters** panel (click the *pencil* icon ✏️ in the top right corner of the panel to add/remove fields).

| Default Filter          | Purpose                                                      |
|-------------------------|--------------------------------------------------------------|
| **Microorganism** | Select the microorganism to display on the map. By default, the organism with the highest number of observations is selected. If none is selected, **all** will be aggregated.|
| **Antimicrobial**       | Displays results for the selected antimicrobial. By default, the antimicrobial with the highest number of observations is selected. If none is selected, **all** will be aggregated.|
| **Sample Source / Site**| Restrict to selected body sites.                             |
| **Species (Host)**      | Select one or more host species.                             |
| **Timeframe**           | Specify a date range (manual or quick-select).               |

Several additional filters are available to refine the data shown. While many are self-explanatory, the following filters warrant further clarification:

| Filter                    | Description                                                                                                                  |
|---------------------------|------------------------------------------------------------------------------------------------------------------------------|
| **Antimicrobial Class**   | Displays only antimicrobials belonging to the selected class(es). Useful for focusing on specific drug groups.              |
| **Resistant to**          | Filters the dataset to include only isolates resistant to a selected antimicrobial. Helpful for exploring cross-resistance patterns. |
| **WHO AWaRe Class**       | Limits the display to antimicrobials categorized as *Access*, *Watch*, or *Reserve* according to the [WHO AWaRe classification](https://www.who.int/publications/i/item/2021-aware-classification). |

> *Note:* Applying filters will reduce the sample size and may affect stability of estimated susceptiblity.

---

## 🎛 Plot Controls<a name="plot-controls"></a>

No additional controls exist at this time.

---

## 🔹 Legend<a name="legend"></a>

A legend is displayed on the right-hand side of the map, indicating which colors correspond to specific ranges of the **proportion of isolates interpreted as susceptible (S)**.

The color scale is **binned**, meaning it is divided into discrete intervals rather than a continuous gradient. This helps simplify interpretation but may reduce granularity between similar values.

To flag regions with limited data (i.e., fewer than 30 isolates, as per [CLSI M39](https://clsi.org/shop/standards/m39/)), a **hatched fill pattern** is applied instead of a solid color. This serves as a visual cue that the result should be interpreted with caution, as small sample sizes may not provide reliable estimates.

---

## 📤 Output<a name="output"></a>

- **Save Report** &mdash; download the current plot as an HTML file. The report includes the displayed plot, and all active filters used to generate it. Rendering may take a moment. 


---

## 💬 Feedback<a name="feedback"></a>

We are continually making improvements to this app and this display. Please report issues or suggestions via [GitHub Issues](https://github.com/ksobkowich/AMRDataVisualizer/issues).
