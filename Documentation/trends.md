<!-- File: trends.md -->
# 📊 Trends Tab

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

The **Trends** tab provides a time-based view of antimicrobial susceptibility. It displays the **percentage of bacterial isolates interpreted as Susceptible (S)** for a selected antimicrobial over time, using a time series plot.

Date information is extracted from either a single `'Date'` column or from separate `'Year'`, `'Month'`, and/or `'Day'` columns in your dataset. The resulting plot illustrates how **the proportion of susceptible isolates** changes over time. Hovering over any point on the plot reveals additional details, including the **number of isolates tested** and the **percentage found to be susceptible**.

Multiple antimicrobial agents may be selected at one time to produce multiple plotted series.

> ⚠️ **Note:** To maintain compliance with [CLSI M39](https://clsi.org/shop/standards/m39/) guidelines, time points with fewer than 30 observations are automatically **rolled forward** into the next time interval until the minimum sample size is met. This approach also provides a natural smoothing effect.

---

## 📖 How to Interpret the Figure<a name="how-to-interpret-the-figure"></a>

Time series plots show how **antimicrobial susceptibility trends** evolve over time. Each point on the plot represents the **percentage of isolates interpreted as Susceptible (S)** for a given antimicrobial during a specific time point.

These plots are useful for:

- Identifying **long-term trends** (e.g., increasing or decreasing susceptibility)
- Spotting **sudden changes or shifts**, which may suggest emerging resistance or changes in testing practices
- Comparing patterns across antimicrobials, microorganisms, or regions

---

## ⚠️ Points of Caution<a name="points-of-caution"></a>

- **Small Sample Sizes**  
  Time points with low numbers of isolates may produce unstable or misleading percentages.

- **Changes in Testing Behavior**  
  Apparent shifts in resistance may reflect changes in laboratory testing practices, submission patterns, or diagnostic protocols—not necessarily true biological change.

- **Aggregation Effects**  
  Trends may differ depending on how the data are grouped (e.g., by species, region, or site). Always consider applied filters when interpreting the plot.

> Remember to always interpret patterns and trends in the context of your data’s scope, sample sizes, and any applied filters.

---

## 🧰 Data Filters<a name="data-filters"></a>

Customize the antibiogram using the **Filters** panel (click the *pencil* icon ✏️ in the top right corner of the panel to add/remove fields).

| Default Filter          | Purpose                                                      |
|-------------------------|--------------------------------------------------------------|
| **Microorganism** | Select the microorganism to display on the series. By default, the organism with the highest number of observations is selected. If none is selected, **all** will be aggregated.|
| **Antimicrobial**       | Displays results for the selected antimicrobial. By default, the antimicrobial with the highest number of observations is selected. If none is selected, **all** will be presented as individual series.|
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
To help interpret trends in noisy datasets, the **'Controls'** menu provides multiple options for smoothing the time series plots. These can make underlying patterns more visible by reducing short-term fluctuations.

- **Rolling Mean**  
  Applies a **centered moving average** to the data. This smooths out noise by averaging each value with its neighbors within a defined "window."  
  - **Window**: The number of time points (e.g., months or years) to include on either side of each data point. A larger window produces a smoother line but may obscure short-term trends.

- **LOESS (Locally Estimated Scatterplot Smoothing)**  
  Fits a **nonlinear regression curve** to the data using locally weighted fitting. LOESS is ideal for capturing complex, non-linear trends.  
  - **Span**: Controls how much of the data is used in each local regression. A smaller span produces a more flexible curve (closely follows the data), while a larger span results in a smoother, more generalized line.

> ⚠️ **Note:** While smoothing can clarify trends, it may also mask important variation. Always compare the smoothed line to the raw data before drawing conclusions.

---

## 🔹 Legend<a name="legend"></a>

A legend appears on the right-hand side of the time series plot **only when multiple lines are displayed** (e.g., when comparing more than one antimicrobial or group). If a single series is plotted, no legend will be shown, as it is not needed for interpretation.

---

## 📤 Output<a name="output"></a>

- **Save Report** &mdash; download the current plot as an HTML file. The report includes the displayed plot, and all active filters used to generate it. Rendering may take a moment. 


---

## 💬 Feedback<a name="feedback"></a>

We are continually making improvements to this app and this display. Please report issues or suggestions via [GitHub Issues](https://github.com/ksobkowich/AMRDataVisualizer/issues).
