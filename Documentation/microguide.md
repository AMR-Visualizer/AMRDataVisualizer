<!-- File: trends.md -->
# 📊 Trends Tab

## Table of Contents
1. [Overview](#overview)
2. [What's Included](#whats-included)
3. [Points of Caution](#points-of-caution)
4. [Data Filters](#data-filters)
5. [Feedback](#feedback)

---

## 🧭 Overview<a name="overview"></a>

The **Microguide** tab offers high-level microbiological, clinical, and epidemiological information about the microorganisms found in your dataset. This content is publicly available and sourced directly from [Firstline](https://firstline.org/), a clinical decision support platform.

On the **left-hand side**, you’ll find a frequency plot showing the number of isolates per bacterial species. This plot supports a limited set of filters and serves as an interactive navigation tool. **Clicking on any bar** in the plot will automatically load the corresponding microorganism information in the **right-hand panel**.

If species-specific information is unavailable, the Microguide will display more general information at the **genus level**, when possible.

Alternatively, you can use the dropdown menu above the right panel to manually select a microorganism and view its details.

> ⚠️ **Note:** The **Microguide** currently focuses on microorganisms and information relevant to veterinary medicine. Support for human pathogens and expanded species lists is under development.

---

## 📖 What's Included<a name="whats-included"></a>

| Section                | Description                                                                 |
|------------------------|-----------------------------------------------------------------------------|
| **Microorganism Name** | The full scientific name of the organism (e.g., *Staphylococcus pseudintermedius*). |
| **Precautions**        | Recommended infection control practices (e.g., Standard Precautions).       |
| **Gram Stain**         | Classification based on Gram staining (e.g., Gram-positive, Gram-negative). |
| **Oxygen Tolerance**   | Aerobic, anaerobic, or facultative anaerobic characteristics.               |
| **Notes**              | Key microbiological traits such as morphology, coagulase status, or other identifiers. |
| **Treatment Considerations** | Suggested first-line and second-line antimicrobials, including resistance considerations and guidelines for reserving higher-tier drugs. |
| **Epidemiology**       | Describes typical host associations, colonization patterns, and relevance in clinical contexts. |
| **Diagnosis**          | Recommended diagnostic approach, such as culture.                           |
| **Associated Syndromes** | Common clinical presentations and body systems affected by the organism.    |
| **Additional Information** | Other relevant details that may inform interpretation or decision-making. |

---

## ⚠️ Points of Caution<a name="points-of-caution"></a>

This display is for informational and educational purposes only. It is not intended to replace clinical judgment or veterinary oversight.

---

## 🧰 Data Filters<a name="data-filters"></a>

| Default Filter          | Purpose                                                      |
|-------------------------|--------------------------------------------------------------|
| **Sample Source / Site**| Restrict to selected body site(s).                           |
| **Species (Host)**      | Restrict to selected species.                                |
| **Region**              | If present in your data. Restrict to selected geographic region.|
| **Subregion**           | If present in your data. Restrict to selected geographic subregion.|

---

## 💬 Feedback<a name="feedback"></a>

We are continually making improvements to this app and this display. Please report issues or suggestions via [GitHub Issues](https://github.com/ksobkowich/AMRDataVisualizer/issues).

