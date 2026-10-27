# AMR Visualizer: A Shiny App for the Rapid Analysis of Diagnostic Antimicrobial Susceptibility Test Data

**Kurtis E. Sobkowich**<sup>1</sup>, **Claudia Gisela Cobo Angel**<sup>\*,2,3</sup>, **Joshua Glass**<sup>\*,2</sup>, **Abdolreza Mosaddegh**<sup>\*,2,4</sup>, **Zvonimir Poljak**<sup>1</sup>, **Donald Szlosek**<sup>5</sup>, **J. Scott Weese**<sup>6,7</sup>, **Casey L. Cazer**<sup>2,8</sup>  

<sup>1</sup> Department of Population Medicine, Ontario Veterinary College, University of Guelph  
<sup>2</sup> Department of Clinical Sciences, College of Veterinary Medicine, Cornell University  
<sup>3</sup> ICARS - International Centre for Antimicrobial Resistance Solutions  
<sup>4</sup> Multidisciplinary Graduate Engineering Department, College of Engineering, Northeastern University  
<sup>5</sup> IDEXX Laboratories Ltd.  
<sup>6</sup> Department of Pathobiology, Ontario Veterinary College, University of Guelph  
<sup>7</sup> Centre for Public Health and Zoonoses, Ontario Veterinary College, University of Guelph  
<sup>8</sup> Department of Public and Ecosystem Health, College of Veterinary Medicine, Cornell University  

<sub>\*These authors contributed equally to this work.</sub>

# Summary

Antimicrobial resistance (AMR) is a critical global health threat that demands consistent, comparable, data-driven analysis. Yet, the complexity of working with AMR data, and the variability in analytical methods, often hampers comparison across studies and institutions. While open-source tools offer potential for standardization, they frequently require substantial statistical programming knowledge and impose rigid data formatting constraints, limiting their accessibility and adoption. These barriers contribute to fragmented surveillance and disconnected analytical efforts.

To address this challenge, we developed a standardized, user-friendly AMR analysis pipeline implemented as an interactive R Shiny dashboard. The tool enables users to import historical antimicrobial susceptibility testing (AST) data with minimal formatting and conduct robust analyses through an intuitive interface. By applying a consistent pipeline across datasets, it facilitates faster, more accessible analysis and improves comparability of resistance trends and patterns across institutions and studies.

![System architecture of the AMR Visualizer.](figures/system_architecture.png)

# Statement of Need

Antimicrobial susceptibility test (AST) data from diagnostic laboratories are a valuable but underused resource for epidemiologists, microbiologists, and clinicians in human and veterinary medicine [@10.1093/ofid/ofw172.673]. This underuse limits their potential to inform empiric therapy and strengthen AMR surveillance [@doi:10.1128/jcm.00184-07]. Laboratories face barriers to adopting new tools, including costs, limited staff expertise, privacy concerns, and the need for stakeholder buy-in [@10.1093/jac/dkae279]. Open-source packages can reduce reporting time and improve accuracy [@10.1093/jacamr/dlac143], but they require programming skills that many people managing AST data lack. AMR Visualizer addresses this with an open-source, customizable graphical interface that supports rapid, standardized exploratory analysis and follows good practice in AMR data cleaning and interpretation.

# State of the Field

Existing AMR tools fall into three groups. The `AMR` R package [@JSSv104i03] is a peer-reviewed library for taxonomy resolution, EUCAST/CLSI interpretation, and resistance analysis. It accepts data in many formats, but only through user-written code. WHONET (<https://whonet.org>) is a free, widely adopted Windows desktop application that supports EUCAST and CLSI human and veterinary breakpoints and imports data from laboratory systems through its separate BacLink utility. Shiny applications such as RadaR (<https://github.com/ceefluz/radar>) target hospital stewardship data, and others visualize public datasets such as ATLAS.


# Approach

The AMR Visualizer is designed with flexibility and accessibility in mind as it attempts to limit the amount of preprocessing required before data are uploaded. This "format-free upload" philosophy introduces several challenges that need to be overcome to accommodate the diverse formats of antimicrobial susceptibility testing (AST) data encountered across institutions. Data from different institutions are likely to differ in both format (e.g., column structure/layout) and content (e.g., naming conventions), both of which must be addressed.

![AMR Visualizer data import interface, allowing users to manually map column headers to the required features for analysis.](figures/screenshot1.png)

## *Harmonized Formatting*

The application accepts data in both wide format (one row per sample with multiple test result columns) and long format (multiple rows per sample, each representing a single test). If data are provided in wide format, the app automatically reshapes them into long format using pattern recognition to enable standardized downstream analysis. To handle datasets with different column structures, the app uses keyword-based detection to identify common fields (e.g., date, organism, antimicrobial). Users can review and adjust these column mappings to correct any misclassifications. Additional preprocessing steps ensure consistency, for example, dates provided as separate day, month, and year columns are combined and converted into a standardized format. The aim is to ensure that, regardless of the original structure or layout, all uploaded data are standardized into a common format for analysis.

## *Harmonized Content*

Organism and antimicrobial names are standardized with the controlled vocabularies and taxonomies of the `AMR` R package [@JSSv104i03], applied through rule-based fuzzy matching. This handles misspellings, abbreviations, and synonyms such as generic versus trade drug names. Antimicrobial class and microorganism Gram-stain are inferred from the same package. For mapping, user-supplied region and subregion entries are matched against locally stored geographic boundary files. Additional columns can be retained for filtering or subgrouping but are not cleaned or standardized.

## *Analysis*

By standardizing diverse data sources into a consistent structure and nomenclature, the AMR Visualizer enables uniform analytical workflows across varied datasets. This harmonization simplifies comparisons between laboratories by reconciling synonymous terms and formatting inconsistencies. In contexts where sharing raw data is limited by intellectual property concerns, patient confidentiality, or regulatory restrictions, the application offers an alternative: users can generate and share aggregated, anonymized summary tables that contain no patient-identifying information.

The application is developed in R, utilizing Shiny for the user interface, `dplyr` and `tidyr` for data transformation, and `ggplot2`, `DT`, `Leaflet`, and `Plotly` for data visualization.

## *Data Privacy*

In adherence to data privacy considerations, all processing occurs locally within the user's environment, and no uploaded data are stored externally. Users have the option to download the cleaned dataset for external use or future re-upload, in which case the data processing steps are skipped to improve efficiency.

# Software Design

AMR Visualizer is built in R Shiny as separate modules for data import, cleaning, transformation, and visualization. This separation makes the software easier to maintain and extend, for example by adding new analysis tabs or guidelines without touching the ingestion pipeline. Cleaned data are held in a common internal format, and reactive programming lets users filter and explore results interactively without rerunning the cleaning steps. The main trade-off is that accepting heterogeneous input increases implementation complexity, which we accepted to reduce the preprocessing burden on users.

# Research Impact Statement

Routinely collected AST data could guide empiric therapy and surveillance, but much of it goes unanalyzed because the people who hold it cannot use code-based tools. AMR Visualizer removes that barrier. It is a free, open-source application that takes minimally formatted laboratory exports through harmonization, breakpoint interpretation, and analysis in a single workflow, so a laboratory can go from raw export to antibiograms, geographic maps, temporal trends, and MDR patterns without writing code. Because every dataset passes through the same pipeline, results are comparable across institutions, and aggregated, anonymized summary tables can be shared when raw data cannot leave the laboratory.

The software has been tested in academic and collaborative veterinary AMR settings to summarize historical susceptibility data, generate customized antibiograms, and examine temporal and geographic resistance patterns. Sample data, a Docker image, and a demonstration video allow new users to reproduce these analyses.

# Usage

This tool is intended for professionals with access to clinical microbiology data and a working understanding of antimicrobial resistance. No background in epidemiology, programming, or statistics is required, but users should be aware of the limitations of diagnostic laboratory data and avoid overgeneralizing its applicability.

Users can load bundled public sample data, upload raw .csv or .parquet files, or upload a previously cleaned file downloaded from the app. Uploaded data are not stored after the app is closed, so users are encouraged to download cleaned data for later use. On upload, the app checks embedded metadata to determine whether the file has already been cleaned; if so, cleaning is skipped. Otherwise, the app guides the user through column mapping and data cleaning before analysis. Data may be pre-interpreted susceptible/intermediate/resistant (S/I/R) results or raw minimum inhibitory concentration (MIC) values; for MICs, users choose a European Committee on Antimicrobial Susceptibility Testing (EUCAST) or Clinical and Laboratory Standards Institute (CLSI) breakpoint guideline for interpretation. Multiple cleaned files can be uploaded together and are merged automatically, so routine updates (e.g., monthly data) only require cleaning the newest file. A log documents all changes to organism and antimicrobial names, which users can also adjust manually.

Following data processing, several analysis tabs become available to the user. Available tabs include:

-   **Overview** - Generate descriptive statistics and frequency plots to summarize the dataset.

-   **MIC Tables** - Interpretation tables summarizing how MIC values were interpreted according to the selected breakpoints.

-   **Antibiogram** - Rapidly construct customized antibiograms through a combination of data filters and controls.

-   **Maps** - Visualize regional antimicrobial susceptibility through choropleth maps.

-   **Trends** - Analyze temporal trends in antimicrobial susceptibility.

-   **MDR** - Explore multidrug resistance (MDR) patterns using correlation matrices to identify relationships between resistance profiles.

-   **Explore** - Generate customizable summary tables of susceptible, intermediate, and resistant isolates based on user-defined strata.

  Across all tabs, filters and controls are provided such that users can assess resistance in specific scenarios. Tables and figures are readily saved as images and `html` documents, with all applied filters recorded for transparency and reproduction.
  
![AMR Visualizer antibiogram tab.](figures/screenshot2.png)

# Acknowledgements

We gratefully acknowledge the contributions of numerous individuals whose support and input were instrumental in the development of this antimicrobial resistance (AMR) analysis application. We thank the domain experts, epidemiologists, microbiologists, and developers who provided valuable feedback and guidance throughout the design and testing phases.

In particular, we would like to acknowledge the following contributors:\
*Gayatri Anil, Wendy Anne Beauvais, Emma Bollig, Melissa Boyd, Stephen Cole, Anne Deckert, Jaclyn Dietrich, Amy Fridlund, Amelia Frye, Jennifer Granick, Cassandra Guarino, Carolyn (Lynn) Guptill‑Yoran, Kassie Marino, Uli Muellner, Maho Okumura, Poppy Pakinui, Santosh Kumar Satapathy, & Nick Snellgrove*

All listed contributors are acknowledged for their collaborative input, thoughtful discussions, and/or testing support and are listed in alphabetical order to reflect equal contributions.

# Availability and Community Guidelines

The AMR Visualizer is under active development, with continuous enhancements aimed at expanding its features and supporting analyses across diverse microbial species and geographic contexts. Users and contributors are encouraged to report bugs, suggest new features, or contribute code through the project's GitHub repository. This collaborative approach ensures the tool remains responsive to the evolving needs of the AMR research community.  

The application is accessible through the following channels:

-   **Docker Image:**\
    `ksobkowich/amrdata-visualizer`\
-   **Source Code:**\
    Available on [GitHub](https://github.com/ksobkowich/AMRDataVisualizer) under the MIT License.\
    **Run Locally:**\
    Clone the repository or launch directly in R using:\
    `shiny::runGitHub("ksobkowich/AMRDataVisualizer")`\
-   **Demonstration Video:**\
    A walkthrough is available on [YouTube](https://www.youtube.com/watch?v=QYW0U3wZqoQ&ab_channel=KurtisSobkowich).

The application comes pre-loaded with sample data to help users explore its features and functionality.

# AI Usage Disclosure

Generative AI tools were used in a limited support capacity during software development and manuscript preparation. AI assistance was used primarily for brainstorming, code debugging support, wording refinement, and editorial feedback. All scientific content, software design decisions, analytical logic, validation, and final manuscript text were reviewed and approved by the authors. No AI system was used to generate results, perform analyses, or make scientific conclusions.

# References

