# Attencion! This is a WIP
# IRAMUTEQ in R 

[![R-v4.0+](https://img.shields.io/badge/R-v4.0+-blue.svg)](https://www.r-project.org/)
[![License: GPL-v3](https://img.shields.io/badge/License-GPL--v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

This repository provides a **programmatic reproduction** of the analyses performed by the IRAMUTEQ software (version 0.8 Alpha 7) directly within the R environment. The goal is to bypass the Graphical User Interface (GUI), allowing for greater methodological transparency, parameter customization, and workflow automation for textual data analysis.

## Motivation and Key Advantages

While IRAMUTEQ is a powerful interface, executing it via R scripts offers:
* **Transparency:** Direct inspection of classification algorithms and dimensionality reduction techniques.
* **Reproducibility:** Easily replicate analyses across different datasets without manual clicks.
* **Customization:** Freedom to adjust plot themes, lemmatization rules, and frequency thresholds that are restricted in the GUI.
* **Integration:** Connect results directly with modern NLP and visualization packages (e.g., ggplot2, tidytext).

## Implemented Analyses
This project aims to cover the core functionalities of IRAMUTEQ:
- [ ] **Textual Statistics:** Word frequency, active and supplementary forms.
- [ ] **Word Clouds:** Advanced aesthetic customization.
- [ ] **Similarity Analysis:** Based on graph theory.
- [ ] **DHC (Reinert Method):** Descending Hierarchical Classification and cluster analysis.
- [ ] **Correspondence Analysis (CA):** Factorial visualization of classes and segments.

## Source and Credits

This project utilizes and adapts the computational routines originally developed by **Pierre Ratinaud**, 
the author of the IRAMUTEQ software.

* **Original Author:** Pierre Ratinaud (Laboratoire LERASS).
* **Copyright:** (c) 2008-2011 Pierre Ratinaud.
* **Original License:** GNU/GPL.
* **Source Repository:** [GitLab Huma-Num - IRAMUTEQ Rscripts](https://gitlab.huma-num.fr/pratinaud/iramuteq/-/tree/master/Rscripts)

### Why this adaptation?
Although IRAMUTEQ is a well-established tool, using its scripts directly in R enables:
1. **Methodological Transparency:** Inspection of every step in the statistical processing.
2. **Modernization:** Compatibility adjustments for recent R versions (4.0+) and visualization packages.
3. **Customization:** Modification of lemmatization parameters and dictionaries that are rigid within the GUI.

