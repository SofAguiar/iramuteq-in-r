
# Attention! This is a WIP
# IRAMUTEQ in R 

# iramuteq-in-r


**iramuteq-in-r** is a repository for reverse-engineering and reimplementing the **IRaMuTeQ** methodological workflow in **R** (R-only), with a strong focus on:

- **Explicit parameter control** (no hidden GUI defaults)
- **Methodological transparency** (traceable, auditable pipelines)
- **Reproducibility** (seed, versions, checksums, and per-run logs)

> Status: bootstrap structure created on 2026-02-26. Functions under `R/` are currently **stubs** and will be implemented iteratively.

---

## Goal

Provide a pipeline that can ingest an IRaMuTeQ-style corpus (UCI/UCE + `****` separators and `*etoile` variables) and perform:

1. **Corpus reading and parsing**
2. **Cleaning and tokenization**
3. **UCE segmentation** (character- or token-based, with optional “soft” splitting)
4. **Lemmatization via IRaMuTeQ lexique** (compatible with `lexique_*.txt` when supplied)
5. **Sparse matrices and lexical tables** (UCE×terms; groups×terms)
6. **Analyses**:
   - Corpus statistics (freq, hapax, active/supplementary forms)
   - Specificities + CA (Correspondence Analysis)
   - **Labbé** intertextual distance
   - **Reinert/CHD** classification
   - **Similitude** (graph)
   - **Word cloud**
7. **Transparent exports** (CSV/GraphML/PNG/SVG + Quarto/RMarkdown report)

---

## What this repo is NOT

- Not a GUI.
- It does not claim immediate 1:1 replication of IRaMuTeQ outputs — the target is **compatibility + traceability**, validated with test corpora.
- It does not automatically redistribute IRaMuTeQ dictionaries/resources if licensing restricts it.
  (When needed, users provide local paths via configuration.)

---

## Repository structure

- `R/` — pipeline steps and analyses (pure functions + explicit parameters)
- `inst/extdata/config.example.yml` — run configuration example
- `scripts/cli.R` — CLI entrypoint (planned)
- `tests/` — `testthat` tests (validation and regression)
- `vignettes/` — methodological documentation (Quarto/RMarkdown)
- `.github/workflows/` — CI with `R CMD check`

---

## Installation (development)

```r
# From the project root
install.packages("remotes")
remotes::install_local(".")
```

> R package name (DESCRIPTION): **iramuteqinr**  
> Git repository name: **iramuteq-in-r**

---

## Running (planned)

Runs will be driven by a YAML configuration file to guarantee traceability:

- Example: `inst/extdata/config.example.yml`

Planned CLI usage:

```bash
Rscript scripts/cli.R --config path/to/config.yml
```

---

## Transparency & reproducibility principles

Each run should produce:

- `params.json` / `params.yml` (effective parameters)
- `sessionInfo()` (R and package versions)
- `seed` and RNG state (when applicable)
- **checksums** of input files
- intermediate artifacts (e.g., segmented UCEs, sparse matrices, lexical tables)
- final artifacts (CSVs, plots, graphs) + a report

---

## Short roadmap

1. Implement `read_corpus_iramuteq()` (`****` parsing + `*etoile` variables)
2. Implement UCE segmentation (char/token + `douce` behavior)
3. Port lexique/lemmatization (compatible with `lexique_*.txt`)
4. Build sparse matrices and lexical tables
5. Implement analyses (stats → specificities/CA → Labbé → Reinert → similitude → wordcloud)
6. Pipeline runner + CLI + Quarto report

---

## Contributing

- Issues and PRs are welcome.
- Priority: **document parameters**, **record methodological decisions**, and **add tests** that compare against IRaMuTeQ outputs.

---

## References (context)

- IRaMuTeQ (original project): see Pierre Ratinaud's repository.
- Reinert/CHD implementations in R (optional backend): `rainette`.
- Hypergeometric specificities: `textometry` (where applicable).
