# Master Thesis Code

## Introduction

This repository contains all code scripts developed for my master thesis research: *A cohort and molecular epidemiological study of the association between platelet count and colorectal cancer survival*.

The project aims to explore the relationship between platelet count and colorectal cancer survival using data from the UK Biobank and West China cohorts. The repository is designed to ensure reproducibility, transparency, and robustness in the computational workflows associated with the thesis.

## File Descriptions

| File                       | Description                                                                                |
| -------------------------- | ------------------------------------------------------------------------------------------ |
| `00a_ukb_survival_data.py` | Processes survival data for the UK Biobank cohort.                                         |
| `00b_ukb_extra_data.py`    | Processes additional data for the UK Biobank cohort.                                       |
| `01a_hx_survival_data.py`  | Processes survival data for the West China cohort.                                         |
| `01b_hx_extra_data.py`     | Processes additional data for the West China cohort.                                       |
| `02_baseline_stats.R`      | Generates baseline statistics for both cohorts.                                            |
| `03_survival_curve.R`      | Creates survival curves for the UK Biobank and West China cohorts.                         |
| `04_coxph_forest.R`        | Performs Cox proportional hazards models and generates forest plots.                       |
| `05_coxph_rcs.R`           | Visualizes restricted cubic splines for Cox proportional hazards models.                   |
| `06_coxph_rolling.R`       | Implements rolling Cox proportional hazards models.                                        |
| `07_genetic_instruments.R` | Identifies genetic instruments for Mendelian randomization.                                |
| `08_gwas_info.R`           | Processes GWAS-related information for the West China cohort.                              |
| `09_plink_data.sh`         | Processes SNP data using PLINK, including sex discrepancy checks, PCA, and SNP extraction. |
| `10_gwas_coxph.R`          | Conducts Cox proportional hazards regression for SNP data.                                 |
| `11_2smr.R`                | Performs two-sample Mendelian randomization analysis.                                      |
| `12_prs.R`                 | Calculates polygenic risk scores.                                                          |
| `13_nlmr.R`                | Conducts nonlinear Mendelian randomization analysis.                                       |
| `14_eqtl_mr.R`             | Performs eQTL Mendelian randomization analysis.                                            |
| `15_gene_survival.R`       | Analyzes the association between gene expression and survival.                             |
| `16_gene_enrichment.R`     | Conducts gene enrichment analysis.                                                         |
| `run_all.sh`               | A bash script to sequentially execute all numbered analysis scripts and log the results.   |

## Usage

1. This project is developed using [Python](https://www.python.org/), [R](https://www.r-project.org/), and [PLINK](https://www.cog-genomics.org/plink/1.9/). Ensure these tools are correctly configured.
2. The Python environment is managed by [uv](https://docs.astral.sh/uv/). Use `uv sync` in the terminal to reproduce the Python environment.
3. The R environment is managed by the [renv](https://rstudio.github.io/renv/) package. Use `renv::restore()` in R to rebuild the R environment.
4. PLINK version `1.9` is required. Ensure it is available in `$PATH`.
5. Run the scripts sequentially by their numerical prefixes or execute `run_all.sh` to automate the process.

## Author

Changtao Li

## License

This project is licensed under the MIT License.
