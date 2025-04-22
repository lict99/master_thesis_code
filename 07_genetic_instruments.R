# %%
# Attaching packages and functions
library("readr")
library("dplyr")
library("TwoSampleMR")

# %%
# Setting up output directory
output_dir <- "results/07"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# %%
# Reading exposure data from OpenGWAS
exposure_data <- read_csv("data/snp/open_gwas/ebi-a-GCST90002358.csv") |>
  filter(pval.exposure < 5e-8 & between(eaf.exposure, 0.05, 0.95)) |>
  as.data.frame() |>
  format_data(
    type = "exposure",
    phenotype_col = "exposure",
    snp_col = "SNP",
    beta_col = "beta.exposure",
    se_col = "se.exposure",
    eaf_col = "eaf.exposure",
    effect_allele_col = "effect_allele.exposure",
    other_allele_col = "other_allele.exposure",
    pval_col = "pval.exposure",
    id_col = "id.exposure",
    chr_col = "chr.exposure",
    pos_col = "pos.exposure",
    samplesize_col = "samplesize.exposure"
  ) |>
  clump_data(
    clump_kb = 10000,
    clump_r2 = 0.001,
    clump_p1 = 1,
    clump_p2 = 1,
    pop = "EAS",
    bfile = "data/snp/1kg.v3/EAS",
    plink_bin = system("which plink", intern = TRUE)
  )

# %%
# Saving exposure data
save(exposure_data, file = file.path(output_dir, "exposure_data.rda"))

# Saving SNPs
write_delim(
  exposure_data["SNP"],
  file = file.path(output_dir, "iv.txt"),
  delim = " ",
  col_names = FALSE
)
