# %%
# Attaching packages
library("readr")
library("dplyr")
library("GWASTools")
library("SNPRelate")
library("survival")

# %%
# Setting up output directory
output_dir <- "results/10"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# %%
# Reading West China data
hx_data <- read_csv("results/01/hx_data.csv") |>
  mutate(
    sex_male = case_match(
      sex,
      "male" ~ 1,
      "female" ~ 0,
      .default = NA_real_
    )
  ) |>
  select(id, age, sex_male, os, os_time, css, css_time, dfs, dfs_time)

# %%
# Reading PCA data
pca <- read_delim(
  "results/09/pca5.eigenvec",
  delim = " ",
  col_names = c("fid", "iid", paste0("pc", 1:5))
)

# %%
# Merging PCA data with West China data
hx_df <- inner_join(
  pca,
  hx_data,
  by = join_by(iid == id)
) |>
  as.data.frame()

# %%
# Reading SNP sample data
plink_samples <- read_delim(
  "results/09/data_final.fam",
  delim = " ",
  col_names = FALSE
) |>
  as.data.frame()

# %%
# Checking if sample IDs match
if (!identical(hx_df$iid, plink_samples[[1]])) {
  stop("Sample IDs do not match", call. = FALSE)
}

# %%
# Performing Cox proportional hazards regression for selected SNPs
for (event in c("os", "css", "dfs")) {
  gds_file <- tempfile(fileext = ".gds")

  # transforming the PLINK bed files to GDS format
  snpgdsBED2GDS(
    bed.fn = "results/09/data_final.bed",
    fam.fn = "results/09/data_final.fam",
    bim.fn = "results/09/data_final.bim",
    out.gdsfn = gds_file
  )

  gds <- GdsGenotypeReader(
    gds_file,
    YchromCode = 24L,
    XYchromCode = 25L
  )

  # Extraction of SNP information
  gt_info <- data.frame(
    snp_id = getSnpID(gds),
    chromosome = getChromosome(gds),
    position = getPosition(gds),
    effect_allele = getAlleleA(gds),
    other_allele = getAlleleB(gds)
  )

  # Checking if sample IDs match
  scan_id <- getScanID(gds)
  if (!identical(scan_id, hx_df[["iid"]])) {
    stop("Not aligned with sample IDs", call. = FALSE)
  }

  # Setting up covariates
  covars <- c("age", "sex_male", paste0("pc", 1:5))

  # selecting necessary columns
  fit_df <- hx_df |>
    select(all_of(c(covars, event, paste0(event, "_time")))) |>
    as.data.frame()

  # Getting genotype coded by number of allele A
  gt_a <- getGenotype(
    gds,
    snp = c(1, -1),
    drop = FALSE,
    use.names = FALSE,
    transpose = TRUE
  ) |>
    as.data.frame()

  # Releasing resources
  close(gds)
  unlink(gds_file)

  # Performing Cox proportional hazards regression
  coxph_list <- lapply(
    gt_a,
    function(gt) {
      df <- cbind(data.frame(gt = gt), fit_df)
      df <- na.omit(df)

      # Calculating effect allele frequency
      eaf <- sum(df[["gt"]]) / (2 * nrow(df))

      fml <- as.formula(
        paste(
          sprintf("Surv(time = %s, event = %s)", paste0(event, "_time"), event),
          paste(c("gt", covars), collapse = " + "),
          sep = " ~ "
        )
      )
      fit <- summary(coxph(formula = fml, data = df))

      return(
        data.frame(
          coef = fit[["coefficients"]]["gt", "coef"],
          se = fit[["coefficients"]]["gt", "se(coef)"],
          p_value = fit[["coefficients"]]["gt", "Pr(>|z|)"],
          eaf = eaf,
          hr = fit[["conf.int"]]["gt", "exp(coef)"],
          hr_l95 = fit[["conf.int"]]["gt", "lower .95"],
          hr_u95 = fit[["conf.int"]]["gt", "upper .95"],
          n = fit[["n"]],
          n_event = fit[["nevent"]]
        )
      )
    }
  )

  model_results <- cbind(gt_info, do.call("rbind", coxph_list))

  # Saving results
  write_csv(
    model_results,
    file = file.path(output_dir, sprintf("gwas_%s_coxph.csv", event))
  )
}
