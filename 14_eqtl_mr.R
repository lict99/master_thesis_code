# %%
# Attaching packages
library("readr")
library("dplyr")
library("Qtlizer")
library("openxlsx2")
library("TwoSampleMR")

# %%
# Setting up output directory
output_dir <- "results/14"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# %%
# Loading IV information
load("results/11/mr_iv_info.rda")

iv_list <- lapply(
  mr_iv_info,
  function(x) x[x[["mr_keep"]], , drop = FALSE][["SNP"]]
)

# Checking if the IVs are identical across OS, CSS, and DFS
# We make sure that the IVs are identical and use one of them
if (identical(iv_list$os, iv_list$css) && identical(iv_list$os, iv_list$dfs)) {
  iv <- iv_list$os
} else {
  stop("Not identical IVs between OS, CSS, and DFS", call. = FALSE)
}

# %%
# Getting and saving eQTL information from Qtlizer database
if (file.exists("data/cache/eqtl_query.csv")) {
  eqtl_query <- read_csv("data/cache/eqtl_query.csv")
} else {
  eqtl_query <- get_qtls(iv)
  write_csv(eqtl_query, "data/cache/eqtl_query.csv", na = "")
}

# %%
# Filtering eQTL information for GTEx v8 Colon tissues
eqtl_gtex_colon <- eqtl_query |>
  filter(
    source == "GTEx v8",
    tissue %in% c("Colon - Sigmoid", "Colon - Transverse")
  ) |>
  arrange(as.numeric(p)) |>
  distinct(sentinel, gene, ensgid, .keep_all = TRUE) |>
  arrange(as.numeric(chr), as.numeric(var_pos_hg19))

# %%
# Reading GTEx v8 eQTL data for Colon tissues
gtex_sigm <- read_delim(
  "data/gtex_v8_eqtl/Colon_Sigmoid.v8.signif_variant_gene_pairs.txt.gz"
) |>
  mutate(gene_id = substr(gene_id, 1, 15))

gtex_tran <- read_delim(
  "data/gtex_v8_eqtl/Colon_Transverse.v8.signif_variant_gene_pairs.txt.gz"
) |>
  mutate(gene_id = substr(gene_id, 1, 15))

# %%
# Extracting eQTL information and checking if the eQTL information is consistent
# with the GTEx database
eqtl_info <- lapply(
  seq_len(nrow(eqtl_gtex_colon)),
  function(i) {
    # Information from the Qtlizer database
    snp <- eqtl_gtex_colon[i, "sentinel", drop = TRUE]
    chr <- eqtl_gtex_colon[i, "chr", drop = TRUE]
    pos_hg19 <- eqtl_gtex_colon[i, "var_pos_hg19", drop = TRUE]
    pos_hg38 <- eqtl_gtex_colon[i, "var_pos_hg38", drop = TRUE]
    gene <- eqtl_gtex_colon[i, "gene", drop = TRUE]
    gene_id <- eqtl_gtex_colon[i, "ensgid", drop = TRUE]
    ea <- eqtl_gtex_colon[i, "ea", drop = TRUE]
    nea <- eqtl_gtex_colon[i, "nea", drop = TRUE]
    beta <- eqtl_gtex_colon[i, "beta", drop = TRUE] |> as.numeric()
    p <- eqtl_gtex_colon[i, "p", drop = TRUE] |> as.numeric()
    tissue <- eqtl_gtex_colon[i, "tissue", drop = TRUE]

    if (tissue == "Colon - Sigmoid") {
      database <- gtex_sigm
    } else if (tissue == "Colon - Transverse") {
      database <- gtex_tran
    } else {
      stop("Unknown tissue", call. = FALSE)
    }

    # Information from the GTEx database
    # {chr}_{pos_first_ref_base}_{ref_seq}_{alt_seq}_b38
    # nea is ref, ea is alt
    variant_id <- paste(paste0("chr", chr), pos_hg38, nea, ea, "b38", sep = "_")

    db_idx <- variant_id == database$variant_id & gene_id == database$gene_id

    if (sum(db_idx) != 1) {
      stop("Duplicate or missing entry", call. = FALSE)
    }

    slope <- database[db_idx, "slope", drop = TRUE] |> as.numeric()
    se <- database[db_idx, "slope_se", drop = TRUE] |> as.numeric()
    pval <- database[db_idx, "pval_nominal", drop = TRUE] |> as.numeric()

    #  Checking if beta are equal
    if (!isTRUE(all.equal(beta, slope, tolerance = 1e-3))) {
      stop("Beta are not equal", call. = FALSE)
    }

    # Checking if p-values are equal
    if (!isTRUE(all.equal(p, pval, tolerance = 1e-3))) {
      stop("P-values are not equal", call. = FALSE)
    }

    pval_threshold <- database[db_idx, "pval_nominal_threshold", drop = TRUE] |>
      as.numeric()

    # Checking if p-value is significant
    if (pval > pval_threshold) {
      stop("Not significant variant-gene pair", call. = TRUE)
    }

    return(
      data.frame(
        variant = snp,
        chr = chr,
        pos_hg19 = pos_hg19,
        pos_hg38 = pos_hg38,
        ea_alt = ea,
        oa_ref = nea,
        gene = gene,
        gene_id = gene_id,
        beta = slope,
        se = se,
        pvalue = pval,
        tissue = tissue,
        tss_distance = database[db_idx, "tss_distance", drop = TRUE]
      )
    )
  }
)

# %%
# Saving eQTL information
write_xlsx(bind_rows(eqtl_info), file = file.path(output_dir, "eqtl_info.xlsx"))

# %%
# Formatting eQTL data as exposure data
eqtl_exp_data <- lapply(
  eqtl_info,
  function(eqtl) {
    eqtl |>
      mutate(
        phenotype = gene,
        id = paste(variant, gene, sep = " -> "),
        eaf = NA_real_
      ) |>
      as.data.frame() |>
      format_data(
        type = "exposure",
        phenotype_col = "phenotype",
        snp_col = "variant",
        beta_col = "beta",
        se_col = "se",
        pval_col = "pvalue",
        eaf_col = "eaf",
        effect_allele_col = "ea_alt",
        other_allele_col = "oa_ref",
        chr_col = "chr",
        pos_col = "pos_hg19",
        id_col = "id"
      )
  }
)

# %%
# Loading West China survival GWAS data as outcome data
load("results/11/gwas_coxph.rda")

# %%
# Performing Mendelian randomization by Wald ratio method
eqtl_mr <- lapply(
  gwas_coxph,
  function(oct_data) {
    result_list <- lapply(
      eqtl_exp_data,
      function(exp_data) {
        harmonise_data(exp_data, oct_data, action = 1) |>
          mr(method_list = c("mr_wald_ratio")) |>
          generate_odds_ratios() |>
          mutate(
            or_fmt = sprintf("%.2f (%.2f - %.2f)", or, or_lci95, or_uci95),
            pval_fmt = if_else(pval < 0.001, "< 0.001", sprintf("%.3f", pval))
          )
      }
    )

    return(bind_rows(result_list))
  }
)

# %%
# Saving eQTL Mendelian randomization results
write_xlsx(eqtl_mr, file = file.path(output_dir, "eqtl_mr.xlsx"))
