# %%
library("openxlsx2")
library("readr")
library("dplyr")
library("survival")
library("ggplot2")
library("ggtext")
library("ggrepel")
library("showtext")

source("functions/font_config.R", local = TRUE)

showtext_auto()

# %%
output_dir <- "results/15"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# %%
eqtl_genes <- read_xlsx("results/14/eqtl_info.xlsx")

# %%
surv_data <- read_xlsx("data/tcga/clinical_data.xlsx", start_col = 2) |>
  mutate(
    barcode = paste(bcr_patient_barcode, "01A", sep = "-"),
    age = age_at_initial_pathologic_diagnosis,
    gender_male = case_match(gender, "MALE" ~ 1, "FEMALE" ~ 0)
  ) |>
  select(
    barcode, type, age, gender_male,
    OS, OS.time, DSS, DSS.time, DFI, DFI.time, PFI, PFI.time
  )

# %%
gene_expr <- lapply(
  c("COAD", "READ"),
  function(x) {
    read_csv(
      sprintf("data/tcga/tpm-TCGA-%s.csv", x),
      name_repair = function(nm) if_else(nm == "", "gene_id", nm)
    ) |>
      mutate(gene_id = gsub("(.+)\\..+", "\\1", gene_id)) |>
      filter(gene_id %in% eqtl_genes$gene_id) |>
      (function(data) {
        gene_id <- data[["gene_id"]]
        gene_id_col <- colnames(data) %in% "gene_id"
        samples <- colnames(data)[!gene_id_col] |> substr(1, 16)
        value <- as.data.frame(t(data[!gene_id_col]))
        colnames(value) <- gene_id
        value["sample"] <- samples
        return(value)
      })() |>
      filter(grepl("^.+01A$", sample)) |>
      distinct(sample, .keep_all = TRUE)
  }
) |>
  bind_rows()

# %%
fit_data <- inner_join(
  gene_expr,
  surv_data,
  by = join_by(sample == barcode)
)

if (!all(fit_data$type %in% c("COAD", "READ"))) {
  stop("Unknown cancer type", call. = FALSE)
}

# %%
hr_genes <- lapply(
  list(os = "OS", dss = "DSS", dfi = "DFI", pfi = "PFI"),
  function(event) {
    lapply(
      seq_len(nrow(eqtl_genes)),
      function(i) {
        gene_id <- eqtl_genes[i, "gene_id", drop = TRUE]
        gene <- eqtl_genes[i, "gene", drop = TRUE]

        data <- fit_data |>
          select(
            all_of(
              c(gene_id, "age", "gender_male", event, paste0(event, ".time"))
            )
          ) |>
          as.data.frame() |>
          (function(x) {
            x["group"] <- if_else(x[[gene_id]] > median(x[[gene_id]]), 1, 0)
            return(x)
          })()

        fml <- as.formula(
          paste(
            sprintf(
              "Surv(time = %s, event = %s)",
              paste0(event, ".time"),
              event
            ),
            "group + age + gender_male",
            sep = " ~ "
          )
        )

        fit <- summary(coxph(formula = fml, data = data))

        return(
          data.frame(
            event = event,
            n = fit$n,
            n_event = fit$nevent,
            gene = gene,
            gene_id = gene_id,
            hr = fit$conf.int["group", "exp(coef)"],
            hr_l95 = fit$conf.int["group", "lower .95"],
            hr_u95 = fit$conf.int["group", "upper .95"],
            pvalue = fit$coefficients["group", "Pr(>|z|)"]
          )
        )
      }
    ) |>
      bind_rows() |>
      mutate(fdr = p.adjust(pvalue, method = "BH"))
  }
)

write_xlsx(hr_genes, file.path(output_dir, "hr_genes.xlsx"))
# %%
for (plot_data in hr_genes) {
  event <- plot_data$event[1]

  event_char <- switch(event,
    OS = "总体生存期",
    DSS = "癌症特异性生存期",
    DFI = "无疾病生存期",
    PFI = "无进展生存期",
    stop("Unknown event", call. = FALSE)
  )

  p <- plot_data |>
    mutate(
      color = case_when(
        hr > 1 & pvalue < 0.05 ~ "red",
        hr < 1 & pvalue < 0.05 ~ "blue",
        .default = "gray"
      ),
      label = if_else(
        color %in% c("red", "blue"),
        sprintf("italic('%s')", gene),
        NA
      )
    ) |>
    ggplot() +
    geom_hline(yintercept = -log10(0.05), linetype = 2, color = "gray") +
    geom_vline(xintercept = 1, linetype = 2, color = "gray") +
    geom_point(aes(x = hr, y = -log10(pvalue), color = color)) +
    geom_text_repel(
      aes(x = hr, y = -log10(pvalue), label = label),
      parse = TRUE,
      na.rm = TRUE,
      size = 3,
      seed = 99
    ) +
    labs(
      x = sprintf("风险比 (%s)", event_char),
      y = "-log<sub>10</sub> (P 值)"
    ) +
    scale_color_manual(
      breaks = c("red", "blue", "gray"),
      values = c("#BC3C29FF", "#0072B5FF", "gray50")
    ) +
    theme_classic(base_family = font_zh) +
    theme(
      legend.position = "none",
      axis.text = element_text(color = "black"),
      axis.title = element_markdown()
    )

  ggsave(
    file.path(output_dir, sprintf("gene_%s.pdf", tolower(event))),
    plot = p,
    width = 4,
    height = 4
  )
}
