# %%
# Attach packages and functions
library("openxlsx2")
library("org.Hs.eg.db")
library("clusterProfiler")
library("ggplot2")
library("ggtext")
library("showtext")

source("functions/font_config.R", local = TRUE)

showtext_auto()

# %%
# Setting up output directory
output_dir <- "results/16"
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# %%
# Reading eQTL information
eqtl_genes <- read_xlsx("results/14/eqtl_info.xlsx")

# %%
# Performing GO enrichment analysis
go <- enrichGO(
  eqtl_genes[["gene_id"]],
  OrgDb = "org.Hs.eg.db",
  keyType = "ENSEMBL",
  ont = "BP"
)

# %%
# Plotting GO enrichment results
p <- dotplot(go) +
  labs(
    x = "基因比例",
    size = "基因数量"
  ) +
  scale_fill_gradient(
    name = "调整后 P 值",
    low = "#BC3C29FF",
    high = "#0072B5FF"
  ) +
  scale_size_continuous(
    breaks = c(2, 3, 4),
    range = c(3, 8)
  ) +
  scale_y_discrete(
    labels = c("&alpha;-亚麻酸代谢过程", "亚油酸代谢过程", "烯烃化合物代谢过程")
  ) +
  theme(
    text = element_text(family = font_zh),
    panel.grid.minor = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_markdown()
  )

ggsave(
  file.path(output_dir, "enrichment.pdf"),
  plot = p,
  width = 6,
  height = 5
)
