library(tidyverse)
all <- all2

scatter <- function(gene1, gene2, code, alpha) {
  
  gene1 <- ensym(gene1)
  gene2 <- ensym(gene2)
  code <- ensym(code)
  
  filtered_data <- all |>
    select({{gene1}}, {{gene2}}, Type, sample_type, Subtype_mRNA) |>
    filter(if (code != "TCGA") Type == code else TRUE) |>
    filter(sample_type != "Solid Tissue Normal") |>
    filter(sample_type != "Recurrent Tumor") |>
    filter(sample_type != "Additional - New Primary") |>
    filter(sample_type != "Additional Metastatic") |>
    filter(Subtype_mRNA == "LumB") 
  
  pvalue <- cor.test(filtered_data$ASS1, filtered_data$SLC7A5)
  pvalue <- pvalue[[3]]
  formatted_pvalue = format(pvalue, scientific = TRUE, digits = 4)
  n = count(filtered_data)
  
  cor_value <- cor(filtered_data[[gene1]], filtered_data[[gene2]])
  
  label <- paste("BRCA (LumB Subtype)", "\n", "r =", round(cor_value, 4), "\n", "p-value =" , formatted_pvalue, "\n", "n =", n)
  
  ggplot(filtered_data, aes(x = {{gene1}}, y = {{gene2}})) + 
    geom_point(alpha = alpha, size = 2) +
    annotate("text", x = Inf, y = -Inf, label = label, hjust = 1, vjust = -0.5, size = 4.5, color = "black", parse = FALSE) +
    geom_smooth(method = "lm") +
    theme_classic() +
    theme(axis.text.x = element_text(size = 18)) +
    theme(axis.text.y = element_text(size = 18)) +
    theme(axis.title.x = element_text(size = 20)) +
    theme(axis.title.y = element_text(size = 20)) +
    scale_x_continuous(breaks = seq(1, 16, 4))
}

# Run

scatter(SLC7A5, ASS1, BRCA, 0.4)