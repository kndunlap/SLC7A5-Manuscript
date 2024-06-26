library(tidyverse)
library(ggrepel)
library(janitor)
## Import and clean
screen <- read.csv("mle_all.gene_summary copy.csv")
screen <- clean_names(screen)
screen <- as_tibble(screen)



## Define special points for Z Score
SLC7A5_plot <- screen |>
  filter(gene == "SLC7A5")
other_genes <- screen |>
  filter(gene %in% c("SLC7A1", "ASL"))

## Plot Z Score 
screen |>
  ggplot(aes(x = oehigh_z, y = oelow_z)) +
  geom_point(alpha = 0.4) +
  theme_light() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.title.x = element_text(size = 13)) +
  theme(axis.title.y = element_text(size = 13)) +
  theme(axis.line = element_line(linewidth = .5)) +
  theme(axis.text = element_text(size = 15)) +
  geom_point(data = SLC7A5_plot, color = "#FF3333", size = 2.1) +
  geom_point(data = other_genes, color = "darkred", size = 2.1) +
  geom_abline(slope = 1, intercept = 0, color = "#FF0000") +
  geom_text_repel(data = SLC7A5_plot, direction = "x", size = 5.5, force = 12, aes(label = gene)) +
  geom_text_repel(data = other_genes, size = 5, force = 12, aes(label = gene)) +
  labs(x = "High Arg Gene Score",
       y = "Low Arg Gene Score",
       title = expression(A375m^"ASS1-OE")) +
  scale_x_continuous(limits = c(-9, 3), breaks = seq(-9, 3, by = 3)) +
  scale_y_continuous(limits = c(-15, 5), breaks = seq(-15, 5, by = 5))

ggsave("Gene Score.jpg", dpi = 300, width = 8, height = 6, units = "in")