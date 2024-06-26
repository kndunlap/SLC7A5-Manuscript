library(tidyverse)
library(ggrepel)
library(janitor)
## Import and clean
screen <- read.csv("mle_all.gene_summary copy.csv")
screen <- clean_names(screen)
screen <- as_tibble(screen)

## Assign Rank
rank_screen <-
  screen |> 
  arrange(oelow_z) |>
  mutate(
    oelow_rank = 1:nrow(screen)
  ) |>
  arrange(oehigh_z) |>
  mutate(
    oehigh_rank = 1:nrow(screen)
  ) 

## Define special points for Rank Plot
SLC7A5_plot <- rank_screen |>
  filter(gene == "SLC7A5")
other_genes <- rank_screen |>
  filter(gene %in% c("SLC7A1", "ASL"))

## Plot Rank
rank_screen |>
  ggplot(aes(x = log2(oehigh_rank), y = log2(oelow_rank))) +
  geom_point(alpha = 0.6) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.title.x = element_text(face = "bold")) +
  theme(axis.title.y = element_text(face = "bold")) +
  theme(axis.line = element_line(linewidth = .5)) +
  theme(axis.text = element_text(size = 15)) +
  geom_point(data = SLC7A5_plot, color = "#FF0000", size = 2.3) +
  geom_point(data = other_genes, color = "darkred", size = 2.3) +
  geom_text_repel(data = SLC7A5_plot, size = 4.5, force = 8, aes(label = gene)) +
  geom_text_repel(data = other_genes, size = 4.5, force = 8, aes(label = gene)) +
  labs(
    x = expression(High~Arg~Log[2]~Gene~Rank),
    y = expression(Low~Arg~Log[2]~Gene~Rank),
    title = expression(A375m^"ASS1-OE")
  )
ggsave("Gene Rank.jpg", dpi = 300, width = 8, height = 6, units = "in")