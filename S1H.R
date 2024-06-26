library(tidyverse)
otc <- read_csv("OTC_expression_export.csv")

A375 <- otc |>
  filter(ModelID == "ACH-000219")  

otc |>
  ggplot(aes(x = tpm, y = gene)) +
  geom_jitter() +
  geom_point(data = A375, color = "magenta", size = 2.3) +
  scale_color_identity() +
  scale_alpha_identity() +
  scale_size_identity() +
  theme(
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    panel.background = element_rect(fill = "white"),  
    plot.background = element_rect(fill = "white"),  
    panel.border = element_rect(color = "black", fill = NA, size = 1)  
  ) +
  labs(x = NULL,
       y = NULL)

ggsave("OTC_expression_A375highlight.pdf",
       width = 4, height = 2, useDingbats = F, bg = "transparent")