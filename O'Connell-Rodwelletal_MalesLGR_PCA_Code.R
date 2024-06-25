---
# Title of code: "Principal Components Analysis of Male LGR Rumbles"
# Author of code: "Alessio Pignatelli"
# Date last updated: "June 24th, 2024"
  
# Publication: "The use of vocal coordination in male African elephant group departures: evidence of active leadership and consensus"
# Authors: Caitlin E. O'Connell-Rodwell, Jodie L. Berezin, Alessio Pignatelli, Timothy C. Rodwell
  
# Any questions about the data or code can be directed to the corresponding author: CEO-R, ceoconnell[at]stanford.edu
---

library(FactoMineR)
library(factoextra)
library(tidyverse)
library(RColorBrewer)
library(gridExtra)

setwd()
data <- read.csv()

# Perform PCA
pc <- princomp(~ Freq5 + CenterFreq + Freq95 + BW90 + Duration, data = data)

# Explained variance plot
fviz_eig(pc, addlabels = TRUE, main="Explained variance by principle components")

# Plot of variables in PCA2
fviz_pca_var(pc, col.var = "black")

# Create dataframe with data and first two principle components
data_pca <- bind_cols(data %>% select(ElephantID, Tape), as.data.frame(pc$scores[,1:2])) # note that "tape" refers to the event number.

data_pca <- data_pca %>%
  mutate(ElephantID = case_when(
    ElephantID == "105_69_1" ~ "105/69 (1)",
    ElephantID =="105_69_2" ~ "105/69 (2)",
    ElephantID == "61_132_1" ~ "61/132 (1)",
    ElephantID == "61_132_2" ~ "61/132 (2)",
    TRUE ~ ElephantID
  ))

colors <- colorRampPalette(brewer.pal(12, "Set3"))(length(unique(data_pca$ElephantID)))

# Plot of PCA performed together with single legend/colorscheme
ggplot(data_pca, aes(x = Comp.1, y = Comp.2, color = ElephantID)) +
  geom_point() +
  facet_wrap(~ Tape, ncol = 2, nrow = 4) +
  xlim(-0.7, 0.7) + ylim(-0.7, 0.7) +
  scale_color_manual(values = colors) + 
  labs(
    x = "PC 1",
    y = "PC 2",
    title = "PCA Plot by Event"
  ) +
  theme_minimal() 

# Plot of PCA performed together with multiple legend/colorscheme
create_plot <- function(tape_data) {
  ggplot(tape_data, aes(x = Comp.1, y = Comp.2, color = ElephantID)) +
    geom_point() +
    xlim(-0.7, 0.7) + ylim(-0.7, 0.7) +
    scale_color_manual(values = colors) +
    labs(
      x = "PC 1",
      y = "PC 2",
      title = tape_data$Tape[1]
    ) +
    theme_minimal() 
}

plots <- lapply(split(data_pca, data_pca$Tape), create_plot)

grid.arrange(grobs = plots, ncol = 2, nrow = 4)


# Function to analyze each tape (event) individually
analyze_tape_and_save <- function(tape_data, tape_name) {
  pc <- princomp(~ Freq5 + CenterFreq + Freq95 + BW90 + Duration, data = tape_data)
  
  explained_variance_plot <- fviz_eig(pc, addlabels = TRUE, main = paste("Explained variance by principle components for", tape_name))
  
  variables_plot <- fviz_pca_var(pc, col.var = "black")
  
  data_pca <- bind_cols(tape_data %>% select(ElephantID, Tape), as.data.frame(pc$scores[, 1:2]))
  
  colors <- colorRampPalette(brewer.pal(12, "Set3"))(length(unique(data_pca$ElephantID)))
  pca_plot <- ggplot(data_pca, aes(x = Comp.1, y = Comp.2, color = ElephantID)) +
    geom_point() +
    xlim(-0.8, 0.8) + ylim(-0.8, 0.8) +
    scale_color_manual(values = colors) +
    labs(
      x = "PC 1",
      y = "PC 2",
      title = paste("PCA Plot for", tape_name)
    ) +
    theme_minimal()
  
  ggsave(filename = file.path(getwd(), "figures", tape_name, paste0("explained_variance_", tape_name, ".tiff")), plot = explained_variance_plot, width = 8, height = 6, dpi = 300)
  ggsave(filename = file.path(getwd(), "figures", tape_name, paste0("variables_plot_", tape_name, ".tiff")), plot = variables_plot, width = 8, height = 6, dpi = 300)
  ggsave(filename = file.path(getwd(), "figures", tape_name, paste0("pca_plot_", tape_name, ".tiff")), plot = pca_plot, width = 8, height = 6, dpi = 300)
}

tapes <- unique(data$Tape)
for (tape in tapes) {
  tape_data <- data %>% filter(Tape == tape)
  analyze_tape_and_save(tape_data, tape)
}
