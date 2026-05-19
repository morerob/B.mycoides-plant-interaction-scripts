# Install packages if needed
# install.packages(c("tidyverse", "reshape2", "viridis"))

library(readxl)
library(tidyverse)
library(reshape2)
library(viridis)

# ---- 1. Load your data ----
file_path <- "data/raw/CFU/Bmycoides_root_leaf_colonization_raw.xlsx"
sheet_name <- "heatmap"
data <- read_excel(file_path, sheet = sheet_name)

# ---- 2. Reshape to long format ----
data_long <- melt(data, id.vars = "Day", 
                  variable.name = "Organ", 
                  value.name = "logUFC")
data_long <- data_long %>% filter(Organ != "Stem")


# ---- 3. Clean up names and order ----
data_long$Organ <- recode(data_long$Organ,
                          "Root" = "Root",
                          "Stem" = "Stem",
                          "X3ª_Leaf" = "3rd Leaf",
                          "X4ª_Leaf" = "4th Leaf",
                          "X3a_Leaf" = "3rd Leaf",
                          "X4a_Leaf" = "4th Leaf")

data_long$Organ <- factor(data_long$Organ,
                          levels = c("Root", "Stem", "3rd Leaf", "4th Leaf"))

data_long$Day <- factor(data_long$Day, levels = data$Day)

# ---- 4. Convert 0 → NA so that tiles become blank ----
data_long$logUFC[data_long$logUFC == 0] <- NA

# ---- 5. Plot heatmap ----
p <- ggplot(data_long, aes(x = Day, y = Organ, fill = logUFC)) +
  geom_tile(color = "white", linewidth = 0.5) +
  scale_fill_gradientn(
    colours = c("#ffffcc", "#ffeda0", "#feb24c", "#f03b20", "#bd0026"),
    na.value = "white",
    name = expression(log[10]*"(CFU)+1")
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 0, size = 12),
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    panel.grid = element_blank()
  ) +
  labs(
    title = "Temporal and spatial distribution of the bacterium",
    x = "Day"
  )
ggsave("Figure1_CFU_heatmap.png", p, width = 8, height = 5, dpi = 300)
