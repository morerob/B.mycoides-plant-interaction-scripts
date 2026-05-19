### ============================================================
### 1) LIBRARIES
### ============================================================
library(ggplot2)
library(dplyr)
library(readxl)
library(tidyr)
library(openxlsx)

### ============================================================
### 2) LOAD COMBINED DATA A+B (RAW CFU/g)
### ============================================================
#setwd("C:/Users/marta/Documents/TESIS MARTA (propio)/Experimentos/EXP 311025")
#raw_AB <- read_excel("colonization_combined_withzeros.xlsx")
setwd("C:/Users/...")
raw_AB <- read_excel("data/raw/CFU/Bmycoides_validation_colonization_raw.xlsx")
### ============================================================
### 3) FILTER CM AND DAYS 3,5,7,10 
### ============================================================
cm_AB <- raw_AB %>%
  filter(Genotype == "CM",
         Organ %in% c("Root","Leaf3","Leaf4"),
         Day %in% c(3,5,7,10)) %>%
  mutate(
    logValue = log10(Value + 1),
    Day = as.numeric(as.character(Day))
  ) %>%
  select(Day, Organ, logValue)

### ============================================================
### 4) CALCULATE MEAN PER DAY & ORGAN
### ============================================================
cm_mean <- cm_AB %>%
  group_by(Day, Organ) %>%
  summarise(
    mean_log = mean(logValue, na.rm = TRUE),
    .groups = "drop"
  )

### Order axes
cm_mean$Organ <- factor(cm_mean$Organ, levels = c("Root","Leaf3","Leaf4"))
cm_mean$Day <- factor(cm_mean$Day, levels = c(3,5,7,10))

### ============================================================
### 5) EXPORT DATA USED TO EXCEL
### ============================================================
wb <- createWorkbook()
addWorksheet(wb, "CM_A+B_mean_values")
writeData(wb, "CM_A+B_mean_values", cm_mean)
saveWorkbook(wb, "CM_AplusB_heatmap_values.xlsx", overwrite = TRUE)

### ============================================================
### 6) HEATMAP (0 VALUES = WHITE TILE)
### ============================================================
p_heatmap <- ggplot(cm_mean,
                    aes(x = Day, y = Organ,
                        fill = ifelse(mean_log == 0, NA, mean_log))) +
  
  geom_tile(color = "white", linewidth = 1.2) +
  
  scale_fill_gradientn(
    colors = c("#ffffe0", "#ffe49c", "#fdbb73", "#f47a50", "#d93636", "#a50026"),
    na.value = "white",
    name = "log10(CFU) + 1"
  ) +
  
  labs(
    title = "Spatial distribution of the bacterium (CM – Experiments A+B)",
    x = "Day",
    y = "Organ"
  ) +
  
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 16)
  )

print(p_heatmap)

ggsave("Supplementary_validation_heatmap.png",
       p_heatmap, width = 9, height = 5, dpi = 300)
