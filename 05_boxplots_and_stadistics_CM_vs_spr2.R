
### ============================================================
### 1) LIBRARIES
### ============================================================
library(ggplot2)
library(dplyr)
library(readxl)
library(ggbeeswarm)
library(openxlsx)
### ============================================================
### 2) LOAD COMBINED DATA
### ============================================================
data <- read_excel("data/Bmycoides_colonization_raw_all_experiments.xlsx") %>%
  filter(Value > 0) %>%   # remove zeros
  mutate(
    logValue = log10(Value + 1),
    Genotype = factor(Genotype, levels = c("CM", "spr2")),
    Day = factor(Day, levels = c(3, 5, 7, 10)),
    Organ = factor(
      Organ,
      levels = c("Root", "Leaf3", "Leaf4"),
      labels = c("Root", "3rd leaf", "4th leaf")
    )
  )

### ============================================================
### 3) STATISTICS (Wilcoxon CM vs spr2)
### ============================================================

stats <- data %>%
  group_by(Organ, Day) %>%
  filter(n_distinct(Genotype) == 2) %>%
  summarise(
    p_value = wilcox.test(logValue ~ Genotype)$p.value,
    .groups = "drop"
  ) %>%
  mutate(
    significance = case_when(
      p_value > 0.05 ~ "ns",
      p_value <= 0.05 & p_value > 0.01 ~ "*",
      p_value <= 0.01 & p_value > 0.001 ~ "**",
      p_value <= 0.001 ~ "***"
    )
  )

### ============================================================
### 4) PREPARE POSITIONS FOR STATISTICS
### ============================================================

stats_for_plot <- stats %>%
  mutate(
    x_min = as.numeric(Day) - 0.18,
    x_max = as.numeric(Day) + 0.18
  ) %>%
  left_join(
    data %>%
      group_by(Organ, Day) %>%
      summarise(base_y = max(logValue), .groups = "drop"),
    by = c("Organ", "Day")
  ) %>%
  mutate(
    y_line = base_y + 0.45,
    y_sig  = base_y + 1.1,
    y_pval = base_y + 1.6
  )

### ============================================================
### 5) FINAL PLOT
### ============================================================

p_col <- ggplot(
  data,
  aes(x = Day, y = logValue, fill = Genotype, color = Genotype)
) +
  
  geom_boxplot(
    alpha = 0.6,
    width = 0.6,
    outlier.shape = NA
  ) +
  
  ggbeeswarm::geom_quasirandom(
    size = 2.3,
    alpha = 0.9,
    dodge.width = 0.6
  ) +
  
  # comparison line
  geom_segment(
    data = stats_for_plot,
    aes(x = x_min, xend = x_max, y = y_line, yend = y_line),
    inherit.aes = FALSE,
    linewidth = 0.9
  ) +
  
  # significance symbols
  geom_text(
    data = stats_for_plot,
    aes(x = as.numeric(Day), y = y_sig, label = significance),
    inherit.aes = FALSE,
    size = 6.5
  ) +
  
  # numeric p-values (discrete)
  geom_text(
    data = stats_for_plot,
    aes(x = as.numeric(Day), y = y_pval,
        label = paste0("p=", sprintf("%.3f", p_value))),
    inherit.aes = FALSE,
    size = 4.5
  ) +
  
  facet_wrap(~ Organ, scales = "free_y", ncol = 1) +
  
  scale_fill_manual(
    values = c("CM" = "#4C72B0", "spr2" = "#DD8452")
  ) +
  scale_color_manual(
    values = c("CM" = "#4C72B0", "spr2" = "#DD8452")
  ) +
  
  scale_y_continuous(limits = c(0, NA)) +
  
  labs(
    title = "Bacterial colonization levels",
    x = "Days post inoculation",
    y = expression(log[10]~"(CFU g"^{-1}~"+ 1)"),
    fill = "Genotype",
    color = "Genotype"
  ) +
  
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(size = 20, face = "bold"),
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.text.x  = element_text(size = 15),
    axis.text.y  = element_text(size = 15),
    strip.text   = element_text(size = 18, face = "bold"),
    legend.title = element_text(size = 18),
    legend.text  = element_text(size = 18),
    legend.position = "top",
    plot.margin = margin(t = 20, r = 10, b = 10, l = 10)
  )

print(p_col)

### ============================================================
### 6) SAVE FIGURE
### ============================================================

ggsave(
  "colonization_boxplots_clean.png",
  p_col,
  width = 9,
  height = 16,
  dpi = 300
)
