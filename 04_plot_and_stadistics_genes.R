library(tidyverse)
library(readxl)
library(openxlsx)
library(ggpubr)
library(rstatix)
library(patchwork)

# -------------------------
# CONFIGURACIÓN
# -------------------------
file_path <- ("Genes 020925 R ajustados completos.xlsx")
output_dir <- dirname(file_path)

sheets <- excel_sheets(file_path)
wb <- createWorkbook()
plot_list <- list()

# -------------------------
# LOOP PRINCIPAL
# -------------------------
for (sheet in sheets) {
  
  if (grepl("statistics", sheet, ignore.case = TRUE)) next
  
  cat("Procesando:", sheet, "\n")
  
  data <- read_excel(file_path, sheet = sheet)
  
  data <- data %>%
    mutate(
      Day = factor(Day, levels = sort(unique(as.numeric(Day)))),
      Condition = trimws(Condition),
      Value = as.numeric(Value)
    ) %>%
    drop_na(Value)
  
  # ESTADÍSTICA
  stat_test <- data %>%
    group_by(Day) %>%
    wilcox_test(Value ~ Condition) %>%
    add_significance() %>%
    mutate(y.position = max(data$Value, na.rm = TRUE) * 1.05)
  
  # -------------------------
  # PLOT 
  # -------------------------
  p <- ggplot(data, aes(x = Day, y = Value, color = Condition, group = Condition)) +
    
    # Réplicas individuales (Atenuadas para evitar saturación)
    geom_point(
      aes(shape = Condition),
      alpha = 0.3, 
      position = position_dodge(width = 0.3),
      size = 1.5
    ) +
    
    # Líneas de tendencia (Medias)
    stat_summary(
      fun = mean,
      geom = "line",
      linewidth = 0.8,
      position = position_dodge(width = 0.3)
    ) +
    
    # Barras de Error (SEM - Crucial para rigor científico)
    stat_summary(
      fun.data = mean_se,
      geom = "errorbar",
      width = 0.2,
      linewidth = 0.7,
      position = position_dodge(width = 0.3)
    ) +
    
    # Estética de Paper
    theme_classic(base_size = 12) +
    labs(
      title = sheet,
      y = "Relative expression",
      x = "Days Post Inoculation"
    ) +
    scale_color_manual(values = c("C" = "black", "Bm" = "#00A087")) +
    scale_shape_manual(values = c("C" = 16, "Bm" = 17)) +
    
    theme(
      legend.position = "none", # Se recogen en el panel final
      plot.title = element_text(face = "italic", size = 14, hjust = 0.5),
      axis.text = element_text(color = "black"),
      axis.line = element_line(linewidth = 0.6)
    )
  
  
  if (any(stat_test$p < 0.05)) {
    p <- p + stat_pvalue_manual(
      stat_test %>% filter(p < 0.05), 
      label = "p.signif", 
      tip.length = 0.01
    )
  } else {
    
    p <- p + annotate("text", x = 1, y = max(data$Value)*1.1, label = "ns", size = 3, hjust = 0)
  }
  
  plot_list[[sheet]] <- p
}

# -------------------------
# PANEL FINAL Y EXPORTACIÓN
# -------------------------
# Agrupar por tejido (asumiendo nombres como "LOX D Leaf", "LOX D Root")
leaf_plots <- plot_list[grepl("Leaf", names(plot_list))]
root_plots <- plot_list[grepl("Root", names(plot_list))]

# Combinar con patchwork
final_plot <- (wrap_plots(leaf_plots, ncol = 1) | wrap_plots(root_plots, ncol = 1)) +
  plot_layout(guides = "collect") & 
  theme(legend.position = "bottom")

# Guardar con dimensiones de New Phytologist (2 columnas = ~180mm)
ggsave(
  file.path(output_dir, "Figure_1_Expression.pdf"), # PDF es mejor para calidad de impresión
  final_plot,
  width = 180, 
  height = 220, 
  units = "mm",
  device = "pdf"
)
