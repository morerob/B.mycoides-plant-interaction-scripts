library(dplyr)
library(ggplot2)
library(readxl)

### ============================================================
### 1) Cargar datos y crear colonized
### ============================================================
data <- read_excel("data/Bmycoides_colonization_raw_all_experiments.xlsx") %>%
  mutate(
    colonized = ifelse(Value > 0, 1, 0),
    Genotype = factor(Genotype, levels = c("CM","spr2")),
    Day = factor(Day),
    Organ = factor(Organ)
  )

### ============================================================
### 2) Calcular prevalencias
### ============================================================
prev_df <- data %>%
  group_by(Organ, Day, Genotype) %>%
  summarise(
    prevalence = 100 * mean(colonized),
    n = n(),
    colonized_n = sum(colonized),
    .groups = "drop"
  )

### ============================================================
### 3) Fisher test Organ x Day
### ============================================================
pvals <- data %>%
  group_by(Organ, Day) %>%
  do({
    sub <- .
    
    tab <- table(sub$Genotype, sub$colonized)
    
    # Solo ejecutar Fisher si hay 2 genotipos y 2 resultados (0 y 1)
    if(length(unique(sub$Genotype)) == 2 &&
       length(unique(sub$colonized)) == 2){
      
      p <- fisher.test(tab)$p.value
      
    } else {
      p <- NA
    }
    
    tibble(p_value = p)
  }) %>%
  ungroup() %>%
  mutate(
    p_label = ifelse(
      is.na(p_value),
      "NA",
      sprintf("p = %.3f", p_value)   # etiqueta bonita
    ),
    ypos = 105
  )

### unir pvals con  prev_df para graficar
plot_df <- prev_df %>%
  left_join(pvals, by = c("Organ","Day"))

### ============================================================
### 4) GRAFICA FINAL CON p VALUE
### ============================================================
p <- ggplot(prev_df,
            aes(x = Day, y = prevalence,
                color = Genotype, group = Genotype, shape = Genotype)) +
  
  geom_line(size = 1.3) +
  geom_point(size = 3) +
  
  # Añadir p values (solo una vez por panel/día)
  geom_text(data = plot_df %>% filter(Genotype == "spr2"),
            aes(x = Day, y = ypos, label = p_label),
            color = "black", size = 5, vjust = 0) +
  
  facet_wrap(~ Organ, ncol = 1) +
  
  scale_color_manual(values = c("CM" = "#2B6CB0", "spr2" = "#C05621")) +
  scale_shape_manual(values = c("CM" = 16, "spr2" = 17)) +
  
  scale_y_continuous(limits = c(0,110), breaks = seq(0,100,25)) +
  
  labs(
    title = "Prevalence with Fisher p-values",
    y = "% colonized",
    x = "Day"
  ) +
  
  theme_minimal(base_size = 16)

print(p)

### ============================================================
### Guardar
### ============================================================
ggsave("prevalence_with_pvalues.png",
       p, width = 8, height = 14, dpi = 300)
