
library(pheatmap)
library(grid)

# 1. Load data and cela

datos <- read.csv("metaboloma_tomate_Bm_roots.csv", row.names = 1, check.names = FALSE)
datos_matriz <- as.matrix(datos)
datos_matriz[] <- suppressWarnings(as.numeric(as.character(datos_matriz)))
datos_matriz[is.na(datos_matriz)] <- 0

filas_con_varianza <- apply(datos_matriz, 1, sd) > 0
datos_final <- datos_matriz[filas_con_varianza, ]
colnames(datos_final) <- c("3", "4", "5", "6", "7", "8", "9", "10")

# 2. Plot
pheatmap(datos_final, 
         cluster_cols = FALSE, 
         cluster_rows = TRUE, 
         scale = "row", 
         color = colorRampPalette(c("#2166AC", "#F7F7F7", "#B2182B"))(100),
         border_color = "white",
         main = "Roots Metabolic Profile: Tomato - B. mycoides Interaction",
         fontsize_row = 10,
         angle_col = 0,
         cellheight = 20   # <- esto crea espacio vertical
)

grid.text("Days Post Inoculation",
          x = 0.44,
          y = 0.05,
          gp = gpar(fontsize = 12, fontface = "bold"))
