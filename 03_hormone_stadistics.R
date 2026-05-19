library(readxl)
library(dplyr)
library(openxlsx)

# Folder containing all Excel files to analyze
input_folder <- "C:/Users/marta/Documents/TESIS MARTA (propio)/3. BM-PLANT/Experimentos/Hormone time curse/Excels hormonas a analizar"

# List all .xlsx files in the folder
excel_files <- list.files(
  path = input_folder,
  pattern = "\\.xlsx$",
  full.names = TRUE
)

# Exclude already generated result files
excel_files <- excel_files[!grepl("Results", excel_files)]

# Loop over each Excel file
for (input_file in excel_files) {
  
  # Create output file name based on input file
  output_file <- file.path(
    input_folder,
    paste0(tools::file_path_sans_ext(basename(input_file)),
           "_Results_Mann_Whitney.xlsx")
  )
  
  # Get all sheet names from current file
  sheets <- excel_sheets(input_file)
  
  # Create a new workbook for results
  wb <- createWorkbook()
  
  # Loop over each sheet
  for (current_sheet in sheets) {
    
    data <- read_excel(input_file, sheet = current_sheet)
    
    clean_data <- data %>%
      select(Day, Condition, Phytohormone) %>%
      mutate(
        Day = as.factor(Day),
        Condition = as.factor(Condition)
      )
    
    results_mw <- clean_data %>%
      group_by(Day) %>%
      summarise(
        n_C = sum(Condition == "C"),
        n_Bm = sum(Condition == "Bm"),
        median_C = median(Phytohormone[Condition == "C"], na.rm = TRUE),
        median_Bm = median(Phytohormone[Condition == "Bm"], na.rm = TRUE),
        p_value = wilcox.test(
          Phytohormone ~ Condition,
          data = pick(Phytohormone, Condition),
          exact = FALSE
        )$p.value,
        test = "Mann-Whitney U test (non-parametric)",
        .groups = "drop"
      ) %>%
      mutate(
        significance = case_when(
          p_value < 0.001 ~ "***",
          p_value < 0.01  ~ "**",
          p_value < 0.05  ~ "*",
          TRUE            ~ "ns"
        )
      )
    
    addWorksheet(wb, current_sheet)
    writeData(wb, current_sheet, results_mw)
  }
  
  # Save results for this file
  saveWorkbook(wb, output_file, overwrite = TRUE)
}

