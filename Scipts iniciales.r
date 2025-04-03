# Script Title: Excel File Import and TSV Export

# To import from an Excel file to R:
install.packages("readxl")
# Load the library
library(readxl)
# Define the file path:
archivo_excel <- "C:/Users/claud/OneDrive/Escritorio/4ºBiología/PRÁCTICAS + TFG/Archivos/Tareas iniciales/ciMGEs_summary_table_no_duplicates_definitivo.xlsx" 
# Read the first sheet of the Excel file
datos <- read_excel(archivo_excel)
# Install readr if it is not installed
install.packages("readr")

# Load the library
library(readr)
# Ensure the library is loaded
library(readr)
# Define the save path
ruta_guardado <- "C:/Users/claud/OneDrive/Escritorio/ciMGEs_summary_table.tsv" 
# Save in TSV format
write_tsv(datos, ruta_guardado)








# Script Title: GC Percentage to Proportion Conversion

# Load the file
data <- read.table("C:/Users/claud/OneDrive/Escritorio/4ºBiología/PRÁCTICAS + TFG/Archivos iniciales/Tareas iniciales/ciMGEs_summary_table_no_duplicates_definitivo.tsv", 
                   header = TRUE, sep = "\t")
# Check the column names
colnames(data)
[1] "Filename"                      "Type"                          "Length..bp."                  
[4] "GC.Content...."                "Relaxase.Type"                 "Mating.pair.formation.systems"
# Transform the 'GC' column from percentage to proportion
data$GC.Content.... <- data$GC.Content.... / 100
# View the first values to verify the transformation
head(data$GC.Content....)
[1] 0.6384 0.3967 0.3744 0.3841 0.3509 0.4570
write.table(data, "C:/Users/claud/OneDrive/Escritorio/4ºBiología/PRÁCTICAS + TFG/Archivos iniciales/Tareas iniciales/ciMGEs_summary_table_modificado.tsv", 
            sep = "\t", row.names = FALSE)






# Script Title: Defense Systems – MGE Names Correction and Column Selection

# Defense systems: Fix MGE names. Select columns: Defense system type (col2), subtype (col3), Defense/antidefense (col4).
# To properly name the file:
# Load these two libraries:
library(dplyr)
library(readr)  
# To load the file:
# Load the TSV file
# Load the file without header
data <- read.delim("C:/Users/claud/OneDrive/Escritorio/4ºBiología/PRÁCTICAS + TFG/Archivos/ciMGEs_defense_systems_no_duplicates.tsv", header = FALSE, sep = "\t", stringsAsFactors = FALSE)
 
# Modify the first column to keep the original prefix + _ICE# or _IME# with its number 
data[,1] <- sub("(_ICE\\d+|_IME\\d+|_AICE//d+).*", "\\1", data[,1])

# EXPLANATION OF THIS COMMAND:
# data[,1]
# This selects the first column of the dataframe data.
# data[,1] means "all rows" (,) and "the first column" (1).
# sub("(_ICE\\d+|_IME\\d+).*", "\\1", data[,1])
# This part uses the sub() function to perform a text substitution on each value of the first column.
# Syntax of sub(pattern, replacement, x)
# • pattern: Regular expression that searches for the text to be replaced.
# • replacement: Text with which to replace what is found.
# • x: The column on which the function is applied.
# Expression       Explanation
# _ICE\\d+         Searches for _ICE followed by a number (\\d+ means "one or more digits").
# _IME\\d+         Searches for _IME followed by a number.
# .*               Means "anything after that", which we want to remove.
# • \\1 refers to what was captured in parentheses in the regular expression.
# That is, it keeps _ICE# or _IME# and removes everything else.

# Save the file WITHOUT header
write.table(data, "C:/Users/claud/OneDrive/Escritorio/4ºBiología/PRÁCTICAS + TFG/Archivos/ciMGEs_defense_systems_modified.tsv", sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE)
# SECOND PART OF TASK 2: SELECT THE COLUMNS
# Select only columns 1, 2, 3 and 4
data_subset <- data[, c(1,2,3,4)]

# Save the file without header
write.table(data_subset, "C:/Users/claud/OneDrive/Escritorio/4ºBiología/PRÁCTICAS + TFG/Archivos/ciMGEs_defense_systems_seleccion.tsv", sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE)

# To make the last row become the header:
install.packages("data.table")
library(data.table)

# Define the file path
file_path <- "C:/Users/claud/OneDrive/Escritorio/4ºBiología/PRÁCTICAS + TFG/Archivos/Tareas iniciales/ciMGEs_defense_systems_definitivo.tsv"

# Load the file
df <- fread(file_path, sep = "\t", header = FALSE) # Load without headers

# Take the last row as header
new_header <- as.character(df[nrow(df), ])  # Last row as a character vector
df <- df[-nrow(df), ]  # Remove the last row

# Assign the new column names
setnames(df, new_header)

# Save the modified file
fwrite(df, file_path, sep = "\t", quote = FALSE)







# Script Title: Genome Taxonomy Extraction and Formatting

# EXTRACTION OF TWO COLUMNS:
library(tidyverse)
ruta_archivo <- "C:/Users/claud/OneDrive/Escritorio/4ºBiología/PRÁCTICAS + TFG/Archivos iniciales/gtdbtk.bac120.summary.tsv"
df <- read_tsv(ruta_archivo)
df_filtrado <- df %>% select(user_genome, classification)
write_tsv(df_filtrado, "C:/Users/claud/OneDrive/Escritorio/4ºBiología/PRÁCTICAS + TFG/Archivos iniciales/genomas_filtrados.tsv")

# To format the first column (ask Jaime if I should create a new one or rewrite it)
ruta <- "C:/Users/claud/OneDrive/Escritorio/4ºBiología/PRÁCTICAS + TFG/Archivos iniciales/Tareas iniciales/genomas_filtrados_CASI.tsv"
df <- read.table(ruta, header = TRUE, sep = "\t", stringsAsFactors = FALSE)
 
View(df)
df <- df %>%
    mutate(Nombre_Acortado = sub("^([^_]+_[^_]+).*", "\\1", df[[1]]))  # Extract up to the second underscore "_"
write.table(df, "genomas_filtrados_CASI_modificado.tsv", sep = "\t", row.names = FALSE, quote = FALSE)
getwd()  # This command is to check where it is saved

# MOVE COLUMN AND CHANGE ITS NAME:
archivo <- archivo[, c("Nombre_Acortado", names(archivo)[-3])]
archivo <- read.table("C:/Users/claud/OneDrive/Escritorio/4ºBiología/PRÁCTICAS + TFG/Archivos iniciales/Tareas iniciales/genomas_filtrados_CASI_modificado.tsv", header = TRUE, sep = "\t")
archivo <- archivo[, c("Nombre_Acortado", names(archivo)[-3])]

colnames(archivo)[1] <- "bacterial_genome" 
write.table(archivo, "C:/Users/claud/OneDrive/Escritorio/4ºBiología/PRÁCTICAS + TFG/Archivos iniciales/Tareas iniciales/genomas_filtrados_definitivo.tsv", sep = "\t", row.names = FALSE)












# Script Title: ciMGEs AMR Mapping and Grouping

library(dplyr)
library(readr)
library(data.table)
archivo_datos <- "C:/Users/claud/OneDrive/Escritorio/4ºBiología/PRÁCTICAS + TFG/Archivos iniciales/ciMGEs_AMR.tsv"
archivo_mapeo <- "C:/Users/claud/OneDrive/Escritorio/4ºBiología/PRÁCTICAS + TFG/Archivos iniciales/Tareas iniciales/matching_table_good.tsv"

datos <- read_tsv(archivo_datos, col_names = FALSE)
mapeo <- read_tsv(archivo_mapeo, col_names = FALSE)
View(datos)
View(mapeo)
colnames(datos) <- as.character(unlist(datos[nrow(datos), ]))
datos <- datos[-nrow(datos), ]
View(datos)
View(mapeo)
colnames(mapeo) <- c("nombre_nuevo", "nombre_actual")
datos[[2]] <- mapeo$nombre_nuevo[match(datos[[2]], mapeo$nombre_actual)]
write_tsv(datos, "C:/Users/claud/OneDrive/Escritorio/datos_mapeados.tsv", col_names = TRUE)
                                                                                                     
data_grouped <- datos %>%
    group_by(.data[[colnames(datos)[2]]], .data[[colnames(datos)[11]]]) %>%  # Group by ciMGE and antibiotic
    summarise(across(everything(), ~ toString(unique(.))), .groups = "drop")
data_grouped <- data_grouped[, colnames(datos), drop = FALSE]
 
View(data_grouped)
output_path_with_header <- "C:/Users/claud/OneDrive/Escritorio/4ºBiología/PRÁCTICAS + TFG/Archivos iniciales/ciMGEs_AMR_grouped_with_header.tsv"
fwrite(data_grouped, output_path_with_header, sep = "\t", quote = FALSE, row.names = FALSE, col.names = TRUE)
 
ciMGEs_AMR <- read.delim("C:/Users/claud/OneDrive/Escritorio/4ºBiología/PRÁCTICAS + TFG/Archivos iniciales/ciMGEs_AMR.tsv", header=FALSE)









# Script Title: Genome Taxonomy Processing and Cleanup

# Load the file
file_path <- "C:/Users/claud/OneDrive/Escritorio/4ºBiología/PRÁCTICAS + TFG/Archivos iniciales/Tareas iniciales/genomas_filtrados_definitivo.tsv"
df <- read.delim(file_path, sep = "\t", header = TRUE, stringsAsFactors = FALSE)

# Extract the taxonomic prefixes from the "classification" column
library(stringr)
taxonomic_levels <- unique(unlist(str_extract_all(df$classification, "[a-z]__")))

# Display the found taxonomic levels
print(taxonomic_levels)
df$num_levels <- sapply(strsplit(df$classification, ";"), length)

# Filter rows that do not have exactly 7 levels (Domain, Phylum, Class, Order, Family, Genus, Species)
diferentes <- df[df$num_levels != 7, ]

# Display the rows with different classification
print(diferentes)
# Then:
# Load the file
file_path <- "C:/Users/claud/OneDrive/Escritorio/4ºBiología/PRÁCTICAS + TFG/Archivos iniciales/Tareas iniciales/genomas_filtrados_definitivo.tsv"
df <- read.delim(file_path, sep = "\t", header = TRUE, stringsAsFactors = FALSE)

# Split the "classification" column into different taxonomic levels
library(tidyr)  # To separate into columns

df_tax <- df %>%
  separate(classification, into = c("Domain", "Phylum", "Class", "Order", "Family", "Genus", "Species"), 
           sep = ";", fill = "right", extra = "drop")

# Replace "Unclassified Bacteria" values with NA in the taxonomic columns
df_tax[df_tax$Domain == "Unclassified Bacteria", 2:7] <- NA

# Display the new table
print(df_tax)

# BUT ONE ROW IS MISSING FROM THE 7003:
df_tax$user_genome[7003] <- "GCF_002158865.1_ASM215886v1_genomic"

# Remove the letters (d__, p__, etc.) from the taxonomic columns
df_tax[, c("Domain", "Phylum", "Class", "Order", "Family", "Genus", "Species")] <- 
    lapply(df_tax[, c("Domain", "Phylum", "Class", "Order", "Family", "Genus", "Species")], 
           function(x) gsub("[a-z]__", "", x))

write.table(df_tax, "C:/Users/claud/OneDrive/Escritorio/genomas_filtrados_limpio.tsv", sep = "\t", row.names = FALSE, quote = FALSE)







# Script Title: Adding Bacterial Genome Column to ciMGEs Summary Table

library(dplyr)
input_file <- "C:\\Users\\claud\\OneDrive\\Escritorio\\4ºBiología\\PRÁCTICAS + TFG\\Archivos iniciales\\Tareas iniciales\\ciMGEs_summary_table_definitivo.tsv"
data <- read.delim(input_file, header = TRUE, stringsAsFactors = FALSE)
data <- data %>%
    mutate(bacterial_genome = sub("^(.*?_.*?)_.*", "\\1", replicon)) %>%
    select(bacterial_genome, everything())
output_file <- "C:\\Users\\claud\\OneDrive\\Escritorio\\4ºBiología\\PRÁCTICAS + TFG\\ciMGEs_summary_table_definitivo_con_bacterial_genome.tsv"
write.table(data, output_file, sep = "\t", row.names = FALSE, quote = FALSE)
ciMGEs_summary_table_definitivo_con_bacterial_genome <- read.delim("C:/Users/claud/OneDrive/Escritorio/4ºBiología/PRÁCTICAS + TFG/ciMGEs_summary_table_definitivo_con_bacterial_genome.tsv")









# Script Title: Plasmids TXSScan Summary Grouping

library(data.table)

# Define the file path
input_path <- "C:/Users/claud/OneDrive/Escritorio/4ºBiología/PRÁCTICAS + TFG/Archivos/plasmids_TXSScan_summary.tsv"
output_path <- "C:/Users/claud/OneDrive/Escritorio/4ºBiología/PRÁCTICAS + TFG/Archivos/Tareas iniciales/plasmids_TXSScan_summary_definitvo.tsv"

# Load the data without altering the column order
data <- fread(input_path, sep = "\t", header = TRUE, quote = "", na.strings = "", fill = TRUE, showProgress = FALSE)

# Group by sys_id and concatenate unique values of each column
data_grouped <- data[, lapply(.SD, function(x) toString(unique(x))), by = .(sys_id, model_fqn)]
# Save the resulting file
fwrite(data_grouped, output_path, sep = "\t", quote = FALSE, row.names = FALSE, col.names = TRUE)

View(data_grouped)









# Script Title: Plasmids AMR Grouping with Header

library(dplyr)
library(data.table)
library(readr)

file_path <- "C:/Users/claud/OneDrive/Escritorio/4ºBiología/PRÁCTICAS + TFG/Archivos iniciales/plasmids_AMR.tsv"

data <- fread(file_path, sep = "\t", header = FALSE)
View(data)
data_grouped <- data %>%
    group_by(V2, V11) %>%  # Group by plasmid (col 2) and antibiotic (col 11)
    summarise(across(everything(), ~ toString(unique(.))), .groups = "drop")

data_grouped <- data_grouped[, names(data), with = FALSE]
View(data)
View(data_grouped)
colnames(data_grouped) <- as.character(unlist(data_grouped[1, ]))  # Use the first row as column names
data_grouped <- data_grouped[-1, ]
output_path_with_header <- "C:/Users/claud/OneDrive/Escritorio/4ºBiología/PRÁCTICAS + TFG/Archivos iniciales/plasmids_AMR_grouped_with_header.tsv"
fwrite(data_grouped, output_path_with_header, sep = "\t", quote = FALSE, row.names = FALSE, col.names = TRUE)






# Script Title: Plasmids Defense Systems File Cleanup

# OTHER: Since the file was very large, I opened it in Excel, removed some columns, saved it so that it occupies less space, then viewed it in RStudio, saved as TSV, and that’s it.
# FIX NAMES:
library(dplyr)
library(readr)
archivo <- "C:/Users/claud/OneDrive/Escritorio/4ºBiología/PRÁCTICAS + TFG/Archivos/Tareas iniciales/plasmids_defense_systems_definitivo.tsv"
datos <- read.table(archivo, sep = "\t", header = TRUE, stringsAsFactors = FALSE)
View(datos)
datos[,1] <- sub("(_(NC|NZ)_[0-9]+\\.?[0-9]*)_.*", "\\1", datos[,1])
datos[,1] <- sub("(_(NC|NZ)_[^_]+)_.*", "\\1", datos[,1])
write.table(datos, archivo, sep = "\t", quote = FALSE, row.names = FALSE)
write.table(datos, archivo sep = "\t", quote = FALSE, row.names = FALSE)
archivo_salida <- "C:/Users/claud/OneDrive/Escritorio/4ºBiología/PRÁCTICAS + TFG/Archivos/plasmids_defense_systems_definitivo_MODIFICADO.tsv"
write.table(datos, archivo_salida, sep = "\t", quote = FALSE, row.names = FALSE, col.names = TRUE)
cat("File saved at:", archivo_salida, "\n")








# Script TitlE: ciMGEs TXSScan Summary Grouping

library(data.table)
input_path <- "C:/Users/claud/OneDrive/Escritorio/4ºBiología/PRÁCTICAS + TFG/Archivos/ciMGEs_TXSScan_summary_no_duplicates.tsv"
output_path <- "C:/Users/claud/OneDrive/Escritorio/4ºBiología/PRÁCTICAS + TFG/Archivos/ciMGEs_TXSScan_summary_grouped.tsv"
data <- fread(input_path, sep = "\t", header = TRUE, quote = "", na.strings = "")

View(data)
data <- fread(input_path, sep = "\t", header = TRUE, quote = "", na.strings = "", fill = TRUE, showProgress = FALSE)
if (any(is.na(data[nrow(data), ]))) {
    data <- data[-nrow(data), ]  # Remove if it appears to be a footer
}
data <- fread(input_path, sep = "\t", header = TRUE, quote = "", na.strings = "", fill = TRUE, showProgress = FALSE)

data_grouped <- data[, lapply(.SD, function(x) toString(unique(x))), by = .(sys_id, model_fqn)]
View(data_grouped)
fwrite(data_grouped, output_path, sep = "\t", quote = FALSE, row.names = FALSE, col.names = TRUE)







