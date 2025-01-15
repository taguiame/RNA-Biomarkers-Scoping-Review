setwd("~/Desktop/Review Paper")

# Load necessary libraries
library(tidyr)   # For data manipulation
library(dplyr)   # For data wrangling
library(ggplot2) # For visualization
library(pheatmap) # For heatmap
library(tibble)

#Binary matrix
# Step 1: Load your CSV file
# Replace 'your_file.csv' with the path to your dataset
data <- read.csv("Sample.csv", header = TRUE, stringsAsFactors = FALSE)

# Step 2: Combine all genes from the dataset into a single unique list
unique_genes <- unique(unlist(data))

# Step 3: Remove empty entries or NAs from the unique gene list
unique_genes <- unique_genes[unique_genes != "" & !is.na(unique_genes)]

# Step 4: Create a binary matrix
binary_matrix <- sapply(data, function(column) {
  as.integer(unique_genes %in% column)  # Check if each unique gene is in the column
})

# Step 5: Assign row names (unique genes) and column names
rownames(binary_matrix) <- unique_genes
colnames(binary_matrix) <- colnames(data)

# Step 6: Convert to a data frame (optional)
binary_matrix_df <- as.data.frame(binary_matrix)

# Step 7: Save the binary matrix to a CSV file (optional)
write.csv(binary_matrix_df, "binary_matrix.csv", row.names = TRUE)

# Step 8: View the result
print(binary_matrix_df)

#heatmap ####

# Assume 'binary_matrix' is your binary dataset
# Rows are genes, columns are signatures (0 or 1)



# Calculate gene frequency
gene_frequency <- rowSums(binary_matrix)

# Filter genes listed in 5 or more signatures
filtered_genes <- names(gene_frequency[gene_frequency >= 10])
filtered_binary_matrix <- binary_matrix[filtered_genes, ]

# Create a data frame for annotation
row_annotation <- data.frame(Frequency = gene_frequency[filtered_genes])
rownames(row_annotation) <- filtered_genes

# Define custom colors for frequency
annotation_colors <- list(
  Frequency = colorRampPalette(c("lightblue", "blue", "darkblue"))(max(row_annotation$Frequency))
)
# Create a heatmap with row annotations
pheatmap(filtered_binary_matrix,
         color = c("white", "midnightblue"),
         cluster_rows = F,
         cluster_cols = F,
         main = "Genes in 10 or More Signatures",
         annotation_row = row_annotation, # Add annotation
         annotation_colors = annotation_colors, # Custom color
         fontsize_row = 12,
         fontsize_col = 10)

view(filtered_genes)


# SCRATCH ####
data_df <- as.data.frame(data)
data_df[data_df == ''] <- NA

all_genes <- unique(c(data_df))
list(c(column_lists))

binary_matrix <- sapply(list(c(column_lists)), function(signature) {
  as.integer(all_genes %in% signature) })

rownames(binary_matrix) <- all_genes
colnames(binary_matrix) <- colnames(data)

view(binary_matrix)

r <- unique(data_df)
r[r == ''] <- NA
#make the binary matrix
matrix <- as.data.frame.matrix(table(stack(data_df))) 

r |>
  pivot_longer(everything()) |>
  drop_na() |>
  mutate(binary=1L) |>
  pivot_wider(names_from=value, values_from=binary, values_fill=0L)


data_list <- as.list(data)
GeneID <- sort(unique(unlist(data_list)))
mat <- t(+sapply(data_list, "%in%", x = GeneID))  ## matrix output
colnames(mat) <- GeneID

data.frame(mat)

tib <- as_tibble(Sample)
tib %>% pivot_longer(everything()) %>% drop_na() %>%  mutate(binary=1L) %>%
  pivot_wider(names_from=value, values_from=binary, values_fill=0L)

# Step 1: Read your CSV file
data <- read.csv("Data_X.csv", header = TRUE, stringsAsFactors = FALSE)

# Step 2: Create a list for each column named by its header
column_lists <- lapply(colnames(data), function(col) data[[col]])

# Step 3: Assign column headers as names for the lists
names(column_lists) <- colnames(data)

# Step 4: Print the lists
view(column_lists)

# Step 1: Combine all lists into a single vector
all_genes <- unlist(column_lists)

# Step 2: Remove blank or empty entries
all_genes <- setdiff(all_genes, c("", NA))  # Exclude empty strings and NAs

# Step 3: Extract only unique genes
unique_genes <- unique(all_genes)

# Step 4: Convert the vector into a data frame
unique_genes_df <- data.frame(Unique_Genes = unique_genes)

