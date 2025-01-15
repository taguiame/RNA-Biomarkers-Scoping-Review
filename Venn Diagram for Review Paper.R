setwd("~/Desktop/Review Paper")

library(VennDiagram)
library(ggvenn)
library(ggplot2)
library(ggsignif)
library(ggpubr)

# Step 1: Load your CSV file
# Replace 'your_file.csv' with the path to your dataset
Microarray_list <- read.csv("Microarray.csv", header = TRUE, stringsAsFactors = FALSE)
RNA_seq_list <- read.csv("RNA-Seq.csv", header = TRUE, stringsAsFactors = FALSE)
Whole_blood_list <- read.csv("Whole-blood.csv", header = TRUE, stringsAsFactors = FALSE)
PBMC_list <- read.csv("PBMC.csv", header = TRUE, stringsAsFactors = FALSE)

# Step 2: Combine all genes from the dataset into a single unique list
Array_unique_genes <- unique(unlist(Microarray_list))
Seq_unique_genes <- unique(unlist(RNA_seq_list))
Whole_unique_genes <- unique(unlist(Whole_blood_list))
PBMC_unique_genes <- unique(unlist(PBMC_list))


Methodology <- list(Array = Array_unique_genes, RNASeq = Seq_unique_genes)

v1 <- ggvenn(Methodology, 
             fill_color = c("steelblue", "firebrick"),
             stroke_color = "transparent",
             set_name_size = 4.5,
             text_size = 6) + 
  ggtitle("Methodology") +
  theme(plot.title = element_text(size = 20, hjust = 0.5))

Tissue <- list(Whole_blood = Whole_unique_genes, PBMC = PBMC_unique_genes)

v2 <- ggvenn(Tissue, 
             fill_color = c("steelblue", "firebrick"),
             stroke_color = "transparent",
             set_name_size = 4.5,
             text_size = 6) + 
  ggtitle("Tissue") +
  theme(plot.title = element_text(size = 20, hjust = 0.5))

venn <- ggarrange(v1, v2,
                  ncol = 1, nrow = 2, common.legend = TRUE, legend="bottom") 
venn

# Overlap between Array and RNASeq
overlap_list1 <- intersect(Methodology$Array, Methodology$RNASeq)
overlap_methodology <- as.data.frame(overlap_list1) # Genes common to Array and RNASeq

# Overlap between Whole blood and PBMC
overlap_list2 <- intersect(Tissue$Whole_blood, Tissue$PBMC)
overlap_tissue <- as.data.frame(overlap_list2) # Genes common to Array and RNASeq

#saving overlap files in csv for supplemental
write.csv(overlap_methodology, "overlap_methodology.csv", row.names = FALSE)
write.csv(overlap_tissue, "overlap_tissue.csv", row.names = FALSE)
