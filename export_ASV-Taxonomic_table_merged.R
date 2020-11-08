#' Julio Fornazier (julio.fornazier@gmail.com)
#' 

# Indexes ----------------------------------------------------------------

#'phyloseq
#'export table
#'ASV table
#'Taxonomic
#'


# Observações -------------------------------------------------------------

#' Script para exportar tabela com dados de abundancia e taxonomia
#' O arquivo de entrada é um objeto phyloseq
#' 


# Pacotes ---------------------------------------------------------

library(phyloseq)
library(dplyr)


# Script -----------------------------------------------------------

# Phyloseq object
data(GlobalPatterns)
ps <- GlobalPatterns

# Rodar o conjunto de funções
{
  otu.table <- otu_table(ps) %>%
    as("matrix") %>%
    as_tibble(rownames = "OTU")
  
  tax.table <- tax_table(ps) %>%
    as("matrix") %>%
    as_tibble(rownames = "OTU")
  
  merged.table <- otu.table %>%
    left_join(tax.table, by = "OTU") 
  
  rm(otu.table, tax.table)
}

#Tabela agrupada
merged.table

#Salvar arquivo
write.table(merged.table, "ASV-Taxonomic-Tables-merged.txt", sep="\t", quote=F, col.names=NA)



