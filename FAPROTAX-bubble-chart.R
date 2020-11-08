#' Julio Fornazier (julio.fornazier@gmail.com)
#' 
#' 

# Indexes ----------------------------------------------------------------

#'phyloseq
#'FAPROTAX prediction
#'functional prediction 
#'bubble chart
#'

# Observações -------------------------------------------------------------

#' Script para exportar tabela com dados de abundancia e taxonomia
#' O arquivo de entrada é um objeto phyloseq
#' Cria o input para FAPROTAX
#' Importa output FAPROTAX e gera um bubble chart com resultados
#' 

# Pacotes ---------------------------------------------------------

library(phyloseq)
library(vegan)
library(reshape2)
library(ggplot2)
library(tibble)
library(dplyr)
library(tidyverse)

# Script -----------------------------------------------------------

# Phyloseq object
data(GlobalPatterns)
ps <- GlobalPatterns

# Exportar tabela para FAPROTAX

{
  #Extrai ASV table
  otu.table <- otu_table(ps) %>%
    as("matrix") %>%
    as_tibble(rownames = "OTU")
  
  #Extrai taxonomic table
  tax.table <- tax_table(ps) %>%
    as("matrix") %>%
    as_tibble(rownames = "OTU")
  
  #Agrupa as duas tabelas
  merged.table <- otu.table %>%
    left_join(tax.table, by = "OTU") 
  
  #Cria uma nova coluna com taxonomia agrupada
  merged.table$taxonomy <- paste(merged.table$Kingdom,
                                 merged.table$Phylum,
                                 merged.table$Class,
                                 merged.table$Order,
                                 merged.table$Family,
                                 merged.table$Genus, 
                                 sep = ";")
  
  #Remove colunas de níveis taxonomicos
  merged.table <- merged.table %>% select(-(Kingdom:Species))
  
  #Salva planilha pronta para entrada no FAPROTAX
  write.table(merged.table, "ASV-table_FAPROTAX-input.tsv", sep="\t", quote=F, row.names=F) #Add "OTU" as the first column header manually in excel
  
  #Exclui tabelas de ASV e taxonomia criadas
  rm(otu.table, tax.table)
}


# Rodar FAPROTAX
#' Rodar comando no terminal R
#' Ajustar caminhos dos arquivos
#' 

#   python FAPROTAX_1.2.3/FAPROTAX_1.2.3/collapse_table.py -i ASV-table_FAPROTAX-input.tsv -o outputs_faprotax/func_table.tsv 
#   -g FAPROTAX_1.2.3/FAPROTAX_1.2.3/FAPROTAX.txt -d taxonomy -c '#' -v -r outputs_faprotax/report.txt -s outputs_faprotax/sub_tables/ 
#   --out_groups2records_table outputs_faprotax/out.txt


# Carregar output FAPROTAX, processar planilha e gerar grafico de bolhas

{
  #Carra output FAPROTAX e exclui coluna OTUID
  func.table <- as.data.frame(t(read.delim("~/R-codes/outputs_faprotax/func_table.tsv", row.names=1, check.names=F) %>% 
              select(-"OTU"))) 
  
  #Exclui funções sem ocorrencia e transforma tabela para abundancia relativa
  func.table.ra <- subset(func.table, select=colSums(func.table) != 0) %>% 
              decostand(method = "total", MARGIN = 2, na.rm = T)
  
  #Multiplica Abundancia relativa por 100
  func.table.ra <- func.table.ra*100
  
  #Adiciona uma coluna com o nome das amostras
  func.table.ra$samples <- rownames(func.table.ra)

  #Adiciona uma coluna com metadado
  func.table.ra$SampleType <- sample_data(ps)$SampleType
  
  #Converte tabela para formato long
  func.table.long <- melt(func.table.ra, id.vars = c("samples","SampleType"), variable.name = "Function")

  #Gerar grafico de bolhas
  ggplot(func.table.long, aes(samples, Function, size = value, color=SampleType)) +
  geom_point(stat="identity") +
  guides(color = guide_legend(override.aes = list(size=5)))+
  labs(colour="Tratamento", size = "Abundância relativa")+
  scale_size(range = c(1, 8), limits = c(1,100), breaks = c(1,25,50,75,100)) +
  ylab("Função predita") +
  xlab("") +
  theme_bw()+
  theme(axis.title.x = element_text(face = "bold", size=12, color = "black"),
        legend.title = element_text(face = "bold", size=12, color = "black"),
        legend.position="right",
        legend.text = element_text(size = 14),
        axis.title.y = element_text(face = "bold", size=12, color = "black", angle = 90),
        axis.text.x = element_text(face = "plain", angle = 45, hjust = 1.1, vjust = 1.1, size=10, color = "black"),
        axis.text.y = element_text(face = "plain", size=10, vjust = 0.3, color = "black"),
        panel.grid.major = element_line(colour = "gray", size=0.5, linetype = 3),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black", size=0.5),
        panel.border = element_rect(colour = "black", fill=NA, size=1.1))

}


