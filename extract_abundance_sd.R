#' Julio Fornazier (julio.fornazier@gmail.com)
#' 

# Indexes ----------------------------------------------------------------

#'phyloseq
#'relative abundance
#'standard desviation
#'

# Observações -------------------------------------------------------------

#' Script para extrair informações de abundância relativa e desvio padrão de um nível taxonômico em função de metadados
#' O arquivo de entrada é um objeto phyloseq
#' 

# Pacotes ---------------------------------------------------------

library(phyloseq)
library(stats)

# Script -----------------------------------------------------------

# Phyloseq object
data(GlobalPatterns)
ps <- GlobalPatterns
 

# Agrupar ASV a nível de família
ps.family <- tax_glom(ps, "Family", 
                      NArm=T, 
                      bad_empty=c(NA, "", " ", "\t", "uncultured")) 

# Transformar para abundância relativa
ps.family.ra  = transform_sample_counts(ps.family, 
                                        function(x) x / sum(x)) 

# Filtrar Familias mais abundantes
ps.family.ra.top = filter_taxa(ps.family.ra, 
                               function(x) mean(x) > 1e-2, 
                               TRUE) 

#' Obter abundancia relativa em função dos metadados disponíveis
#' Para obter a média substituir "sum" por "mean"
#' Usar Abundance ~ [variável que pretende obter a informação]+[variável2]
#' Usando "Abundance ~ ." comando retorna "Abundance" em função de todos os metadados disponíveis
df.melt.sum <- aggregate(Abundance ~ Family + SampleType, 
                         data = psmelt(ps.family.ra.top), 
                         sum, 
                         simplify = T) 

#' Obter desvio padrão em função dos metadados disponíveis
#' Aplica-se o mesmo descrito anteriormente
df.melt.sd <- aggregate(Abundance ~ Family + SampleType, 
                        data = psmelt(ps.family.ra.top), 
                        sd, 
                        simplify = T) 
