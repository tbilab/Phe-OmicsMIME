library(clusterProfiler)
# library(dbplyr, warn.conflicts = FALSE)
# if (!require("BiocManager", quietly = TRUE)) {install.packages("BiocManager")}
# BiocManager::install("biomaRt",force = T)
# library(biomaRt)
library(org.Hs.eg.db)
gene_names = sample(genes$Description,10,replace=F)
gene_list = data.frame(symbol=gene_names) %>%
  mutate(entrez=mapIds(org.Hs.eg.db, keys = gene_names,
       column = "ENTREZID", keytype = "SYMBOL")) %>% 
  filter(!is.na(entrez))
kk <- enrichKEGG(gene = gene_list$entrez,keyType = 'kegg',
                 organism = 'hsa')
res = kk@result
browseKEGG(kk, 'hsa05215')
