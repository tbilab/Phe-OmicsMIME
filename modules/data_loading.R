source("modules/load_library.R")
source("modules/helpers_func.R")
load("data/phecode_def.rda")
load("data/phecode_descriptions.rda")
load("data/default_sel.rda")

tidy_connect = readRDS("data/all_res.rds")
# tidy_connect$from = str_replace(tidy_connect$from,"-",".")
# tidy_connect$to = str_replace(tidy_connect$to,"-",".")
# phecode_def$description = str_replace(phecode_def$description,"-",".")
vumc_res = readRDS("data/vumc_phewas_res.rds")
# vumc_res$variable = str_replace(vumc_res$variable,"-",".")
ukbb_res = readRDS("data/omicspred_phewas_res.rds")
# ukbb_res$`Trait ID` = str_replace(ukbb_res$`Trait ID`,"-",".")
data_network_initial = readRDS("data/data_network_initial.rds")
# data_network_initial$nodes$id = str_replace(data_network_initial$nodes$id,"-",".")
# data_network_initial$edges$source = str_replace(data_network_initial$edges$source,"-",".")
# data_network_initial$edges$target = str_replace(data_network_initial$edges$target,"-",".")
upset_results_initial = readRDS("data/upset_results_initial.rds")
marginalData_initial = readRDS("data/marginalData_initial.rds")
# data_heatmap_initial = readRDS("data/data_heatmap_initial.rds")
# data_heatmap_gene_initial = readRDS("data/data_heatmap_gene_initial.rds")
# data_heatmap_protein_initial = readRDS("data/data_heatmap_protein_initial.rds")
# data_heatmap_metabolite_initial = readRDS("data/data_heatmap_metabolite_initial.rds")
# data_heatmap_phecode_initial = readRDS("data/data_heatmap_phecode_initial.rds")
# data_network_depression = readRDS("data/data_network_depression.rds")
# res_cluster = readRDS("data/res_cluster.rds")
# vumc_res = readRDS("data/vumc_res.rds")
# vumc_score = aws.s3::s3read_using(read.csv,object = "s3://tbilab/vis_tool/PheOmicsMultimorbidity/vumc_res.csv")
# vumc_score = read_delim("vumc_res.csv",delim = ",") 
# vumc_score = aws.s3::save_object("s3://tbilab/vis_tool/PheOmicsMultimorbidity/vumc_res.csv") %>%
  # data.table::fread()

phecode_in = unique(c(normalize_phecodes(ukbb_res$Phenotype),vumc_res$phecode))
phecodes <- phecode_def %>% dplyr::select(phecode,description) %>%
  dplyr::select(phecode) %>%
  join_phecode_info(cols_to_join = c("description", "category")) %>%
  rename(Description=description) %>%
  filter(phecode %in% phecode_in)
# # #jak2
# phecodes = bind_rows(phecodes,data.frame(phecode=c("other aUPD","overlap v617f aUPD","GGCC/GGCC","GGCC/TCTT"),
#                                          Description=c("other aUPD","overlap v617f aUPD","GGCC/GGCC","GGCC/TCTT"),
#                                          category=c("other aUPD","overlap v617f aUPD","GGCC/GGCC","GGCC/TCTT")))

nodes = ukbb_res %>%
  dplyr::select(node = Description,type=Type) %>%
  distinct(node,.keep_all = T) %>%
  bind_rows(phecodes %>% dplyr::select(node=Description) %>% mutate(type="phecode")) %>%
  mutate(type=case_when(type=="Gene expression"~"gene",
                        type=="Protein"~"protein",
                        type=="Metabolite"~"metabolite",
                        type=="phecode"~"phecode")) %>%
  bind_rows(vumc_res %>%
              dplyr::select(node = variable,type=group) %>%
              distinct(node,.keep_all = T)) %>%
  # filter(!is.na(node)) %>%
  distinct(node,.keep_all = T)

##jak2
# phe.result = readRDS("data/phe_res_jak2.rds")
# nodes = bind_rows(nodes,phe.result %>%
#                     dplyr::select(node=description) %>% mutate(type="phecode")) %>%
#   distinct(node,.keep_all = T)
# nodes = bind_rows(nodes,data.frame(node=c("other aUPD","overlap v617f aUPD","GGCC/GGCC","GGCC/TCTT"),
#                                    type=rep("phecode",4)))
  
phecodes = phecodes %>%
  rename(code=phecode)

genes = nodes %>%
  filter(type == "gene") %>%
  dplyr::select(Description=node) %>%
  distinct(Description,.keep_all = T) %>%
  mutate(code="gene")

proteins = nodes %>%
  filter(type == "protein") %>%
  dplyr::select(Description=node) %>%
  distinct(Description,.keep_all = T) %>%
  mutate(code="protein")

metabolites = nodes %>%
  filter(type == "metabolite") %>%
  dplyr::select(Description=node) %>%
  distinct(Description,.keep_all = T) %>%
  mutate(code="metabolite")

all_dat = bind_rows(phecodes %>% mutate(group="phecode"),
                    genes %>% mutate(group="gene"),
                    proteins %>% mutate(group="protein"),
                    metabolites %>% mutate(group="metabolite"))

# ##prepare pre-selected data
# tidy_connect = res %>%
#   filter(!is.na(from) & !is.na(to)) %>%
#   mutate(connection_type=case_when(connection_type=="Gene expression"~"gene",
#                                    connection_type=="Protein"~"protein",
#                                    connection_type=="Metabolite"~"metabolite"))
# 
# ## 1st layer nodes connected with user selected phecode
# conn_first = tidy_connect %>%
#   filter(from %in% "Depression") %>%
#   dplyr::select(node = to) %>%
#   bind_rows(tidy_connect %>%
#               filter(to %in% "Depression") %>%
#               dplyr::select(node = from)) %>%
#   distinct(node) %>%
#   pull(node)
# ## phecode connected with molecular-level nodes
# conn_second = tidy_connect %>%
#   filter(from %in% conn_first)%>%
#   dplyr::select(node = to) %>%
#   bind_rows(tidy_connect %>%
#               filter(to %in% conn_first) %>%
#               dplyr::select(node = from)) %>%
#   distinct(node) %>%
#   pull(node)
# # conn = unique(c(current_code,conn_first,conn_second))
# tidy_connect = tidy_connect %>%
#   filter(from %in% conn_first | to %in% conn_first) %>%
#   dplyr::select(from,to) %>%
#   bind_rows(.,tidy_connect %>%
#               filter(from %in% conn_second | to %in% conn_second) %>%
#               dplyr::select(from,to)) %>%
#   graph_from_data_frame(., directed=FALSE) %>%
#   simplify %>%
#   as_data_frame
# 
# nodes = tidy_connect %>%
#   dplyr::select(node=from) %>%
#   bind_rows(
#     tidy_connect %>%
#       dplyr::select(node=to)
#   ) %>%
#   distinct(node,.keep_all = T) %>%
#   left_join(.,nodes,by="node") %>%
#   distinct(node,.keep_all = T) %>%
#   arrange(type) %>%
#   mutate(selected = ifelse(node %in% "Depression","yes","no"))
# 
# # Save data for JS visualization
# dat = list(
#   nodes = nodes %>% dplyr::rename(id = node),
#   edges = tidy_connect %>% dplyr::rename(source = from, target = to)
# )
# saveRDS(dat,"data/data_network_depression.rds")
# 
# 
# 
# 
