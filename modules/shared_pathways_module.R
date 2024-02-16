shared_pathways_UI = function(id,app_ns){
  ns <- . %>% NS(id)() %>% app_ns()
  
  fluidRow(
    column(12,offset = 0, style='padding:0px;',
           box(title = strong("Shared Biological Pathways"),
               width = 12,
               solidHeader=F,offset = 0,style='padding:0px;',
               style='overflow-y: auto; padding-right: 0px;padding-top: 0px;padding-bottom: 0px;padding-left: 5px;height:30vh;',
               
               fluidRow(
                 column(12,div(class = "subtitle","Selected Nodes:",style="font-size: 1.5rem;color:black;padding-left: 5px;padding-top:8px;")),
                 column(12,textOutput(ns("node_info"))),
                 column(12,
                 hr(),
                 column(12,div(class = "subtitle","Metascape Analysis")),
                 column(6,downloadButton(class="buttonstyle",ns("download_table"), "Download Results")),
                 column(12, withSpinner(DTOutput(ns("pathway_table"))),hide.ui = FALSE)
               )
               )
           )
    )
  ) ##fluidRow
  
}

shared_pathways_Server <- function(input,output,session,current_phecode,current_description,current_institution,visualize_network,clicked_node_id,return_preselected) {
  ns <- session$ns
  
  if(is.null(clicked_node_id())) {
    ##1st layer connected nodes
    conn_nodes = purrr::map(str_split(current_description,","),function(x){
      conn_first = tidy_connect %>%
        filter(institution == current_institution) %>%
        filter(from %in% x) %>%
        dplyr::select(node = to) %>%
        bind_rows(tidy_connect %>%
                    filter(institution == current_institution) %>%
                    filter(to %in% x) %>%
                    dplyr::select(node = from)) %>%
        distinct(node) %>%
        pull(node)
      conn_first
    })
    shared_nodes_first = data.frame(ID=Reduce(intersect,conn_nodes))
    if(nrow(shared_nodes_first)!=0){
      shared_nodes_first = shared_nodes_first %>% left_join(.,nodes %>% dplyr::rename(ID=node),by="ID")
    } else{
      shared_nodes_first = shared_nodes_first 
    }
    
    ##selected node ID
    code_id <- reactiveVal(glue("Please select a node in the bipartite network"))
    output$node_info <- renderText(glue("{code_id()}"))
    
    ##pathway analysis
    if(nrow(shared_nodes_first)!=0){
      
      if(sum(current_description %in% nodes$node[nodes$type=="phecode"])>=1){
        path_dat = bind_rows(shared_nodes_first) %>%
          filter(type!="phenotype") %>%
          filter(type=="gene")
        
        if(nrow(path_dat)!=0){
          # gene_list = data.frame(symbol=path_dat$ID) %>%
          #   dplyr::mutate(entrez=mapIds(org.Hs.eg.db, keys = path_dat$ID,
          #                        column = "ENTREZID", keytype = "SYMBOL")) %>% 
          #   dplyr::filter(!is.na(entrez)) %>%
          #   data.frame()
          # kk <- enrichKEGG(gene = gene_list$entrez,keyType = 'kegg',
          #                  organism = 'hsa')
          # if(is.null(kk)){path_res=data.frame(ID=c())} else {path_res=kk@result}
          path_res = data.frame(ID=c())
        } else {
          path_res = data.frame(ID=c())
        }
      } else {
        path_res = data.frame(ID=c())
      }
    } else {
      path_res = data.frame(ID=c())
    }
    ##connected nodes information
    output$pathway_table <- renderDT({
      datatable(path_res,
                rownames = FALSE,
                #options = list(displayStart = start_index - 2),
                options = list(
                  scrollX = "300px",
                  scrollY = "300px",
                  pageLength = 30, lengthChange = FALSE
                )
      )
    },server = FALSE)
    
    
    download_pathway_res <- reactive(path_res)

    output$download_table <- downloadHandler(
      filename = function() {
        paste0("pathway_analysis_result",Sys.time(),".csv")
      },
      content = function(file) {
        write.csv(download_pathway_res(), file, row.names = FALSE,
                  col.names = T,quote = F)
      }
    )
    
    # observeEvent(input$visualize_pathway,{
    #   
    #   
    #   
    # })
    
  } else if(!is.null(clicked_node_id())) {
    
    ##1st layer connected nodes
    conn_nodes = purrr::map(str_split(clicked_node_id(),","),function(x){
      conn_first = tidy_connect %>%
        filter(institution == current_institution) %>%
        filter(from %in% x) %>%
        dplyr::select(node = to) %>%
        bind_rows(tidy_connect %>%
                    filter(institution == current_institution) %>%
                    filter(to %in% x) %>%
                    dplyr::select(node = from)) %>%
        distinct(node) %>%
        pull(node)
      conn_first
    })
    shared_nodes_first = data.frame(ID=Reduce(intersect,conn_nodes))
    if(nrow(shared_nodes_first)!=0){
      shared_nodes_first = shared_nodes_first %>% left_join(.,nodes %>% dplyr::rename(ID=node),by="ID")
    } else{
      shared_nodes_first = shared_nodes_first 
    }
    
    ##selected node ID
    code_id <- reactiveVal(glue("{clicked_node_id()};"))
    output$node_info <- renderText(glue("{code_id()}"))
    
    ##pathway analysis
    if(nrow(shared_nodes_first)!=0){
      
      if(sum(clicked_node_id() %in% nodes$node[nodes$type=="phecode"])>=1){
        path_dat = bind_rows(shared_nodes_first) %>%
          filter(type!="phenotype") %>%
          filter(type=="gene")
        
        if(nrow(path_dat)!=0){
          # gene_list = data.frame(symbol=path_dat$ID) %>%
          #   dplyr::mutate(entrez=mapIds(org.Hs.eg.db, keys = path_dat$ID,
          #                        column = "ENTREZID", keytype = "SYMBOL")) %>% 
          #   dplyr::filter(!is.na(entrez))
          # kk <- enrichKEGG(gene = gene_list$entrez,keyType = 'kegg',
          #                  organism = 'hsa')
          # if(is.null(kk)){path_res=data.frame()} else {path_res=kk@result}
          path_res = data.frame()
        } else {
          path_res = data.frame()
        }
      } else {
      path_res = data.frame()
      }
    } else {
      path_res = data.frame()
    }
    ##connected nodes information
    output$pathway_table <- renderDT({
      datatable(path_res,
                rownames = FALSE,
                #options = list(displayStart = start_index - 2),
                options = list(
                  scrollX = "300px",
                  scrollY = "300px",
                  pageLength = 30, lengthChange = FALSE
                )
      )
    },server = FALSE)
    
    
    download_pathway_res <- reactive(path_res)
    
    output$download_table <- downloadHandler(
      filename = function() {
        paste0("pathway_analysis_result",Sys.time(),".csv")
      },
      content = function(file) {
        write.csv(download_pathway_res(), file, row.names = FALSE,
                  col.names = T,quote = F)
      }
    )
    
    
  }
  
  
  
}












