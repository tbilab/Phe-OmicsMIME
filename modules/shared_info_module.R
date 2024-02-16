shared_info_UI = function(id,app_ns){
  ns <- . %>% NS(id)() %>% app_ns()
  
  # tagList(
    fluidRow(
      column(12,offset = 0, style='padding:0px;',
        box(title = strong("Shared Mechanism"),offset = 0,style='padding:0px;', 
            width = 12,
            solidHeader=F,
        style='overflow-y: auto; padding-right: 0px;padding-top: 0px;padding-bottom: 0px;padding-left: 5px;height:30vh;',
        
        fluidRow(
        column(12,div(class = "subtitle","Selected Nodes:",style="font-size: 1.5rem;color:black;padding-top:8px;padding-left:5px;")),
        column(12,textOutput(ns("node_info"))),
        hr(),
        # div(strong("Choose node from the table and click on 'GPT-API' to discover the existing disease multimorbidities associated with your selected node."),
        #     style="font-size: 1.3rem;color:black"),
          # column(12,
          #        actionButton(ns("gpt_api"), "GPT-API",style="font-size: 1.5rem;float:left;background-color:#D3D3D3;")
          #        ),
          # column(12,
          #        textOutput(ns("gpt_output"))),
          column(12,
                 hr(),
          column(6,
                 ##1st layer shared 
                 # div(
                 div(class = "subtitle","1st Layer: Shared Nodes Among Selected Nodes"),
                 downloadButton(class="buttonstyle",ns("download_table1"), "Download Results"),
                 DTOutput(ns("conn_first_table"))
                 ),
          column(6,
                 # div(
                 div(class = "subtitle","2nd Layer: Nodes Connected to 1st Layer Nodes"),
                 downloadButton(class="buttonstyle",ns("download_table2"), "Download Results"),
                 DTOutput(ns("conn_second_table"))
                 )
          )
    )
  )
  )
  )
}

shared_info_Server <- function(input,output,session,current_phecode,current_description,current_institution,visualize_network,clicked_node_id,preselected_node_id,return_preselected) {
  
  ## update node information of user selected, received from omics_network.js
  # observeEvent(clicked_node_id,{
  
  if(is.null(clicked_node_id())) {
    # ##1st layer connected nodes
    # conn_nodes = purrr::map(str_split(current_description,","),function(x){
    #   conn_first = tidy_connect %>%
    #     filter(institution == current_institution) %>%
    #     filter(from %in% x) %>%
    #     dplyr::select(node = to) %>%
    #     bind_rows(tidy_connect %>%
    #                 filter(institution == current_institution) %>%
    #                 filter(to %in% x) %>%
    #                 dplyr::select(node = from)) %>%
    #     distinct(node) %>%
    #     pull(node)
    #   conn_first
    # })
    # shared_nodes_first = data.frame(ID=Reduce(intersect,conn_nodes))
    # if(nrow(shared_nodes_first)!=0){
    #   shared_nodes_first = shared_nodes_first %>% left_join(.,nodes %>% dplyr::rename(ID=node),by="ID")
    # } else{
    #   shared_nodes_first = shared_nodes_first 
    # }
    #   
    # 
    # ##second layer connection nodes
    # shared_nodes_second = tidy_connect %>%
    #   filter(institution == current_institution) %>%
    #   filter(from %in% shared_nodes_first$ID) %>%
    #   dplyr::select(node = to,connection_type) %>%
    #   bind_rows(tidy_connect %>%
    #               filter(institution == current_institution) %>%
    #               filter(to %in% shared_nodes_first$ID) %>%
    #               dplyr::select(node = from,connection_type)) %>%
    #   distinct(node,.keep_all = T) %>%
    #   dplyr::rename(ID=node,type=connection_type) %>%
    #   mutate(type = ifelse(ID %in% phecode_def$description,"phenotype",type))
    
    ##selected node ID
    code_id <- reactiveVal(glue("Please select a node in the bipartite network"))
    output$node_info <- renderText(glue("{code_id()}"))
    
    shared_nodes_first = data.frame(ID=c(),type=c())
    shared_nodes_second = data.frame(ID=c(),type=c())
    ##1st layer connected nodes information
    output$conn_first_table <- renderDT({
      datatable(shared_nodes_first,
                rownames = FALSE,
                #options = list(displayStart = start_index - 2),
                options = list(
                  scrollX = "300px",
                  scrollY = "300px",
                  pageLength = 30, lengthChange = FALSE
                )
      )
    },server = FALSE)
    
    ##2nd layer connected nodes information
    output$conn_second_table <- renderDT({
      datatable(shared_nodes_second,
                rownames = FALSE,
                #options = list(displayStart = start_index - 2),
                options = list(
                  scrollX = "300px",
                  scrollY = "300px",
                  pageLength = 30, lengthChange = FALSE
                )
      )
    },server = FALSE)
    
    download_nodes_first <- reactive(shared_nodes_first)
    download_nodes_second <- reactive(shared_nodes_second)
    output$download_table1 <- downloadHandler(
      filename = function() {
        paste0("associated_mechanism",Sys.time(),".csv")
      },
      content = function(file) {
        write.csv(download_nodes_first(), file, row.names = FALSE,
                  col.names = T,quote = F)
      }
    )
    
    output$download_table2 <- downloadHandler(
      filename = function() {
        paste0("shared_mechanism",Sys.time(),".csv")
      },
      content = function(file) {
        write.csv(download_nodes_second(), file, row.names = FALSE,
                  col.names = T,quote = F)
      }
    )

    
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
    
    ##second layer connection nodes
    shared_nodes_second = tidy_connect %>%
      filter(institution == current_institution) %>%
      filter(from %in% shared_nodes_first$ID) %>%
      dplyr::select(node = to,connection_type) %>%
      bind_rows(tidy_connect %>%
                  filter(institution == current_institution) %>%
                  filter(to %in% shared_nodes_first$ID) %>%
                  dplyr::select(node = from,connection_type)) %>%
      distinct(node,.keep_all = T) %>%
      dplyr::rename(ID=node,type=connection_type) %>%
      mutate(type = ifelse(ID %in% phecode_def$description,"phenotype",type))
    
    ## add pvalue, etc.
    shared_nodes_first = shared_nodes_first %>%
      left_join(., tidy_connect %>% filter((from %in% clicked_node_id() & to %in% shared_nodes_first$ID) | 
                                             (to %in% clicked_node_id() & from %in% shared_nodes_first$ID)) %>%
                  dplyr::select(from,to,hazard_ratio,pvalue,phecode_category) %>%
                  bind_rows(
                    tidy_connect %>% filter((from %in% clicked_node_id() & to %in% shared_nodes_first$ID) | 
                                              (to %in% clicked_node_id() & from %in% shared_nodes_first$ID)) %>%
                      dplyr::select(to,from,hazard_ratio,pvalue)
                  ) %>%
                  dplyr::select(ID=from,hazard_ratio,pvalue),by="ID") %>%
      distinct(.,.keep_all = T)
    
    shared_nodes_second = shared_nodes_second %>%
      left_join(., tidy_connect %>% filter((from %in% clicked_node_id() & to %in% shared_nodes_first$ID) | 
                                             (to %in% clicked_node_id() & from %in% shared_nodes_first$ID)) %>%
                  dplyr::select(from,to,hazard_ratio,pvalue,phecode_category) %>%
                  bind_rows(
                    tidy_connect %>% filter((from %in% clicked_node_id() & to %in% shared_nodes_first$ID) | 
                                              (to %in% clicked_node_id() & from %in% shared_nodes_first$ID)) %>%
                      dplyr::select(to,from,hazard_ratio,pvalue)
                  ) %>%
                  dplyr::select(ID=from,hazard_ratio,pvalue),by="ID") %>%
      distinct(.,.keep_all = T)
    
    ##selected node ID
    code_id <- reactiveVal(glue("{clicked_node_id()};"))
    output$node_info <- renderText(glue("{code_id()}"))
    # observeEvent(return_preselected(),{
    #   code_id <- code_id(glue(NULL))
    #   output$node_info <- renderText(glue("{code_id()}"))
    # 
    #   ##1st layer connected nodes information
    #   output$conn_first_table <- renderDT({
    #     datatable(data.frame(ID=c(),type=c()),
    #               rownames = FALSE,
    #               #options = list(displayStart = start_index - 2),
    #               options = list(
    #                 scrollX = "300px",
    #                 scrollY = "300px",
    #                 pageLength = 30, lengthChange = FALSE
    #               )
    #     )
    #   },server = FALSE)
    # 
    #   ##2nd layer connected nodes information
    #   output$conn_second_table <- renderDT({
    #     datatable(data.frame(ID=c(),type=c()),
    #               rownames = FALSE,
    #               #options = list(displayStart = start_index - 2),
    #               options = list(
    #                 scrollX = "300px",
    #                 scrollY = "300px",
    #                 pageLength = 30, lengthChange = FALSE
    #               )
    #     )
    #   },server = FALSE)
    # })
    
    ##1st layer connected nodes information
    output$conn_first_table <- renderDT({
      datatable(shared_nodes_first,
                rownames = FALSE,
                #options = list(displayStart = start_index - 2),
                options = list(
                  scrollX = "300px",
                  scrollY = "300px",
                  pageLength = 30, lengthChange = FALSE
                ),
                selection = list(mode = "single")
      )
    },server = FALSE)
    
    ##2nd layer connected nodes information
    output$conn_second_table <- renderDT({
      datatable(shared_nodes_second,
                rownames = FALSE,
                #options = list(displayStart = start_index - 2),
                options = list(
                  scrollX = "300px",
                  scrollY = "300px",
                  pageLength = 30, lengthChange = FALSE
                ),
                selection = list(mode = "single")
      )
    },server = FALSE)
    
    ## user select node from the table of shared nodes among selected nodes
    current_shared_first <- reactiveVal(NULL)
    observeEvent(input$conn_first_table_rows_selected,{
      current_shared_first(shared_nodes_first$ID[input$conn_first_table_rows_selected])
    })
    
    observeEvent(input$gpt_api,{
      output$gpt_output <- renderText({
        # Construct the command
        args <- paste(current_shared_first(),clicked_node_id())
        arg1 <- current_shared_first()
        # Properly quote the argument for use in a shell command
        quoted_arg1 <- shQuote(arg1)
        arg2 <- paste0(clicked_node_id(),collapse = ",")
        # Properly quote the argument for use in a shell command
        quoted_arg2 <- shQuote(arg2)
        
        # Execute the command
        output <- system2("python3", args=c("gpt_api.py",quoted_arg1,quoted_arg2), stdout = TRUE)
        
        paste(output, collapse = "\n")
      })
      ns <- session$ns
      ##pop up window
      showModal(modalDialog(
        textOutput(ns("gpt_output")),
        footer=tagList(
          modalButton('close')
        )
      ))})
    observeEvent(input$close,{removeModal()})
    
    download_nodes_first <- reactive(shared_nodes_first)
    download_nodes_second <- reactive(shared_nodes_second)
    output$download_table1 <- downloadHandler(
      filename = function() {
        paste0("associated_mechanism",Sys.time(),".csv")
      },
      content = function(file) {
        write.csv(download_nodes_first(), file, row.names = FALSE,
                  col.names = T,quote = F)
      }
    )
    
    output$download_table2 <- downloadHandler(
      filename = function() {
        paste0("shared_mechanism",Sys.time(),".csv")
      },
      content = function(file) {
        write.csv(download_nodes_second(), file, row.names = FALSE,
                  col.names = T,quote = F)
      }
    )
    
  }
      
}