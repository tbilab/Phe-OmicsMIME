multipartite_network_UI = function(id,app_ns){
  ns <- . %>% NS(id)() %>% app_ns()
  fluidRow(
    useShinyjs(),  
    shinyjs::extendShinyjs(text = "shinyjs.refresh_page = function() { location.reload(); }", functions = "refresh_page"),
    tags$head(
      tags$script(HTML(sprintf('
                   function sendClickedNodeToShiny(selectedNodes) {
                   const selectedNodeIDs = Array.from(selectedNodes, node => node.id);
                   // Include a timestamp or a counter to ensure Shiny detects the change
    const dataToSend = {
        selectedNodeIDs: selectedNodeIDs,
        timestamp: new Date().getTime() // Use a timestamp to force Shiny to recognize the change
    };
                   Shiny.onInputChange("%s", dataToSend);
                   }
                   
                   function sendPreSelectNodeToShiny(preselectedNodes) {
                   const preselectedNodeIDs = Array.from(preselectedNodes, node => node.id);
                   Shiny.onInputChange("%s", preselectedNodeIDs);
                   }
                   
                   Shiny.addCustomMessageHandler("resetClickedNode", function(message) {
                                 Shiny.setInputValue("%s", null);
                   })',
                               ns("clicked_node_id"),
                               ns("preselected_node_id"),
                               ns("clicked_node_id"))
      )),
    ),
    column(12,offset = 0, style='padding:0px;',
           box(title = strong("Bipartite Network"),
               width = 12,
               solidHeader=F,
               style='overflow-y: auto; padding-right: 0px;padding-top: 15px;padding-bottom: 0px;padding-left: 5px;height:70vh;',
               
               fluidRow(
                 column(3,actionButton(ns("first_layer"), "1st-layer network",class="buttonstyle"
                                       # style="font-size: 1.5rem;float:left;background-color:#D3D3D3;"
                 )),
                 column(3,actionButton(ns("second_layer"), "2nd-layer network",class="buttonstyle")),
                 column(3,actionButton(ns("return"), "Revert to pre-selected network",class="buttonstyle")),
                 column(1,""),
                 column(2,actionButton(ns("update_upset"), "Update upset",class="buttonstyle")),
                 column(width=12,style="padding-bottom:0px;margin-bottom:0px",
                        # hr(),
                        withSpinner(r2d3::d3Output(ns("network"),width = "100%",height="60vh"),
                                    hide.ui = FALSE)
                 ) #column
               )#fluidrow
           ) #box
    )
    # , #column
    
  ) ##fluidRow
  
}

multipartite_network_Server = function(input,output,session,current_description,current_institution,visualize_network,current_phecode,update_network,update_clicked_id){
  ns <- session$ns
  
  # Reactive value to track clicked nodes
  clicked_nodes <- reactiveVal(NULL)
  
  observeEvent(update_network(),{

    updated_ids <- update_clicked_id()
    clicked_nodes(updated_ids)
    
    session$sendCustomMessage(type = 'updateD3Selection', message = updated_ids)
    
  })
  
  observeEvent(input$clicked_node_id,{
    if(length(input$clicked_node_id$selectedNodeIDs)==0){
      clicked_nodes(NULL)
    } else {
      clicked_nodes(input$clicked_node_id$selectedNodeIDs)
    }
  })
  
  ## update data for network visualization
  data_for_network = reactiveVal(data_network_initial)
  
  ## observe visualization of the network
  observeEvent(visualize_network,{
    
    # ## 1st layer nodes connected with user selected phecode
    # conn_first = tidy_connect %>%
    #   filter(institution == current_institution()) %>%
    #   filter(from %in% current_description()) %>%
    #   dplyr::select(node = to) %>%
    #   bind_rows(tidy_connect %>%
    #               filter(institution == current_institution()) %>%
    #               filter(to %in% current_description()) %>%
    #               dplyr::select(node = from)) %>%
    #   distinct(node) %>%
    #   pull(node)
    # 
    # conn = unique(c(current_description,conn_first))
    ##1st layer nodes connected to the user selection
    tidy_connect_sub = tidy_connect %>%
      filter(institution %in% current_institution) %>%
      filter(from %in% current_description | to %in% current_description) %>%
      # filter(from %in% conn_first | to %in% conn_first) %>%
      dplyr::select(from,to) %>%
      graph_from_data_frame(., directed=FALSE) %>%
      igraph::simplify(.) %>%
      as_data_frame
    
    vertices_first = tidy_connect_sub %>%
      dplyr::select(node=from) %>%
      bind_rows(
        tidy_connect_sub %>%
          dplyr::select(node=to)
      ) %>%
      distinct(node,.keep_all = T)
    
    vertices = bind_rows(vertices_first) %>%
      left_join(.,nodes,by="node") %>%
      distinct(node,.keep_all = T) %>%
      arrange(type) %>%
      mutate(selected = ifelse(node %in% current_description,"yes","no")) 
    #   mutate(index = 1:nrow(.)) %>%
    #   mutate(color = case_when(type == "gene"~"#689030",
    #                            type == "protein"~"#5E738F",
    #                            type == "metabolite"~"#AD6F3B",
    #                            type == "phecode"~"#673770")) %>%
    #   dplyr::rename(name=node) %>%
    #   mutate(id=index,tooltip=name,size = ifelse(type=="phecode",0.3,0.1),isphecode = ifelse(type=="phecode",T,F),inverted = ifelse(type=="phecode",F,NA),selectable=T) 
    # 
    # tidy_connect_sub = tidy_connect_sub %>%
    #   left_join(.,vertices %>% dplyr::select(from=name,index),by="from") %>%
    #   dplyr::rename(source = index) %>%
    #   left_join(.,vertices %>% dplyr::select(to=name,index),by="to") %>%
    #   dplyr::rename(target = index) %>%
    #   dplyr::select(source,target)
    
    # Save data for JS visualization
    dat = list(
      nodes = vertices %>% dplyr::rename(id = node),
      edges = bind_rows(tidy_connect_sub) %>% dplyr::rename(source = from, target = to),
      extra = data.frame(nothing=Sys.time())
    )
    data_for_network(dat)
  })
  
  ### observe revert to pre-selected
  observeEvent(input$return,{
    
    # shinyjs::js$refresh_page()
    ##1st layer nodes connected to the user selection
    tidy_connect_sub = tidy_connect %>%
      filter(institution %in% current_institution) %>%
      filter(from %in% current_description | to %in% current_description) %>%
      # filter(from %in% conn_first | to %in% conn_first) %>%
      dplyr::select(from,to) %>%
      graph_from_data_frame(., directed=FALSE) %>%
      igraph::simplify(.) %>%
      as_data_frame

    vertices_first = tidy_connect_sub %>%
      dplyr::select(node=from) %>%
      bind_rows(
        tidy_connect_sub %>%
          dplyr::select(node=to)
      ) %>%
      distinct(node,.keep_all = T)

    vertices = bind_rows(vertices_first) %>%
      left_join(.,nodes,by="node") %>%
      distinct(node,.keep_all = T) %>%
      arrange(type) %>%
      mutate(selected = ifelse(node %in% current_description,"yes","no"))


    # Save data for JS visualization
    dat = list(
      nodes = vertices %>% dplyr::rename(id = node),
      edges = bind_rows(tidy_connect_sub) %>% dplyr::rename(source = from, target = to),
      extra = data.frame(nothing=Sys.time())
    )
    data_for_network(dat)
    clicked_nodes(NULL)
    # Send a custom message to JavaScript to reset clicked_node_id
    session$sendCustomMessage('resetClickedNode', list())
    
  })
  
  
  ### observe 1st layer network
  observeEvent(input$first_layer,{
    
    if(length(input$clicked_node_id$selectedNodeIDs)==0){
      clicked_nodes(NULL)
    } else {
      clicked_nodes(input$clicked_node_id$selectedNodeIDs)
    }
    
    # if(is.null(clicked_nodes())){
    #   ##1st layer nodes connected to the user selection
    #   tidy_connect_sub = tidy_connect %>%
    #     filter(institution %in% current_institution) %>%
    #     filter(from %in% current_description | to %in% current_description) %>%
    #     # filter(from %in% conn_first | to %in% conn_first) %>%
    #     dplyr::select(from,to) %>%
    #     graph_from_data_frame(., directed=FALSE) %>%
    #     simplify %>%
    #     as_data_frame
    #   
    #   vertices_first = tidy_connect_sub %>%
    #     dplyr::select(node=from) %>%
    #     bind_rows(
    #       tidy_connect_sub %>%
    #         dplyr::select(node=to)
    #     ) %>%
    #     distinct(node,.keep_all = T)
    #   
    #   vertices = bind_rows(vertices_first) %>%
    #     left_join(.,nodes,by="node") %>%
    #     distinct(node,.keep_all = T) %>%
    #     arrange(type) %>%
    #     mutate(selected = ifelse(node %in% current_description,"yes","no")) 
    #   
    # } else 
      if(!is.null(clicked_nodes())){
      ##1st layer nodes connected to the user selection
      tidy_connect_sub = tidy_connect %>%
        filter(institution %in% current_institution) %>%
        filter(from %in% unique(clicked_nodes()) | to %in% unique(clicked_nodes())) %>%
        # filter(from %in% conn_first | to %in% conn_first) %>%
        dplyr::select(from,to) %>%
        graph_from_data_frame(., directed=FALSE) %>%
        igraph::simplify(.) %>%
        as_data_frame
      
      vertices_first = tidy_connect_sub %>%
        dplyr::select(node=from) %>%
        bind_rows(
          tidy_connect_sub %>%
            dplyr::select(node=to)
        ) %>%
        distinct(node,.keep_all = T)
      
      vertices = bind_rows(vertices_first) %>%
        left_join(.,nodes,by="node") %>%
        distinct(node,.keep_all = T) %>%
        arrange(type) %>%
        mutate(selected = ifelse(node %in% c(clicked_nodes()),"yes","no"))
      
     
    # Save data for JS visualization
    dat = list(
      nodes = vertices %>% dplyr::rename(id = node),
      edges = tidy_connect_sub %>% dplyr::rename(source = from, target = to), 
      extra = data.frame(nothing=Sys.time()) 
    ) 
    data_for_network(dat)
      }
  })
  
  ## observe visualization of the network
  observeEvent(input$second_layer,{
    if(length(input$clicked_node_id$selectedNodeIDs)==0){
      clicked_nodes(NULL)
    } else {
      clicked_nodes(input$clicked_node_id$selectedNodeIDs)
    }
    # ## 1st layer nodes connected with user selected phecode
    # conn_first = tidy_connect %>%
    #   filter(institution == current_institution()) %>%
    #   filter(from %in% current_description()) %>%
    #   dplyr::select(node = to) %>%
    #   bind_rows(tidy_connect %>%
    #               filter(institution == current_institution()) %>%
    #               filter(to %in% current_description()) %>%
    #               dplyr::select(node = from)) %>%
    #   distinct(node) %>%
    #   pull(node)
    # 
    # conn = unique(c(current_description,conn_first))
    
    # if(is.null(clicked_nodes())){
    #   ##1st layer nodes connected to the user selection
    #   tidy_connect_sub = tidy_connect %>%
    #     filter(institution %in% current_institution) %>%
    #     filter(from %in% current_description | to %in% current_description) %>%
    #     # filter(from %in% conn_first | to %in% conn_first) %>%
    #     dplyr::select(from,to) %>%
    #     graph_from_data_frame(., directed=FALSE) %>%
    #     simplify %>%
    #     as_data_frame
    #   ##2nd layer nodes connected to the 1st layer nodes
    #   tidy_connect_second = tidy_connect %>%
    #     filter(institution %in% current_institution) %>%
    #     filter(!(from %in% current_description)) %>%
    #     filter(!(to %in% current_description)) %>%
    #     filter(from %in% unique(c(tidy_connect_sub$from,tidy_connect_sub$to)) | to %in% unique(c(tidy_connect_sub$from,tidy_connect_sub$to))) %>%
    #     dplyr::select(from,to) %>%
    #     graph_from_data_frame(., directed=FALSE) %>%
    #     simplify %>%
    #     as_data_frame
    #   
    #   vertices_first = tidy_connect_sub %>%
    #     dplyr::select(node=from) %>%
    #     bind_rows(
    #       tidy_connect_sub %>%
    #         dplyr::select(node=to)
    #     ) %>%
    #     distinct(node,.keep_all = T)
    #   vertices_second = tidy_connect_second %>%
    #     filter(!from %in% vertices_first$node) %>%
    #     dplyr::select(node=from) %>%
    #     bind_rows(
    #       tidy_connect_second %>%
    #         filter(!to %in% vertices_first$node) %>%
    #         dplyr::select(node=to)
    #     ) %>%
    #     distinct(node,.keep_all = T)
    #   
    #   vertices = bind_rows(vertices_first,vertices_second) %>%
    #     left_join(.,nodes,by="node") %>%
    #     distinct(node,.keep_all = T) %>%
    #     arrange(type) %>%
    #     mutate(selected = ifelse(node %in% current_description,"yes","no")) 
    #   #   mutate(index = 1:nrow(.)) %>%
    #   #   mutate(color = case_when(type == "gene"~"#689030",
    #   #                            type == "protein"~"#5E738F",
    #   #                            type == "metabolite"~"#AD6F3B",
    #   #                            type == "phecode"~"#673770")) %>%
    #   #   dplyr::rename(name=node) %>%
    #   #   mutate(id=index,tooltip=name,size = ifelse(type=="phecode",0.3,0.1),isphecode = ifelse(type=="phecode",T,F),inverted = ifelse(type=="phecode",F,NA),selectable=T) 
    #   # 
    #   # tidy_connect_sub = tidy_connect_sub %>%
    #   #   left_join(.,vertices %>% dplyr::select(from=name,index),by="from") %>%
    #   #   dplyr::rename(source = index) %>%
    #   #   left_join(.,vertices %>% dplyr::select(to=name,index),by="to") %>%
    #   #   dplyr::rename(target = index) %>%
    #   #   dplyr::select(source,target)
    # } else 
      if(!is.null(clicked_nodes())){
      ##1st layer nodes connected to the user selection
      tidy_connect_sub = tidy_connect %>%
        filter(institution %in% current_institution) %>%
        filter(from %in% unique(clicked_nodes()) | to %in% unique(clicked_nodes())) %>%
        # filter(from %in% conn_first | to %in% conn_first) %>%
        dplyr::select(from,to) %>%
        graph_from_data_frame(., directed=FALSE) %>%
        igraph::simplify(.) %>%
        as_data_frame
      ##2nd layer nodes connected to the 1st layer nodes
      tidy_connect_second = tidy_connect %>%
        filter(institution %in% current_institution) %>%
        filter(!(from %in% unique(c(current_description,clicked_nodes())))) %>%
        filter(!(to %in% unique(c(current_description,clicked_nodes())))) %>%
        filter(from %in% unique(c(tidy_connect_sub$from,tidy_connect_sub$to)) | to %in% unique(c(tidy_connect_sub$from,tidy_connect_sub$to))) %>%
        dplyr::select(from,to) %>%
        graph_from_data_frame(., directed=FALSE) %>%
        igraph::simplify(.) %>%
        as_data_frame
      
      vertices_first = tidy_connect_sub %>%
        dplyr::select(node=from) %>%
        bind_rows(
          tidy_connect_sub %>%
            dplyr::select(node=to)
        ) %>%
        distinct(node,.keep_all = T)
      vertices_second = tidy_connect_second %>%
        filter(!from %in% vertices_first$node) %>%
        dplyr::select(node=from) %>%
        bind_rows(
          tidy_connect_second %>%
            filter(!to %in% vertices_first$node) %>%
            dplyr::select(node=to)
        ) %>%
        distinct(node,.keep_all = T)
      
      vertices = bind_rows(vertices_first,vertices_second) %>%
        left_join(.,nodes,by="node") %>%
        distinct(node,.keep_all = T) %>%
        arrange(type) %>%
        mutate(selected = ifelse(node %in% clicked_nodes(),"yes","no")) 
      
    
    # Save data for JS visualization
    dat = list(
      nodes = vertices %>% dplyr::rename(id = node),
      edges = bind_rows(tidy_connect_sub,tidy_connect_second) %>% dplyr::rename(source = from, target = to),
      extra = data.frame(nothing=Sys.time())
    )
    data_for_network(dat)
      }
  })
  
  output$network = r2d3::renderD3({
    # if(is.null(close_network())){
    #   r2d3(
    #     data=data_network_depression,
    #     script = "inst/d3/omics_network.js",
    #     # dependencies = "inst/style.css",
    #     options = list(r2d3.theme = list(background="#F2F3F6")),
    #     container = "svg",
    #     d3_version = "5"
    #   )
    # } else{
    # jsonlite::write_json(data_for_network(),path="inst/d3/original_data.json")
    r2d3(
      data=data_for_network(),
      script = "inst/omics_network.js",
      # dependencies = "inst/style.css",
      options = list(r2d3.theme = list(background="white")),
      container = "svg",
      d3_version = "5"
    )
    # }
    
    # r2d3(
    #   data = jsonlite::toJSON(data_for_network()),
    #   options = list(
    #     viz_type = 'free', 
    #     update_freq = 1
    #   ),
    #   width = '100%',
    #   height = '60%',
    #   script = here::here('inst/network_update.js'),
    #   dependencies = c("d3-jetpack", here::here('inst/helpers.js')),
    #   container = 'div',
    #   d3_version = "4"
    # )
  })
  
  app_data <- reactiveValues(
    update_upset = NULL,
    return_preselected = NULL,
  )
  observeEvent(input$update_upset,{
    app_data$update_upset = 1
  })
  observeEvent(input$return_preselected,{
    app_data$return_preselected = 1
  })
  
  return(
    reactive({
      list(
        clicked_node_id = clicked_nodes(),
        preselected_node_id = input$preselected_node_id,
        update_upset = input$update_upset,
        return_preselected = input$return
      )
    })
  )
  
}