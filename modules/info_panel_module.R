info_panel_UI = function(id,app_ns){
  ns <- . %>% NS(id)() %>% app_ns()
  
  tagList(
    
    box(title = strong("Information Panel"),
        offset = 0, 
        style='overflow-y: auto; padding-right: 0px;padding-top: 0px;padding-bottom: 0px;padding-left: 5px;height:106vh;',
        width = 12,
        solidHeader=F,
        
        div(class = "subtitle",
          "Pre-selected Nodes",style="padding-top:10px;"),
        textOutput(ns("preselected_nodes")),
        # hr(),
        # div(strong("Correlation with major systems"),style="font-size: 1.5rem;color:black"),
        hr(),
        div(class = "subtitle",
          "User Manual",
            style="font-size: 1.5rem;color:black;padding:0px"
            ),
        
        div(class = "subtitle","Bipartite Network"),
        p("Interact with the network plot by selecting the nodes of interest; these clicked nodes will be highlighted in red for easy identification. Additionally, any other nodes that are connected to all of the selected nodes will be highlighted in their respective category colors. This feature not only distinguishes your selections but also visually emphasizes their connections within the network."),
        div(class = "subtitle","Multimorbidity Upset Plot"),
        p("The upset plot is interactive with the bipartite network plot. When nodes in the network plot are selected and update upset button is clicked, their intersections are dynamically represented in the upset plot."),
        div(class = "subtitle","Shared Mechanism"),
        p("The shared mechanism is designed to interact dynamically with the bipartite network plot. It highlights the universal connectivity by displaying 1st layer nodes that are directly connected to all the nodes selected by the user. Additionally, it features synchronized nodes, which represent nodes linked to 1st layer nodes. This interactivity provides a comprehensive view of both direct and indirect relationships within the network."),
        div(class = "subtitle","Shared Biological Pathways"),
        p("This module displays the results of enriched pathway analysis, corresponding to genes associated with the disease phenotypes selected by the user in the bipartite network. It shows the relevant biological pathways shared among these phenotypes. If user select biomolecule nodes, the biological pathway analysis will be hidden")
       
    )
    
  )
}

info_panel_Server <- function(input,output,session,current_phecode,current_description,current_institution,visualize_network) {
  
  code_id <- reactive(glue("{current_phecode}:","{current_description};"))
  output$preselected_nodes <- renderText(
    glue("{code_id()}")
    # cat(cat(paste0(current_phecode,":",current_description,"\n")))
    )
}







