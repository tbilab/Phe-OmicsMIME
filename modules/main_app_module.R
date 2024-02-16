main_app_UI = function(id){
  ns = NS(id)
  
  fluidRow(
    tags$head(
      tags$style(HTML('
        .container-fluid {
          padding-left: 0 !important;
          padding-right: 0 !important;
          margin-right: 0 !important;
          margin-left: 00 !important;
        }
        .tight-column {
          padding-left: 0 !important;
          padding-right: 0 !important;
          margin-left: 0 !important;
          margin-right: 0 !important;
        }
        .first-column {
          padding-right: 0 !important;
          padding-left: 5 !important;
          margin-left: 0 !important;
          margin-right: 0 !important;
        }
        .last-column {
          padding-right: 0 !important;
          padding-left: 5 !important;
          margin-left: 0 !important;
          margin-right: 0 !important;
        }
      '))
    ),
    
    titlePanel(
      div(h4("Phe-Omics Multimorbidity Explorer",style='padding-top:0px;padding-bottom:0px;padding-left:10px'),style='margin-top:-15px;margin-bottom:-5px'),
      windowTitle = "Phe-Omics Multimorbidity Explorer"
    ),
    fluidRow(
      column(
        width = 2,
        class = "tight-column",  # Add this class to your column
        info_panel_UI("info_panel",ns)
      ),
      column(
        width = 10,
        class = "tight-column",
        fluidRow(
          column(
            width = 7,
            class = "first-column",  # Add this class to your column
            multipartite_network_UI("multipartite_network",ns),
            shared_info_UI("shared_info",ns)
          ),  
          column(
            width = 5,
            class = "last-column",
            upset_plot_UI("upset_plot",ns)
          )
          ,
          # column(
          #   width = 7,
          #   class = "first-column",  # Add this class to your column
          #   shared_info_UI("shared_info",ns)
          # ),
          column(
            width = 5,
            class = "last-column",  # Add this class to your column
            shared_pathways_UI("shared_pathways",ns)
          )
        )
      )
    )
  )
}


main_app_Server = function(input,output,session,current_phecode,current_description,current_institution,visualize_network){
  ns <- session$ns
  app_data <- reactiveValues(
    current_phecode = NULL,
    current_description = NULL,
    current_institution = NULL,
    # current_data = NULL,
    visualize_network = FALSE,
    clicked_node_id = NULL,
    preselected_node_id = NULL,
    update_upset = NULL,
    return_preselected = NULL,
    update_network = NULL,
    update_clicked_id = NULL
  )
  
  ##info
  observe({
    info <- callModule(
      info_panel_Server, "info_panel",
      current_phecode = current_phecode,
      current_description = current_description, 
      current_institution = current_institution,
      # current_data = all_data$current_data,
      visualize_network = visualize_network
    )
    
  })
  
  ## network
  observe({
    network <- callModule(
      multipartite_network_Server, "multipartite_network",
      current_description = current_description,
      current_institution = current_institution,
      # current_data = all_data$current_data,
      visualize_network = visualize_network,
      current_phecode = current_phecode,
      update_network = reactive(app_data$update_network),
      update_clicked_id = reactive(app_data$update_clicked_id)
    )
    
    observeEvent(network(),{
      # network_data <- network()
      app_data$clicked_node_id <- network()$clicked_node_id
      app_data$preselected_node_id <- network()$preselected_node_id
      app_data$update_upset <- network()$update_upset
      app_data$return_preselected <- network()$return_preselected
    })
  })
  
  ##upset plot
  observe({
    upset_plot <- callModule(
      upset_plot_Server, "upset_plot",
      current_phecode = current_phecode,
      current_description = current_description, 
      current_institution = current_institution,
      # current_data = all_data$current_data,
      visualize_network = visualize_network,
      clicked_node_id = reactive(app_data$clicked_node_id),
      preselected_node_id = app_data$preselected_node_id,
      update_upset = reactive(app_data$update_upset),
      return_preselected = reactive(app_data$return_preselected)
    )
    
    observeEvent(upset_plot(),{
      
      app_data$update_network <- upset_plot()$update_network
      app_data$update_clicked_id <- upset_plot()$update_clicked_node
      
    })
  })
  
  ##shared info
  observe({
    shared_info_table <- callModule(
      shared_info_Server, "shared_info",
      current_phecode = current_phecode,
      current_description = current_description, 
      current_institution = current_institution,
      # current_data = all_data$current_data,
      visualize_network = visualize_network,
      clicked_node_id = reactive(app_data$clicked_node_id),
      preselected_node_id = app_data$preselected_node_id,
      return_preselected = reactive(app_data$return_preselected)
    )
  })
  
  ##biological pathways
  observe({
    shared_pathways_vis <- callModule(
      shared_pathways_Server, "shared_pathways",
      current_phecode = current_phecode,
      current_description = current_description, 
      current_institution = current_institution,
      # current_data = all_data$current_data,
      visualize_network = visualize_network,
      clicked_node_id = reactive(app_data$clicked_node_id),
      # preselected_node_id = app_data$preselected_node_id,
      return_preselected = reactive(app_data$return_preselected)
    )
  })
  
}
