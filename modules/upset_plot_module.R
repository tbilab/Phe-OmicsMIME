upset_plot_UI = function(id,app_ns){
  ns <- . %>% NS(id)() %>% app_ns()
  
  fluidRow(
    column(12,offset = 0, style='padding:0px;',
           box(title = strong("Multimorbidity Upset Plot"),
               width = 12,
               solidHeader=TRUE,
               style='overflow-y: auto; padding-right: 0px;padding-top: 0px;padding-bottom: 0px;padding-left: 5px;height:70vh;',
               
               fluidRow(
                 # column(6,actionButton(ns("update_upset"), "Update Upset Plot",style="font-size: 1.5rem;float:left;background-color:#D3D3D3;")),
                 column(width=12,
                        withSpinner(r2d3::d3Output(ns("upset_plot"),width = "100%",height = "750px"),
                                    hide.ui = FALSE)
                        )
               )
           )
    )
  ) ##fluidRow
  
}

upset_plot_Server <- function(input,output,session,current_phecode,current_description,current_institution,visualize_network,clicked_node_id,preselected_node_id,update_upset) {
  ns <- session$ns
     
     upset_results_react <- reactiveVal(upset_results_initial)
     marginalData_react <- reactiveVal(marginalData_initial)
     
     observeEvent(visualize_network,{
       ##prepare the upset plot
       upset_dat = tidy_connect %>%
         filter(institution %in% current_institution) %>%
         filter(from %in% unique(c(current_description)) | to %in% unique(c(current_description))) %>%
         # filter(from %in% conn_first | to %in% conn_first) %>%
         dplyr::select(from,to) %>%
         graph_from_data_frame(., directed=FALSE) %>%
         simplify %>%
         as_data_frame

       show_upset_node = unique(c(current_description))

       upset_dat_res = purrr::map(show_upset_node,function(i){

         group_to = upset_dat %>%
           filter(to == i) %>%
           dplyr::select(from) %>%
           distinct(.) %>%
           pull(from)
         group_from = upset_dat %>%
           filter(from == i) %>%
           dplyr::select(to) %>%
           distinct(.) %>%
           pull(to)
         c(group_to,group_from)

       })
       names(upset_dat_res) = show_upset_node
       ##convert list to intersection matrix
       upset_data = fromList(upset_dat_res)
       if(!is.null(dim(upset_data))){
         rownames(upset_data) = unique(unlist(upset_dat_res))
       } else {
         colname = names(upset_data)[1]
         upset_data = data.frame(upset_data)
         colnames(upset_data) = colname
         rownames(upset_data) = unique(unlist(upset_dat_res))
       }

       upset_results <- list()
       for(i in 1:ncol(upset_data)){
         combinations = combn(colnames(upset_data),i,simplify = F)
         for (comb in combinations) {
           intersection <- calculate_intersection(comb, upset_data)
           upset_results[[paste(comb, collapse = "/")]] <- intersection
         }
       }

       upset_results = upset_results %>%
         flatten() %>%
         keep(~all(.!=0)) %>%
         map_df(.,as_tibble,.id = "pattern") %>%
         rename(count=value) %>%
         mutate(code = str_count(pattern,"/")+1) %>%
         arrange(code,desc(count)) 

       marginalData = upset_results %>%
         filter(code == 1) %>%
         dplyr::select(-code) %>%
         rename(code=pattern) %>%
         jsonlite::toJSON()

       upset_results_react(upset_results)
       marginalData_react(marginalData)
     })
     
     observeEvent(update_upset(),{
       # if(update_upset == T){
       ##prepare the upset plot
       upset_dat = tidy_connect %>%
         filter(institution %in% current_institution) %>%
         filter(from %in% unique(c(clicked_node_id())) | to %in% unique(c(clicked_node_id()))) %>%
         # filter(from %in% conn_first | to %in% conn_first) %>%
         dplyr::select(from,to) %>%
         graph_from_data_frame(., directed=FALSE) %>%
         simplify %>%
         as_data_frame
       
       show_upset_node = unique(c(clicked_node_id()))
       
       upset_dat_res = purrr::map(show_upset_node,function(i){
         
         group_to = upset_dat %>%
           filter(to == i) %>%
           dplyr::select(from) %>%
           distinct(.) %>%
           pull(from)
         group_from = upset_dat %>%
           filter(from == i) %>%
           dplyr::select(to) %>%
           distinct(.) %>%
           pull(to)
         c(group_to,group_from)
         
       })
       names(upset_dat_res) = show_upset_node
       ##convert list to intersection matrix
       upset_data = fromList(upset_dat_res)
       if(!is.null(dim(upset_data))){
         rownames(upset_data) = unique(unlist(upset_dat_res))
       } else {
         colname = names(upset_data)[1]
         upset_data = data.frame(as.matrix(upset_data))
         colnames(upset_data) = colname
         rownames(upset_data) = unique(unlist(upset_dat_res))
       }
       
       upset_results <- list()
       for(i in 1:ncol(upset_data)){
         combinations = combn(colnames(upset_data),i,simplify = F)
         for (comb in combinations) {
           intersection <- calculate_intersection(comb, upset_data)
           upset_results[[paste(comb, collapse = "/")]] <- intersection
         }
       }
       
       upset_results = upset_results %>%
         flatten() %>%
         keep(~all(.!=0)) %>%
         map_df(.,as_tibble,.id = "pattern") %>%
         rename(count=value) %>%
         mutate(code = str_count(pattern,"/")+1) %>%
         arrange(code,desc(count))
       
       marginalData = upset_results %>%
         filter(code == 1) %>%
         dplyr::select(-code) %>%
         rename(code=pattern) %>%
         jsonlite::toJSON()
       upset_results_react(upset_results)
       marginalData_react(marginalData)
     
     })
     
     output$upset_plot <- r2d3::renderD3({
       
       r2d3(
         data=upset_results_react(),
         script = "inst/upset_plot.js",
         # dependencies = "inst/style.css",
         options = list(r2d3.theme = list(background="white"),marginalData=marginalData_react()),
         container = "svg",
         dependencies = "d3-jetpack",
         d3_version = "4"
       )
     })
      
} 














