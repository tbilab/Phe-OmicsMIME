starting_code <- c("250.20","250.70","272.10","278.10","401.10","594.10","296.20")
# starting_code <- c("other aUPD","overlap v617f aUPD","GGCC/GGCC","GGCC/TCTT")
starting_description = c("Type 2 diabetes", "Diabetic retinopathy", "Hyperlipidemia", "Obesity", "Essential hypertension", "Calculus of kidney", "Depression")
# starting_description = c("other aUPD","overlap v617f aUPD","GGCC/GGCC","GGCC/TCTT")
starting_row = c(113,122,144,164,206,292,465)
# starting_row = c(672,673,674,675)
# Used in data table to both select correct row and navigate table to that row
start_index <- which(phecodes$code %in% starting_code)
data_loading_UI <- function(id) {
  ns <- NS(id)
  
    fluidPage(
      useShinydashboard(),
      # use a gradient in background
      setBackgroundColor(
        color = c("#F2F3F6"),
        gradient = "linear",
        direction = "bottom"
      ),
      titlePanel(
         div(h4("Phe-Omics Multimorbidity Explorer",style='padding-top:0px;padding-bottom:0px;padding-left:10px'),style='margin-top:-15px;margin-bottom:-5px'),
         windowTitle = "Phe-Omics Multimorbidity Explorer"
      ),
      
        sidebarLayout(
          sidebarPanel(
            style = "background-color: white;height:90vh",
            
          selectInput(ns("institution"),label = div(class = "subtitle","Selecting Institution",style="font-size: 2rem;color:black;float:left"),
                      choices = list("UKB"="ukb","VUMC"="vumc")),
          hr(),
          ##select phecode/molecular centric
          # div(strong("Exploring Disease Phenotype or Biomolecules?",style="font-size: 2rem;color:black")),
          # DTOutput(ns("data_selection")),
          selectInput(ns("data_selection"),label = div(class = "subtitle","Exploring Disease Phenotypes or Biomolecules?",style="font-size: 2rem;color:black;float:left"),
                      choices = list("Phenotype"="phecode","Gene"="gene","Protein"="protein","Metabolite"="metabolite")),
          div("Current selections:",style = "font-size:1.7rem;color: black;
                                                padding-left: 10px;
                                                display: flex;align-items:
                                                center;justify-content: space-evenly;"),
          div(span(textOutput(ns("current_code_label"),inline = TRUE),style = "font-size:1.7rem;color:black;"),
              style = "padding-left: 10px;
                     display: flex;align-items:
                     center;justify-content: space-evenly;"),
          hr(),
          div(actionButton(ns("reset_selection"), "Reset Selection",class="buttonstyle1"),
          actionButton(ns("visualize"), "Visualize Bipartite Network",class="buttonstyle1")
          )
          
        ),
        
        mainPanel(style = "background-color: white;height:90vh;",
          div(class = "subtitle","Select Disease Phenotype or Biomolecule of Interest",style="padding-top: 20px;font-size: 2rem;color:black;float:left"),
          DTOutput(ns("code_selection")),
          hr(),
          actionButton(ns("bring_top"), "Bring Selected Rows Top",class="buttonstyle1")
        )
        ) #full panel
    )
}

data_loading <- function(input, output, session) {
  current_institution <- reactive({input$institution})
  # current_data <- reactiveVal("Phecode")
  current_phecode <- reactiveVal(starting_code)
  current_description <- reactiveVal(starting_description)
  current_row <- reactiveVal(starting_row)
  dat <- reactiveValues(table_data = all_dat %>% filter(group=="phecode"))
  
  app_data <- reactiveValues(
    current_phecode = NULL,
    current_description = NULL,
    current_institution = NULL,
    # current_data = NULL,
    visualize_network = FALSE
  )
  
  ## select data centric of interest
  # output$data_selection <- renderDT({
  #   dat_sel = data.frame(Category = c("Phecode","Gene","Protein","Metabolite"))
  #   datatable(dat_sel,
  #             rownames = FALSE,colnames = "",
  #             options = list(dom="t"),
  #             # caption = strong("Interactive Data Selection",style="font-size: 1.7rem;"),
  #             selection = list(mode = "single",selected=c(1)))
  # },server = FALSE)
  
  observeEvent(input$data_selection,{
    if(identical(input$data_selection,"phecode")){
      dat$table_data = all_dat %>% filter(group=="phecode")
    } else if(identical(input$data_selection,"gene")){
      dat$table_data = all_dat %>% filter(group=="gene")
    } else if(identical(input$data_selection,"protein")){
      dat$table_data = all_dat %>% filter(group=="protein")
    } else if(identical(input$data_selection,"metabolite")){
      dat$table_data = all_dat %>% filter(group=="metabolite")
    }
  })
  
  ## user select specific phenotypes/biomolecules
  output$code_selection <- renderDT({
    
    datatable(dat$table_data,
              # caption = "Select Disease Phenotype or Biomolecule of Interest",
              # rownames = FALSE,
              #options = list(displayStart = start_index - 2),
              options = list(
                scrollX = "300px",
                scrollY = "300px",
                pageLength = 30, lengthChange = FALSE
              ),    
              selection = list(mode = 'multiple',selected=current_row())
    )
    },server = TRUE)
  
  # observeEvent(input$data_selection_rows_selected,{
  #   current_data(c("Phecode","Gene","Protein","Metabolite")[input$data_selection_rows_selected])
  # })
  
  observeEvent(input$code_selection_rows_selected,{
  
      # current_row(input$code_selection_rows_selected)
      current_description(dat$table_data$Description[input$code_selection_rows_selected])
      current_phecode(dat$table_data$code[input$code_selection_rows_selected])
    
  })
  
  observeEvent(input$bring_top,{
    
    selected_rows <- input$code_selection_rows_selected
    
    row_order <- order(
      seq_along(dat$table_data[[1]]) %in% selected_rows,
      decreasing = TRUE
    )
    dat$table_data <- dat$table_data[row_order,]
    
    proxy <- DT::dataTableProxy("code_selection")
    DT::replaceData(proxy,dat$table_data)
    DT::selectRows(proxy,seq_along(selected_rows))

  })
  
  ##after user select specific code table
  code_id <- reactive(glue("{current_phecode()}:", "{current_description()}\n"))
  output$current_code_label <- renderText(glue("{code_id()}"))
  # visualize_network <- reactive({input$visualize})
  observeEvent(input$visualize, {
    # removeModal()
    app_data$current_phecode = current_phecode()
    app_data$current_description = current_description()
    app_data$current_institution = current_institution()
    # app_data$current_data = current_data()
    app_data$visualize_network = TRUE
  })
  #reset selections if user select reset
  observeEvent(input$reset_selection,{
    current_description(NULL)
    current_phecode(NULL)
    output$code_selection <- renderDT({
      # if(identical(input$data_selection,"phecode")){
      #   dat$table_data = all_dat %>% filter(group=="phecode")
      # } else if(identical(input$data_selection,"gene")){
      #   dat$table_data = all_dat %>% filter(group=="gene")
      # } else if(identical(input$data_selection,"protein")){
      #   dat$table_data = all_dat %>% filter(group=="protein")
      # } else if(identical(input$data_selection,"metabolite")){
      #   dat$table_data = all_dat %>% filter(group=="metabolite")
      # }
      datatable(dat$table_data,
                rownames = FALSE,
                #options = list(displayStart = start_index - 2),
                options = list(
                  scrollX = "300px",
                  scrollY = "300px"
                ),    
                selection = list(mode = 'multiple')
      )},server = FALSE)
    
  })

  return(
    reactive({
      if(app_data$visualize_network){
        list(
          current_phecode = app_data$current_phecode,
          current_description = app_data$current_description, 
          current_institution = app_data$current_institution,
          # current_data = app_data$current_data,
          visualize_network = app_data$visualize_network
        )  
      } else {
        NULL
      }
    })
  )
  
}


