

################################################################################
# define the server function

server <- function(input,output,session) {
  
  # Reactivity:
  # As a general rule, use reactive() when you just want to update something based on a new value (e.g. a user input)
  # and use reactiveVal or reactiveValues when you have an object that you want to maintain a state
  
  # # Reactive objects for trends:
  cpc_codes <- reactive({
    req(input$cpc_codes)
     })

   text_labels_input1 <- reactive({
     req(input$trends_input2)
   })

    text_labels_input2 <- reactive({
     req(input$trends_input3)
   })

   # # Reactive objects for comp:
   text_labels_input3 <- reactive({
     req(input$comp_input1)
   })

   text_labels_input4 <- reactive({
     req(input$comp_input2)
   })

   text_labels_input5 <- reactive({
     req(input$comp_input3)
   })
   
   competition <- reactiveValues(dt=data.frame(),plot = plotly_empty())

   ##Here we create our output and stuff
   observeEvent(input$go,{
     dt <- cpc %>% filter(grepl(pattern = paste(input$cpc_codes,sep ='',collapse='|'),x = cpc$cpc_group,ignore.case = T))
     dt <- merge(dt,patent,by = 'patent_id')
     # merge with assignee
     dt <- merge(dt,assignee,by = 'patent_id') # why is this dropping? no assignee?
     competition$dt <- head(dt)
     
     output$competiton_dt <- renderDataTable({competition$dt})
     

     
   })
     

  
  
}

