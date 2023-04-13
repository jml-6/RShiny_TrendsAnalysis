

################################################################################
# define the server function

server <- function(input,output,session) {

   
   competition <- reactiveValues(dt=data.frame(),plot = plotly_empty())

   ##Here we create our output and stuff
   observeEvent(input$generate_competitive_position, {
     dt <- cpc %>% filter(grepl(pattern = paste(input$cpc_codes,sep ='',collapse='|'),x = cpc$cpc_group,ignore.case = T))
     dt <- merge(dt,patent,by = 'patent_id')
     # merge with assignee
     dt <- merge(dt,assignee,by = 'patent_id') # why is this dropping? no assignee?
     competition$dt <- head(dt)
     output$competiton_dt <- renderDataTable({competition$dt})
     
     

     
   })
   

  
  
}

