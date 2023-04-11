

################################################################################
# define the server function

server <- function(input,output,session) {
  
  # Reactivity:
  # As a general rule, use reactive() when you just want to update something based on a new value (e.g. a user input)
  # and use reactiveVal or reactiveValues when you have an object that you want to maintain a state
  
  # # Reactive objects for trends:
  text_labels_input <- reactive({
    req(input$trends_input1)
    paste('You entered: ',input$trends_input1,sep = '',collapse = '')
     })
   output$trends_output1 <- renderText({text_labels_input()})
   
   text_labels_input1 <- reactive({
     req(input$trends_input2)
     paste('You entered: ',input$trends_input2,sep = '',collapse = '')
   })
   output$trends_output2 <- renderText({text_labels_input1()})
  
    text_labels_input2 <- reactive({
     req(input$trends_input3)
     paste('You entered: ',input$trends_input3,sep = '',collapse = '')
   })
   output$trends_output3 <- renderText({text_labels_input2()})
   
   # # Reactive objects for comp:
   text_labels_input3 <- reactive({
     req(input$comp_input1)
     paste('You entered: ',input$comp_input1,sep = '',collapse = '')
   })
   output$comp_output1 <- renderText({text_labels_input3()})
   
   text_labels_input4 <- reactive({
     req(input$comp_input2)
     paste('You entered: ',input$comp_input2,sep = '',collapse = '')
   })
   output$comp_output2 <- renderText({text_labels_input4()})
   
   text_labels_input5 <- reactive({
     req(input$comp_input3)
     paste('You entered: ',input$comp_input3,sep = '',collapse = '')
   })
   output$comp_output3 <- renderText({text_labels_input5()})
   
   observeEvent(input$trends_button, {
     session$sendCustomMessage(type = 'testmessage',
                               message = 'Thank you for clicking')
   })
   randomVals <- eventReactive(input$go, {
     runif(input$n)
   })
   
   output$plot <- renderPlot({
     hist(randomVals())
   })

  
  # # Reactive objects:
  # market_cpc_input_text <- reactive({
  #   req(input$market_cpcs_input)
  #   paste(input$market_cpcs_input,sep = '',collapse = ', ')
  #   })
  # output$market_cpc_output <- renderText({market_cpc_input_text()})
  
  
  # Reactive values and observers
  # r_list <- reactiveValues(txt = 'You entered: ',txt2 = 'You typed: ')
  # observe({
  #   output$text_labels_output <- renderText({
  #     paste(r_list$txt, input$submarket_labels_input,sep = '',collapse = '')
  #   })
  #   })
  
  
  # # ObserveEvent
  # r_list <- reactiveValues(txt = 'You entered: ',txt2 = 'You typed: ')
  # observeEvent(input$generate_competitive_positioning,{
  #   r_list$txt <- paste(r_list$txt, input$submarket_labels_input,sep = '',collapse = '')
  # })
  # output$text_labels_output <- renderText({
  #   r_list$txt
  # })
  
  
  # # Debugging - browser()
  #   r_list <- reactiveValues(txt = 'You entered: ',txt2 = 'You typed: ')
  #   observeEvent(input$generate_competitive_positioning,{
  #     browser()
  #     temp <- 1
  #     print(temp*5)
  #     r_list$txt <- paste(r_list$txt, input$submarket_labels_input,sep = '',collapse = '')
  # 
  #   })
  #   output$text_labels_output <- renderText({
  #     r_list$txt
  #   })
  
  
}

