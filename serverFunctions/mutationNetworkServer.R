# Copyright (C) 2022 Skyler T. Kramer
# Full GNU GPL version 3 license found in LICENSE.txt

mutationNetworkServer <- function(input, output, session){
  
  ## MUTATION NETWORK - DATA GENERATION
  mutationNetworkDF <- reactive({
    
    if(check_fileInput(finput = isolate(input$mutationNetwork_input))){
      showNotification("No file provided!", type = "error", duration = NULL)
      return(NULL)
    } else if(check_emptyString(stringInput = isolate(input$mutationNetwork_startNode))){
      showNotification("Must supply valid start sequence!", type = "error", duration = NULL)
      return(NULL)
    } else if(check_regexInputs(pattern = "[^a-zA-Z0-9]", separator = " ", userInputs = isolate(input$mutationNetwork_startNode))){
      showNotification("Start sequence must be alphanumeric!", type = "error", duration = NULL)
      return(NULL)
    } else if(check_emptyString(stringInput = isolate(input$mutationNetwork_endNode))){
      showNotification("Must supply valid end sequence!", type = "error", duration = NULL)
      return(NULL)
    } else if(check_regexInputs(pattern = "[^a-zA-Z0-9]", separator = " ", userInputs = isolate(input$mutationNetwork_endNode))){
      showNotification("End sequence must be alphanumeric!", type = "error", duration = NULL)
      return(NULL)
    } else if(check_nlines(fpath = isolate(input$mutationNetwork_input$datapath), nlines = 10000)){
      showNotification("Too many lines in uploaded file!", type = "error", duration = NULL)
      return(NULL)
    } else{
      
      # input checks
      req(isolate(input$mutationNetwork_maxCost) %in% 1:5)
      
      # find mutational intermediates
      promises::future_promise({
        
        fa_formatInput(fastaInput = isolate(input$mutationNetwork_input$datapath)) %>% 
          fa_mutationNetwork(
            fa2DF = .,
            startNode = isolate(input$mutationNetwork_startNode),
            endNode = isolate(input$mutationNetwork_endNode),
            maxCost = isolate(input$mutationNetwork_maxCost)
          )
      })
    }
  }) %>% 
    bindCache(
      input$mutationNetwork_input,
      input$mutationNetwork_startNode,
      input$mutationNetwork_endNode,
      input$mutationNetwork_maxCost,
      Sys.Date(),
      cache = "session"
    ) %>% 
    bindEvent(input$mutationNetwork_start)
  
  ## MUTATION NETWORK - DATA OUTPUT; this is a data.table if a path was found or a character string if not
  output$mutationNetwork_DT_output <- DT::renderDataTable({
    
    mutationNetworkDF() %...>% 
      DT::datatable(filter = list(position = "top", plain = TRUE, clear = FALSE), rownames = FALSE)
  })
  
  # output$mutationNetwork_text_output <- renderUI(
  #   
  #   if(!is.data.frame(mutationNetworkDF())){
  #     mutationNetworkDF()
  #   }
  # )
  
  ## MUTATION NETWORK DOWNLOAD
  output$mutationNetwork_download <- downloadHandler(
    
    # set filename
    filename = function(){
      "mutationNetwork.csv"
    },
    
    # set file content
    content = function(file){
      
      # format data for output as CSV file
      mutationNetworkDF() %...>% 
        write.csv(., file, row.names = FALSE, quote = TRUE)
    }
  )
}
