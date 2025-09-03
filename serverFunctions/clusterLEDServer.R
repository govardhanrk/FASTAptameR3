# Copyright (C) 2022 Skyler T. Kramer
# Full GNU GPL version 3 license found in LICENSE.txt

clusterLEDServer <- function(input, output, session){
  
  ## CLUSTER - DATA GENERATION
  clusterDF <- reactive({
    
    if(check_fileInput(finput = isolate(input$clusterInput))){
      showNotification("No file provided!", type = "error", duration = NULL)
      return(NULL)
    } else if(check_extension(fpath = isolate(input$clusterInput$datapath), extensions = "fasta")){
      showNotification("Please supply a file with the correct extension!", type = "error", duration = NULL)
      return(NULL)
    } else if(isolate(input$clusterSlider_totalClusters) == 0){
      showNotification("Must generate at least one cluster to proceed!", type = "error", duration = NULL)
      return(NULL)
    } else{
      
      # input checks
      req(isolate(input$clusterSlider_minReads) %in% 0:1000)
      req(isolate(input$clusterSlider_maxLED) %in% 1:20)
      req(isolate(input$clusterSlider_totalClusters) %in% 5:1000)
      req(isolate(input$clusterButton_keepNC) %in% c("Yes", "No"))
      req(isolate(input$clusterDownloadType) %in% c("FASTA", "CSV"))
      
      # capture output
      withCallingHandlers({
        shinyjs::html("clusterTextOutput", "")
        
        # cluster
        promises::future_promise({
          
          fa_formatInput(fastaInput = isolate(input$clusterInput$datapath)) %>% 
            fa_clusterLED(
              fa2DF = .,
              minReads = isolate(input$clusterSlider_minReads),
              maxLED = isolate(input$clusterSlider_maxLED),
              totalClusters = isolate(input$clusterSlider_totalClusters),
              keepNC = ifelse(isolate(input$clusterButton_keepNC) == "Yes", TRUE, FALSE)
            )
        })
      },
      
      # redirect output to text in UI
      message = function(m){
        shinyjs::html(id = "clusterTextOutput", html = m$message, add = FALSE)
      })
    }
  }) %>% 
    bindCache(
      input$clusterInput,
      input$clusterSlider_minReads,
      input$clusterSlider_maxLED,
      input$clusterSlider_totalClusters,
      input$clusterButton_keepNC,
      Sys.Date(),
      cache = "session"
    ) %>% 
    bindEvent(input$clusterStart)
  
  ## CLUSTER - DATA OUTPUT
  output$clusterOutput <- DT::renderDataTable({
    clusterDF() %...>% 
      DT::datatable(filter = list(position = "top", plain = TRUE, clear = FALSE), rownames = FALSE)
  })
  
  ## CLUSTER - DATA DOWNLOAD
  output$clusterDownload <- downloadHandler(
    
    # set filename
    filename = function(){
      
      gsub(
        paste0("\\.", tools::file_ext(isolate(input$clusterInput$name))),
        paste0("-cluster\\.", tolower(isolate(input$clusterDownloadType))),
        isolate(input$clusterInput$name)
      )
    },
    
    # set file content
    content = function(file){
      
      # format data for output as FASTA file
      if(isolate(input$clusterDownloadType) == "FASTA"){
        
        clusterDF() %...>%
          {.[input[["clusterOutput_rows_all"]],]} %...>%
          fa_formatOutput(outputData = .) %...>% 
          write.table(., file, col.names = FALSE, row.names = FALSE, quote = FALSE)
        
      } else{
        
        clusterDF() %...>% 
          {.[input[["clusterOutput_rows_all"]],]} %...>%
          write.csv(., file, quote = FALSE, row.names = FALSE)
        
      }
    }
  )
}
