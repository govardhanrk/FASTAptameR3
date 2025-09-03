# Copyright (C) 2022 Skyler T. Kramer
# Full GNU GPL version 3 license found in LICENSE.txt

clusterEnrichServer <- function(input, output, session){
  
  ## CLUSTER ENRICH - UPDATE FILE SELECTIONS
  observe({
    updateSelectizeInput(session = session, inputId = "clusterEnrich_selectInput", choices = input$clusterEnrichInput$name)
  })
  
  ## CLUSTER ENRICH - DATA GENERATION
  clusterEnrichDF <- eventReactive(input$clusterEnrichStart, {
    
    if(check_fileInput(finput = isolate(input$clusterEnrichInput))){
      showNotification("No file provided!", type = "error", duration = NULL)
      return(NULL)
    } else if(check_numFiles(finput = isolate(input$clusterEnrichInput), isExact = FALSE, numFiles = 2)){
      showNotification("Supply at least 2 files!", type = "error", duration = NULL)
      return(NULL)
    } else if(check_numOrdered(fselected = isolate(input$clusterEnrich_selectInput), isExact = FALSE, numFiles = 2)){
      showNotification("Please order your files!", type = "error", duration = NULL)
      return(NULL)
    } else if(check_validOrder(fselected = isolate(input$clusterEnrich_selectInput))){
      showNotification("Asterisk is a placeholder and not a valid file ordering!", duration = NULL)
      return(NULL)
    } else if(check_extensions(finput = isolate(input$clusterEnrichInput), extensions = "csv")){
      showNotification("Please confirm that all files have the correct extension!", type = "error", duration = NULL)
      return(NULL)
    } else{
      
      fa_clusterEnrich(
        clusterCSVs = isolate(input$clusterEnrichInput[match(input$clusterEnrich_selectInput, input$clusterEnrichInput$name),]$datapath),
        fileNames = isolate(input$clusterEnrichInput[match(input$clusterEnrich_selectInput, input$clusterEnrichInput$name),]$name)
      )
    }
  })
  
  ## CLUSTER ENRICH - DATA OUTPUT
  output$clusterEnrichOutput <- DT::renderDataTable(DT::datatable({
    clusterEnrichDF()
  }, filter = list(position = "top", plain = TRUE, clear = FALSE), rownames = FALSE
  ))
  
  ## CLUSTER ENRICH - SUMMARY DOWNLOAD
  output$clusterEnrichDownload_summary <- downloadHandler(
    
    # set filename
    filename = "clusterEnrich_summary.csv",
    
    # set file content
    content = function(file){
      
      # format data for output as CSV file
      write.csv(clusterEnrichDF()[input[["clusterEnrichOutput_rows_all"]],], file, row.names = FALSE, quote = FALSE)
    }
  )
  
  ## CLUSTER ENRICH - ENRICHMENT COMPUTATION
  clusterEnrichOutput_enrichDF <- eventReactive(input$clusterEnrichStart, {
    
    if(is.null(clusterEnrichDF())){
      return(NULL)
    } else{
      
      fa_clusterEnrich_tracker(trackerDF = clusterEnrichDF())
    }
  })
  
  ## CLUSTER ENRICH - DATA OUTPUT FOR ENRICHMENT TABLE
  output$clusterEnrichOutput_enrichTable <- DT::renderDataTable(DT::datatable({
    clusterEnrichOutput_enrichDF()
  }, filter = list(position = "top", plain = TRUE, clear = FALSE), rownames = FALSE
  ))
  
  ## CLUSTER ENRICH - ENRICH DOWNLOAD
  output$clusterEnrichDownload_enrich <- downloadHandler(
    
    # set filename
    filename = "clusterEnrich_enrich.csv",
    
    # set file content
    content = function(file){
      
      # format data for output as CSV file
      write.csv(
        clusterEnrichOutput_enrichDF()[input[["clusterEnrichOutput_enrichDF_rows_all"]],],
        file,
        row.names = FALSE, quote = FALSE
      )
    }
  )
  
  ## CLUSTER ENRICH - RENDER LINE PLOT
  clusterEnrichTrackingPlot <- eventReactive(input$clusterEnrichStart, {
    
    if(is.null(clusterEnrichDF())){
      return(NULL)
    } else{
      
      fa_clusterEnrichTracker(clusterEnrichDF = clusterEnrichDF())
    }
  })
  
  ## CLUSTER ENRICH - DISPLAY LINE PLOT
  output$clusterEnrichTrackingOutput <- plotly::renderPlotly(
    clusterEnrichTrackingPlot()
  )
}
