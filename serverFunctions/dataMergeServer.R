# Copyright (C) 2022 Skyler T. Kramer
# Full GNU GPL version 3 license found in LICENSE.txt

dataMergeServer <- function(input, output, session){
  
  ## DATA MERGE - UPDATE FILE SELECTIONS
  observe({
    updateSelectizeInput(session = session, inputId = "dataMerge_selectInput", choices = input$dataMerge_input$name)
  })
  
  ## DATA MERGE - DATA GENERATION
  dataMergeDF <- reactive({
    
    if(check_fileInput(finput = isolate(input$dataMerge_input))){
      showNotification("No files provided!", type = "error", duration = NULL)
      return(NULL)
    } else if(check_numFiles(finput = isolate(input$dataMerge_input), isExact = FALSE, numFiles = 2)){
      showNotification("Please supply at least 2 files!", type = "error", duration = NULL)
      return(NULL)
    } else if(check_numOrdered(fselected = isolate(input$dataMerge_selectInput), isExact = FALSE, numFiles = 2)){
      showNotification("Please order all of your files!", type = "error", duration = NULL)
      return(NULL)
    } else if(check_validOrder(fselected = isolate(input$dataMerge_selectInput))){
      showNotification("Asterisk is a placeholder and not a valid file ordering!", duration = NULL)
      return(NULL)
    } else if(check_extensions(finput = isolate(input$dataMerge_input), extensions = "fasta")){
      showNotification("Please confirm that all files have the correct extension!", type = "error", duration = NULL)
      return(NULL)
    } else{
      
      # input checks
      req(isolate(input$dataMerge_mergeType) %in% c("Union", "Intersection", "Left"))
      req(grepl(pattern = "^#([A-Fa-f0-9]{6}|[A-Fa-f0-9]{3})$", x = isolate(input$dataMerge_persistencePlot_baroutline)))
      req(grepl(pattern = "^#([A-Fa-f0-9]{6}|[A-Fa-f0-9]{3})$", x = isolate(input$dataMerge_persistencePlot_barfill)))
      req(grepl(pattern = "^#([A-Fa-f0-9]{6}|[A-Fa-f0-9]{3})$", x = isolate(input$dataMerge_UpSetPlot_barfill)))
      
	  # type of merge
	  # mergeType <- switch(isolate(input$dataMerge_mergeType), Union = "full", Intersection = "inner", Left = "left")
	  mergeType <- isolate(input$dataMerge_mergeType)
      # merge files
      promises::future_promise({
        
        # format input FA2 FASTA files in parallel, and then merge them
        purrr::map(
          .x = isolate(input$dataMerge_input[match(input$dataMerge_selectInput, input$dataMerge_input$name),]$datapath),
          .f = fa_formatInput
        ) %>% 
          fa_dataMerge(fa2DF_list = ., mergeType = mergeType)
      })
    }
  }) %>% 
    bindCache(
      input$dataMerge_input,
      input$dataMerge_selectInput,
      input$dataMerge_mergeType,
      Sys.Date(),
      cache = "session"
    ) %>% 
    bindEvent(input$dataMerge_start)
  
  ## DATA MERGE - DATA OUTPUT
  output$dataMerge_output <- DT::renderDataTable({
    
    dataMergeDF() %...>% 
      DT::datatable(filter = list(position = "top", plain = TRUE, clear = FALSE), rownames = FALSE)
  })
  
  ## DATA MERGE - DATA DOWNLOAD
  output$dataMerge_download <- downloadHandler(
    
    # set filename
    filename = function(){
      "dataMerge.csv"
    },
    
    # set file content
    content = function(file){
      
      # format data for output as CSV file
      dataMergeDF() %...>% 
        {.[input[["dataMerge_output_rows_all"]],]} %...>% 
        write.csv(., file, row.names = FALSE, quote = FALSE)
    }
  )
  
  ## DATA MERGE - GENERATE SEQUENCE PERSISTENCE PLOT
  dataMerge_sequencePersistencePlot <- reactive({
    
    if(check_fileInput(finput = isolate(input$dataMerge_input))){
      return(NULL)
    } else if(check_numFiles(finput = isolate(input$dataMerge_input), isExact = FALSE, numFiles = 2)){
      return(NULL)
    } else if(check_numOrdered(fselected = isolate(input$dataMerge_selectInput), isExact = FALSE, numFiles = 2)){
      return(NULL)
    } else if(check_validOrder(fselected = isolate(input$dataMerge_selectInput))){
      return(NULL)
    } else{
      
      # plot sequence persistence
      promises::future_promise({
        
        # format FA2 FASTA files in parallel
        purrr::map(
          .x = isolate(input$dataMerge_input[match(input$dataMerge_selectInput, input$dataMerge_input$name),]$datapath),
          .f = fa_formatInput
        ) %>% 
          fa_dataMerge_seqPersistence(
            fa2DF_list = .,
            xaxis = isolate(input$dataMerge_persistencePlot_xaxis),
            yaxis = isolate(input$dataMerge_persistencePlot_yaxis),
            plot_title = isolate(input$dataMerge_persistencePlot_title),
            bar_outline = isolate(input$dataMerge_persistencePlot_baroutline),
            bar_fill = isolate(input$dataMerge_persistencePlot_barfill)
          )
      })
    }
  }) %>% 
    bindCache(
      input$dataMerge_input,
      input$dataMerge_selectInput,
      input$dataMerge_persistencePlot_xaxis,
      input$dataMerge_persistencePlot_yaxis,
      input$dataMerge_persistencePlot_title,
      input$dataMerge_persistencePlot_baroutline,
      input$dataMerge_persistencePlot_barfill,
      Sys.Date(),
      cache = "session"
    ) %>% 
    bindEvent(input$dataMerge_persistencePlotStart)
  
  ## DATA MERGE - RENDER SEQUENCE PERSISENCE PLOT
  output$dataMerge_sequencePersistence <- plotly::renderPlotly({
    
    if(is.null(dataMerge_sequencePersistencePlot())){
      return(NULL)
    } else{
      
      dataMerge_sequencePersistencePlot()
    }
  })
  
  ## DATA MERGE - GENERATE UpSetR PLOT
  dataMerge_UpSetRPlot <- reactive({
    
    if(check_fileInput(finput = isolate(input$dataMerge_input))){
      return(NULL)
    } else if(check_numFiles(finput = isolate(input$dataMerge_input), isExact = FALSE, numFiles = 2)){
      return(NULL)
    } else if(check_numOrdered(fselected = isolate(input$dataMerge_selectInput), isExact = FALSE, numFiles = 2)){
      return(NULL)
    } else if(check_validOrder(fselected = isolate(input$dataMerge_selectInput))){
      return(NULL)
    } else{
      
      # format FA2 FASTA files in parallel, and then make UpSetR plot
      promises::future_promise({
        
        purrr::map(
          .x = isolate(input$dataMerge_input$datapath),
          .f = fa_formatInput
        ) %>% 
          fa_dataMerge_UpSetR(
            fa2DF_list = .,
            fastaNames = isolate(input$dataMerge_input$name),
            xaxis = isolate(input$dataMerge_UpSetPlot_xaxis),
            yaxis = isolate(input$dataMerge_UpSetPlot_yaxis),
            bar_fill = isolate(input$dataMerge_UpSetPlot_barfill)
          )
      })
    }
  }) %>% 
    bindCache(
      input$dataMerge_input,
      input$dataMerge_selectInput,
      input$dataMerge_UpSetPlot_xaxis,
      input$dataMerge_UpSetPlot_yaxis,
      input$dataMerge_UpSetPlot_barfill,
      Sys.Date(),
      cache = "session"
    ) %>% 
    bindEvent(input$dataMerge_UpSetPlotStart)
  
  ## DATA MERGE - RENDER UpSetR PLOT
  output$dataMerge_UpSetR <- renderPlot({
    
    if(is.null(dataMerge_UpSetRPlot())){
      return(NULL)
    } else{
      
      dataMerge_UpSetRPlot()
    }
  })
}
