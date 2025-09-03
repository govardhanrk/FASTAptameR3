# Copyright (C) 2022 Skyler T. Kramer
# Full GNU GPL version 3 license found in LICENSE.txt

distanceServer <- function(input, output, session){
  
  ## DISTANCE - DATA GENERATION
  distanceDF <- reactive({
    
    if(check_fileInput(finput = isolate(input$distanceInput))){
      showNotification("No file provided!", type = "error", duration = NULL)
      return(NULL)
    } else if(check_extension(fpath = isolate(input$distanceInput$datapath), extensions = "fasta")){
      showNotification("Please supply a file with the correct extension!", type = "error", duration = NULL)
      return(NULL)
    } else if(check_emptyString(stringInput = isolate(input$querySequence))){
      showNotification("Must supply valid query sequence!", type = "error", duration = NULL)
      return(NULL)
    } else if(check_regexInputs(pattern = "[^a-zA-Z0-9]", separator = " ", userInputs = isolate(input$querySequence))){
      showNotification("Query sequence must be alphanumeric!", type = "error", duration = NULL)
      return(NULL)
    } else{
      
      # input checks
      req(isolate(input$distanceDownloadType) %in% c("FASTA", "CSV"))
      req(grepl(pattern = "^#([A-Fa-f0-9]{6}|[A-Fa-f0-9]{3})$", x = isolate(input$distance_histogram_baroutline)))
      req(grepl(pattern = "^#([A-Fa-f0-9]{6}|[A-Fa-f0-9]{3})$", x = isolate(input$distance_histogram_barfill)))
      
      # compute distance
      promises::future_promise({
        
        fa_formatInput(fastaInput = isolate(input$distanceInput$datapath)) %>% 
          fa_distance(
            fa2DF = .,
            querySequence = isolate(input$querySequence)
          )
      })
    }
  }) %>% 
    bindCache(
      input$distanceInput,
      input$querySequence,
      Sys.Date(),
      cache = "session"
    ) %>% 
    bindEvent(input$distanceStart)
  
  ## DISTANCE - DATA OUTPUT
  output$distanceOutput <- DT::renderDataTable({
    
    distanceDF() %...>% 
      DT::datatable(filter = list(position = "top", plain = TRUE, clear = FALSE), rownames = FALSE)
  })
  
  ## DISTANCE - DATA DOWNLOAD
  output$distanceDownload <- downloadHandler(
    
    # set filename
    filename = function(){
      
      gsub(
        paste0("\\.", tools::file_ext(isolate(input$distanceInput$name))),
        paste0("-distance\\.", tolower(isolate(input$distanceDownloadType))),
        isolate(input$distanceInput$name)
      )
    },
    
    # set file content
    content = function(file){
      
      if(isolate(input$distanceDownloadType) == "FASTA"){
        
        distanceDF() %...>% 
          {.[input[["distanceOutput_rows_all"]],]} %...>% 
          fa_formatOutput(outputData = .) %...>% 
          write.table(., file, quote = FALSE, row.names = FALSE, col.names = FALSE)
        
      } else{
        
        distanceDF() %...>% 
          {.[input[["distanceOutput_rows_all"]],]} %...>% 
          write.csv(., file, quote = FALSE, row.names = FALSE)
        
      }
    }
  )
  
  ## DISTANCE - DISTANCE HISTOGRAM - PLOT
  distance_histogram <- reactive({
    
    if(is.null(distanceDF())){
      return(NULL)
    } else{
      
      # make plot
      distanceDF() %...>% 
        fa_distance_histogram(
          distanceData = .,
          xaxis = isolate(input$distance_histogram_xaxis),
          yaxes = c(
            isolate(input$distance_histogram_yaxis1),
            isolate(input$distance_histogram_yaxis2)
          ),
          plot_title = isolate(input$distance_histogram_title),
          bar_outline = isolate(input$distance_histogram_baroutline),
          bar_fill = isolate(input$distance_histogram_barfill)
        )
    }
  }) %>% 
    bindCache(
      input$distanceInput,
      input$querySequence,
      input$distance_histogram_xaxis,
      input$distance_histogram_yaxis1,
      input$distance_histogram_yaxis2,
      input$distance_histogram_title,
      input$distance_histogram_baroutline,
      input$distance_histogram_barfill,
      Sys.Date(),
      cache = "session"
    ) %>%
    bindEvent(input$distance_histStart)
  
  ## DISTANCE - DISTANCE HISTOGRAM - RENDER
  output$distance_histOutput <- plotly::renderPlotly({
    
    if(is.null(distance_histogram())){
      showNotification("Please generate a data table before plotting!", type = "error", duration = NULL)
      return(NULL)
    } else{
      
      distance_histogram()
    }
  })
}
