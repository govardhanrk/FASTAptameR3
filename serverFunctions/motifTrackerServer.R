# Copyright (C) 2022 Skyler T. Kramer
# Full GNU GPL version 3 license found in LICENSE.txt

motifTrackerServer <- function(input, output, session){
  
  ## MOTIF TRACKER - UPDATE FILE SELECTIONS
  observe({
    updateSelectizeInput(session = session, inputId = "motifTracker_selectInput", choices = input$motifTrackerInput$name)
  })
  
  ## MOTIF TRACKER - DATA GENERATION
  motifTrackerDF <- reactive({
    
    if(check_fileInput(finput = isolate(input$motifTrackerInput))){
      showNotification("No files provided!", type = "error", duration = NULL)
      return(NULL)
    } else if(check_numFiles(finput = isolate(input$motifTrackerInput), isExact = FALSE, numFiles = 2)){
      showNotification("Supply at least 2 files!", type = "error", duration = NULL)
      return(NULL)
    } else if(check_numOrdered(fselected = isolate(input$motifTracker_selectInput), isExact = FALSE, numFiles = 2)){
      showNotification("Please order at least 2 files!", type = "error", duration = NULL)
      return(NULL)
    } else if(check_validOrder(fselected = isolate(input$motifTracker_selectInput))){
      showNotification("Asterisk is a placeholder and not a valid file ordering!", duration = NULL)
      return(NULL)
    } else if(check_emptyString(stringInput = isolate(input$motifInput_query))){
      showNotification("Must supply valid motif(s)!", type = "error", duration = NULL)
      return(NULL)
    } else if(check_regexInputs(pattern = "[^a-zA-Z0-9]", separator = ",| |\n", userInputs = isolate(input$motifInput_query))){
      showNotification("Query list may not contain special characters other than commas!", type = "error", duration = NULL)
      return(NULL)
    } else if(check_regexInputs(pattern = "[^a-zA-Z0-9]", separator = " |\n", userInputs = isolate(input$motifInput_alias))){
      showNotification("Alias list may not contain any special characters!", type = "error", duration = NULL)
      return(NULL)
    } else if(check_queryAlias(queries = isolate(input$motifInput_query), aliases = isolate(input$motifInput_alias))){
      showNotification("When aliases are provided, there must be one for each query!", type = "error", duration = NULL)
      return(NULL)
    } else if(check_extensions(finput = isolate(input$motifTrackerInput), extensions = "fasta")){
      showNotification("Please confirm that all files have the correct extension!", type = "error", duration = NULL)
      return(NULL)
    } else{
      
      # input checks
      req(isolate(input$motifTrackerButton_queryType) %in% c("Motif", "Sequence"))
      req(isolate(input$motifTrackerButton_motifType) %in% c("Nucleotide", "AminoAcid", "String"))
      req(isolate(input$motifTracker_line_palette) %in% c("Accent", "Dark2", "Paired", "Pastel1", "Pastel2", "Set1", "Set2"))
      
      # track motifs or sequences
      promises::future_promise({
        
        # check if user is supplying motifs or sequences
        if(isolate(input$motifTrackerButton_queryType) == "Motif"){
          
          # format FA2 FASTA files in parallel, and then track motifs through populations
          purrr::map(
            .x = isolate(input$motifTrackerInput[match(input$motifTracker_selectInput, input$motifTrackerInput$name),]$datapath),
            .f = fa_formatInput
          ) %>%
            fa_motif_motifTracker(
              fa2DF_list = .,
              fileNames = isolate(input$motifTrackerInput[match(input$motifTracker_selectInput, input$motifTrackerInput$name),]$name),
              queryList = isolate(input$motifInput_query) %>% strsplit(split = "\n") %>% unlist() %>% gsub("\\s", "", .),
              queryAliases = isolate(input$motifInput_alias) %>% strsplit(split = "\n") %>% unlist() %>% gsub("\\s", "", .),
              motifType = isolate(input$motifTrackerButton_motifType)
            )
        } else{
          
          # format FA2 FASTA files in parallel, and then track sequences through populations
          purrr::map(
            .x = isolate(input$motifTrackerInput[match(input$motifTracker_selectInput, input$motifTrackerInput$name),]$datapath),
            .f = fa_formatInput
          ) %>% 
            fa_motif_sequenceTracker(
              fa2DF_list = .,
              fileNames = isolate(input$motifTrackerInput[match(input$motifTracker_selectInput, input$motifTrackerInput$name),]$name),
              queryList = isolate(input$motifInput_query) %>% strsplit(split = "\n") %>% unlist() %>% gsub("\\s", "", .),
              queryAliases = isolate(input$motifInput_alias) %>% strsplit(split = "\n") %>% unlist() %>% gsub("\\s", "", .)
            )
        }
      })
    }
  }) %>% 
    bindCache(
      input$motifTrackerInput,
      input$motifTracker_selectInput,
      input$motifInput_query,
      input$motifInput_alias,
      input$motifTrackerButton_motifType,
      Sys.Date(),
      cache = "session"
    ) %>% 
    bindEvent(input$motifTrackerStart)
  
  ## MOTIF TRACKER - DATA DISPLAY
  output$motifTrackerOutput <- DT::renderDataTable({
    
    motifTrackerDF() %...>% 
      DT::datatable(rownames = FALSE)
  })
  
  ## MOTIF TRACKER - DATA DOWNLOAD
  output$motifTrackerDownload_summary <- downloadHandler(
    
    # set filename
    filename = function(){
      "motifTracker_summary.csv"
    },
    
    # set file content
    content = function(file){
      
      # format data for output as CSV file
      motifTrackerDF() %...>% 
        write.csv(., file, row.names = FALSE, quote = FALSE)
    }
  )
  
  ## MOTIF TRACKER - ENRICHMENT GENERATION
  motifTrackerDF_enrich <- reactive({
    
    if(is.null(motifTrackerDF())){
      return(NULL)
    } else{
      
      # make plot
      motifTrackerDF() %...>% 
        fa_motif_trackerEnrichment(trackerDF = .)
    }
  }) %>% 
    bindCache(
      input$motifTrackerInput,
      input$motifTracker_selectInput,
      input$motifInput_query,
      input$motifInput_alias,
      input$motifTrackerButton_motifType,
      Sys.Date(),
      cache = "session"
    ) %>% 
    bindEvent(input$motifTrackerStart)
  
  ## MOTIF TRACKER - ENRICHMENT DISPLAY
  output$motifTrackerOutput_enrichmentTable <- DT::renderDataTable({
    
    motifTrackerDF_enrich() %...>% 
      DT::datatable(rownames = FALSE)
  })
  
  ## MOTIF TRACKER - ENRICHMENT DOWNLOAD
  output$motifTrackerDownload_enrich <- downloadHandler(
    
    # set filename
    filename = function(){
      "motifTracker_enrich.csv"
    },
    
    # set file content
    content = function(file){
      
      # format data for output as CSV file
      motifTrackerDF_enrich() %...>% 
        write.csv(., file, row.names = FALSE, quote = TRUE)
    }
  )
  
  ## MOTIF TRACKER - PLOT GENERATION
  trackerPlot <- reactive({
    
    if(is.null(motifTrackerDF())){
      return(NULL)
    } else{
      
      if(isolate(input$motifTrackerButton_queryType) == "Motif"){
        
        # make plot for motif tracking
        motifTrackerDF() %...>% 
          fa_motif_motifTrackerPlot(
            trackerDF = .,
            xaxis = isolate(input$motifTracker_line_xaxis),
            yaxis = isolate(input$motifTracker_line_yaxis),
            plot_title = isolate(input$motifTracker_line_title),
            colour_palette_disc = isolate(input$motifTracker_line_palette)
          )
      } else{
        
        # make plot for sequence tracking
        motifTrackerDF() %...>% 
          fa_motif_sequenceTrackerPlot(
            trackerDF = .,
            xaxis = isolate(input$motifTracker_line_xaxis),
            yaxis = isolate(input$motifTracker_line_yaxis),
            plot_title = isolate(input$motifTracker_line_title),
            colour_palette_disc = isolate(input$motifTracker_line_palette)
          )
      }
    }
  }) %>% 
    bindCache(
      input$motifTrackerInput,
      input$motifTracker_selectInput,
      input$motifInput_query,
      input$motifInput_alias,
      input$motifTrackerButton_motifType,
      input$motifTracker_line_xaxis,
      input$motifTracker_line_yaxis,
      input$motifTracker_line_title,
      input$motifTracker_line_palette,
      Sys.Date(),
      cache = "session"
    ) %>% 
    bindEvent(input$motifTracker_plotStart)
  
  ## MOTIF TRACKER - RENDER PLOT
  output$motifTrackerPlot <- plotly::renderPlotly({
    
    if(is.null(trackerPlot())){
      return(NULL)
    } else{
      trackerPlot()
    }
  })
}
