# Copyright (C) 2022 Skyler T. Kramer
# Full GNU GPL version 3 license found in LICENSE.txt

motifSearchServer <- function(input, output, session){
  
  ## MOTIF SEARCH - DATA GENERATION
  motifSearchDF <- reactive({
    
    if(check_fileInput(finput = isolate(input$motifSearchInput))){
      showNotification("No file provided!", type = "error", duration = NULL)
      return(NULL)
    } else if(check_extension(fpath = isolate(input$motifSearchInput$datapath), extensions = "fasta")){
      showNotification("Please supply a file with the correct extension!", type = "error", duration = NULL)
      return(NULL)
    } else if(check_emptyString(isolate(input$motifInput_search))){
      showNotification("Must supply valid pattern(s)!", type = "error", duration = NULL)
      return(NULL)
    } else if(check_regexInputs(pattern = "[^a-zA-Z0-9]", separator = ",| ", userInputs = isolate(input$motifInput_search))){
      showNotification("Pattern list must be alphanumeric!", type = "error", duration = NULL)
      return(NULL)
    } else{
      
      # input checks
      req(isolate(input$motifSearchButton_highlight) %in% c("Yes", "No"))
      req(isolate(input$motifSearchButton_partial) %in% c("Yes", "No"))
      req(isolate(input$motifSearchButton_motifType) %in% c("Nucleotide","AminoAcid", "String"))
      req(isolate(input$motifSearchDownloadType) %in% c("FASTA", "CSV"))
      
      # search for motifs
      promises::future_promise({
        
        fa_formatInput(fastaInput = isolate(input$motifSearchInput$datapath)) %>% 
          fa_motifSearch(
            fa2DF = .,
            motif = isolate(input$motifInput_search) %>% gsub("\\s", "", .) %>% strsplit(split = ",") %>% unlist(),
            highlight = ifelse(isolate(input$motifSearchButton_highlight) == "Yes", TRUE, FALSE),
            partial = ifelse(isolate(input$motifSearchButton_partial) == "Yes", TRUE, FALSE),
            motifType = isolate(input$motifSearchButton_motifType)
          )
      })
    }
  }) %>% 
    bindCache(
      input$motifSearchInput,
      input$motifInput_search,
      input$motifSearchButton_highlight,
      input$motifSearchButton_partial,
      input$motifSearchButton_motifType,
      Sys.Date(),
      cache = "session"
    ) %>% 
    bindEvent(input$motifSearchStart)
  
  ## MOTIF SEARCH - DATA DISPLAY
  output$motifSearchOutput <- DT::renderDataTable({
    
    motifSearchDF() %...>% 
      DT::datatable(
        filter = list(position = "top", plain = TRUE, clear = FALSE),
        rownames = FALSE,
        options = list(
          searchHighlight = TRUE,
          search = list(
            regex = TRUE,
            search = fa_motif_format(motifList = isolate(input$motifInput_search), motifType = isolate(input$motifSearchButton_motifType))
          )
        )
      )
  })
  
  ## MOTIF SEARCH - DATA DOWNLOAD
  output$motifSearchDownload <- downloadHandler(
    
    # set filename
    filename = function(){
      
      gsub(
        paste0("\\.", tools::file_ext(isolate(input$motifSearchInput$name))),
        paste0("-search\\.", tolower(isolate(input$motifSearchDownloadType))),
        isolate(input$motifSearchInput$name)
      )
    },
    
    # set file content
    content = function(file){
      
      # format data for output as FASTA file
      if(isolate(input$motifSearchDownloadType) == "FASTA"){
        
        motifSearchDF() %...>% 
          {.[input[["motifSearchOutput_rows_all"]],]} %...>% 
          fa_formatOutput(outputData = .) %...>% 
          write.table(., file, quote = FALSE, row.names = FALSE, col.names = FALSE)
        
      } else{
        
        motifSearchDF() %...>% 
          {.[input[["motifSearchOutput_rows_all"]],]} %...>% 
          write.csv(., file, quote = FALSE, row.names = FALSE)
        
      }
    }
  )
}
