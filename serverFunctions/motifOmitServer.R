# Copyright (C) 2022 Skyler T. Kramer
# Full GNU GPL version 3 license found in LICENSE.txt

motifOmitServer <- function(input, output, session){
  
  ## MOTIF Omit - DATA GENERATION
  motifOmitDF <- reactive({
    
    if(check_fileInput(finput = isolate(input$motifOmitInput))){
      showNotification("No file provided!", type = "error", duration = NULL)
      return(NULL)
    } else if(check_extension(fpath = isolate(input$motifOmitInput$datapath), extensions = "fasta")){
      showNotification("Please supply a file with the correct extension!", type = "error", duration = NULL)
      return(NULL)
    } else if(check_emptyString(isolate(input$motifInput_omit))){
      showNotification("Must supply valid pattern(s)!", type = "error", duration = NULL)
      return(NULL)
    } else if(check_regexInputs(pattern = "[^a-zA-Z0-9]", separator = ",| ", userInputs = isolate(input$motifInput_omit))){
      showNotification("Pattern list must be alphanumeric!", type = "error", duration = NULL)
      return(NULL)
    } else{
      
      # input checks
      req(isolate(input$motifOmitButton_partial) %in% c("Yes", "No"))
      req(isolate(input$motifOmitButton_motifType) %in% c("Nucleotide","AminoAcid", "String"))
      req(isolate(input$motifOmitDownloadType) %in% c("FASTA", "CSV"))
      
      # omit motifs
      promises::future_promise({
        
        fa_formatInput(fastaInput = isolate(input$motifOmitInput$datapath)) %>% 
          fa_motifOmit(
            fa2DF = .,
            motif = isolate(input$motifInput_omit) %>% gsub("\\s", "", .) %>% strsplit(split = ",") %>% unlist(),
            partial = ifelse(isolate(input$motifOmitButton_partial) == "Yes", TRUE, FALSE),
            motifType = isolate(input$motifOmitButton_motifType)
          )
      })
    }
  }) %>% 
    bindCache(
      input$motifOmitInput,
      input$motifInput_omit,
      input$motifOmitButton_partial,
      input$motifOmitButton_motifType,
      Sys.Date(),
      cache = "session"
    ) %>% 
    bindEvent(input$motifOmitStart)
  
  ## MOTIF Omit - DATA DISPLAY
  output$motifOmitOutput <- DT::renderDataTable({
    
    motifOmitDF() %...>% 
      DT::datatable(filter = list(position = "top", plain = TRUE, clear = FALSE), rownames = FALSE)
  })
  
  ## MOTIF Omit - DATA DOWNLOAD
  output$motifOmitDownload <- downloadHandler(
    
    # set filename
    filename = function(){
      
      gsub(
        paste0("\\.", tools::file_ext(isolate(input$motifOmitInput$name))),
        paste0("-omit\\.", tolower(isolate(input$motifOmitDownloadType))),
        isolate(input$motifOmitInput$name)
      )
    },
    
    # set file content
    content = function(file){
      
      # format data for output as FASTA file
      if(isolate(input$motifOmitDownloadType) == "FASTA"){
        
        motifOmitDF() %...>% 
          {.[input[["motifOmitOutput_rows_all"]],]} %...>% 
          fa_formatOutput(outputData = .) %...>% 
          write.table(., file, quote = FALSE, row.names = FALSE, col.names = FALSE)
        
      } else{
        
        motifOmitDF() %...>% 
          {.[input[["motifOmitOutput_rows_all"]],]} %...>% 
          write.csv(., file, quote = FALSE, row.names = FALSE)
        
      }
    }
  )
}
