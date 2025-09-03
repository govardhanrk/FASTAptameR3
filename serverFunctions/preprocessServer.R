# Copyright (C) 2022 Skyler T. Kramer
# Full GNU GPL version 3 license found in LICENSE.txt

preprocessServer <- function(input, output, session){
  
  ## PREPROCESS - DATA GENERATION
  preprocessDF <- reactive({
    
    if(check_fileInput(finput = isolate(input$preprocessInput))){
      showNotification("No file provided!", type = "error", duration = NULL)
      return(NULL)
    } else if(check_extension(fpath = isolate(input$preprocessInput$datapath), extensions = c("fq", "fa", "fastq", "fasta", "gz"))){
      showNotification("Please supply a file with the correct extension!", type = "error", duration = NULL)
      return(NULL)
    } else if(check_regexInputs(pattern = "[^a-zA-Z0-9]", separator = " ", userInputs = isolate(input$preprocess_const5p))){
      showNotification("5' constant region must be alphanumeric!", type = "error", duration = NULL)
      return(NULL)
    } else if(check_regexInputs(pattern = "[^a-zA-Z0-9]", separator = " ", userInputs = isolate(input$preprocess_const3p))){
      showNotification("3' constant region must be alphanumeric!", type = "error", duration = NULL)
      return(NULL)
    } else{
      
      # input checks
      req(min(isolate(input$preprocess_lengthRange)) >= 3 & max(isolate(input$preprocess_lengthRange)) <= 500)
      req(isolate(input$preprocess_maxError) %in% 0.001:1)
      
      # preprocess data
      promises::future_promise({
        
        fa_preprocess(
          inputPath = isolate(input$preprocessInput$datapath),
          const5p = isolate(input$preprocess_const5p),
          const3p = isolate(input$preprocess_const3p),
          length_range = isolate(input$preprocess_lengthRange),
          max_error = isolate(input$preprocess_maxError)
        )
      })
    }
  }) %>% 
    bindCache(
      input$preprocessInput,
      input$preprocess_const5p,
      input$preprocess_const3p,
      input$preprocess_lengthRange,
      input$preprocess_maxError,
      Sys.Date(),
      cache = "session"
    ) %>% 
    bindEvent(input$preprocessStart)
  
  ## PREPROCESS - DATA OUTPUT
  output$preprocessOutput <- DT::renderDataTable({
    
    preprocessDF() %>% 
      DT::datatable(filter = list(position = "top", plain = TRUE, clear = FALSE), rownames = FALSE)
  })
  
  ## PREPROCESS - DATA DOWNLOAD
  output$preprocessDownload <- downloadHandler(
    
    # set filename
    filename = function(){
      
      gsub(
        paste0("\\.", tools::file_ext(isolate(input$preprocessInput$name))),
        "-preprocess\\.fasta",
        isolate(input$preprocessInput$name)
      )
    },
    
    # set file content
    content = function(file){
      
      # format data for output as FASTA file
      preprocessDF() %...>% 
        dplyr::pull(Sequence) %...>% 
        writeLines(., file)
    }
  )
}
