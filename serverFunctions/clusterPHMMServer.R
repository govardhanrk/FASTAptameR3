# Copyright (C) 2022 Skyler T. Kramer
# Full GNU GPL version 3 license found in LICENSE.txt

clusterPHMMServer <- function(input, output, session){
  
  ## CLUSTER PHMM - DERIVE PHMM
  phmmObject <- reactive({
    
    if(check_fileInput(finput = isolate(input$clusterPHMMInput))){
      showNotification("No file provided!", type = "error", duration = NULL)
      return(NULL)
    } else if(check_extension(fpath = isolate(input$clusterPHMMInput$datapath), extensions = "fasta")){
      showNotification("Please supply a file with the correct extension!", type = "error", duration = NULL)
      return(NULL)
    } else{
      
      # input checks
      req(isolate(input$clusterPHMM_numSeq) %in% 10:1000)
      req(isolate(input$clusterPHMM_seqLength) %in% 10:200)
      
      # derive the PHMM from the cluster MSA
      promises::future_promise({
        
        fa_formatInput(fastaInput = isolate(input$clusterPHMMInput$datapath)) %>% 
          fa_phmm_create(fa2DF_msa = .)
      })
    }
  }) %>% 
    bindCache(
      input$clusterPHMMInput,
      Sys.Date(),
      cache = "session"
    ) %>% 
    bindEvent(input$clusterPHMMStart)
  
  ## CLUSTER PHMM - SIMULATE SEQUENCES
  phmmSimulated <- reactive({
    
    if(is.null(phmmObject())){
      return(NULL)
    } else{
      
      # simulate sequences
      phmmObject() %...>% 
        fa_phmm_simulate(
          msaPHMM = .,
          numSeq = isolate(input$clusterPHMM_numSeq),
          seqLength = isolate(input$clusterPHMM_seqLength)
        )
    }
  }) %>% 
    bindCache(
      input$clusterPHMMInput,
      input$clusterPHMM_numSeq,
      input$clusterPHMM_seqLength,
      Sys.Date(),
      cache = "session"
    ) %>% 
    bindEvent(input$clusterPHMMStart)
  
  ## CLUSTER PHMM - DATA OUTPUT
  output$clusterPHMMOutput <- renderTable(phmmSimulated(), rownames = TRUE, colnames = FALSE)
  
  ## CLUSTER - DATA DOWNLOAD
  output$clusterPHMMDownload_PHMM <- downloadHandler(
    
    # set filename
    filename = function(){
      "PHMM.txt"
    },
    
    # set file content
    content = function(file){
      
      phmmObject() %...>% 
        aphid::writePHMM(x = ., file = file)
    }
  )
  
  ## CLUSTER - DATA DOWNLOAD
  output$clusterPHMMDownload_seqs <- downloadHandler(
    
    # set filename
    filename = function(){
      "PHMM_Simulation.fasta"
    },
    
    # set file content
    content = function(file){
      
      phmmSimulated() %...>%
        writeLines(., file)
    }
  )
}
