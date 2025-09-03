# Copyright (C) 2022 Skyler T. Kramer
# Full GNU GPL version 3 license found in LICENSE.txt

clusterMSAServer <- function(input, output, session){
  
  ## CLUSTER MSA - GET CLUSTERS IN DATA
  msa_clusters_in_data <- reactive({
    
    if(check_fileInput(finput = isolate(input$clusterMSAInput))){
      showNotification("No file provided!", type = "error", duration = NULL)
      return(NULL)
    } else if(check_extension(fpath = isolate(input$clusterMSAInput$datapath), extensions = "fasta")){
      showNotification("Please supply a file with the correct extension!", type = "error", duration = NULL)
      return(NULL)
    } else if(check_clusterInID(fpath = isolate(input$clusterMSAInput$datapath))){
      showNotification("Please provided a file from FASTAptameR-Cluster!", type = "error", duration = NULL)
      return(NULL)
    } else{
      
      # get available clusters
      promises::future_promise({
        fa_getClusters(clusterPath = isolate(input$clusterMSAInput$datapath))
      }, packages = "msa")
    }
  }) %>% 
    bindCache(
      input$clusterMSAInput,
      Sys.Date(),
      cache = "session"
    ) %>% 
    bindEvent(input$clusterMSA_getClusters)
  
  ## CLUSTER MSA - UPDATE CLUSTER SELECTIONS
  observe({
    
    req(!is.null(msa_clusters_in_data()))
    
    msa_clusters_in_data() %...>% 
      updateSelectizeInput(session = session, inputId = "msa_cluster", choices = .)
  })
  
  ## CLUSTER MSA - DATA GENERATION
  clusterMSADF <- reactive({
    
    if(check_fileInput(finput = isolate(input$clusterMSAInput))){
      showNotification("No file provided!", type = "error", duration = NULL)
      return(NULL)
    } else if(check_extension(fpath = isolate(input$clusterMSAInput$datapath), extensions = "fasta")){
      showNotification("Please supply a file with the correct extension!", type = "error", duration = NULL)
      return(NULL)
    } else if(check_clusterInID(fpath = isolate(input$clusterMSAInput$datapath))){
      showNotification("Please provided a file from FASTAptameR-Cluster!", type = "error", duration = NULL)
      return(NULL)
    } else if(check_clusterSelection(cluster = isolate(input$msa_cluster))){
      showNotification("Please select a valid cluster!", type = "error", duration = NULL)
      return(NULL)
    } else{
      
      # input checks: converting the selected cluster to numeric type should not introduce NA
      req(!is.na(as.numeric(isolate(input$msa_cluster))))
      req(isolate(input$msa_seqType) %in% c("Nucleotide", "AminoAcid"))
      req(isolate(input$clusterMSA_mutinfo_palette) %in% c("magma", "inferno", "plasma", "viridis", "cividis", "rocket", "mako", "turbo"))
      req(grepl(pattern = "^#([A-Fa-f0-9]{6}|[A-Fa-f0-9]{3})$", x = isolate(input$clusterMSA_entropy_baroutline)))
      req(grepl(pattern = "^#([A-Fa-f0-9]{6}|[A-Fa-f0-9]{3})$", x = isolate(input$clusterMSA_entropy_barfill)))
      
      # run MSA
      promises::future_promise({
        
        fa_formatInput(fastaInput = isolate(input$clusterMSAInput$datapath)) %>% 
          fa_clusterMSA(
            fa2DF_cluster = .,
            clusterSelection = as.numeric(isolate(input$msa_cluster)),
            seq_type = ifelse(isolate(input$msa_seqType) == "Nucleotide", "dna", "protein")
          )
      })
    }
  }) %>% 
    bindCache(
      input$clusterMSAInput,
      input$msa_cluster,
      Sys.Date(),
      cache = "session"
    ) %>% 
    bindEvent(input$clusterMSAStart)
  
  ## CLUSTER MSA - DATA OUTPUT
  output$clusterMSAOutput <- DT::renderDataTable(DT::datatable({
    clusterMSADF()
  }, filter = list(position = "top", plain = TRUE, clear = FALSE), rownames = FALSE
  ))
  
  output$clusterMSAOutput <- DT::renderDataTable({
    clusterMSADF() %...>% 
      DT::datatable(filter = list(position = "top", plain = TRUE, clear = FALSE), rownames = FALSE)
  })
  
  ## CLUSTER MSA - DATA DOWNLOAD
  output$clusterMSADownload <- downloadHandler(
    
    # set filename
    filename = function(){
      "clusterMSA.csv"
      paste0("cluster", isolate(input$msa_cluster), "_MSA.", tolower(isolate(input$clusterMSADownloadType)))
    },
    
    # set file content
    content = function(file){
      
      # format data for output as FASTA or CSV file
      if(isolate(input$clusterMSADownloadType) == "FASTA"){
        
        clusterMSADF() %...>%
          {.[input[["clusterMSAOutput_rows_all"]],]} %...>%
          write.table(., file, row.names = FALSE, col.names = FALSE, quote = FALSE)
        
      } else{
        
        clusterMSADF() %...>%
          {.[input[["clusterMSAOutput_rows_all"]],]} %...>%
          write.csv(., file, row.names = FALSE, quote = FALSE)
      }
    }
  )
  
  ## CLUSTER MSA - ENTROPY BY POSITION - PLOT
  clusterMSAentropy <- reactive({

    if(is.null(clusterMSADF())){
      return(NULL)
    } else{
      
      # make entropy-by-position plot
      clusterMSADF() %...>% 
        fa_clusterMSA_entropy(
          msa_df = .,
          xaxis = isolate(input$clusterMSA_entropy_xaxis),
          yaxis = isolate(input$clusterMSA_entropy_yaxis),
          legend_title = isolate(input$clusterMSA_entropy_legend),
          plot_title = isolate(input$clusterMSA_entropy_title),
          bar_outline = isolate(input$clusterMSA_entropy_baroutline),
          bar_fill = isolate(input$clusterMSA_entropy_barfill)
        )
    }
  }) %>% 
    bindCache(
      input$clusterMSAInput,
      input$msa_cluster,
      input$clusterMSA_entropy_xaxis,
      input$clusterMSA_entropy_yaxis,
      input$clusterMSA_entropy_legend,
      input$clusterMSA_entropy_title,
      input$clusterMSA_entropy_baroutline,
      input$clusterMSA_entropy_barfill,
      Sys.Date(),
      cache = "session"
    ) %>% 
    bindEvent(input$clusterMSA_entropyStart)
  
  ## CLUSTER MSA - ENTROPY BY POSITION - RENDER
  output$clusterMSAOutput_entropy <- plotly::renderPlotly({

    if(is.null(clusterMSADF())){
      return(NULL)
    } else{

      clusterMSAentropy()
    }
  })
  
  ## CLUSTER MSA - MUTUAL INFORMATION - PLOT
  clusterMSAmutinfo <- reactive({
    
    if(is.null(clusterMSADF())){
      return(NULL)
    } else{
      
      # make mutual information plot
      clusterMSADF() %...>%
        fa_clusterMSA_mutInfo(
          msa_df = .,
          xaxis = isolate(input$clusterMSA_mutinfo_xaxis),
          yaxis = isolate(input$clusterMSA_mutinfo_yaxis),
          legend_title = isolate(input$clusterMSA_mutinfo_legend),
          plot_title = isolate(input$clusterMSA_mutinfo_title),
          fill_palette_cont = isolate(input$clusterMSA_mutinfo_palette)
        )
    }
  }) %>% 
    bindCache(
      input$clusterMSAInput,
      input$msa_cluster,
      input$clusterMSA_mutinfo_xaxis,
      input$clusterMSA_mutinfo_yaxis,
      input$clusterMSA_mutinfo_legend,
      input$clusterMSA_mutinfo_title,
      input$clusterMSA_mutinfo_palette,
      Sys.Date(),
      cache = "session"
    ) %>% 
    bindEvent(input$clusterMSA_mutinfoStart)
  
  ## CLUSTER MSA - MUTUAL INFORMATION - RENDER
  output$clusterMSAOutput_mutinfo <- plotly::renderPlotly({
    
    if(is.null(clusterMSADF())){
      return(NULL)
    } else{
      
      clusterMSAmutinfo()
    }
  })
}
