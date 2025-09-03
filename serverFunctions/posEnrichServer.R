# Copyright (C) 2022 Skyler T. Kramer
# Full GNU GPL version 3 license found in LICENSE.txt

posEnrichServer <- function(input, output, session){
  
  ## POSITION ENRICH - GET CLUSTERS IN DATA
  posEnrich_clusters_in_data <- reactive({
    
    if(check_fileInput(finput = isolate(input$posEnrichInput))){
      showNotification("No file provided!", type = "error", duration = NULL)
      return(NULL)
    } else if(check_extension(fpath = isolate(input$posEnrichInput$datapath), extensions = "csv")){
      showNotification("Please supply a file with the correct extension!", type = "error", duration = NULL)
      return(NULL)
    } else{
      
      # get available clusters
      promises::future_promise({
        read.csv(isolate(input$posEnrichInput$datapath)) %>% dplyr::pull(Cluster) %>% unique()
      })
    }
  }) %>% 
    bindCache(
      input$posEnrichInput,
      Sys.Date(),
      cache = "session"
    ) %>% 
    bindEvent(input$posEnrich_getClusters)
  
  ## POSITION ENRICH - UPDATE CLUSTER SELECTIONS
  observe({
    
    req(!is.null(posEnrich_clusters_in_data()))
    
    posEnrich_clusters_in_data() %...>% 
      updateSelectizeInput(session = session, inputId = "posEnrich_cluster", choices = .)
  })
  
  ## POSITION ENRICH - DATA GENERATION
  posEnrichDF <- reactive({
    
    if(check_fileInput(finput = isolate(input$posEnrichInput))){
      showNotification("No file provided!", type = "error", duration = NULL)
      return(NULL)
    } else if(check_extension(fpath = isolate(input$posEnrichInput$datapath), extensions = "csv")){
      showNotification("Please supply a file with the correct extension!", type = "error", duration = NULL)
      return(NULL)
    } else if(check_reclusterInID(fpath = isolate(input$posEnrichInput$datapath))){
      showNotification("Please supply a file from the Recluster module!", type = "error", duration = NULL)
      return(NULL)
    } else if(check_clusterSelection(cluster = isolate(input$posEnrich_cluster))){
      showNotification("Please select a valid cluster!", type = "error", duration = NULL)
      return(NULL)
    } else{
      
      # check inputs
      req(isolate(input$posEnrich_seqType) %in% c("Nucleotide","AminoAcid"))
      req(!is.na(as.numeric(isolate(input$posEnrich_cluster))))
      req(isolate(input$posEnrich_heatmap_palette) %in% c("magma", "inferno", "plasma", "viridis", "cividis", "rocket", "mako", "turbo"))
      req(grepl(pattern = "^#([A-Fa-f0-9]{6}|[A-Fa-f0-9]{3})$", x = isolate(input$posEnrich_barplot_baroutline)))
      req(grepl(pattern = "^#([A-Fa-f0-9]{6}|[A-Fa-f0-9]{3})$", x = isolate(input$posEnrich_barplot_barfill)))
      
      # run position enrichment
      promises::future_promise({
        
        read.csv(isolate(input$posEnrichInput$datapath)) %>% 
          fa_posEnrich(
            fa2DF_recluster = .,
            cluster_selection = as.numeric(isolate(input$posEnrich_cluster)),
            seq_type = ifelse(isolate(input$posEnrich_seqType) == "Nucleotide", "dna", "protein")
          )
      }, packages = "msa")
    }
  }) %>% bindCache(
    input$posEnrichInput,
    input$posEnrich_cluster,
    input$posEnrich_seqType,
    Sys.Date(),
    cache = "session"
  ) %>% 
    bindEvent(input$posEnrich_start)
  
  ## POSITION ENRICH  - DATA DISPLAY
  output$posEnrichOutput <- DT::renderDataTable({
    posEnrichDF() %...>% 
      DT::datatable(filter = list(position = "top", plain = TRUE, clear = FALSE), rownames = FALSE)
  })
  
  ## CLUSTER DIVERSITY - DATA DOWNLOAD
  output$posEnrich_Download <- downloadHandler(
    
    # set filename
    filename = function(){
      "positionEnrichment.csv"
    },
    
    # set file content
    content = function(file){
      
      # format data for output as CSV file
      posEnrichDF() %...>%
        {.[input[["posEnrichOutput_rows_all"]],]} %...>%
        write.csv(., file, row.names = FALSE, quote = FALSE)
    }
  )
  
  ## POSITION ENRICH - ENRICHMENT BY POSITION - PLOT
  posEnrich_barplot <- reactive({
    
    if(is.null(posEnrichDF())){
      return(NULL)
    } else{
      
      # make bar plot
      posEnrichDF() %...>% 
        fa_posEnrich_barplot(
          fa2DF_posEnrich = .,
          xaxis = isolate(input$posEnrich_barplot_xaxis),
          yaxis = isolate(input$posEnrich_barplot_yaxis),
          plot_title = isolate(input$posEnrich_barplot_title),
          bar_outline = isolate(input$posEnrich_barplot_baroutline),
          bar_fill = isolate(input$posEnrich_barplot_barfill)
        )
    }
  }) %>% 
    bindCache(
      input$posEnrichInput,
      input$posEnrich_cluster,
      input$posEnrich_seqType,
      input$posEnrich_barplot_xaxis,
      input$posEnrich_barplot_yaxis,
      input$posEnrich_barplot_title,
      input$posEnrich_barplot_baroutline,
      input$posEnrich_barplot_barfill,
      Sys.Date(),
      cache = "session"
    ) %>% 
    bindEvent(input$posEnrich_barStart)
  
  ## POSITION ENRICH - ENRICHMENT BY POSITION - RENDER
  output$posEnrich_barplot_output <- plotly::renderPlotly({
    
    if(is.null(posEnrichDF())){
      return(NULL)
    } else{
      
      posEnrich_barplot()
    }
  })
  
  ## POSITION ENRICH - ENRICHMENT BY POSITION AND CHARACTER - PLOT
  posEnrich_heatmap <- reactive({
    
    if(is.null(posEnrichDF())){
      return(NULL)
    } else{
      
      # make bar plot
      posEnrichDF() %...>% 
        fa_posEnrich_heatmap(
          fa2DF_posEnrich = .,
          xaxis = isolate(input$posEnrich_heatmap_xaxis),
          yaxis = isolate(input$posEnrich_heatmap_yaxis),
          legend_title = isolate(input$posEnrich_heatmap_legend),
          plot_title = isolate(input$posEnrich_heatmap_title),
          fill_palette_cont = isolate(input$posEnrich_heatmap_palette)
        )
    }
  }) %>% 
    bindCache(
      input$posEnrichInput,
      input$posEnrich_cluster,
      input$posEnrich_seqType,
      input$posEnrich_heatmap_xaxis,
      input$posEnrich_heatmap_yaxis,
      input$posEnrich_heatmap_legend,
      input$posEnrich_heatmap_title,
      input$posEnrich_heatmap_palette,
      Sys.Date(),
      cache = "session"
    ) %>% 
    bindEvent(input$posEnrich_heatStart)
  
  ## POSITION ENRICH - ENRICHMENT BY POSITION AND CHARACTER - RENDER
  output$posEnrich_heatmap_output <- plotly::renderPlotly({
    
    if(is.null(posEnrichDF())){
      return(NULL)
    } else{
      
      posEnrich_heatmap()
    }
  })
}
