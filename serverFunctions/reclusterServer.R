# Copyright (C) 2022 Skyler T. Kramer
# Full GNU GPL version 3 license found in LICENSE.txt

reclusterServer <- function(input, output, session){
  
  ## RECLUSTER - RENDER LINE PLOT
  reclusterHeatmap <- reactive({
    
    if(check_fileInput(finput = isolate(input$reclusterInput1))){
      showNotification("Upload a file to first input!", type = "error", duration = NULL)
      return(NULL)
    } else if(check_fileInput(finput = isolate(input$reclusterInput2))){
      showNotification("Upload a file to second input!", type = "error", duration = NULL)
      return(NULL)
    } else if(check_extension(fpath = isolate(input$reclusterInput1$datapath), extensions = "fasta")){
      showNotification("File 1 does not have the correct extension!", type = "error", duration = NULL)
      return(NULL)
    } else if(check_extension(fpath = isolate(input$reclusterInput2$datapath), extensions = "fasta")){
      showNotification("File 2 does not have the correct extension!", type = "error", duration = NULL)
      return(NULL)
    } else if(check_clusterInID(fpath = isolate(input$reclusterInput1$datapath)) | check_clusterInID(fpath = isolate(input$reclusterInput2$datapath))){
      showNotification("Only supply clustered files!", type = "error", duration = NULL)
      return(NULL)
    } else{
      
      # make heat map
      promises::future_promise({
        
        # read and format files for reclustering
        fa2DF1_cluster <- fa_formatInput(fastaInput = isolate(input$reclusterInput1$datapath))
        fa2DF2_cluster <- fa_formatInput(fastaInput = isolate(input$reclusterInput2$datapath))
        
        # heat map
        fa_recluster_heatmap(
          fa2DF1_cluster = fa2DF1_cluster,
          fa2DF2_cluster = fa2DF2_cluster,
          xaxis = isolate(input$recluster_heatmap_xaxis),
          yaxis = isolate(input$recluster_heatmap_yaxis),
          legend_title = isolate(input$recluster_heatmap_legend),
          plot_title = isolate(input$recluster_heatmap_title),
          fill_palette_cont = isolate(input$recluster_heatmap_palette)
        )
      })
    }
  }) %>% 
    bindCache(
      input$reclusterInput1,
      input$reclusterInput2,
      input$recluster_heatmap_xaxis,
      input$recluster_heatmap_yaxis,
      input$recluster_heatmap_legend,
      input$recluster_heatmap_palette,
      Sys.Date(),
      cache = "session"
    ) %>% 
    bindEvent(input$reclusterStart_heatmap)
  
  ## RECLUSTER - DISPLAY LINE PLOT
  output$reclusterHeatmapOutput <- plotly::renderPlotly(
    reclusterHeatmap()
  )
  
  ## RECLUSTER - DATA GENERATION
  reclusterDF <- reactive({
    
    if(check_fileInput(finput = isolate(input$reclusterInput1))){
      showNotification("Upload file to first input!", type = "error", duration = NULL)
      return(NULL)
    } else if(check_fileInput(finput = isolate(input$reclusterInput2))){
      showNotification("Upload file to second input!", type = "error", duration = NULL)
      return(NULL)
    } else if(check_extension(fpath = isolate(input$reclusterInput1$datapath), extensions = "fasta")){
      showNotification("File 1 does not have the correct extension!", type = "error", duration = NULL)
      return(NULL)
    } else if(check_extension(fpath = isolate(input$reclusterInput2$datapath), extensions = "fasta")){
      showNotification("File 2 does not have the correct extension!", type = "error", duration = NULL)
      return(NULL)
    } else if(check_clusterInID(fpath = isolate(input$reclusterInput1$datapath)) | check_clusterInID(fpath = isolate(input$reclusterInput2$datapath))){
      showNotification("Only supply clustered files!", type = "error", duration = NULL)
      return(NULL)
    } else{
      
      # input checks
      req(isolate(input$reclusterSlider_maxLED) %in% 1:20)
      req(isolate(input$recluster_heatmap_palette) %in% c("magma", "inferno", "plasma", "viridis", "cividis", "rocket", "mako", "turbo"))
      req(grepl(pattern = "^#([A-Fa-f0-9]{6}|[A-Fa-f0-9]{3})$", x = isolate(input$recluster_popSize_baroutline)))
      req(isolate(input$recluster_popSize_palette) %in% c("Accent", "Dark2", "Paired", "Pastel1", "Pastel2", "Set1", "Set2"))
      req(grepl(pattern = "^#([A-Fa-f0-9]{6}|[A-Fa-f0-9]{3})$", x = isolate(input$recluster_RPU_baroutline)))
      req(isolate(input$recluster_RPU_palette) %in% c("Accent", "Dark2", "Paired", "Pastel1", "Pastel2", "Set1", "Set2"))
      req(grepl(pattern = "^#([A-Fa-f0-9]{6}|[A-Fa-f0-9]{3})$", x = isolate(input$recluster_LED_baroutline)))
      req(grepl(pattern = "^#([A-Fa-f0-9]{6}|[A-Fa-f0-9]{3})$", x = isolate(input$recluster_LED_barfill)))
      req(grepl(pattern = "^#([A-Fa-f0-9]{6}|[A-Fa-f0-9]{3})$", x = isolate(input$recluster_boxplot_baroutline)))
      req(grepl(pattern = "^#([A-Fa-f0-9]{6}|[A-Fa-f0-9]{3})$", x = isolate(input$recluster_boxplot_barfill)))
      
      # recluster
      promises::future_promise(
        {
          # read and format files for reclustering
          fa2DF1_cluster <- fa_formatInput(fastaInput = isolate(input$reclusterInput1$datapath))
          fa2DF2_cluster <- fa_formatInput(fastaInput = isolate(input$reclusterInput2$datapath))
          
          # recluster
          fa_recluster(
            fa2DF1_cluster = fa2DF1_cluster,
            fa2DF2_cluster = fa2DF2_cluster,
            led_threshold = isolate(input$reclusterSlider_maxLED)
          )
        }, seed = TRUE
      )
    }
  }) %>% 
    bindCache(
      input$reclusterInput1,
      input$reclusterInput2,
      input$reclusterSlider_maxLED,
      Sys.Date(),
      cache = "session"
    ) %>% 
    bindEvent(input$reclusterStart)
  
  ## RECLUSTER - DATA OUTPUT
  output$reclusterOutput <- DT::renderDataTable({
    
    reclusterDF() %...>% 
      DT::datatable(filter = list(position = "top", plain = TRUE, clear = FALSE), rownames = FALSE)
  })
  
  ## RECLUSTER - DOWNLOAD
  output$reclusterDownload <- downloadHandler(
    
    # set filename
    filename = "recluster.csv",
    
    # set file content
    content = function(file){
      
      # format data for output as CSV file
      reclusterDF() %...>% 
        {.[input[["reclusterOutput_rows_all"]],]} %...>% 
        write.csv(., file, row.names = FALSE, quote = FALSE)
    }
  )
  
  ## RECLUSTER - GENERATE POPULATION BAR PLOT
  recluster_popSize <- reactive({
    
    if(is.null(reclusterDF())){
      showNotification("Generate super-clusters before plotting!", type = "error", duration = NULL)
      return(NULL)
    } else{
      
      # make plot
      reclusterDF() %...>% 
        fa_recluster_popSize(
          recluster_df = .,
          xaxis = isolate(input$recluster_popSize_xaxis),
          yaxis = isolate(input$recluster_popSize_yaxis),
          legend_title = isolate(input$recluster_popSize_legend),
          plot_title = isolate(input$recluster_popSize_title),
          bar_outline = isolate(input$recluster_popSize_baroutline),
          fill_palette_disc = isolate(input$recluster_popSize_palette)
        )
    }
  }) %>% 
    bindCache(
      input$reclusterInput1,
      input$reclusterInput2,
      input$reclusterSlider_maxLED,
      input$recluster_popSize_xaxis,
      input$recluster_popSize_yaxis,
      input$recluster_popSize_legend,
      input$recluster_popSize_title,
      input$recluster_popSize_baroutline,
      input$recluster_popSize_palette,
      Sys.Date(),
      cache = "session"
    ) %>% 
    bindEvent(input$reclusterPopSize_start)
  
  ## RECLUSTER - RENDER POPULATION BAR PLOT
  output$reclusterPopSizeOutput <- plotly::renderPlotly({
    
    if(is.null(recluster_popSize())){
      return(NULL)
    } else{
      
      recluster_popSize()
    }
  })
  
  ## RECLUSTER - GENERATE AVERAGE RPU BAR PLOT
  recluster_RPU <- reactive({
    
    if(is.null(reclusterDF())){
      showNotification("Generate super-clusters before plotting!", type = "error", duration = NULL)
      return(NULL)
    } else{
      
      # make plot
      reclusterDF() %...>% 
        fa_recluster_RPU(
          recluster_df = .,
          xaxis = isolate(input$recluster_RPU_xaxis),
          yaxis = isolate(input$recluster_RPU_yaxis),
          legend_title = isolate(input$recluster_RPU_legend),
          plot_title = isolate(input$recluster_RPU_title),
          bar_outline = isolate(input$recluster_RPU_baroutline),
          fill_palette_disc = isolate(input$recluster_RPU_palette)
        )
    }
  }) %>% 
    bindCache(
      input$reclusterInput1,
      input$reclusterInput2,
      input$reclusterSlider_maxLED,
      input$recluster_RPU_xaxis,
      input$recluster_RPU_yaxis,
      input$recluster_RPU_legend,
      input$recluster_RPU_title,
      input$recluster_RPU_baroutline,
      input$recluster_RPU_palette,
      Sys.Date(),
      cache = "session"
    ) %>% 
    bindEvent(input$reclusterRPU_start)
  
  ## RECLUSTER - RENDER AVERAGE RPU BAR PLOT
  output$reclusterRPUOutput <- plotly::renderPlotly({
    
    if(is.null(recluster_RPU())){
      return(NULL)
    } else{
      
      recluster_RPU()
    }
  })
  
  ## RECLUSTER - GENERATE AVERAGE LED BAR PLOT
  recluster_LED <- reactive({
    
    if(is.null(reclusterDF())){
      showNotification("Generate super-clusters before plotting!", type = "error", duration = NULL)
      return(NULL)
    } else{
      
      # make plot
      reclusterDF() %...>% 
        fa_recluster_LED(
          recluster_df = .,
          xaxis = isolate(input$recluster_LED_xaxis),
          yaxis = isolate(input$recluster_LED_yaxis),
          plot_title = isolate(input$recluster_LED_title),
          bar_outline = isolate(input$recluster_LED_baroutline),
          bar_fill = isolate(input$recluster_LED_barfill)
        )
    }
  }) %>% 
    bindCache(
      input$reclusterInput1,
      input$reclusterInput2,
      input$reclusterSlider_maxLED,
      input$recluster_LED_xaxis,
      input$recluster_LED_yaxis,
      input$recluster_LED_title,
      input$recluster_LED_baroutline,
      input$recluster_LED_barfill,
      Sys.Date(),
      cache = "session"
    ) %>% 
    bindEvent(input$reclusterLED_start)
  
  ## RECLUSTER - RENDER AVERAGE LED BAR PLOT
  output$reclusterLEDOutput <- plotly::renderPlotly({
    
    if(is.null(recluster_LED())){
      return(NULL)
    } else{
      
      recluster_LED()
    }
  })
  
  ## RECLUSTER - GENERATE SEQUENCE ENRICHMENT BOX PLOT
  recluster_enrich <- reactive({
    
    if(is.null(reclusterDF())){
      showNotification("Generate super-clusters before plotting!", type = "error", duration = NULL)
      return(NULL)
    } else{
      
      # make plot
      reclusterDF() %...>% 
        fa_recluster_enrich(
          recluster_df = .,
          xaxis = isolate(input$recluster_boxplot_xaxis),
          plot_title = isolate(input$recluster_boxplot_title),
          box_outline = isolate(input$recluster_boxplot_baroutline),
          box_fill = isolate(input$recluster_boxplot_barfill)
        )
    }
  }) %>% 
    bindCache(
      input$reclusterInput1,
      input$reclusterInput2,
      input$reclusterSlider_maxLED,
      input$recluster_boxplot_xaxis,
      input$recluster_boxplot_title,
      input$recluster_boxplot_baroutline,
      input$recluster_boxplot_barfill,
      Sys.Date(),
      cache = "session"
    ) %>% 
    bindEvent(input$reclusterEnrich_start)
  
  ## RECLUSTER - RENDER SEQUENCE ENRICHMENT BOX PLOT
  output$reclusterEnrichOutput <- plotly::renderPlotly({
    
    if(is.null(recluster_enrich())){
      return(NULL)
    } else{
      
      recluster_enrich()
    }
  })
}
