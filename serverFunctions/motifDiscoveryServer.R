# Copyright (C) 2022 Skyler T. Kramer
# Full GNU GPL version 3 license found in LICENSE.txt

motifDiscoveryServer <- function(input, output, session){
  
  ## MOTIF DISCOVERY - DATA GENERATION
  motifDiscoveryDF <- reactive({
    
    if(check_fileInput(isolate(input$motifDiscovery_input))){
      showNotification("No file provided!", type = "error", duration = NULL)
      return(NULL)
    } else if(check_extension(fpath = isolate(input$motifDiscovery_input$datapath), extensions = "fasta")){
      showNotification("Please supply a file with the correct extension!", type = "error", duration = NULL)
      return(NULL)
    } else{
      
      # input checks
      req(isolate(input$motifDiscovery_minReads) %in% 0:1000)
      req(min(isolate(input$motifDiscovery_lengthRange)) >= 3 & max(isolate(input$motifDiscovery_lengthRange)) <= 15)
      req(isolate(input$motifDiscovery_scatter_palette) %in% c("magma", "inferno", "plasma", "viridis", "cividis", "rocket", "mako", "turbo"))
      
      # discover motifs
      promises::future_promise({
        
        fa_formatInput(fastaInput = isolate(input$motifDiscovery_input$datapath)) %>% 
          fa_fsbc_motifDiscovery(
            fa2DF = .,
            minReads = isolate(input$motifDiscovery_minReads),
            lengthRange = isolate(input$motifDiscovery_lengthRange)
          )
      })
    }
  }) %>% 
    bindCache(
      input$motifDiscovery_input,
      input$motifDiscovery_minReads,
      input$motifDiscovery_lengthRange,
      Sys.Date(),
      cache = "session"
    ) %>% 
    bindEvent(input$motifDiscovery_start)
  
  ## MOTIF DISCOVERY - DATA OUTPUT
  output$motifDiscovery_output <- DT::renderDataTable({
    
    motifDiscoveryDF() %...>% 
      DT::datatable(filter = list(position = "top", plain = TRUE, clear = FALSE), rownames = FALSE)
  })
  
  ## MOTIF DISCOVERY DOWNLOAD
  output$motifDiscovery_download <- downloadHandler(
    
    # set filename
    filename = function(){
      "motifDiscovery.csv"
    },
    
    # set file content
    content = function(file){
      
      # format data for output as CSV file
      motifDiscoveryDF() %...>% 
        {.[input[["motifDiscovery_output_rows_all"]],]} %...>% 
        write.csv(., file, row.names = FALSE, quote = TRUE)
    }
  )
  
  ## MOTIF DISCOVERY - BUBBLE PLOT - PLOT
  motifDiscovery_plot <- reactive({
    
    if(is.null(motifDiscoveryDF())){
      return(NULL)
    } else{
      
      # make plot
      motifDiscoveryDF() %...>% 
        fa_fsbc_motifDiscoveryPlot(
          motif_discovery_df = .,
          xaxis = isolate(input$motifDiscovery_scatter_xaxis),
          yaxis = isolate(input$motifDiscovery_scatter_yaxis),
          legend_title = isolate(input$motifDiscovery_scatter_legend),
          plot_title = isolate(input$motifDiscovery_scatter_title),
          colour_palette_cont = isolate(input$motifDiscovery_scatter_palette)
        )
    }
  }) %>% 
    bindCache(
      input$motifDiscovery_input,
      input$motifDiscovery_minReads,
      input$motifDiscovery_lengthRange,
      input$motifDiscovery_scatter_xaxis,
      input$motifDiscovery_scatter_yaxis,
      input$motifDiscovery_scatter_legend,
      input$motifDiscovery_scatter_title,
      input$motifDiscovery_scatter_palette,
      Sys.Date(),
      cache = "session"
    ) %>% 
    bindEvent(input$motifDiscovery_scatterStart)
  
  ## MOTIF DISCOVERY - BUBBLE PLOT - RENDER
  output$motifDiscovery_plot <- plotly::renderPlotly({
    
    if(is.null(motifDiscovery_plot())){
      return(NULL)
    } else{
      
      motifDiscovery_plot()
    }
  })
}
