# Copyright (C) 2022 Skyler T. Kramer
# Full GNU GPL version 3 license found in LICENSE.txt

seqEnrichServer <- function(input, output, session){
  
  ## SEQUENCE ENRICHMENT - DATA GENERATION
  enrichDF <- reactive({
    
    if(check_fileInput(finput = isolate(input$enrichInput1))){
      showNotification("Upload file to the first input!", type = "error", duration = NULL)
      return(NULL)
    } else if(check_fileInput(finput = isolate(input$enrichInput2))){
      showNotification("Upload file to the second input!", type = "error", duration = NULL)
      return(NULL)
    } else if(check_extension(fpath = isolate(input$enrichInput1$datapath), extensions = "fasta")){
      showNotification("File 1 does not have the correct extension!", type = "error", duration = NULL)
      return(NULL)
    } else if(check_extension(fpath = isolate(input$enrichInput2$datapath), extensions = "fasta")){
      showNotification("File 2 does not have the correct extension!", type = "error", duration = NULL)
      return(NULL)
    } else{
      
      # input checks
      req(isolate(input$enrichKeepNA) %in% c("Yes", "No"))
      req(grepl(pattern = "^#([A-Fa-f0-9]{6}|[A-Fa-f0-9]{3})$", x = isolate(input$enrich_hist_baroutline)))
      req(grepl(pattern = "^#([A-Fa-f0-9]{6}|[A-Fa-f0-9]{3})$", x = isolate(input$enrich_hist_barfill)))
      req(grepl(pattern = "^#([A-Fa-f0-9]{6}|[A-Fa-f0-9]{3})$", x = isolate(input$enrich_rpuScatter_pointcolour)))
      req(grepl(pattern = "^#([A-Fa-f0-9]{6}|[A-Fa-f0-9]{3})$", x = isolate(input$enrich_raScatter_pointcolour)))
      
      # calculate enrichment
      promises::future_promise({
        
        # read and format files for enrichment analysis
        fa2DF1 <- fa_formatInput(fastaInput = isolate(input$enrichInput1$datapath))
        fa2DF2 <- fa_formatInput(fastaInput = isolate(input$enrichInput2$datapath))
        
        # calculate enrichment
        fa_enrich(
          fa2DF1 = fa2DF1,
          fa2DF2 = fa2DF2,
          keepNA = ifelse(isolate(input$enrichKeepNA) == "Yes", TRUE, FALSE)
        )
      })
    }
  }) %>% 
    bindCache(
      input$enrichInput1,
      input$enrichInput1,
      input$enrichKeepNA,
      Sys.Date(),
      cache = "session"
    ) %>% 
    bindEvent(input$enrichStart)
  
  ## SEQUENCE ENRICHMENT - DATA OUTPUT
  output$enrichOutput <- DT::renderDataTable({
    
    enrichDF() %...>% 
      DT::datatable(filter = list(position = "top", plain = TRUE, clear = FALSE), rownames = FALSE)
  })
  
  ## SEQUENCE ENRICHMENT - DATA DOWNLOAD
  output$enrichDownload <- downloadHandler(
    
    # set filename
    filename = function(){
      "enrich.csv"
    },
    
    # set file content
    content = function(file){
      
      # format data for output as CSV file
      enrichDF() %...>% 
        {.[input[["enrichOutput_rows_all"]],]} %...>% 
        write.csv(., file, row.names = FALSE, quote = FALSE)
    }
  )
  
  ## SEQUENCE ENRICHMENT - GENERATE LOG2(ENRICHMENT) HISTOGRAM
  enrich_histogram <- reactive({
    
    if(is.null(enrichDF())){
      showNotification("Generate an enrichment table!", type = "error", duration = NULL)
      return(NULL)
    } else{
      
      # make plot
      enrichDF() %...>% 
        fa_enrich_histogram(
          enrichDF = .,
          xaxis = isolate(input$enrich_hist_xaxis),
          yaxis = isolate(input$enrich_hist_yaxis),
          plot_title = isolate(input$enrich_hist_title),
          bar_outline = isolate(input$enrich_hist_baroutline),
          bar_fill = isolate(input$enrich_hist_barfill)
        )
    }
  }) %>% 
    bindCache(
      input$enrichInput1,
      input$enrichInput2,
      input$enrichKeepNA,
      input$enrich_hist_xaxis,
      input$enrich_hist_yaxis,
      input$enrich_hist_title,
      input$enrich_hist_baroutline,
      input$enrich_hist_barfill,
      Sys.Date(),
      cache = "session"
    ) %>% 
    bindEvent(input$fcHistStart)
  
  ## SEQUENCE ENRICHMENT - RENDER LOG2(ENRICHMENT) HISTOGRAM
  output$fcHistOutput <- plotly::renderPlotly({
    
    if(is.null(enrich_histogram())){
      return(NULL)
    } else{
      
      enrich_histogram()
    }
  })
  
  ## SEQUENCE ENRICHMENT - GENERATE RPU SCATTER PLOT
  enrich_rpuScatterPlot <- reactive({
    
    if(is.null(enrichDF())){
      showNotification("Generate an enrichment table!", type = "error", duration = NULL)
      return(NULL)
    } else{
      
      # make plot
      enrichDF() %...>% 
        fa_enrich_scatter(
          enrichDF = .,
          xaxis = isolate(input$enrich_rpuScatter_xaxis),
          yaxis = isolate(input$enrich_rpuScatter_yaxis),
          plot_title = isolate(input$enrich_rpuScatter_title),
          point_colour = isolate(input$enrich_rpuScatter_pointcolour)
        )
    }
  }) %>% 
    bindCache(
      input$enrichInput1,
      input$enrichInput2,
      input$enrichKeepNA,
      input$enrich_rpuScatter_xaxis,
      input$enrich_rpuScatter_yaxis,
      input$enrich_rpuScatter_title,
      input$enrich_rpuScatter_pointcolour,
      Sys.Date(),
      cache = "session"
    ) %>% 
    bindEvent(input$rpuScatterStart)
  
  ## SEQUENCE ENRICHMENT - RENDER RPM SCATTER PLOT
  output$rpuScatterOutput <- plotly::renderPlotly({
    
    if(is.null(enrich_rpuScatterPlot())){
      return(NULL)
    } else{
      
      enrich_rpuScatterPlot()
    }
  })
  
  ## SEQUENCE ENRICHMENT - GENERATE RA PLOT
  enrich_raPlot <- reactive({
    
    if(is.null(enrichDF())){
      showNotification("Generate an enrichment table!", type = "error", duration = NULL)
      return(NULL)
    } else{
      
      # make plot
      enrichDF() %...>%
        fa_enrich_ra(
          enrichDF = .,
          xaxis = isolate(input$enrich_raScatter_xaxis),
          yaxis = isolate(input$enrich_raScatter_yaxis),
          plot_title = isolate(input$enrich_raScatter_title),
          point_colour = isolate(input$enrich_raScatter_pointcolour)
        )
    }
  }) %>% 
    bindCache(
      input$enrichInput1,
      input$enrichInput2,
      input$enrichKeepNA,
      input$enrich_raScatter_xaxis,
      input$enrich_raScatter_yaxis,
      input$enrich_raScatter_title,
      input$enrich_raScatter_pointcolour,
      Sys.Date(),
      cache = "session"
    ) %>% 
    bindEvent(input$raStart)
  
  ## SEQUENCE ENRICHMENT - RENDER RA PLOT
  output$raOutput <- plotly::renderPlotly({
    
    if(is.null(enrich_raPlot())){
      return(NULL)
    } else{
      
      enrich_raPlot()
    }
  })
}
