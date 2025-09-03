# Copyright (C) 2022 Skyler T. Kramer
# Full GNU GPL version 3 license found in LICENSE.txt

recountServer <- function(input, output, session){
  
  ## COUNT - DATA GENERATION
  recountDF <- reactive({
    
    if(check_fileInput(isolate(input$recountInput1)) | check_fileInput(isolate(input$recountInput2))){
      showNotification("Please upload two files!", type = "error", duration = NULL)
      return(NULL)
    } else if(check_extension(fpath = isolate(input$recountInput1$datapath), extensions = "fasta")){
      showNotification("File 1 does not have the correct extension!", type = "error", duration = NULL)
      return(NULL)
    } else if(check_extension(fpath = isolate(input$recountInput2$datapath), extensions = "fasta")){
      showNotification("File 2 does not have the correct extension!", type = "error", duration = NULL)
      return(NULL)
    } else{
      
      # input checks
      req(isolate(input$recountSlider_scalingFactor) %in% c(1e0, 1e1, 1e2, 1e3, 1e4, 1e5, 1e6))
      req(isolate(input$recountSlider_minReads) %in% 0:1000)
      req(isolate(input$recountSlider_maxRanks) %in% 10:1000)
      req(grepl(pattern = "^#([A-Fa-f0-9]{6}|[A-Fa-f0-9]{3})$", x = isolate(input$recount_rpr_linecolour)))
      req(grepl(pattern = "^#([A-Fa-f0-9]{6}|[A-Fa-f0-9]{3})$", x = isolate(input$recount_histogram_baroutline)))
      req(grepl(pattern = "^#([A-Fa-f0-9]{6}|[A-Fa-f0-9]{3})$", x = isolate(input$recount_histogram_barfill)))
      
      # recount
      promises::future_promise({
        
        # read and format files for recounting
        fa2DF1 <- fa_formatInput(fastaInput = isolate(input$recountInput1$datapath))
        fa2DF2 <- fa_formatInput(fastaInput = isolate(input$recountInput2$datapath))
        
        # recount data
        fa_recount(
          fa2DF1 = fa2DF1,
          fa2DF2 = fa2DF2,
          scaling_factor = as.numeric(isolate(input$recountSlider_scalingFactor))
        )
      })
    }
  }) %>% 
    bindCache(
      input$recountInput1,
      input$recountInput2,
      input$recountSlider_scalingFactor,
      Sys.Date(),
      cache = "session"
    ) %>% 
    bindEvent(input$recountStart)
  
  ## COUNT - DATA DISPLAY
  output$recountOutput <- DT::renderDataTable({
    
    recountDF() %...>% 
      DT::datatable(filter = list(position = "top", plain = TRUE, clear = FALSE), rownames = FALSE)
  })
  
  ## COUNT - DOWNLOAD
  output$recountDownload <- downloadHandler(
    
    filename = function(){
      paste0("recount\\.", tolower(isolate(input$recountDownloadType)))
    },
    
    content = function(file){
      
      # check if FASTA or CSV
      if(isolate(input$recountDownloadType) == "FASTA"){
        
        recountDF() %...>% 
          {.[input[["recountOutput_rows_all"]],]} %...>% 
          fa_formatOutput(outputData = .) %...>% 
          write.table(., file, quote = FALSE, row.names = FALSE, col.names = FALSE)
        
      } else{
        
        recountDF() %...>% 
          {.[input[["recountOutput_rows_all"]],]} %...>% 
          write.csv(., file, quote = FALSE, row.names = FALSE)
      }
    }
  )
  
  ## COUNT - READS PER RANK - PLOT
  recount_rpr <- reactive({
    
    if(is.null(recountDF())){
      return(NULL)
    } else{
      
      # make plot
      recountDF() %...>% 
        fa_count_rpr(
          countData = .,
          minReads = isolate(input$recountSlider_minReads),
          maxRanks = isolate(input$recountSlider_maxRanks),
          xaxis = isolate(input$recount_rpr_xaxis),
          yaxis = isolate(input$recount_rpr_yaxis),
          plot_title = isolate(input$recount_rpr_title),
          line_color = isolate(input$recount_rpr_linecolour)
        )
      
    }
  }) %>% 
    bindCache(
      input$recountInput1,
      input$recountInput2,
      input$recountSlider_scalingFactor,
      input$recountSlider_minReads,
      input$recountSlider_maxRanks,
      input$recount_rpr_xaxis,
      input$recount_rpr_yaxis,
      input$recount_rpr_title,
      input$recount_rpr_linecolour,
      Sys.Date(),
      cache = "session"
    ) %>% 
    bindEvent(input$recount_rprPlotStart)
  
  ## COUNT - READS PER RANK - RENDER
  output$recount_rprPlotOutput <- plotly::renderPlotly({
    
    if(is.null(recount_rpr())){
      showNotification("Please generate a data table before plotting!", type = "error", duration = NULL)
      return(NULL)
    } else{
      recount_rpr()
    }
  })
  
  ## COUNT - SEQUENCE LENGTH HISTOGRAM - PLOT
  recount_histogram <- reactive({
    
    if(is.null(recountDF())){
      return(NULL)
    } else{
      
      # make plot
      recountDF() %...>% 
        fa_count_histogram(
          countData = .,
          xaxis = isolate(input$recount_histogram_xaxis),
          yaxes = c(isolate(input$recount_histogram_yaxis1), isolate(input$recount_histogram_yaxis1)),
          plot_title = isolate(input$recount_histogram_title),
          bar_outline = isolate(input$recount_histogram_baroutline),
          bar_fill = isolate(input$recount_histogram_barfill)
        )
    }
  }) %>% 
    bindCache(
      input$recountInput1,
      input$recountInput2,
      input$recountSlider_scalingFactor,
      input$recount_histogram_xaxis,
      input$recount_histogram_yaxis1,
      input$recount_histogram_yaxis2,
      input$recount_histogram_title,
      input$recount_histogram_baroutline,
      input$recount_histogram_barfill,
      Sys.Date(),
      cache = "session"
    ) %>% 
    bindEvent(input$recount_seqHistStart)
  
  ## COUNT - SEQUENCE LENGTH HISTOGRAM - RENDER
  output$recount_seqHistOutput <- plotly::renderPlotly({
    
    if(is.null(recount_histogram())){
      showNotification("Please generate a data table before plotting!", type = "error", duration = NULL)
      return(NULL)
    } else{
      recount_histogram()
    }
  })
}
