# Copyright (C) 2022 Skyler T. Kramer
# Full GNU GPL version 3 license found in LICENSE.txt

countServer <- function(input, output, session){
  
  ## COUNT - DATA GENERATION
  countDF <- reactive({
    
    if(check_fileInput(isolate(input$countInput))){
      showNotification("No file or link provided!", type = "error", duration = NULL)
      return(NULL)
    } else if(check_extension(fpath = isolate(input$countInput$datapath), extensions = c("fq", "fa", "fastq", "fasta", "gz"))){
      showNotification("Please supply a file with the correct extension!", type = "error", duration = NULL)
      return(NULL)
    } else{
      
      # input checks
      req(isolate(input$countButton_scalingFactor) %in% c(1e0, 1e1, 1e2, 1e3, 1e4, 1e5, 1e6))
      req(isolate(input$reverseComplement) %in% c("Yes", "No"))
      req(grepl(pattern = "^#([A-Fa-f0-9]{6}|[A-Fa-f0-9]{3})$", x = isolate(input$count_rpr_linecolour)))
      req(grepl(pattern = "^#([A-Fa-f0-9]{6}|[A-Fa-f0-9]{3})$", x = isolate(input$count_histogram_baroutline)))
      req(grepl(pattern = "^#([A-Fa-f0-9]{6}|[A-Fa-f0-9]{3})$", x = isolate(input$count_histogram_barfill)))
      req(grepl(pattern = "^#([A-Fa-f0-9]{6}|[A-Fa-f0-9]{3})$", x = isolate(input$count_abundance_baroutline)))
      req(grepl(pattern = "^#([A-Fa-f0-9]{6}|[A-Fa-f0-9]{3})$", x = isolate(input$count_abundance_barfill)))
      req(isolate(input$count_abundance_singleton) %in% c("Yes", "No"))
      # req(sum(!is.na(isolate(input$count_abundance_newBreaks) %>% gsub("\\s", "", .) %>% strsplit(., split = ",") %>% unlist() %>% as.numeric())) == 0)
      req(!grepl(pattern = "[^0-9,]", x = isolate(input$count_abundance_newBreaks)))
      
      # capture output
      withCallingHandlers({
        shinyjs::html("countTextOutput", "")
        
        # count data
        promises::future_promise({
          
          fa_count(
            dataInput = isolate(input$countInput$datapath),
            reverseComplement = ifelse(isolate(input$reverseComplement) == "Yes", TRUE, FALSE),
            scaling_factor = as.numeric(isolate(input$countButton_scalingFactor))
          )
        })
        
      },
      
      # redirect output to text in UI
      message = function(m){
        shinyjs::html(id = "countTextOutput", html = m$message, add = FALSE)
      })
    }
  }) %>% 
    bindCache(
      input$countInput,
      input$reverseComplement,
      input$countButton_scalingFactor,
      Sys.Date(),
      cache = "session"
    ) %>% 
    bindEvent(input$countStart)
  
  ## COUNT - DATA DISPLAY
  output$countOutput <- DT::renderDataTable({
    countDF() %...>% 
      DT::datatable(filter = list(position = "top", plain = TRUE, clear = FALSE), rownames = FALSE)
  })
  
  ## COUNT - METADATA
  output$countUI_seqCounts <- renderUI({
    
    if(is.null(countDF())){
      HTML(paste(""))
    } else{
      
      countDF() %...>% 
        fa_count_metadata() %...>% 
        paste(., collapse = "<br/>") %...>% 
        HTML()
    }
  })
  
  ## COUNT - DOWNLOAD
  output$countDownload <- downloadHandler(
    
    filename = function(){
      gsub(
        paste0("\\.", tools::file_ext(isolate(input$countInput$name))),
        paste0("-count\\.", tolower(isolate(input$countDownloadType))),
        isolate(input$countInput$name)
      )
    },
    
    content = function(file){
      
      if(isolate(input$countDownloadType) == "FASTA"){
        
        countDF() %...>% 
          {.[input[["countOutput_rows_all"]],]} %...>% 
          fa_formatOutput(outputData = .) %...>% 
          write.table(., file, quote = FALSE, row.names = FALSE, col.names = FALSE)
        
      } else{
        
        countDF() %...>% 
          {.[input[["countOutput_rows_all"]],]} %...>% 
          write.csv(., file, quote = FALSE, row.names = FALSE)
        
      }
    }
  )
  
  ## COUNT - READS PER RANK - PLOT
  count_rpr <- reactive({
    
    if(is.null(countDF())){
      return(NULL)
    } else{
      
      # input checks
      req(isolate(input$countSlider_minReads) %in% 0:1000)
      req(isolate(input$countSlider_maxRanks) %in% 10:1000)
      
      # generate plot
      countDF() %...>% 
        fa_count_rpr(
          countData = .,
          minReads = isolate(input$countSlider_minReads),
          maxRanks = isolate(input$countSlider_maxRanks),
          xaxis = isolate(input$count_rpr_xaxis),
          yaxis = isolate(input$count_rpr_yaxis),
          plot_title = isolate(input$count_rpr_title),
          line_color = isolate(input$count_rpr_linecolour)
        )
    }
  }) %>% 
    bindCache(
      input$countInput,
      input$reverseComplement,
      input$countButton_scalingFactor,
      input$countSlider_minReads,
      input$countSlider_maxRanks,
      input$count_rpr_xaxis,
      input$count_rpr_yaxis,
      input$count_rpr_title,
      input$count_rpr_linecolour,
      Sys.Date(),
      cache = "session"
    ) %>% 
    bindEvent(input$count_rprPlotStart)
  
  ## COUNT - READS PER RANK - RENDER
  output$count_rprPlotOutput <- plotly::renderPlotly({
    
    if(is.null(count_rpr())){
      showNotification("Please generate a data table before plotting!", type = "error", duration = NULL)
      return(NULL)
    } else{
      count_rpr()
    }
  })
  
  ## COUNT - SEQUENCE LENGTH HISTOGRAM - PLOT
  count_histogram <- reactive({
    
    if(is.null(countDF())){
      return(NULL)
    } else{
      
      # make plot
      countDF() %...>% 
        fa_count_histogram(
          countData = .,
          xaxis = isolate(input$count_histogram_xaxis),
          yaxes = c(isolate(input$count_histogram_yaxis1), isolate(input$count_histogram_yaxis1)),
          plot_title = isolate(input$count_histogram_title),
          bar_outline = isolate(input$count_histogram_baroutline),
          bar_fill = isolate(input$count_histogram_barfill)
        )
    }
  }) %>% 
    bindCache(
      input$countInput,
      input$reverseComplement,
      input$countButton_scalingFactor,
      input$count_histogram_xaxis,
      input$count_histogram_yaxis1,
      input$count_histogram_yaxis2,
      input$count_histogram_title,
      input$count_histogram_baroutline,
      input$count_histogram_barfill,
      Sys.Date(),
      cache = "session"
    ) %>% 
    bindEvent(input$count_seqHistStart)
  
  ## COUNT - SEQUENCE LENGTH HISTOGRAM - RENDER
  output$count_seqHistOutput <- plotly::renderPlotly({
    
    if(is.null(count_histogram())){
      showNotification("Please generate a data table before plotting!", type = "error", duration = NULL)
      return(NULL)
    } else{
      count_histogram()
    }
  })
  
  ## COUNT - BINNED ABUNDANCE PLOT - PLOT
  count_abundance <- reactive({
    if(is.null(countDF())){
      return(NULL)
    } else if(
      isolate(input$count_abundance_singleton) == "Yes" & check_regexInputs(pattern = "[^0-9,]", separator = "\\s", userInputs = isolate(input$count_abundance_newBreaks))
    ){
      showNotification("New break points must be numeric and in comma-separated list!")
      return(NULL)
    } else{
      
      # make plot
      countDF() %...>%
        fa_count_binnedAbundance(
          countData = .,
          useSingleton = ifelse(isolate(input$count_abundance_singleton) == "Yes", TRUE, FALSE),
          breaks = isolate(input$count_abundance_newBreaks) %>% gsub("\\s", "", .) %>% strsplit(., split = ",") %>% unlist() %>% as.numeric(),
          xaxis = isolate(input$count_abundance_xaxis),
          yaxis = isolate(input$count_abundance_yaxis),
          plot_title = isolate(input$count_abundance_title),
          bar_outline = isolate(input$count_abundance_baroutline),
          bar_fill = isolate(input$count_abundance_barfill)
        )
    }
  }) %>% 
    bindCache(
      input$countInput,
      input$reverseComplement,
      input$countButton_scalingFactor,
      input$count_abundance_singleton,
      input$count_abundance_newBreaks,
      input$count_abundance_xaxis,
      input$count_abundance_yaxis,
      input$count_abundance_title,
      input$count_abundance_baroutline,
      input$count_abundance_barfill,
      Sys.Date(),
      cache = "session"
    ) %>% 
    bindEvent(input$count_abPlotStart)
  
  ## COUNT - BINNED ABUNDANCE PLOT - RENDER
  output$count_abPlotOutput <- plotly::renderPlotly({
    
    if(is.null(count_abundance())){
      showNotification("Please generate a data table before plotting!", type = "error", duration = NULL)
      return(NULL)
    } else{
      count_abundance()
    }
  })
}
