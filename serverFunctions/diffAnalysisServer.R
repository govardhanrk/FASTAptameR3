# Copyright (C) 2022 Skyler T. Kramer
# Full GNU GPL version 3 license found in LICENSE.txt

diffAnalysisServer <- function(input, output, session){
  
  ## DIFFERENTIAL ANALYSIS - DATA GENERATION
  diffAnalysisDF <- reactive({
    
    if(check_fileInput(finput = isolate(input$diffAnalysis_input1)) | check_fileInput(finput = isolate(input$diffAnalysis_input2))){
      showNotification("No files provided!", type = "error", duration = NULL)
      return(NULL)
    } else if(check_numFiles(finput = isolate(input$diffAnalysis_input1), isExact = FALSE, numFiles = 2)){
      showNotification("Please supply at least 2 files for the first condition!", type = "error", duration = NULL)
      return(NULL)
    } else if(check_numFiles(finput = isolate(input$diffAnalysis_input2), isExact = FALSE, numFiles = 2)){
      showNotification("Please supply at least 2 files for the second condition!", type = "error", duration = NULL)
      return(NULL)
    } else if(check_extensions(finput = isolate(input$diffAnalysis_input1), extensions = "fasta")){
      showNotification("Please confirm that all files have the correct extension!", type = "error", duration = NULL)
      return(NULL)
    } else if(check_extensions(finput = isolate(input$diffAnalysis_input2), extensions = "fasta")){
      showNotification("Please confirm that all files have the correct extension!", type = "error", duration = NULL)
      return(NULL)
    } else{
      
      # input checks
      req(isolate(input$diffAnalysis_pval) >= 0.01 & isolate(input$diffAnalysis_pval) <= 0.5)
      req(grepl(pattern = "^#([A-Fa-f0-9]{6}|[A-Fa-f0-9]{3})$", x = isolate(input$edgeR_plot_sigcolour)))
      req(grepl(pattern = "^#([A-Fa-f0-9]{6}|[A-Fa-f0-9]{3})$", x = isolate(input$edgeR_plot_insigcolour)))
      
      # differential analysis
      promises::future_promise({
        
        # read and format input files
        fa2DF_cond1 <- purrr::map(.x = isolate(input$diffAnalysis_input1$datapath), .f = fa_formatInput)
        fa2DF_cond2 <- purrr::map(.x = isolate(input$diffAnalysis_input2$datapath), .f = fa_formatInput)
        
        # paired differential analysis
        fa_edgeR_pairTest(
          fa2DF_cond1 = fa2DF_cond1,
          fa2DF_cond2 = fa2DF_cond2,
          pcutoff = isolate(input$diffAnalysis_pval)
        )
      })
    }
  }) %>% 
    bindCache(
      input$diffAnalysis_input1,
      input$diffAnalysis_input2,
      input$diffAnalysis_pval,
      Sys.Date(),
      cache = "session"
    ) %>% 
    bindEvent(input$diffAnalysis_start)
  
  ## DIFFERENTIAL ANALYSIS - DATA OUTPUT
  output$diffAnalysis_output <- DT::renderDataTable({
    
    diffAnalysisDF() %...>% 
      DT::datatable(filter = list(position = "top", plain = TRUE, clear = FALSE), rownames = FALSE)
  })
  
  ## DIFFERENTIAL ANALYSIS - DATA DOWNLOAD
  output$diffAnalysis_download <- downloadHandler(
    
    # set filename
    filename = function(){
      "diffAnalysis.csv"
    },
    
    # set file content
    content = function(file){
      
      # format data for output as CSV file
      diffAnalysisDF() %...>% 
        {.[input[["diffAnalysis_output_rows_all"]],]} %...>% 
        write.csv(., file, row.names = FALSE, quote = FALSE)
    }
  )
  
  ## DIFFERENTIAL ANALYSIS - GENERATE SCATTER PLOT
  diffAnalysis_scatterPlot <- reactive({
    
    if(is.null(diffAnalysisDF())){
      return(NULL)
    } else{
      
      # make plot
      diffAnalysisDF() %...>% 
        fa_edgeR_pairTestViz(
          edgeR_table = .,
          xaxis = isolate(input$edgeR_plot_xaxis),
          yaxis = isolate(input$edgeR_plot_yaxis),
          plot_title = isolate(input$edgeR_plot_title),
          point_colours = c(isolate(input$edgeR_plot_sigcolour), isolate(input$edgeR_plot_insigcolour))
        )
    }
  }) %>% 
    bindCache(
      input$diffAnalysis_input1,
      input$diffAnalysis_input2,
      input$diffAnalysis_pval,
      input$edgeR_plot_xaxis,
      input$edgeR_plot_yaxis,
      input$edgeR_plot_title,
      input$edgeR_plot_sigcolour,
      input$edgeR_plot_insigcolour,
      Sys.Date(),
      cache = "session"
    ) %>% 
    bindEvent(input$edgeR_plotStart)
  
  ## DIFFERENTIAL ANALYSIS - RENDER SCATTER PLOT OF LOG(CPM) VS LOG(FC)
  output$diffAnalysis_plot <- plotly::renderPlotly({
    
    if(is.null(diffAnalysis_scatterPlot())){
      return(NULL)
    } else{
      
      diffAnalysis_scatterPlot()
    }
  })
}
