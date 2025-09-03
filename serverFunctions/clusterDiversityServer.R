# Copyright (C) 2022 Skyler T. Kramer
# Full GNU GPL version 3 license found in LICENSE.txt

clusterDiversityServer <- function(input, output, session){
  
  ## CLUSTER DIVERSITY - DATA GENERATION
  clusterDiversityDF <- reactive({
    
    if(check_fileInput(finput = isolate(input$clusterDiversityInput))){
      showNotification("No file provided!", type = "error", duration = NULL)
      return(NULL)
    } else if(check_extension(fpath = isolate(input$clusterDiversityInput$datapath), extensions = "fasta")){
      showNotification("Please supply a file with the correct extension!", type = "error", duration = NULL)
      return(NULL)
    } else if(check_clusterInID(fpath = isolate(input$clusterDiversityInput$datapath))){
      showNotification("Please provided a file from FASTAptameR-Cluster!", type = "error", duration = NULL)
      return(NULL)
    } else{
      
      # input checks
      req(grepl(pattern = "^#([A-Fa-f0-9]{6}|[A-Fa-f0-9]{3})$", x = isolate(input$clusterDiverstiy_metaplot_linecolour1)))
      req(grepl(pattern = "^#([A-Fa-f0-9]{6}|[A-Fa-f0-9]{3})$", x = isolate(input$clusterDiverstiy_metaplot_linecolour2)))
      req(grepl(pattern = "^#([A-Fa-f0-9]{6}|[A-Fa-f0-9]{3})$", x = isolate(input$clusterDiverstiy_metaplot_linecolour3)))
      req(isolate(input$kmerButton_k) %in% 3:5)
      req(length(isolate(input$kmer_clusters)) <= 8)
      req(isolate(input$kmerButton_plotType) %in% c("PCA", "UMAP"))
      req(isolate(input$clusterDiverstiy_kmerplot_palette) %in% c("Accent", "Dark2", "Paired", "Pastel1", "Pastel2", "Set1", "Set2"))
      
      # diversity per cluster
      promises::future_promise({
        
        fa_formatInput(fastaInput = isolate(input$clusterDiversityInput$datapath)) %>% 
          fa_clusterDiversity(fa2DF_cluster = .)
      })
    }
  }) %>% 
    bindCache(
      input$clusterDiversityInput,
      Sys.Date(),
      cache = "session"
    ) %>% 
    bindEvent(input$clusterDiversityStart)
  
  ## CLUSTER DIVERSITY - DATA OUTPUT
  output$clusterDiversityOutput <- DT::renderDataTable({
    clusterDiversityDF() %...>%
      DT::datatable(filter = list(position = "top", plain = TRUE, clear = FALSE), rownames = FALSE)
  })
  
  ## CLUSTER DIVERSITY - DATA DOWNLOAD
  output$clusterDiversityDownload <- downloadHandler(
    
    # set filename
    filename = function(){
      "clusterDiversity.csv"
    },
    
    # set file content
    content = function(file){
      
      # format data for output as CSV file
      clusterDiversityDF() %...>%
        {.[input[["clusterDiversityOutput_rows_all"]],]} %...>%
        write.csv(., file, row.names = FALSE, quote = FALSE)
    }
  )
  
  ## CLUSTER DIVERSITY - METAPLOTS - PLOT
  clusterDiversityMetaplot <- reactive({
    
    if(is.null(clusterDiversityDF())){
      showNotification("Please generate a data table before plotting!", type = "error", duration = NULL)
      return(NULL)
    } else{
      
      clusterDiversityDF() %...>% 
        fa_clusterDiversity_metaplot(
          diversityDF = .,
          xaxis = isolate(input$clusterDiverstiy_metaplot_xaxis),
          yaxes = c(
            isolate(input$clusterDiverstiy_metaplot_yaxis1),
            isolate(input$clusterDiverstiy_metaplot_yaxis2),
            isolate(input$clusterDiverstiy_metaplot_yaxis3)
          ),
          plot_title = isolate(input$clusterDiverstiy_metaplot_title),
          line_colours = c(
            isolate(input$clusterDiverstiy_metaplot_linecolour1),
            isolate(input$clusterDiverstiy_metaplot_linecolour2),
            isolate(input$clusterDiverstiy_metaplot_linecolour3)
          )
        )
    }
  }) %>% 
    bindCache(
      input$clusterDiversityInput,
      input$clusterDiverstiy_metaplot_xaxis,
      input$clusterDiverstiy_metaplot_yaxis1,
      input$clusterDiverstiy_metaplot_yaxis2,
      input$clusterDiverstiy_metaplot_yaxis3,
      input$clusterDiverstiy_metaplot_title,
      input$clusterDiverstiy_metaplot_linecolour1,
      input$clusterDiverstiy_metaplot_linecolour2,
      input$clusterDiverstiy_metaplot_linecolour3,
      Sys.Date(),
      cache = "session"
    ) %>% 
    bindEvent(input$clusterDiversity_metaplotStart)
  
  ## CLUSTER DIVERSITY - METADATA - RENDER
  output$clusterDiverstiyMetaplotOutput <- plotly::renderPlotly({
    
    if(is.null(clusterDiversityMetaplot())){
      return(NULL)
    } else{
      
      clusterDiversityMetaplot()
    }
  })
  
  ## CLUSTER DIVERSITY - UPDATE CLUSTER SELECTIONS
  observe({
    
    clusterDiversityDF() %...>%
      dplyr::pull(Cluster) %...>% 
      updateSelectizeInput(session = session, inputId = "kmer_clusters", choices = .)
  })
  
  ## CLUSTER DIVERSITY - K-MER PCA - PLOT
  kmerPlot <- reactive({
    
    if(check_fileInput(finput = isolate(input$clusterDiversityInput))){
      return(NULL)
    } else if(check_clusterInID(fpath = isolate(input$clusterDiversityInput$datapath))){
      showNotification("Please provide a file from FASTAptameR-Cluster!", type = "error", duration = NULL)
      return(NULL)
    } else if(check_kmerClusters(clusters = isolate(input$kmer_clusters))){
      showNotification("Please select 1-8 clusters to visualize!", type = "error", duration = NULL)
      return(NULL)
    } else if(is.null(clusterDiversityDF())){
      showNotification("Please generate a table before generating this plot!", type = "error", duration = NULL)
      return(NULL)
    } else{
      
      # run kmer analysis
      promises::future_promise(
        {
          fa_formatInput(fastaInput = isolate(input$clusterDiversityInput$datapath)) %>% 
            fa_clusterDiversity_kmer(
              fa2DF_cluster = .,
              kmerSize = as.numeric(isolate(input$kmerButton_k)),
              clustersToPlot = isolate(input$kmer_clusters),
              plotType = isolate(input$kmerButton_plotType),
              xaxis = isolate(input$clusterDiverstiy_kmerplot_xaxis),
              yaxis = isolate(input$clusterDiverstiy_kmerplot_yaxis),
              legend_title = isolate(input$clusterDiverstiy_kmerplot_legend),
              plot_title = isolate(input$clusterDiverstiy_kmerplot_title),
              colour_palette_disc = isolate(input$clusterDiverstiy_kmerplot_palette)
            )
        }, seed = TRUE
      )
    }
  }) %>%
    bindCache(
      input$clusterDiversityInput,
      input$kmerButton_k,
      input$kmer_clusters,
      input$kmerButton_plotType,
      input$clusterDiverstiy_kmerplot_xaxis,
      input$clusterDiverstiy_kmerplot_yaxis,
      input$clusterDiverstiy_kmerplot_legend,
      input$clusterDiverstiy_kmerplot_title,
      input$clusterDiverstiy_kmerplot_palette,
      Sys.Date(),
      cache = "session"
    ) %>% 
    bindEvent(input$kmerStart)
  
  ## CLUSTER DIVERSITY - K-MER PCA - RENDER
  output$kmerOutput <- plotly::renderPlotly({
    kmerPlot()
  })
}
