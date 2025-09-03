# Copyright (C) 2022 Skyler T. Kramer
# Full GNU GPL version 3 license found in LICENSE.txt

translateServer <- function(input, output, session){
  
  ## TRANSLATE - DATA GENERATION
  translateDF <- reactive({
    
    if(check_fileInput(finput = isolate(input$translateInput))){
      showNotification("No file provided!", type = "error", duration = NULL)
      return(NULL)
    } else if(check_extension(fpath = isolate(input$translateInput$datapath), extensions = "fasta")){
      showNotification("Please supply a file with the correct extension!", type = "error", duration = NULL)
      return(NULL)
    } else if(check_regexInputs(pattern = "[^a-zA-Z0-9,]", separator = "\\s", userInputs = isolate(input$translateInput_changes_codons))){
      showNotification("Modifications must be alphanumeric!", type = "error", duration = NULL)
      return(NULL)
    } else if(check_regexInputs(pattern = "[^a-zA-Z0-9,]", separator = "\\s", userInputs = isolate(input$translateInput_changes_outputs))){
      showNotification("Modifications must be alphanumeric!", type = "error", duration = NULL)
      return(NULL)
    } else if(check_translate_mods(codons = isolate(input$translateInput_changes_codons), translations = isolate(input$translateInput_changes_outputs))){
      showNotification("The number of modified codons and outputs must be equal!", type = "error", duration = NULL)
      return(NULL)
    } else{
      
      # input checks
      req(isolate(input$orfButton) %in% 1:3)
      req(isolate(input$convergeButton) %in% c("Yes","No"))
      req(
        isolate(input$translateSelection) %in% c(
           "Standard",
           "Vertebrate mitochondrial",
           "Yeast mitochondrial",
           "Mold, protozoan, and coelenterate mitochondrial + Mycoplasma / Spiroplasma",
           "Invertebrate mitochondrial",
           "Ciliate, dasycladacean and Hexamita nuclear",
           "Echinoderm and flatworm mitochondrial",
           "Euplotid nuclear",
           "Alternative yeast nuclear",
           "Ascidian mitochondrial",
           "Alternative flatworm mitochondrial",
           "Blepharisma nuclear",
           "Chlorophycean mitochondrial",
           "Trematode mitochondrial",
           "Scenedesmus obliquus mitochondrial",
           "Pterobranchia mitochondrial"
         )
      )
      req(isolate(input$translateDownloadType) %in% c("FASTA", "CSV"))
      req(isolate(input$translateSlider_minReads) %in% 0:1000)
      req(isolate(input$translateSlider_maxRanks) %in% 10:1000)
      req(grepl(pattern = "^#([A-Fa-f0-9]{6}|[A-Fa-f0-9]{3})$", x = isolate(input$translate_rpr_linecolour)))
      req(grepl(pattern = "^#([A-Fa-f0-9]{6}|[A-Fa-f0-9]{3})$", x = isolate(input$translate_histogram_baroutline)))
      req(grepl(pattern = "^#([A-Fa-f0-9]{6}|[A-Fa-f0-9]{3})$", x = isolate(input$translate_histogram_barfill)))
      
      # create data.frame of changes to translations
      inputChanges_df <- data.frame(
        Codon = isolate(input$translateInput_changes_codons) %>% gsub("\\s", "", .) %>% strsplit(split = ",") %>% unlist(),
        Translation = isolate(input$translateInput_changes_outputs) %>% gsub("\\s", "", .) %>% strsplit(split = ",") %>% unlist()
      )
      
      # translate
      promises::future_promise({
        
        fa_formatInput(fastaInput = isolate(input$translateInput$datapath)) %>% 
          fa_translate(
            fa2DF = .,
            orf = isolate(input$orfButton),
            converge = ifelse(isolate(input$convergeButton) == "Yes", TRUE, FALSE),
            inputChanges = inputChanges_df,
            translateSelection = isolate(input$translateSelection)
          )
      })
    }
  }) %>% 
    bindCache(
      input$translateInput,
      input$orfButton,
      input$convergeButton,
      input$translateInput_changes_codons,
      input$translateInput_changes_outputs,
      input$translateSelection,
      Sys.Date(),
      cache = "session"
    ) %>% 
    bindEvent(input$translateStart)
  
  ## TRANSLATE - DATA DISPLAY
  output$translateOutput <- DT::renderDataTable({
    
    translateDF() %...>% 
      DT::datatable(filter = list(position = "top", plain = TRUE, clear = FALSE), rownames = FALSE)
  })
  
  ## TRANSLATE - DATA DOWNLOAD
  output$translateDownload <- downloadHandler(
    
    # set filename
    filename = function(){
      
      gsub(
        paste0("\\.", tools::file_ext(isolate(input$translateInput$name))),
        paste0("-translate\\.", tolower(isolate(input$translateDownloadType))),
        isolate(input$translateInput$name)
      )
    },
    
    # set file content
    content = function(file){
      
      if(isolate(input$translateDownloadType) == "FASTA"){
        
        translateDF() %...>% 
          {.[input[["translateOutput_rows_all"]],]} %...>% 
          fa_formatOutput(outputData = .) %...>% 
          write.table(., file, quote = FALSE, row.names = FALSE, col.names = FALSE)
        
      } else{
        
        translateDF() %...>% 
          {.[input[["translateOutput_rows_all"]],]} %...>% 
          write.csv(., file, quote = FALSE, row.names = FALSE)
        
      }
    }
  )
  
  ## TRANSLATE - READS PER RANK - PLOT
  translate_rpr <- reactive({

    if(is.null(translateDF())){
      return(NULL)
    } else{

      # make plot
      translateDF() %...>% 
        fa_count_rpr(
          countData = .,
          minReads = isolate(input$translateSlider_minReads),
          maxRanks = isolate(input$translateSlider_maxRanks),
          xaxis = isolate(input$translate_rpr_xaxis),
          yaxis = isolate(input$translate_rpr_yaxis),
          plot_title = isolate(input$translate_rpr_title),
          line_color = isolate(input$translate_rpr_linecolour)
        )
    }
  }) %>%
    bindCache(
      input$translateInput,
      input$orfButton,
      input$convergeButton,
      input$translateInput_changes,
      input$translateSelection,
      input$translateSlider_minReads,
      input$translateSlider_maxRanks,
      input$translate_rpr_xaxis,
      input$translate_rpr_yaxis,
      input$translate_rpr_title,
      input$translate_rpr_linecolour,
      Sys.Date(),
      cache = "session"
    ) %>%
    bindEvent(input$translate_rprPlotStart)

  ## TRANSLATE - READS PER RANK - RENDER
  output$translate_rprPlotOutput <- plotly::renderPlotly({

    if(is.null(translate_rpr())){
      showNotification("Please generate a data table before plotting!", type = "error", duration = NULL)
      return(NULL)
    } else{

      translate_rpr()
    }
  })
  
  ## TRANSLATE - SEQUENCE LENGTH HISTOGRAM - PLOT
  translate_hist <- reactive({

    if(is.null(translateDF())){
      return(NULL)
    } else{
      
      # make plot
      translateDF() %...>% 
        fa_count_histogram(
          countData = .,
          xaxis = isolate(input$translate_histogram_xaxis),
          yaxes = c(isolate(input$translate_histogram_yaxis1), isolate(input$translate_histogram_yaxis2)),
          plot_title = isolate(input$translate_histogram_title),
          bar_outline = isolate(input$translate_histogram_baroutline),
          bar_fill = isolate(input$translate_histogram_barfill)
        )
    }
  }) %>%
    bindCache(
      input$translateInput,
      input$orfButton,
      input$convergeButton,
      input$translateInput_changes,
      input$translateSelection,
      input$translate_histogram_xaxis,
      input$translate_histogram_yaxis1,
      input$translate_histogram_yaxis2,
      input$translate_histogram_title,
      input$translate_histogram_baroutline,
      input$translate_histogram_barfill,
      Sys.Date(),
      cache = "session"
    ) %>%
    bindEvent(input$translate_seqHistStart)

  ## TRANSLATE - SEQUENCE LENGTH HISTOGRAM - RENDER
  output$translate_seqHistOutput <- plotly::renderPlotly({

    if(is.null(translate_hist())){
      showNotification("Please generate a data table before plotting!", type = "error", duration = NULL)
      return(NULL)
    } else{

      translate_hist()
    }
  })
}
