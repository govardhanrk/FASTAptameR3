# Copyright (C) 2022 Skyler T. Kramer
# Full GNU GPL version 3 license found in LICENSE.txt

clusterDiversityTab <- tabPanel(
  "Diversity",
  
  sidebarLayout(
    sidebarPanel(
      
      # ask for input file
      fileInput(
        "clusterDiversityInput",
        label = strong("Input data:"),
        multiple = FALSE,
        placeholder = "Clustered FASTA file",
        accept = c('.fasta')
      ),
      
      # start button
      actionButton("clusterDiversityStart", label = h5("Start"), style='padding:11px; font-size:80%'),
      
      # download button
      downloadButton("clusterDiversityDownload", label = h5("Download"), style='padding:2px; font-size:80%'),
      
      # horizontal line
      tags$hr(style="border-color: black;"),
      
      # plot customizations for metaplots
      radioButtons(
        "clusterDiverstiy_metaplot_custom",
        label = strong("Adjust default plots of cluster metadata?"),
        choices = c("Yes","No"), selected = "No",
        inline = TRUE
      ),
      shinyBS::bsTooltip("clusterDiverstiy_metaplot_custom", "Customize plot components?"),
      
      conditionalPanel(
        condition = "input.clusterDiverstiy_metaplot_custom == 'Yes'",
        
        # text input for x-axis
        textAreaInput("clusterDiverstiy_metaplot_xaxis", label = strong("X-axis - represents clusters."), value = "Cluster"),
        
        # text and colour inputs for 3x y-axes
        textAreaInput("clusterDiverstiy_metaplot_yaxis1", label = strong("Y-axis 1 - represents unique sequence count."), value = "Seq. count"),
        colourpicker::colourInput("clusterDiverstiy_metaplot_linecolour1", "Color for unique sequence count.", "blue"),
        
        textAreaInput("clusterDiverstiy_metaplot_yaxis2", label = strong("Y-axis 2 - represents total sequence count."), value = "Read count"),
        colourpicker::colourInput("clusterDiverstiy_metaplot_linecolour2", "Color for total sequence count.", "orange"),
        
        textAreaInput("clusterDiverstiy_metaplot_yaxis3", label = strong("Y-axis 3 - represents average LED."), value = "Avg. LED"),
        colourpicker::colourInput("clusterDiverstiy_metaplot_linecolour3", "Color for average LED.", "forestgreen"),
        
        # text input for plot title
        textAreaInput("clusterDiverstiy_metaplot_title", label = strong("Plot title."), value = "Cluster metaplots")
      ),
      
      # start button
      actionButton("clusterDiversity_metaplotStart", label = h5("Cluster metaplots"), style='padding:11px; font-size:80%'),
      shinyBS::bsTooltip("clusterDiversity_metaplotStart", "Plots of unique-/total-sequence count and average LED per cluster"),
      
      tags$head(tags$style("#clusterDiversity_metaplotWindow .modal-body{ min-height:650px}")),
      shinyBS::bsModal(
        id = "clusterDiversity_metaplotWindow",
        title = "Cluster diversity metaplots",
        trigger = "clusterDiversity_metaplotStart",
        size = "large",
        shinycssloaders::withSpinner(plotly::plotlyOutput("clusterDiverstiyMetaplotOutput", height = "600px"))
      ),
      
      # horizontal line
      tags$hr(style="border-color: black;"),
      
      # select value of k for kmer plot
      radioButtons("kmerButton_k", label = strong("k-mer size for plot:"), choices = 3:5, selected = 3, inline = TRUE),
      shinyBS::bsTooltip("kmerButton_k", "Should the plot use based on 3-, 4-, or 5-mers?"), 
      
      # slider for top number of clusters to plot
      selectizeInput("kmer_clusters", label = strong("Plot which clusters?"), choices = "*", multiple = TRUE), 
      shinyBS::bsTooltip("kmer_clusters", "Which clusters should be included in the plot?"),
      
      # select how kmers should be represented
      radioButtons(
        "kmerButton_plotType", label = strong("Plot type:"), choices = c("PCA", "UMAP"), selected = "UMAP", inline = TRUE
      ),
      shinyBS::bsTooltip("kmerButton_plotType", "Should plot coordinates come from PCA or UMAP?"),
      
      # plot customizations for kmer plot
      radioButtons(
        "clusterDiverstiy_kmerplot_custom",
        label = strong("Adjust default k-mer plot?"),
        choices = c("Yes","No"), selected = "No",
        inline = TRUE
      ),
      shinyBS::bsTooltip("clusterDiverstiy_kmerplot_custom", "Customize plot components?"),
      
      conditionalPanel(
        condition = "input.clusterDiverstiy_kmerplot_custom == 'Yes'",
        
        # text input for x-axis, y-axis, legend title, and plot title
        textAreaInput("clusterDiverstiy_kmerplot_xaxis", label = strong("X-axis - 1st dim. of PCA or UMAP."), value = "Dim1"),
        textAreaInput("clusterDiverstiy_kmerplot_yaxis", label = strong("Y-axis - 2nd dim. of PCA or UMAP."), value = "Dim2"),
        textAreaInput("clusterDiverstiy_kmerplot_legend", label = strong("Legend title - represents clusters."), value = "Cluster"),
        textAreaInput("clusterDiverstiy_kmerplot_title", label = strong("Plot title."), value = "Cluster k-mer plot"),
        
        # select input for colour palette
        selectInput(
          "clusterDiverstiy_kmerplot_palette",
          label = strong("Color palette."),
          choices = c("Accent", "Dark2", "Paired", "Pastel1", "Pastel2", "Set1", "Set2"),
          selected = "Dark2",
          multiple = FALSE
        )
      ),
      
      # start button
      actionButton("kmerStart", label = h5("k-mer plot"), style='padding:11px; font-size:80%'),
      shinyBS::bsTooltip("kmerStart", "Plot dimensionally reduced k-mer matrix, colored by cluster"),
      h5("*Characters outside of [A,C,G,T,U] converted to 'X'."),
      
      # window for kmer plot
      shinyBS::bsModal(
        id = "kmerWindow",
        title = "k-mer Plot",
        trigger = "kmerStart",
        size = "large",
        shinycssloaders::withSpinner(plotly::plotlyOutput("kmerOutput"))
      )
    ),
    
    # display count output as data.table
    mainPanel(shinycssloaders::withSpinner(DT::dataTableOutput("clusterDiversityOutput")))
  )
)

