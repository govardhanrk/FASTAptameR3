# Copyright (C) 2022 Skyler T. Kramer
# Full GNU GPL version 3 license found in LICENSE.txt

reclusterTab <- tabPanel(
  "Recluster",
  
  sidebarLayout(
    sidebarPanel(
      
      # ask for first input file
      fileInput(
        "reclusterInput1",
        label = strong("Cluster data 1:"),
        multiple = FALSE,
        placeholder = "FASTA file",
        accept = c(".fasta")
      ),
      
      # ask for second input file
      fileInput(
        "reclusterInput2",
        label = strong("Cluster data 2:"),
        multiple = FALSE,
        placeholder = "FASTA file",
        accept = c(".fasta")
      ),
      
      # plot customizations for heat map
      radioButtons(
        "recluster_heatmap_custom",
        label = strong("Adjust default heat map?"),
        choices = c("Yes","No"), selected = "No",
        inline = TRUE
      ),
      shinyBS::bsTooltip("recluster_heatmap_custom", "Customize plot components?"),
      
      conditionalPanel(
        condition = "input.recluster_heatmap_custom == 'Yes'",
        
        # text input for x-axis, y-axis, legend title, and plot title
        textAreaInput("recluster_heatmap_xaxis", label = strong("X-axis - clusters of 1st population."), value = "Population 1 clusters"),
        textAreaInput("recluster_heatmap_yaxis", label = strong("Y-axis - clusters of 2nd population."), value = "Population 2 clusters"),
        textAreaInput("recluster_heatmap_legend", label = strong("Legend title - represents LED."), value = "LED"),
        textAreaInput("recluster_heatmap_title", label = strong("Plot title."), value = "LED between cluster seeds"),
        
        # fill input for tiles
        selectInput(
          "recluster_heatmap_palette",
          label = strong("Fill palette."),
          choices = c("magma", "inferno", "plasma", "viridis", "cividis", "rocket", "mako", "turbo"),
          selected = "magma",
          multiple = FALSE
        )
      ),
      
      # start button - get LED heat map with cluster seeds
      actionButton("reclusterStart_heatmap", label = h5("LED heatmap"), style='padding:11px; font-size:80%'),
      shinyBS::bsTooltip("reclusterStart_heatmap", "Generate a heatmap showing the LED between cluster seeds."),
      
      # horizontal line
      tags$hr(style="border-color: black;"),
      
      # slider for max LED
      sliderInput("reclusterSlider_maxLED", label = strong("Max. LED:"), min = 1, max = 20, value = 7, step = 1),
      shinyBS::bsTooltip("reclusterSlider_maxLED", "Max. edit distance from seed sequence"),
      
      # start button
      actionButton("reclusterStart", label = h5("Recluster"), style='padding:11px; font-size:80%'),
      
      # download button
      downloadButton("reclusterDownload", label = h5("Download"), style='padding:2px; font-size:80%'),
      
      # horizontal line
      tags$hr(style="border-color: black;"),
      
      # plot customizations for population size plot
      radioButtons(
        "recluster_popSize_custom",
        label = strong("Adjust default population size plot?"),
        choices = c("Yes","No"), selected = "No",
        inline = TRUE
      ),
      shinyBS::bsTooltip("recluster_popSize_custom", "Customize plot components?"),
      
      conditionalPanel(
        condition = "input.recluster_popSize_custom == 'Yes'",
        
        # text input for x-axis, y-axis, legend title, and plot title
        textAreaInput("recluster_popSize_xaxis", label = strong("X-axis - represents each cluster."), value = "Cluster"),
        textAreaInput("recluster_popSize_yaxis", label = strong("Y-axis - represents unique sequence count."), value = "Sequence count"),
        textAreaInput("recluster_popSize_legend", label = strong("Legend title - represents populations."), value = "Population"),
        textAreaInput("recluster_popSize_title", label = strong("Plot title."), value = "Sequence count per cluster"),
        
        # colour and fill inputs for bar outline and fill
        colourpicker::colourInput("recluster_popSize_baroutline", "Color for bar outline.", "black"),
        selectInput(
          "recluster_popSize_palette",
          label = strong("Fill palette."),
          choices = c("Accent", "Dark2", "Paired", "Pastel1", "Pastel2", "Set1", "Set2"),
          selected = "Dark2",
          multiple = FALSE
        )
      ),
      
      # population contributions to each cluster
      actionButton("reclusterPopSize_start", label = h5("Population plot"), style='padding:11px; font-size:80%'),
      shinyBS::bsTooltip("reclusterPopSize_start", "Bar plot showing number of sequences per population per cluster."),
      shinyBS::bsModal(
        id = "reclusterPopSizeWindow",
        title = "Sequence count bar plot",
        trigger = "reclusterPopSize_start",
        size = "large",
        shinycssloaders::withSpinner(plotlyOutput("reclusterPopSizeOutput"))
      ),
      
      # horizontal line
      tags$hr(style="border-color: black;"),
      
      # plot customizations for avg. RPU plot
      radioButtons(
        "recluster_RPU_custom",
        label = strong("Adjust default RPU plot?"),
        choices = c("Yes","No"), selected = "No",
        inline = TRUE
      ),
      shinyBS::bsTooltip("recluster_RPU_custom", "Customize plot components?"),
      
      conditionalPanel(
        condition = "input.recluster_RPU_custom == 'Yes'",
        
        # text input for x-axis, y-axis, legend title, and plot title
        textAreaInput("recluster_RPU_xaxis", label = strong("X-axis - represents each cluster."), value = "Cluster"),
        textAreaInput("recluster_RPU_yaxis", label = strong("Y-axis - represents mean RPU."), value = "Avg. RPU"),
        textAreaInput("recluster_RPU_legend", label = strong("Legend title - represents populations."), value = "Population"),
        textAreaInput("recluster_RPU_title", label = strong("Plot title."), value = "Avg. RPU per cluster"),
        
        # colour selection for bar outline and palette selection for bar fill
        colourpicker::colourInput("recluster_RPU_baroutline", "Color for bar outline.", "black"),
        selectInput(
          "recluster_RPU_palette",
          label = strong("Fill palette."),
          choices = c("Accent", "Dark2", "Paired", "Pastel1", "Pastel2", "Set1", "Set2"),
          selected = "Dark2",
          multiple = FALSE
        )
      ),

      # average RPU per population per cluster
      actionButton("reclusterRPU_start", label = h5("RPU plot"), style='padding:11px; font-size:80%'),
      shinyBS::bsTooltip("reclusterRPU_start", "Bar plot showing average RPU per population per cluster."),
      shinyBS::bsModal(
        id = "reclusterRPUWindow",
        title = "RPU bar plot",
        trigger = "reclusterRPU_start",
        size = "large",
        shinycssloaders::withSpinner(plotlyOutput("reclusterRPUOutput"))
      ),
      
      # horizontal line
      tags$hr(style="border-color: black;"),
      
      # plot customizations for avg. LED plot
      radioButtons(
        "recluster_LED_custom",
        label = strong("Adjust default LED plot?"),
        choices = c("Yes","No"), selected = "No",
        inline = TRUE
      ),
      shinyBS::bsTooltip("recluster_LED_custom", "Customize plot components?"),
      
      conditionalPanel(
        condition = "input.recluster_LED_custom == 'Yes'",
        
        # text input for x-axis, y-axis, legend title, and plot title
        textAreaInput("recluster_LED_xaxis", label = strong("X-axis - represents each cluster."), value = "Cluster"),
        textAreaInput("recluster_LED_yaxis", label = strong("Y-axis - represents mean LED."), value = "Avg. LED"),
        textAreaInput("recluster_LED_title", label = strong("Plot title."), value = "Avg. LED per cluster"),
        
        # colour selection for bar outline and fill
        colourpicker::colourInput("recluster_LED_baroutline", "Color for bar outline.", "black"),
        colourpicker::colourInput("recluster_LED_barfill", "Color for bar fill.", "skyblue")
      ),

      # average LED per population per cluster plot
      actionButton("reclusterLED_start", label = h5("LED plot"), style='padding:11px; font-size:80%'),
      shinyBS::bsTooltip("reclusterLED_start", "Bar plot showing average LED to new cluster seed per population per cluster."),
      shinyBS::bsModal(
        id = "reclusterLEDWindow",
        title = "LED bar plot",
        trigger = "reclusterLED_start",
        size = "large",
        shinycssloaders::withSpinner(plotlyOutput("reclusterLEDOutput"))
      ),
      
      # horizontal line
      tags$hr(style="border-color: black;"),
      
      # plot customizations for box plot
      radioButtons(
        "recluster_boxplot_custom",
        label = strong("Adjust default box plot?"),
        choices = c("Yes","No"), selected = "No",
        inline = TRUE
      ),
      shinyBS::bsTooltip("recluster_boxplot_custom", "Customize plot components?"),
      
      conditionalPanel(
        condition = "input.recluster_boxplot_custom == 'Yes'",
        
        # text input for x-axis, y-axis, legend title, and plot title
        textAreaInput("recluster_boxplot_xaxis", label = strong("X-axis - represents each cluster."), value = "Cluster"),
        textAreaInput("recluster_boxplot_title", label = strong("Plot title."), value = "Sequence enrichment per super-cluster"),
        colourpicker::colourInput("recluster_boxplot_baroutline", "Color for box outline.", "black"),
        colourpicker::colourInput("recluster_boxplot_barfill", "Color for box fill.", "skyblue")
      ),
      
      # enrichment box plot per new cluster
      actionButton("reclusterEnrich_start", label = h5("Box plot"), style='padding:11px; font-size:80%'),
      shinyBS::bsTooltip("reclusterEnrich_start", "Box plot showing the enrichment of sequences for each cluster."),
      shinyBS::bsModal(
        id = "reclusterEnrichWindow",
        title = "Enrichment box plot",
        trigger = "reclusterEnrich_start",
        size = "large",
        shinycssloaders::withSpinner(plotlyOutput("reclusterEnrichOutput"))
      )
    ),
    
    # display count output as data.table
    mainPanel(
      shinycssloaders::withSpinner(plotly::plotlyOutput("reclusterHeatmapOutput", height = "600px")),
      tags$hr(style="border-color: black;"),
      shinycssloaders::withSpinner(DT::dataTableOutput("reclusterOutput"))
    )
  )
)
