# Copyright (C) 2022 Skyler T. Kramer
# Full GNU GPL version 3 license found in LICENSE.txt

# source the sub-tabs
source("./uiTabs/preprocessTab.R")
source("./uiTabs/countTab.R")
source("./uiTabs/recountTab.R")

# merge the sub-tabs
startTab <- tabPanel(
  "Start",
  tabsetPanel(
    preprocessTab,
    countTab,
    recountTab,
    selected = "Count",
    type = "pills"
  )
)
