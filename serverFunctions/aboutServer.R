# Copyright (C) 2022 Skyler T. Kramer
# Full GNU GPL version 3 license found in LICENSE.txt

aboutServer <- function(input, output, session){
  
  # read and display change log
  output$changeLog <- renderUI({
    
    HTML(readLines("./www/Change_Log2.txt"))
  })
}
