##choroplethr graveyard
#library(choroplethr)
#library(choroplethrMaps)
#library(choroplethrZip)

#choroplethr graveyard
#set map arguments  
#  mapargs <- reactive({
#    args <- switch(input$city,
#                   nyc = nyc,
#                   dc = dc)
#    
#    year_select <- args[args$Year == input$year, ]
#    year_select$value <- year_select[[input$value]]
#    year_select$region <- as.character(year_select$Zip.Code)
#    title = paste0(input$city, " Benchmarking Stats by Zip Code:\n", input$value)
#    fips = ifelse(input$city == 'nyc', nyc_fips, dc_fips)
#    zip_choropleth(year_select, title = title, county_zoom = fips)
#  })
#  output$map <- renderPlot({
#    # print the map
#    mapargs()
#  })
#fips <- c(nyc_fips = c(36005, 36047, 36061, 36081, 36085),
#          dc_fips = c(11101))
#
#nyc_fips = c(36005, 36047, 36061, 36081, 36085)
#dc_fips = c(11001)
