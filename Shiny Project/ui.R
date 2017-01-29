## ui.R ##

  home <- tags$html(
    tags$head(
      tags$title('The Energy Benchmarking Dashboard')
    ),
    tags$body(
      h1('Welcome to the Energy Benchmarking Dashboard'),
      p('In the United States, a group of progressive cities have been collecting and reporting energy consumption data within their large buildings.'),
      div(id='myDiv', class='simpleDiv',
          'What insight(s) can we take from the data these cities are releasing? Is there any measurable reduction in energy consumption among reporting buildings?'),
      div(id='myDiv', class='simpleDiv',
          'You can search for insights yourself using the Energy Benchmarking Dashboard.'),
      div(id='myDiv', class='simpleDiv',
          'To get started, click a tab on the panel to the left. Explore maps, graphs, or the data itself.'),
      div(),
      leafletOutput("map", width = "100%", height = "750"),
      absolutePanel(bottom = 580, left = 60,
                    selectizeInput("mapcity", "Select City to Display", 
                                   mapcities, selected = "NYC"),
                    selectizeInput("mapyear", "Pick Year to Display",
                                   2011:2015, selected = 2014)
                    )
      )
      )

  sidebar <- dashboardSidebar(
      sidebarUserPanel("Dashboard", image = "https://img.clipartfest.com/67bd1237f1975977ad47427e2193d429_a78b434b9b3c798244ec90ad1abe72-lightning-bolt-clipart-free_245-400.gif"),
      sidebarMenu(
        menuItem("Home", tabName = "home", icon = icon("home")),
        menuItem("Data Explorer", tabName = "graph", icon = icon("flag")),
        menuItem("City Explorer", tabName = "city", icon = icon("building")),
        menuItem("View Data", tabName = "data", icon = icon("database"))
        )
    )
  
  body <- dashboardBody(
      tabItems(
        tabItem(tabName = "home",
                fluidRow(box(width = 15, 
                             home))),
        tabItem(tabName = "data",
                fluidRow(
                    column(width = 2,
                           tabBox(width = NULL,
                                  tabPanel(h5("Filter"),
                                   checkboxGroupInput('data_years', 'Years to Display:',
                                                      c(2011, 2012, 2013, 2014, 2015), selected = 2013),
                                   checkboxGroupInput('data_cities', 'Cities to Display:',
                                                      cities, selected = "New York City")
                          ))),
                    column(width = 10,
                      box(dataTableOutput("table"), width = 10)))),
        
        tabItem(tabName = "graph",
                fluidRow(
                  column(width = 3,
                     h4("Customize Plot"),
                     checkboxGroupInput('show_years', 'Years to Display:',
                                            c(2011, 2012, 2013, 2014, 2015), selected = 2013),
                     checkboxGroupInput('show_cities', 'Cities to Display:',
                                  cities, selected = "New York City"),
                     sliderInput("xrange", "Set x-axis range", min = 0, max = 1000, value = c(0, 1000)),
                     sliderInput("yrange", "Set y-axis range", min = 0, max = 10000000, value = c(0, 10000000)),
                     selectInput("xvar", "X-axis variable", plotvalues, selected = "NormSourceEUI"),
                     selectInput("yvar", "Y-axis variable", plotvalues, selected = "ReportedGFA")
                  ),
                  column(width = 9,
                          fluidRow(wellPanel(plotOutput("graph1")))))),
        
        tabItem(tabName = "city",
                splitLayout(wellPanel(width = 5,
                        radioButtons("radio", label = h4(strong("Select City to Display")),
                                choices = list("New York City" = "New York City",
                                               "Washington, DC" = "DC", "San Francisco" = "San Francisco"), 
                                          selected = "New York City")),
                            wellPanel(width = 5,
                                h4(textOutput("willitwork")),
                                textOutput("citystats1"),
                                textOutput("citystats2")
                                ),
                            wellPanel(width = 1,
                                h5("Advanced Options:"),
                                checkboxInput("log", "Log Transform Plots", FALSE))),
                fluidRow(box(width = 5, 
                                   plotOutput("city1")),
                         box(width = 5,
                            plotOutput("city2"))),
                fluidRow(box(width = 5,
                             plotOutput("city3")),
                         box(width = 5,
                             plotOutput("city4")))
                )
        ))
  
  shinyUI(
    dashboardPage(
      dashboardHeader(title = "Benchmarking Energy Dashboard"),
      sidebar,
      body
    ))