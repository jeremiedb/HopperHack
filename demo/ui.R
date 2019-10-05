#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)

# Define UI for application that draws a histogram
ui <- dashboardPage(skin = "black",
                    dashboardHeader(title = "HopperHacks19"),

                    ## Sidebar content
                    dashboardSidebar(
                        sidebarMenu(
                            menuItem("Flights", tabName = "flights", icon = icon("plane")),
                            menuItem("Hotels", tabName = "hotels", icon = icon("hotel"))
                        )
                    ),

                    ## Body content
                    dashboardBody(
                        tabItems(
                            # First tab content
                            tabItem(tabName = "flights",
                              box(title = "Flight Informations", width = 6,
                                numericInput("nb_travelers", "Number of travelers:", value = 1, min = 1, max = 10, step = 1),
                                selectizeInput("location_from", "From:", choices = dt_wiki_airports$label_search, selected = "", multiple = FALSE),
                                selectizeInput("location_to", "To:", choices = dt_wiki_airports$label_search, selected = "", multiple = FALSE)
                              )
                            ),

                            # Second tab content
                            tabItem(tabName = "hotels", width = 6,
                                    box(title = "Hotel Informations",
                                        selectizeInput("hotel_cities", label = "Cities to look for:", choices = dt_cities$label_search, selected = "", multiple = T)
                                    ),
                                    textOutput("list")
                            )
                        )
                    )
)
