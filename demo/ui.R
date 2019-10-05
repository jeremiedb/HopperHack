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
                    dashboardHeader(title = "Hotel Recommendation"),

                    ## Sidebar content
                    dashboardSidebar(
                        sidebarMenu(
                            menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
                            menuItem("Widgets", tabName = "widgets", icon = icon("th"))
                        )
                    ),

                    ## Body content
                    dashboardBody(
                        tabItems(
                            # First tab content
                            tabItem(tabName = "dashboard",
                                    fluidRow(
                                      selectInput("Origin", label = "Origin",
                                                  c("Canada" = "canada",
                                                    "USA" = "usa",
                                                    "France" = "france",
                                                    "UK" = "uk",
                                                    "Germany" = "germany"), selected = "Canada", selectize = T),
                                        box(plotOutput("plot1", height = 250)),

                                        box(
                                            title = "Controls",
                                            sliderInput("slider", "Number of observations:", 1, 100, 50)
                                        )
                                    )
                            ),

                            # Second tab content
                            tabItem(tabName = "widgets",
                                    h2("Widgets tab content")
                            )
                        )
                    )
)
