library(shiny)
library(shinydashboard)
library(tidyverse)
library(caret)
##Include herpetology dataset##

dashboardPage(skin="green",
              #title
              dashboardHeader(title="ST 558 Final"),
              #sidebar
              dashboardSidebar(sidebarMenu(
                menuItem("About", tabName = "about", icon = icon("question")),
                menuItem("Data Exploration", tabName = "eda", icon = icon("calculator")),
                menuItem("Modeling", tabName = "model", icon = icon("pen-ruler"), startExpanded = TRUE,
                         menuSubItem("Info", tabName = "modInfo"),
                         menuSubItem("Fitting", tabName = "modFit"),
                         menuSubItem("Prediction", tabName = "modPred")),
                menuItem("Data", tabName = "data", icon = icon("laptop"))
              )),
              #app body
              dashboardBody(
                tabItems(
                  # First tab content
                  tabItem(tabName = "about",
                          fluidRow(
                            #two columns for each of the two items
                            column(6,
                                   #Purpose of the app
                                   h1("Purpose"),
                                   #box to contain description
                                   box(background="green",width=12,
                                       h4("The purpose of this application is _____")
                                   )
                            ),
                            column(6,
                                   #Info on the data
                                   h1("About the Data"),
                                   #box to contain description
                                   box(background="green",width=12,
                                       h4("The data is about ___. It's from ___. More info can be found at the following:")
                                       )
                                   )
                            ),
                          fluidRow(tags$hr()),
                          #Image(s) related to data
                          fluidRow(
                            column(6,
                                   #Purposes of each tab
                                   h1("Layout of this App"),
                                   #box to contain description
                                   box(background="green",width=12,
                                       h4("The app has the following tabs, with the purpose of each tab listed beside it:")
                                   )
                            ),
                            column(6,
                                   #Purposes of each tab
                                   align = "center",
                                   img(src="amphibianCollage.jpg",
                                       width = 600)
                                   )
                            )
                  ),
                  
          ########EDIT HERE AND SERVER########
                  #actual app layout      
                  tabItem(tabName = "app",
                          fluidRow(
                            column(width=3,
                                   box(width=12,background="red",sliderInput("yvalue","Y=Number of Successes",min = 0,max = 30,value = 15)
                                   ),
                                   box(width=12,
                                       title="Hyperparameters of the prior distribution for \\(\\Theta\\)",
                                       background="red",
                                       solidHeader=TRUE,
                                       p("\\(\\frac{\\Gamma(\\alpha+\\beta)}{\\Gamma(\\alpha)\\Gamma(\\beta)}\\theta^{\\alpha-1}(1-\\theta)^{\\beta-1}\\)"),
                                       h5("(Set to 1 if blank.)"),
                                       numericInput("alpha",label=h5("\\(\\alpha\\) Value (> 0)"),value=1,min=0,step=0.1),
                                       numericInput("beta",label=h5("\\(\\beta\\) Value (> 0)"),value=1,min=0,step=0.1)
                                   )
                            ),
                            column(width=9,
                                   fluidRow(
                                     box(width=6,
                                         plotOutput("priorPlot"),
                                         br(),
                                         h4("Prior distribution for the probability of success parameter \\(\\Theta\\).")
                                     ),
                                     box(width=6,
                                         plotOutput("distPlot"),
                                         br(),
                                         h4("Posterior distribution for the probability of success \\(\\Theta\\).")
                                     )
                                   )
                            )
                          )
                  )
                )
              )
)