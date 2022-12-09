library(shiny)
library(shinydashboard)
library(tidyverse)
library(caret)
##amphibians dataset
amph <- read.csv2("amphibians.csv", skip = 1)

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
                            column(3,
                                   #Purpose of the app
                                   h1("Purpose"),
                                   #box to contain description
                                   box(background="green",width=12,
                                       h4("The purpose of this application is to explore and model four different data sets depending on the user's choice.")
                  #########################################################################
                  ### May not do on user choice, but instead have separate tabs for each ##
                  #########################################################################
                                   )
                            ),
                            column(9,
                                   #Info on the data
                                   h1("About the Data"),
                                   fluidRow(
                                     column(3,
                                            box(background="green",width=12,
                                                h2("Diamonds"),
                                                h4("Found in the ", code("ggplot2"), " package, this data set includes prices and other attributes of almost 54,000 daimonds. More information about the data can be found ", strong(a(href="https://ggplot2.tidyverse.org/reference/diamonds.html", "here.", style = "color:pink")))
                                            )
                                       
                                     ),
                                     column(3,
                                            box(background="green",width=12,
                                                h2("Pottery"),
                                                h4("Found in the ", code("HSAUR3"), " package, this data set includes chemical compositions of Romano-British pottery. More information about the data can be found ", strong(a(href="https://cran.r-project.org/web/packages/HSAUR3/HSAUR3.pdf", "here.", style = "color:pink")))
                                            )
                                     ),
                                     column(3,
                                            box(background="green",width=12,
                                                h2("Kanga"),
                                                h4("Found in the ", code("faraway"), " package, this data set includes skull measurements of different species of kangaroo. More information about the data can be found ", strong(a(href="https://www.rdocumentation.org/packages/faraway/versions/1.0.8/topics/kanga", "here.", style = "color:pink")))
                                            )
                                     ),
                                     column(3,
                                             box(background="green",width=12,
                                                 h2("Batting"),
                                                 h4("Found in the ", code("Lahman"), " package, this data set includes batting, pitching, and fielding statistics from 1871 to 2021. More information about the data can be found ", strong(a(href="https://www.seanlahman.com/baseball-archive/statistics/", "here.", style = "color:pink")))
                                             )
                                     )
                                   )
                            ),
                          fluidRow(tags$hr()),
                          #Image(s) related to data
                          fluidRow(
                            column(3,
                                   #Purposes of each tab
                                   h1("Layout"),
                                   #box to contain description
                                   box(background="green",width=12,
                                       h4("The app has the following tabs, with the purpose of each tab listed beside it:",
                                          tags$ul(
                                            tags$li("About - Description of the app and data. You are here."),
                                            tags$li("Data Exploration - Exploratory data analysis (EDA). Gives numerical and graphical summaries of chosen data."),
                                            tags$li("Modeling - ",
                                                    tags$ul(
                                                      tags$li(),
                                                      tags$li(),
                                                      tags$li()
                                                      )
                                                    ),
                                            tags$li()
                                            )
                                          )
                                       )
                                   ),
                            column(9,
                                   fluidRow(
                                     column(3,
                                            align = "center",
                                            img(src="Rare-Green-Diamond-060ct-Natural-Loose-Fancy-Light-Green-Color-GIA-SI1-Oval-254572313852-1024x984.jpg",
                                                width = 200)
                                            ),
                                     column(3,
                                            align = "center",
                                            img(src="5413866585_b3c8f36b53_b.jpg",
                                                width = 200)
                                            ),
                                     column(3,
                                            align = "center",
                                            img(src="kangaroo-1670528960625-5264.jpg",
                                                width = 200)
                                            ),
                                     column(3,
                                            align = "center",
                                            img(src="OSCRFZRKN5FJXFJCBXLEF574VM.jpg",
                                                width = 200)
                                            )
                                     )
                                   )
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