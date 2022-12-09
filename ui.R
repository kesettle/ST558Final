library(shiny)
library(shinydashboard)
library(DT)
library(tidyverse)#for data manipulation, graphing, and diamonds data
library(caret)    #for modeling
library(HSAUR3)   #for pottery data
library(faraway)  #for kanga data
library(Lahman)   #for batting data
#load datasets
data("diamonds")
data("pottery")
data("kanga")
data("Batting")

dashboardPage(skin="green",
              #title
              dashboardHeader(title="ST 558 Final"),
              
              #sidebar
              dashboardSidebar(
                sidebarMenu(
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
                            column(4,
                                   #Purpose of the app
                                   h1("Purpose"),
                                   box(background="green",width=12,
                                       h4("The purpose of this application is to explore and model four different data sets depending on the user's choice.")),
                                   #Purposes of each tab
                                   h1("Layout"),
                                   #box to contain description
                                   box(background="green",width=12,
                                       h4("The app has the following tabs, with the purpose of each tab listed beside it:",
                                          tags$ul(
                                            tags$li(strong("About"), " - Description of the app and data. You are here."),
                                            tags$li(strong("Data Exploration"), " - A section of tabs for exploratory data analysis (EDA) for each data set. Gives numerical and graphical summaries of chosen data."),
                                            tags$li(strong("Modeling"), " - A section of tabs to model a chosen dataset.",
                                                    tags$ul(
                                                      tags$li(strong("Info"), " - Explains what types of models are available, what each are, and the pros and cons of each type. Also gives modeling information for each dataset, such as what the response and what kind of models are available to each.."),
                                                      tags$li(strong("Fitting"), " - A section of tabs for fitting models for each data set. Allows specification for amount of training/testing data, variable selection for each available model, and summaries of each model."),
                                                      tags$li(strong("Prediction"), " - A selection of tabs for using a fitted model to predict a response for each data set. Allows model type selection and model fitting, and gives prediction value for constructed model."))),
                                            tags$li(strong("Data"), " - Displays the dataset. Allows the user to subest and download their dataset of choice."))))),
                            column(8,
                                   #Info on the data with related picture
                                   h1("About the Datasets"),
                                   fluidRow(
                                     column(3,
                                            align = "center",
                                            box(background="green",width=12,
                                                h2("Diamonds"),
                                                h4("Found in the ", code("ggplot2"), " package, this data set includes prices and other attributes of almost 54,000 daimonds. More information about the data can be found ", strong(a(href="https://ggplot2.tidyverse.org/reference/diamonds.html", "here.", style = "color:pink"))),
                                                img(src="diamond.jpg", width = 200))),
                                     column(3,
                                            align = "center",
                                            box(background="green",width=12,
                                                h2("Pottery"),
                                                h4("Found in the ", code("HSAUR3"), " package, this data set includes chemical compositions of Romano-British pottery. More information about the data can be found ", strong(a(href="https://cran.r-project.org/web/packages/HSAUR3/HSAUR3.pdf", "here.", style = "color:pink"))),
                                                img(src="pottery.jpg", width = 200))),
                                     column(3,
                                            align = "center",
                                            box(background="green",width=12,
                                                h2("Kanga"),
                                                h4("Found in the ", code("faraway"), " package, this data set includes skull measurements of different species of kangaroo. More information about the data can be found ", strong(a(href="https://www.rdocumentation.org/packages/faraway/versions/1.0.8/topics/kanga", "here.", style = "color:pink"))),
                                                img(src="kanga.jpg", width = 200))),
                                     column(3,
                                            align = "center",
                                            box(background="green",width=12,
                                                h2("Batting"),
                                                h4("Found in the ", code("Lahman"), " package, this data set includes batting, pitching, and fielding statistics from 1871 to 2021. More information about the data can be found ", strong(a(href="https://www.seanlahman.com/baseball-archive/statistics/", "here.", style = "color:pink"))),
                                                img(src="batting.jpg", width = 200))))
                                   )
                          )
                  ),
                  tabItem(tabName = "eda",
                          tabsetPanel(
                            ##Tab: Diamonds data EDA##
                            tabPanel("Diamonds",
                                     fluidRow(
                                       column(12,
                                              h2("Exploratory Data Analysis"),
                                              box(background="green",width=12, h4("_About EDA_")))),
                                     fluidRow(
                                       column(3,
                                              box(background="green",width=12,
                                                  h3("[Numerical Var selection here]"))),
                                       column(3,
                                              box(background="green",width=12,
                                                  h3("[Numerical Var summaries here]"))),
                                       column(3,
                                              box(background="green",width=12, 
                                                  h3("[Categorical Var selection here]"))),
                                       column(3,
                                              box(background="green",width=12, 
                                                  h3("[Categorical Var summaries here]")))),
                                     fluidRow(
                                       column(4,
                                              box(background="green",width=12,
                                                  h3("[graph selection]"))),
                                       column(8,
                                              box(background="green",width=12,
                                                  h3("[Graph]"))))
                                     ),
                            ##Tab: Pottery data EDA##
                            tabPanel("Pottery",
                                     fluidRow(
                                       column(12,
                                              h2("Exploratory Data Analysis"),
                                              box(background="green",width=12, h4("_About EDA_")))),
                                     fluidRow(
                                       column(3,
                                              box(background="green",width=12,
                                                  h3("[Numerical Var selection here]"))),
                                       column(3,
                                              box(background="green",width=12,
                                                  h3("[Numerical Var summaries here]"))),
                                       column(3,
                                              box(background="green",width=12, 
                                                  h3("[Categorical Var selection here]"))),
                                       column(3,
                                              box(background="green",width=12, 
                                                  h3("[Categorical Var summaries here]")))),
                                     fluidRow(
                                       column(4,
                                              box(background="green",width=12,
                                                  h3("[graph selection]"))),
                                       column(8,
                                              box(background="green",width=12,
                                                  h3("[Graph]"))))
                                     ),
                            ##Tab: Kanga data EDA##
                            tabPanel("Kanga",
                                     fluidRow(
                                       column(12,
                                              h2("Exploratory Data Analysis"),
                                              box(background="green",width=12, h4("_About EDA_")))),
                                     fluidRow(
                                       column(3,
                                              box(background="green",width=12,
                                                  h3("[Numerical Var selection here]"))),
                                       column(3,
                                              box(background="green",width=12,
                                                  h3("[Numerical Var summaries here]"))),
                                       column(3,
                                              box(background="green",width=12, 
                                                  h3("[Categorical Var selection here]"))),
                                       column(3,
                                              box(background="green",width=12, 
                                                  h3("[Categorical Var summaries here]")))),
                                     fluidRow(
                                       column(4,
                                              box(background="green",width=12,
                                                  h3("[graph selection]"))),
                                       column(8,
                                              box(background="green",width=12,
                                                  h3("[Graph]"))))
                                     ),
                            ##Tab: Batting data EDA##
                            tabPanel("Batting",
                                     fluidRow(
                                       column(12,
                                              h2("Exploratory Data Analysis"),
                                              box(background="green",width=12, h4("_About EDA_")))),
                                     fluidRow(
                                       column(3,
                                              box(background="green",width=12,
                                                  h3("[Numerical Var selection here]"))),
                                       column(3,
                                              box(background="green",width=12,
                                                  h3("[Numerical Var summaries here]"))),
                                       column(3,
                                              box(background="green",width=12, 
                                                  h3("[Categorical Var selection here]"))),
                                       column(3,
                                              box(background="green",width=12, 
                                                  h3("[Categorical Var summaries here]")))),
                                     fluidRow(
                                       column(4,
                                              box(background="green",width=12,
                                                  h3("[graph selection]"))),
                                       column(8,
                                              box(background="green",width=12,
                                                  h3("[Graph]"))))
                                     )
                            )
                          ),
                  tabItem(tabName = "modInfo",
                          fluidRow(
                            column(12,
                                   h2("Modeling"),
                                   box(background="green",width=12, h4("_what is this tab section about?_")))),
                          fluidRow(
                            column(4,
                                   h3("Multiple Linear Regression/Generalized Linear Regression"),
                                   box(background="green",width=12, h4("[what is MLR/GLR; pros and cons]"))),
                            column(4,
                                   h3("Regression Tree/Classification Tree"),
                                   box(background="green",width=12, h4("[what is reg./class. tree; pros and cons]"))),
                            column(4,
                                   h3("Random Forest"),
                                   box(background="green",width=12, h4("[what is rand. forest; pros and cons]")))),
                          fluidRow(
                            column(3,
                                   h3("Diamonds"),
                                   box(background="green",width=12, h4("[predicting [price]; MLR/Reg Tree/Rand Forest]"))),
                            column(3,
                                   h3("Pottery"),
                                   box(background="green",width=12, h4("[predicting [kiln]; GLR/Class Tree/Rand Forest]"))),
                            column(3,
                                   h3("Kanga"),
                                   box(background="green",width=12, h4("[predicting [species]; GLR/Class Tree/Rand Forest]"))),
                            column(3,
                                   h3("Batting"),
                                   box(background="green",width=12, h4("[predicting [?]; MLR/Reg Tree/Rand Forest]"))))
                          ),
                  tabItem(tabName = "modFit",
                          tabsetPanel(
                            ##Tab: Diamonds modeling##
                            tabPanel("Diamonds",
                                     fluidRow(
                                       column(12,
                                              box(background="green",width=12, 
                                                  h3("Percent Training Data: [min=.5,max=.85,by=0.01]")))),
                                     fluidRow(
                                       column(4,
                                              h3("Multiple Linear Regression Model"),
                                              box(background="green",width=12, h4("[variable selection]"),
                                                  h4("[fit model button]"),
                                                  h4("[conditional: model summary info generated after button hit]"),
                                                  h4("graph fit (maybe)"))),
                                       column(4,
                                              h3("Regression Tree"),
                                              box(background="green",width=12, h4("[variable selection]"),
                                                  h4("[fit model button]"),
                                                  h4("[conditional: model summary info generated after button hit]"),
                                                  h4("graph fit (maybe)"))),
                                       column(4,
                                              h3("Random Forest"),
                                              box(background="green",width=12, h4("[variable selection]"),
                                                  h4("[fit model button]"),
                                                  h4("[conditional: model summary info generated after button hit]"),
                                                  h4("graph fit (maybe)")))),
                                     fluidRow(
                                       column(2),
                                       column(8, box(background="green",width=12, 
                                                     h3("[about fit criteria, RMSE in particular]"))),
                                       column(2))
                                     ),
                            ##Tab: Pottery modeling##
                            tabPanel("Pottery",
                                     fluidRow(
                                       column(12,
                                              box(background="green",width=12, 
                                                  h3("Percent Training Data: [min=.5,max=.85,by=0.01]")))),
                                     fluidRow(
                                       column(4,
                                              h3("Generalized Linear Regression Model"),
                                              box(background="green",width=12, h4("[variable selection]"),
                                                  h4("[fit model button]"),
                                                  h4("[conditional: model summary info generated after button hit]"),
                                                  h4("graph fit (maybe)"))),
                                       column(4,
                                              h3("Classification Tree"),
                                              box(background="green",width=12, h4("[variable selection]"),
                                                  h4("[fit model button]"),
                                                  h4("[conditional: model summary info generated after button hit]"),
                                                  h4("graph fit (maybe)"))),
                                       column(4,
                                              h3("Random Forest"),
                                              box(background="green",width=12, h4("[variable selection]"),
                                                  h4("[fit model button]"),
                                                  h4("[conditional: model summary info generated after button hit]"),
                                                  h4("graph fit (maybe)")))),
                                     fluidRow(
                                       column(2),
                                       column(8, box(background="green",width=12, 
                                                     h3("[about fit criteria, RMSE in particular]"))),
                                       column(2))
                                     ),
                            ##Tab: Kanga modeling##
                            tabPanel("Kanga",
                                     fluidRow(
                                       column(12,
                                              box(background="green",width=12, 
                                                  h3("Percent Training Data: [min=.5,max=.85,by=0.01]")))),
                                     fluidRow(
                                       column(4,
                                              h3("Generalized Linear Regression Model"),
                                              box(background="green",width=12, h4("[variable selection]"),
                                                  h4("[fit model button]"),
                                                  h4("[conditional: model summary info generated after button hit]"),
                                                  h4("graph fit (maybe)"))),
                                       column(4,
                                              h3("Classification Tree"),
                                              box(background="green",width=12, h4("[variable selection]"),
                                                  h4("[fit model button]"),
                                                  h4("[conditional: model summary info generated after button hit]"),
                                                  h4("graph fit (maybe)"))),
                                       column(4,
                                              h3("Random Forest"),
                                              box(background="green",width=12, h4("[variable selection]"),
                                                  h4("[fit model button]"),
                                                  h4("[conditional: model summary info generated after button hit]"),
                                                  h4("graph fit (maybe)")))),
                                     fluidRow(
                                       column(2),
                                       column(8, box(background="green",width=12, 
                                                     h3("[about fit criteria, RMSE in particular]"))),
                                       column(2))
                                     ),
                            ##Tab: Batting modeling##
                            tabPanel("Batting",
                                     fluidRow(
                                       column(12,
                                              box(background="green",width=12, 
                                                  h3("Percent Training Data: [min=.5,max=.85,by=0.01]")))),
                                     fluidRow(
                                       column(4,
                                              h3("Multiple Linear Regression Model"),
                                              box(background="green",width=12, h4("[variable selection]"),
                                                  h4("[fit model button]"),
                                                  h4("[conditional: model summary info generated after button hit]"),
                                                  h4("graph fit (maybe)"))),
                                       column(4,
                                              h3("Regression Tree"),
                                              box(background="green",width=12, h4("[variable selection]"),
                                                  h4("[fit model button]"),
                                                  h4("[conditional: model summary info generated after button hit]"),
                                                  h4("graph fit (maybe)"))),
                                       column(4,
                                              h3("Random Forest"),
                                              box(background="green",width=12, h4("[variable selection]"),
                                                  h4("[fit model button]"),
                                                  h4("[conditional: model summary info generated after button hit]"),
                                                  h4("graph fit (maybe)")))),
                                     fluidRow(
                                       column(2),
                                       column(8, box(background="green",width=12, 
                                                     h3("[about fit criteria, RMSE in particular]"))),
                                       column(2))
                                     )
                            )
                          ),
              #######################################################################
              ## May adjust var and value input, reduce size of prediction section ##
              #######################################################################
                  tabItem(tabName = "modPred",
                          tabsetPanel(
                            ##Tab: Diamonds prediction##
                            tabPanel("Diamonds",
                                     fluidRow(
                                       column(12,
                                              box(background="green",width=12, 
                                                  h3("[Directions]")))),
                                     fluidRow(
                                       column(5,
                                              box(background="green",width=12, 
                                                  h3("predictor selection"),
                                                  h3("predictor value input"),
                                                  h3("[Predict] button (may move to other box)"))),
                                       column(7,
                                              box(background="green",width=12, 
                                                  h3("conditional: appear after predict button pushed"),
                                                  h3("display model + summary"),
                                                  h3("display prediction value"))))
                                     ),
                            ##Tab: Pottery prediction##
                            tabPanel("Pottery",
                                     fluidRow(
                                       column(12,
                                              box(background="green",width=12, 
                                                  h3("[Directions]")))),
                                     fluidRow(
                                       column(5,
                                              box(background="green",width=12, 
                                                  h3("predictor selection"),
                                                  h3("predictor value input"),
                                                  h3("[Predict] button (may move to other box)"))),
                                       column(7,
                                              box(background="green",width=12, 
                                                  h3("conditional: appear after predict button pushed"),
                                                  h3("display model + summary"),
                                                  h3("display prediction value"))))),
                            ##Tab: Kanga prediction##
                            tabPanel("Kanga",
                                     fluidRow(
                                       column(12,
                                              box(background="green",width=12, 
                                                  h3("[Directions]")))),
                                     fluidRow(
                                       column(5,
                                              box(background="green",width=12, 
                                                  h3("predictor selection"),
                                                  h3("predictor value input"),
                                                  h3("[Predict] button (may move to other box)"))),
                                       column(7,
                                              box(background="green",width=12, 
                                                  h3("conditional: appear after predict button pushed"),
                                                  h3("display model + summary"),
                                                  h3("display prediction value"))))),
                            ##Tab: Batting prediction##
                            tabPanel("Batting",
                                     fluidRow(
                                       column(12,
                                              box(background="green",width=12, 
                                                  h3("[Directions]")))),
                                     fluidRow(
                                       column(5,
                                              box(background="green",width=12, 
                                                  h3("predictor selection"),
                                                  h3("predictor value input"),
                                                  h3("[Predict] button (may move to other box)"))),
                                       column(7,
                                              box(background="green",width=12, 
                                                  h3("conditional: appear after predict button pushed"),
                                                  h3("display model + summary"),
                                                  h3("display prediction value")))))
                            )
                          ),
                  tabItem(tabName = "data",
                          tabsetPanel(
                            ##Tab: Diamonds dataset##
                            tabPanel("Diamonds",
                                     fluidRow(
                                       column(5,
                                              h2("Subset Data"),
                                              box(background="green",width=12,
                                                  h3("[subsetting section]"),
                                                  h3("select vars (columns)"),
                                                  h3("filter rows - select var, select value(s)"))),
                                       column(7,
                                              h2("Data Table"),
                                              box(background="green",width=12, 
                                                  h3("display data/subsetted data")))
                                     ),
                                     fluidRow(
                                       column(4),
                                       column(4,
                                              align = "center",
                                              box(background="green",width=12,
                                                  h3("[button to download the current data displayed]"))),
                                       column(4))
                                     ),
                            ##Tab: Pottery dataset##
                            tabPanel("Pottery",
                                     fluidRow(
                                       column(5,
                                              h2("Subset Data"),
                                              box(background="green",width=12,
                                                  h3("[subsetting section]"),
                                                  h3("select vars (columns)"),
                                                  h3("filter rows - select var, select value(s)"))),
                                       column(7,
                                              h2("Data Table"),
                                              box(background="green",width=12, 
                                                  h3("display data/subsetted data")))
                                     ),
                                     fluidRow(
                                       column(4),
                                       column(4,
                                              align = "center",
                                              box(background="green",width=12,
                                                  h3("[button to download the current data displayed]"))),
                                       column(4))
                                     ),
                            ##Tab: Kanga dataset##
                            tabPanel("Kanga",
                                     fluidRow(
                                       column(5,
                                              h2("Subset Data"),
                                              box(background="green",width=12,
                                                  h3("[subsetting section]"),
                                                  h3("select vars (columns)"),
                                                  h3("filter rows - select var, select value(s)"))),
                                       column(7,
                                              h2("Data Table"),
                                              box(background="green",width=12, 
                                                  h3("display data/subsetted data")))
                                     ),
                                     fluidRow(
                                       column(4),
                                       column(4,
                                              align = "center",
                                              box(background="green",width=12,
                                                  h3("[button to download the current data displayed]"))),
                                       column(4))
                                     ),
                            ##Tab: Batting dataset##
                            tabPanel("Batting",
                                     fluidRow(
                                       column(5,
                                              h2("Subset Data"),
                                              box(background="green",width=12,
                                                  h3("[subsetting section]"),
                                                  h3("select vars (columns)"),
                                                  h3("filter rows - select var, select value(s)"))),
                                       column(7,
                                              h2("Data Table"),
                                              box(background="green",width=12, 
                                                  h3("display data/subsetted data")))
                                     ),
                                     fluidRow(
                                       column(4),
                                       column(4,
                                              align = "center",
                                              box(background="green",width=12,
                                                  h3("[button to download the current data displayed]"))),
                                       column(4))
                                     )
                            )
                          )
                  )
                )
              )