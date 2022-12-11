library(shiny)
library(shinydashboard)
library(DT)           #for displaying data tables
library(tidyverse)    #for data manipulation, graphing, and diamonds data
library(corrplot)     #for correlation plots
library(caret)        #for modeling multiple/generalized linear models
library(tree)         #for modeling regression/classification trees
library(randomForest) #for modeling random forest models
library(HSAUR3)       #for pottery data
library(faraway)      #for kanga data

#load datasets
data("diamonds")
data("pottery", package = "HSAUR3")
data("kanga")
roo <- na.omit(kanga)


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
                                       h4("The purpose of this application is to explore and model three different data sets depending on the user's choice.")),
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
                                     column(4,
                                            align = "center",
                                            box(background="green",width=12,
                                                h2("Diamonds"),
                                                h4("Found in the ", code("ggplot2"), " package, this data set includes prices and other attributes of almost 54,000 daimonds. More information about the data can be found ", strong(a(href="https://ggplot2.tidyverse.org/reference/diamonds.html", "here.", style = "color:pink"))),
                                                img(src="diamond.jpg", width = 200))),
                                     column(4,
                                            align = "center",
                                            box(background="green",width=12,
                                                h2("Pottery"),
                                                h4("Found in the ", code("HSAUR3"), " package, this data set includes chemical compositions of Romano-British pottery. More information about the data can be found ", strong(a(href="https://cran.r-project.org/web/packages/HSAUR3/HSAUR3.pdf", "here.", style = "color:pink"))),
                                                img(src="pottery.jpg", width = 200))),
                                     column(4,
                                            align = "center",
                                            box(background="green",width=12,
                                                h2("Kanga"),
                                                h4("Found in the ", code("faraway"), " package, this data set includes skull measurements of different species of kangaroo. Observations with missing values have been omitted here. More information about the data can be found ", strong(a(href="https://www.rdocumentation.org/packages/faraway/versions/1.0.8/topics/kanga", "here.", style = "color:pink"))),
                                                img(src="kanga.jpg", width = 200))))
                                   )
                          )
                  ),
                  
                  tabItem(tabName = "eda",
                          tabsetPanel(
                            ##Tab: Diamonds data EDA##
                            tabPanel("Diamonds",
                                     fluidRow(
                                       column(12, h2("Exploratory Data Analysis"))),
                                     fluidRow(
                                       column(3,
                                              box(background="green",width=12,
                                                  selectInput(
                                                    "gemNum",
                                                    "Numerical Variables",
                                                    choices = c("Price"="price",
                                                                "Carat"="carat",
                                                                "Length (x)"="x",
                                                                "Width (y)"="y",
                                                                "Depth (z)"="z",
                                                                "Depth percentage (depth)"="depth",
                                                                "Top width relative to widest point (table)"="table")),
                                                  h5(strong("Summary Stats")),
                                                  verbatimTextOutput("gemStats"), 
                                                  selectInput(
                                                    "gemCat",
                                                    "Categorical Variables",
                                                    choices = c("Cut"="cut",
                                                                "Color"="color",
                                                                "Clarity"="clarity")), 
                                                  h5(strong("Levels and Counts")),
                                                  tableOutput("gemLevels"))),
                                       column(9,
                                              box(background="green",width=12,
                                                  selectInput(
                                                    "gemGraphs",
                                                    "Graph Selection",
                                                    #cut, color, clarity opts for boxplots, scatterplots
                                                    choices = c("Correlation Plot"="corrMap",
                                                                "Price Boxplot"="priceBox",
                                                                "Carat Boxplot"="carBox",  
                                                                "Length Boxplot"="xBox",
                                                                "Width Boxplot"="yBox",
                                                                "Depth Boxplot"="zBox",
                                                                "Cut Barplot"="cutBar",      #color, clarity opts
                                                                "Color Barplot"="colBar",    #cut, clarity opts
                                                                "Clarity Barplot"="clrBar",  #cut, color opts
                                                                "Length by Width Scatterplot"="xyScat",
                                                                "Length by Depth Scatterplot"="xzScat",
                                                                "Width by Depth Scatterplot"="yzScat",
                                                                "Depth by Depth % Scatterplot"="zdepthScat",
                                                                "Width by Width % Scatterplot"="ytableScat",
                                                                "Carat by Price Scatterplot"="carpriceScat",
                                                                "Length by Price Scatterplot"="xpriceScat",
                                                                "Width by Price Scatterplot"="ypriceScat",
                                                                "Depth by Price Scatterplot"="zpriceScat")
                                                    ),
                                                  conditionalPanel("input.gemGraphs == 'priceBox' ||
                                                                   input.gemGraphs == 'carBox' ||
                                                                   input.gemGraphs == 'xBox' ||
                                                                   input.gemGraphs == 'yBox' ||
                                                                   input.gemGraphs == 'zBox' ||
                                                                   input.gemGraphs == 'xyScat' ||
                                                                   input.gemGraphs == 'xzScat' ||
                                                                   input.gemGraphs == 'yzScat' ||
                                                                   input.gemGraphs == 'zdepthScat' ||
                                                                   input.gemGraphs == 'ytableScat' ||
                                                                   input.gemGraphs == 'carpriceScat' ||
                                                                   input.gemGraphs == 'xpriceScat' ||
                                                                   input.gemGraphs == 'ypriceScat' ||
                                                                   input.gemGraphs == 'zpriceScat'",
                                                                   radioButtons(
                                                                     "gemBoxScat",
                                                                     "Group by:",
                                                                     choices = c("none",
                                                                                 "cut",
                                                                                 "color",
                                                                                 "clarity"),
                                                                     selected = "none",
                                                                     inline = TRUE
                                                                     )
                                                                   ),
                                                  conditionalPanel("input.gemGraphs == 'cutBar'",
                                                                   radioButtons(
                                                                     "gemCut",
                                                                     "Group by:",
                                                                     choices = c("color",
                                                                                 "clarity",
                                                                                 "neither"),
                                                                     selected = "neither",
                                                                     inline = TRUE
                                                                   )),
                                                  conditionalPanel("input.gemGraphs == 'colBar'",
                                                                   radioButtons(
                                                                     "gemCol",
                                                                     "Group by:",
                                                                     choices = c("cut",
                                                                                 "clarity",
                                                                                 "neither"),
                                                                     selected = "neither",
                                                                     inline = TRUE
                                                                   )),
                                                  conditionalPanel("input.gemGraphs == 'clrBar'",
                                                                   radioButtons(
                                                                     "gemClr",
                                                                     "Group by:",
                                                                     choices = c("cut",
                                                                                 "color",
                                                                                 "neither"),
                                                                     selected = "neither",
                                                                     inline = TRUE
                                                                   ))
                                                  ),
                                              box(background="green",width=12,
                                                  plotOutput("gemPlot"))))
                                     ),
                            
                            ##Tab: Pottery data EDA##
                            tabPanel("Pottery",
                                     fluidRow(
                                       column(12, h2("Exploratory Data Analysis"))),
                                     fluidRow(
                                       column(3,
                                              box(background="green",width=12,
                                                  selectInput(
                                                    "potNum",
                                                    "Numerical Variables",
                                                    choices = c("Aluminium Trioxide (Al2O3)"="Al2O3",
                                                                "Iron Trioxide (Fe2O3)"="Fe2O3",
                                                                "Magnesium Oxide (MgO)"="MgO",
                                                                "Calcium Oxide (CaO)"="CaO",
                                                                "Natrium Oxide (Na2O)"="Na2O",
                                                                "Calium Oxide(K2O)"="K2O",
                                                                "Titanium Oxide (TiO2)"="TiO2",
                                                                "Mangan Oxide (MnO)"="MnO",
                                                                "Barium Oxide (BaO)"="BaO")),
                                                  h5(strong("Summary Stats")),
                                                  verbatimTextOutput("potStats"), 
                                                  selectInput(
                                                    "potCat",
                                                    "Categorical Variables",
                                                    choices = c("Site found (kiln)"="kiln")), 
                                                  h5(strong("Levels and Counts")),
                                                  tableOutput("potLevels"))
                                              ),
                                       column(9,
                                              box(background="green",width=12,
                                                  selectInput(
                                                    "potGraphs",
                                                    "Graph Selection",
                                                    #Kiln opts for boxplots, histograms
                                                    choices = c("Correlation Plot"="corrMap",
                                                                "Al2O3 Boxplot"="Al2O3Box",  
                                                                "Fe2O3 Boxplot"="Fe2O3Box",
                                                                "MgO Boxplot"="MgOBox",
                                                                "CaO Boxplot"="CaOBox",
                                                                "Na2O Boxplot"="Na2OBox",
                                                                "K2O Boxplot"="K2OBox",
                                                                "TiO2 Boxplot"="TiO2Box",
                                                                "MnO Boxplot"="MnOBox",
                                                                "BaO Boxplot"="BaOBox",
                                                                "Al2O3 Histogram"="Al2O3Hist",  
                                                                "Fe2O3 Histogram"="Fe2O3Hist",
                                                                "MgO Histogram"="MgOHist",
                                                                "CaO Histogram"="CaOHist",
                                                                "Na2O Histogram"="Na2OHist",
                                                                "K2O Histogram"="K2OHist",
                                                                "TiO2 Histogram"="TiO2Hist",
                                                                "MnO Histogram"="MnOHist",
                                                                "BaO Histogram"="BaOHist")
                                                    ),
                                                  conditionalPanel("input.potGraphs == 'Al2O3Box' ||
                                                                   input.potGraphs == 'Fe2O3Box' ||
                                                                   input.potGraphs == 'MgOBox' ||
                                                                   input.potGraphs == 'CaOBox' ||
                                                                   input.potGraphs == 'Na2OBox' ||
                                                                   input.potGraphs == 'K2OBox' ||
                                                                   input.potGraphs == 'TiO2Box' ||
                                                                   input.potGraphs == 'MnOBox' ||
                                                                   input.potGraphs == 'BaOBox' ||
                                                                   input.potGraphs == 'Al2O3Hist' ||
                                                                   input.potGraphs == 'Fe2O3Hist' ||
                                                                   input.potGraphs == 'MgOHist' ||
                                                                   input.potGraphs == 'CaOHist' ||
                                                                   input.potGraphs == 'Na2OHist' ||
                                                                   input.potGraphs == 'K2OHist' ||
                                                                   input.potGraphs == 'TiO2Hist' ||
                                                                   input.potGraphs == 'MnOHist' ||
                                                                   input.potGraphs == 'BaOHist'",
                                                                   radioButtons(
                                                                     "opt",
                                                                     "Group by Kiln?",
                                                                     choices = c("no",
                                                                                 "yes"),
                                                                     selected = "no",
                                                                     inline = TRUE
                                                                   ))
                                                  ),
                                              box(background="green",width=12,
                                                  plotOutput("potPlot"))))
                                     ),
                            
                            ##Tab: Kanga data EDA##
                            tabPanel("Kanga",
                                     fluidRow(
                                       column(12, h2("Exploratory Data Analysis"))),
                                     fluidRow(
                                       column(3,
                                              box(background="green",width=12,
                                                  selectInput(
                                                    "rooNum",
                                                    "Numerical Variables",
                                                    choices = c("Basilar Length"="basilar.length",
                                                                "Occipitonasal Length"="occipitonasal.length",
                                                                "Palate Length"="palate.length",
                                                                "Palate Width"="palate.width",
                                                                "Nasal Length"="nasal.length",
                                                                "Nasal Width"="nasal.width",
                                                                "Squamosal Depth"="squamosal.depth",
                                                                "Lacrymal Width"="lacrymal.width",
                                                                "Zygomatic Width"="zygomatic.width",
                                                                "Orbital Width"="orbital.width",
                                                                "Rostal Width"=".rostral.width",
                                                                "Occipital Depth"="occipital.depth",
                                                                "Crest Width"="crest.width",
                                                                "Foramina Length"="foramina.length",
                                                                "Mandible Length"="mandible.length",
                                                                "Mandible Width"="mandible.width",
                                                                "Mandible Depth"="mandible.depth",
                                                                "Ramus Height"="ramus.height")),
                                                  h5(strong("Summary Stats")),
                                                  verbatimTextOutput("rooStats"), 
                                                  selectInput(
                                                    "rooCat",
                                                    "Categorical Variables",
                                                    choices = c("Species"="species",
                                                                "Sex"="sex")), 
                                                  h5(strong("Levels and Counts")),
                                                  tableOutput("rooLevels"))),
                                       column(9,
                                              box(background="green",width=12,
                                                  selectInput(
                                                    "rooGraphs",
                                                    "Graph Selection",
                                                    #sex, species opts for boxplots, histograms, scatterplots
                                                    choices = c("Correlation Plot"="corrMap",
                                                                "Basilar Length Boxplot"="baLBox",  
                                                                "Occipitonasal Length Boxplot"="ocLBox",
                                                                "Palate Length Boxplot"="paLBox",
                                                                "Palate Width Boxplot"="paWBox",
                                                                "Nasal Length Boxplot"="naLBox",
                                                                "Nasal Width Boxplot"="naWBox",
                                                                "Squamosal Depth Boxplot"="sqDBox",
                                                                "Lacrymal Width Boxplot"="laWBox",
                                                                "Zygomatic Width Boxplot"="zyWBox",
                                                                "Orbital Width Boxplot"="orWBox",
                                                                "Rostal Width Boxplot"="roWBox",
                                                                "Occipital Depth Boxplot"="ocDBox",
                                                                "Crest Width Boxplot"="crWBox",
                                                                "Foramina Length Boxplot"="foLBox",
                                                                "Mandible Length Boxplot"="maLBox",
                                                                "Mandible Width Boxplot"="maWBox",
                                                                "Mandible Depth Boxplot"="maDBox",
                                                                "Ramus Height Boxplot"="raHBox",
                                                                "Basilar Length Histogram"="baLHist",  
                                                                "Occipitonasal Length Histogram"="ocLHist",
                                                                "Palate Length Histogram"="paLHist",
                                                                "Palate Width Histogram"="paWHist",
                                                                "Nasal Length Histogram"="naLHist",
                                                                "Nasal Width Histogram"="naWHist",
                                                                "Squamosal Depth Histogram"="sqDHist",
                                                                "Lacrymal Width Histogram"="laWHist",
                                                                "Zygomatic Width Histogram"="zyWHist",
                                                                "Orbital Width Histogram"="orWHist",
                                                                "Rostal Width Histogram"="roWHist",
                                                                "Occipital Depth Histogram"="ocDHist",
                                                                "Crest Width Histogram"="crWHist",
                                                                "Foramina Length Histogram"="foLHist",
                                                                "Mandible Length Histogram"="maLHist",
                                                                "Mandible Width Histogram"="maWHist",
                                                                "Mandible Depth Histogram"="maDHist",
                                                                "Ramus Height Histogram"="raHHist",
                                                                "Species Barplot"="spBar",#sex opt
                                                                "Sex Barplot"="seBar",    #species opt
                                                                "Palate Length by Palate Width Scatterplot"="paLWScat",
                                                                "Nasal Length by Nasal Width Scatterplot"="naLWScat",
                                                                "Mandible Length by Mandible Width Scatterplot"="maLWScat",
                                                                "Mandible Length by Mandible Depth Scatterplot"="maLDScat",
                                                                "Mandible Width by Mandible Depth Scatterplot"="maWDScat")
                                                    ),
                                                  conditionalPanel("input.rooGraphs == 'baLBox' ||
                                                                   input.rooGraphs == 'ocLBox' ||
                                                                   input.rooGraphs == 'paLBox' ||
                                                                   input.rooGraphs == 'paWBox' ||
                                                                   input.rooGraphs == 'naLBox' ||
                                                                   input.rooGraphs == 'naWBox' ||
                                                                   input.rooGraphs == 'sqDBox' ||
                                                                   input.rooGraphs == 'laWBox' ||
                                                                   input.rooGraphs == 'zyWBox' ||
                                                                   input.rooGraphs == 'orWBox' ||
                                                                   input.rooGraphs == 'roWBox' ||
                                                                   input.rooGraphs == 'ocDBox' ||
                                                                   input.rooGraphs == 'crWBox' ||
                                                                   input.rooGraphs == 'foLBox' ||
                                                                   input.rooGraphs == 'maLBox' ||
                                                                   input.rooGraphs == 'maWBox' ||
                                                                   input.rooGraphs == 'maDBox' ||
                                                                   input.rooGraphs == 'raHBox' ||
                                                                   input.rooGraphs == 'baLHist' ||
                                                                   input.rooGraphs == 'ocLHist' ||
                                                                   input.rooGraphs == 'paLHist' ||
                                                                   input.rooGraphs == 'paWHist' ||
                                                                   input.rooGraphs == 'naLHist' ||
                                                                   input.rooGraphs == 'naWHist' ||
                                                                   input.rooGraphs == 'sqDHist' ||
                                                                   input.rooGraphs == 'laWHist' ||
                                                                   input.rooGraphs == 'zyWHist' ||
                                                                   input.rooGraphs == 'orWHist' ||
                                                                   input.rooGraphs == 'roWHist' ||
                                                                   input.rooGraphs == 'ocDHist' ||
                                                                   input.rooGraphs == 'crWHist' ||
                                                                   input.rooGraphs == 'foLHist' ||
                                                                   input.rooGraphs == 'maLHist' ||
                                                                   input.rooGraphs == 'maWHist' ||
                                                                   input.rooGraphs == 'maDHist' ||
                                                                   input.rooGraphs == 'raHHist' ||
                                                                   input.rooGraphs == 'paLWScat' ||
                                                                   input.rooGraphs == 'naLWScat' ||
                                                                   input.rooGraphs == 'maLWScat' ||
                                                                   input.rooGraphs == 'maLDScat' ||
                                                                   input.rooGraphs == 'maWDScat'",
                                                                   radioButtons(
                                                                     "rooBHS",
                                                                     "Group by:",
                                                                     choices = c("sex",
                                                                                 "species",
                                                                                 "neither"),
                                                                     selected = "neither",
                                                                     inline = TRUE
                                                                   )),
                                                  
                                                  conditionalPanel("input.rooGraphs == 'spBar'",
                                                                   radioButtons(
                                                                     "speciesBar",
                                                                     "Group by sex?",
                                                                     choices = c("no",
                                                                                 "yes"),
                                                                     selected = "no",
                                                                     inline = TRUE
                                                                   )),
                                                  conditionalPanel("input.rooGraphs == 'seBar'",
                                                                   radioButtons(
                                                                     "sexBar",
                                                                     "Group by species?",
                                                                     choices = c("no",
                                                                                 "yes"),
                                                                     selected = "no",
                                                                     inline = TRUE
                                                                   ))
                                                  
                                                  ),
                                              box(background="green",width=12,
                                                  plotOutput("rooPlot"))))
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
                            column(4,
                                   h3("Diamonds"),
                                   box(background="green",width=12, 
                                       h4("[predicting [price]; MLR/Reg Tree/Rand Forest]"))),
                            column(4,
                                   h3("Pottery"),
                                   box(background="green",width=12, 
                                       h4("[predicting [kiln]; GLR/Class Tree/Rand Forest]"))),
                            column(4,
                                   h3("Kanga"),
                                   box(background="green",width=12, 
                                       h4("[predicting [species]; GLR/Class Tree/Rand Forest]"))))
                          ),
                  tabItem(tabName = "modFit",
                          tabsetPanel(
                            ##Tab: Diamonds modeling##
                            tabPanel("Diamonds",
                                     fluidRow(
                                       column(12,
                                              h3("Directions"),
                                              box(background="green",width=12, 
                                                  h4("Before fitting models, select the proportion of data you wish to use for training the models. You are allowed a minimum of 50%, and a maximum of 85%. The default is set at 70%."),
                                                  h4("For each model, select the predictors you wish to use. Once you have chosen your predictor variables for a given model, a summary of the model will be displayed, as well as fit criteria. Please note that you must choose at least one variable, as not choosing any variable will result in an error.")))),
                                     fluidRow(
                                       column(12,
                                              box(background="green",width=12,
                                                  numericInput("gemTrain",
                                                               "Proportion of Training Data",
                                                               value = 0.7,
                                                               min = 0.5,
                                                               max = 0.85,
                                                               step = 0.01)))),
                                     fluidRow(
                                       column(4,
                                              h3("Multiple Linear Regression Model"),
                                              box(background="green",width=12, 
                                                  checkboxGroupInput(
                                                    "gemMLRVars",
                                                    "Variable Selection",
                                                    choices = c("carat",
                                                                "cut",
                                                                "color",
                                                                "clarity",
                                                                "x",
                                                                "y",
                                                                "z",
                                                                "depth",
                                                                "table")
                                                  ),
                                                  verbatimTextOutput("gemMLR"))),
                                       column(4,
                                              h3("Regression Tree"),
                                              box(background="green",width=12, 
                                                  checkboxGroupInput(
                                                    "gemTreeVars",
                                                    "Variable Selection",
                                                    choices = c("carat",
                                                                "cut",
                                                                "color",
                                                                "clarity",
                                                                "x",
                                                                "y",
                                                                "z",
                                                                "depth",
                                                                "table")
                                                  ),
                                                  verbatimTextOutput("gemTree"))),
                                       column(4,
                                              h3("Random Forest"),
                                              box(background="green",width=12, 
                                                  checkboxGroupInput(
                                                    "gemForestVars",
                                                    "Variable Selection",
                                                    choices = c("carat",
                                                                "cut",
                                                                "color",
                                                                "clarity",
                                                                "x",
                                                                "y",
                                                                "z",
                                                                "depth",
                                                                "table")
                                                  ),
                                                  verbatimTextOutput("gemRand")))),
                                     fluidRow(
                                       column(2),
                                       column(8, 
                                              box(background="green",width=12, 
                                              h4("The best fitting model has the lowest root mean square error (RMSE)."))),
                                       column(2))
                                     ),
                            ##Tab: Pottery modeling##
                            tabPanel("Pottery",
                                     fluidRow(
                                       column(12,
                                              h3("Directions"),
                                              box(background="green",width=12, 
                                                  h4("Before fitting models, select the proportion of data you wish to use for training the models. You are allowed a minimum of 50%, and a maximum of 85%. The default is set at 70%."),
                                                  h4("For each model, select the predictors you wish to use. Once you have chosen your predictor variables for a given model, press the 'Fit model' button to display a summary of the model, as well as fit criteria. Please note that you must choose at least one variable, as not choosing any variable will result in an error.")))),
                                     fluidRow(
                                       column(12,
                                              box(background="green",width=12, 
                                                  numericInput("potTrain",
                                                               "Proportion of Training Data",
                                                               value = 0.7,
                                                               min = 0.5,
                                                               max = 0.85,
                                                               step = 0.01)))),
                                     fluidRow(
                                       column(4,
                                              h3("Generalized Linear Regression Model"),
                                              box(background="green",width=12, 
                                                  checkboxGroupInput(
                                                    "potGLRVars",
                                                    "Variable Selection",
                                                    choices = c("Al2O3",
                                                                "Fe2O3",
                                                                "MgO",
                                                                "CaO",
                                                                "Na2O",
                                                                "K2O",
                                                                "TiO2",
                                                                "MnO",
                                                                "BaO")
                                                  ),
                                                  verbatimTextOutput("potGLR"))),
                                       column(4,
                                              h3("Classification Tree"),
                                              box(background="green",width=12, 
                                                  checkboxGroupInput(
                                                    "potTreeVars",
                                                    "Variable Selection",
                                                    choices = c("Al2O3",
                                                                "Fe2O3",
                                                                "MgO",
                                                                "CaO",
                                                                "Na2O",
                                                                "K2O",
                                                                "TiO2",
                                                                "MnO",
                                                                "BaO")
                                                  ),
                                                  verbatimTextOutput("potTree"))),
                                       column(4,
                                              h3("Random Forest"),
                                              box(background="green",width=12, 
                                                  checkboxGroupInput(
                                                    "potForestVars",
                                                    "Variable Selection",
                                                    choices = c("Al2O3",
                                                                "Fe2O3",
                                                                "MgO",
                                                                "CaO",
                                                                "Na2O",
                                                                "K2O",
                                                                "TiO2",
                                                                "MnO",
                                                                "BaO")
                                                  ),
                                                  verbatimTextOutput("potRand")))),
                                     fluidRow(
                                       column(2),
                                       column(8, 
                                              box(background="green",width=12, 
                                              h4("The best fitting model has the lowest misclassification error rate."))),
                                       column(2))
                                     ),
                            ##Tab: Kanga modeling##
                            tabPanel("Kanga",
                                     fluidRow(
                                       column(12,
                                              h3("Directions"),
                                              box(background="green",width=12, 
                                                  h4("Before fitting models, select the proportion of data you wish to use for training the models. You are allowed a minimum of 50%, and a maximum of 85%. The default is set at 70%."),
                                                  h4("For each model, select the predictors you wish to use. Once you have chosen your predictor variables for a given model, press the 'Fit model' button to display a summary of the model, as well as fit criteria. Please note that you must choose at least one variable, as not choosing any variable will result in an error.")))),
                                     fluidRow(
                                       column(12,
                                              box(background="green",width=12, 
                                                  numericInput("rooTrain",
                                                               "Proportion of Training Data",
                                                               value = 0.7,
                                                               min = 0.5,
                                                               max = 0.85,
                                                               step = 0.01)))),
                                     fluidRow(
                                       column(4,
                                              h3("Generalized Linear Regression Model"),
                                              box(background="green",width=12, 
                                                  checkboxGroupInput(
                                                    "rooGLRVars",
                                                    "Variable Selection",
                                                    choices = c("sex",
                                                                "basilar.length",
                                                                "occipitonasal.length",
                                                                "palate.length",
                                                                "palate.width",
                                                                "nasal.length",
                                                                "nasal.width",
                                                                "squamosal.depth",
                                                                "lacrymal.width",
                                                                "zygomatic.width",
                                                                "orbital.width",
                                                                ".rostral.width",
                                                                "occipital.depth",
                                                                "crest.width",
                                                                "foramina.length",
                                                                "mandible.length",
                                                                "mandible.width",
                                                                "mandible.depth",
                                                                "ramus.height")
                                                  ),
                                                  verbatimTextOutput("rooGLR"))),
                                       column(4,
                                              h3("Classification Tree"),
                                              box(background="green",width=12, 
                                                  checkboxGroupInput(
                                                    "rooTreeVars",
                                                    "Variable Selection",
                                                    choices = c("sex",
                                                                "basilar.length",
                                                                "occipitonasal.length",
                                                                "palate.length",
                                                                "palate.width",
                                                                "nasal.length",
                                                                "nasal.width",
                                                                "squamosal.depth",
                                                                "lacrymal.width",
                                                                "zygomatic.width",
                                                                "orbital.width",
                                                                ".rostral.width",
                                                                "occipital.depth",
                                                                "crest.width",
                                                                "foramina.length",
                                                                "mandible.length",
                                                                "mandible.width",
                                                                "mandible.depth",
                                                                "ramus.height")
                                                  ),
                                                  verbatimTextOutput("rooTree"))),
                                       column(4,
                                              h3("Random Forest"),
                                              box(background="green",width=12, 
                                                  checkboxGroupInput(
                                                    "rooForestVars",
                                                    "Variable Selection",
                                                    choices = c("sex",
                                                                "basilar.length",
                                                                "occipitonasal.length",
                                                                "palate.length",
                                                                "palate.width",
                                                                "nasal.length",
                                                                "nasal.width",
                                                                "squamosal.depth",
                                                                "lacrymal.width",
                                                                "zygomatic.width",
                                                                "orbital.width",
                                                                ".rostral.width",
                                                                "occipital.depth",
                                                                "crest.width",
                                                                "foramina.length",
                                                                "mandible.length",
                                                                "mandible.width",
                                                                "mandible.depth",
                                                                "ramus.height")
                                                  ),
                                                  verbatimTextOutput("rooRand")))),
                                     fluidRow(
                                       column(2),
                                       column(8, 
                                              box(background="green",width=12, 
                                              h4("The best fitting model has the lowest misclassification error rate."))),
                                       column(2))
                                     )
                            )
                          ),

                  tabItem(tabName = "modPred",
                          tabsetPanel(
                            ##Tab: Diamonds prediction##
                            tabPanel("Diamonds",
                                     fluidRow(
                                       column(12,
                                              h3("Directions"),
                                              box(background="green",width=12, 
                                                  h4("Here, you will have the option of entering values to predict a response. You will be able to specify which model you want to predict with, be it the regression model, tree model, or random forest model. Note that you will need to specify training proportion and the variables for the model you wish to use in the Fitting section to be able to predict. Once you have fit and selected your model, you will be able to input values for your chosen predictors. When you are ready to predict, press the 'Predict' button.")))),
                                     fluidRow(
                                       column(5,
                                              box(background="green",width=12, 
                                                  selectInput("gemSelect",
                                                              "Select Model",
                                                              choices = c("Multiple Linear Regression"="mlr",
                                                                          "Regression Tree"="regtr",
                                                                          "Random Forest"="rf")
                                                  ),
                                                  h3("predictor value input"))),
                                       column(7,
                                              box(background="green",width=12, 
                                                  h3("display model + summary"),
                                                  h3("display prediction"))))
                                     ),
                            ##Tab: Pottery prediction##
                            tabPanel("Pottery",
                                     fluidRow(
                                       column(12,
                                              h3("Directions"),
                                              box(background="green",width=12, 
                                                  h4("Here, you will have the option of entering values to predict a response. You will be able to specify which model you want to predict with, be it the regression model, tree model, or random forest model. Note that you will need to specify training proportion and the variables for the model you wish to use in the Fitting section to be able to predict. Once you have fit and selected your model, you will be able to input values for your chosen predictors. When you are ready to predict, press the 'Predict' button.")))),
                                     fluidRow(
                                       column(5,
                                              box(background="green",width=12, 
                                                  selectInput("potSelect",
                                                              "Select Model",
                                                              choices = c("Generalized Linear Regression"="glr",
                                                                          "Classification Tree"="classtr",
                                                                          "Random Forest"="rf")
                                                  ),
                                                  h3("predictor value input"))),
                                       column(7,
                                              box(background="green",width=12,
                                                  h3("display model + summary"),
                                                  h3("display prediction"))))
                                     ),
                            ##Tab: Kanga prediction##
                            tabPanel("Kanga",
                                     fluidRow(
                                       column(12,
                                              h3("Directions"),
                                              box(background="green",width=12, 
                                                  h4("Here, you will have the option of entering values to predict a response. You will be able to specify which model you want to predict with, be it the regression model, tree model, or random forest model. Note that you will need to specify training proportion and the variables for the model you wish to use in the Fitting section to be able to predict. Once you have fit and selected your model, you will be able to input values for your chosen predictors. When you are ready to predict, press the 'Predict' button.")))),
                                     fluidRow(
                                       column(5,
                                              box(background="green",width=12, 
                                                  selectInput("rooSelect",
                                                              "Select Model",
                                                              choices = c("Generalized Linear Regression"="glr",
                                                                          "Classification Tree"="classtr",
                                                                          "Random Forest"="rf")
                                                  ),
                                                  h3("predictor value input"))),
                                       column(7,
                                              box(background="green",width=12,
                                                  h3("display model + summary"),
                                                  h3("display prediction"))))
                                     )
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
                                                  h3("choose between original data and already filtered data"),
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