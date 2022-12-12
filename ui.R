library(shiny)
library(shinydashboard)
library(DT)           #for displaying data tables
library(tidyverse)    #for data manipulation, graphing, and diamonds data
library(corrplot)     #for correlation plots
library(caret)        #for modeling multiple linear models and predicting with test data
library(nnet)         #for modeling generalized linear models with multiple predictor levels
library(tree)         #for modeling regression/classification trees
library(randomForest) #for modeling random forest models
library(HSAUR3)       #for pottery data
library(faraway)      #for kanga data

#load data sets
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
                                   box(background="green",width=12, h4("This section allows the user to model the available data sets with three kinds of models: a linear regression type model, a tree based model, and a random forest model. Below are the pros and cons of each type of model. Below the that, you will find what models are available for each data set, along with what variables will be the response.")))),
                          fluidRow(
                            column(4,
                                   h3("Multiple Linear Regression/Generalized Linear Regression"),
                                   box(background="green",width=12, 
                                       h4("Pros",
                                          tags$ul(
                                            tags$li("Simple and easy to interpret"),
                                            tags$li("Works well for any data set size"),
                                            tags$li("Able to determine relevance of predictors"),
                                            tags$li("Able to determine outliers"))),
                                       h4("Cons",
                                          tags$ul(
                                            tags$li("Multicollinearity can be a problem"),
                                            tags$li("Does not work with missing data well"),
                                            tags$li("Assumes linear relationship"))))),
                            column(4,
                                   h3("Regression Tree/Classification Tree"),
                                   box(background="green",width=12, 
                                       h4("Pros",
                                          tags$ul(
                                            tags$li("Simple to understand and easy to interpret output"),
                                            tags$li("Predictors don't need to be scaled"),
                                            tags$li("No statistical assumptions necessary"),
                                            tags$li("Built in variable selection"))),
                                       h4("Cons",
                                          tags$ul(
                                            tags$li("Small changes in data can vastly change tree"),
                                            tags$li("Greedy algorithm/No optimal algorithm"),
                                            tags$li("Usually need to prune"))))),
                            column(4,
                                   h3("Random Forest"),
                                   box(background="green",width=12, 
                                       h4("Pros",
                                          tags$ul(
                                            tags$li("Trains fast"),
                                            tags$li("Good for large data sets"),
                                            tags$li("Good prediction accuracy"),
                                            tags$li("Can be used for regression and classification"))),
                                       h4("Cons",
                                          tags$ul(
                                            tags$li("Biased towards data with more variables"),
                                            tags$li("Risk of overfitting"),
                                            tags$li("Can get complex with tuning parameters")))))),
                          fluidRow(
                            column(4,
                                   h3("Diamonds"),
                                   box(background="green",width=12, 
                                       h4("Response: price"),
                                       h4("Available Models:",
                                          tags$ul(
                                            tags$li("Multiple Linear Regression"),
                                            tags$li("Regression Tree"),
                                            tags$li("Random Forest"))))),
                            column(4,
                                   h3("Pottery"),
                                   box(background="green",width=12, 
                                       h4("Response: kiln"),
                                       h4("Available Models:",
                                          tags$ul(
                                            tags$li("Generalized Linear Regression"),
                                            tags$li("Classification Tree"),
                                            tags$li("Random Forest"))))),
                            column(4,
                                   h3("Kanga"),
                                   box(background="green",width=12, 
                                       h4("Response: species"),
                                       h4("Available Models:",
                                          tags$ul(
                                            tags$li("Generalized Linear Regression"),
                                            tags$li("Classification Tree"),
                                            tags$li("Random Forest"))))))
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
                                       column(6,
                                              box(background="green",width=12,
                                                  #select model type
                                                  selectInput("gemSelect",
                                                              "Select Model",
                                                              choices = c("Multiple Linear Regression"="mlr",
                                                                          "Regression Tree"="regtr",
                                                                          "Random Forest"="rf")),
                                                  #variable inputs
                                                  column(6,
                                                         numericInput("caratIn",
                                                                      "Carat",
                                                                      value=0,
                                                                      min = 0,
                                                                      max= 550, #to allow for big gems
                                                                      step=0.01),
                                                         selectInput("cutIn",
                                                                     "Cut",
                                                                     choices = c("Fair",
                                                                                 "Good",
                                                                                 "Very Good",
                                                                                 "Premium",
                                                                                 "Ideal")),
                                                         selectInput("colorIn",
                                                                     "Color",
                                                                     choices = c("J (worst)"="J",
                                                                                 "I",
                                                                                 "H",
                                                                                 "G",
                                                                                 "F",
                                                                                 "E",
                                                                                 "D (best)"="D")),
                                                         selectInput("clarIn",
                                                                     "Clarity",
                                                                     choices = c("I1 (worst)"="I1",
                                                                                 "SI2",
                                                                                 "SI1",
                                                                                 "VS2",
                                                                                 "VS1",
                                                                                 "VVS2",
                                                                                 "VVS1",
                                                                                 "IF (best)"="IF")),
                                                         numericInput("xIn",
                                                                      "Length (x)",
                                                                      value = 0,
                                                                      min = 0,
                                                                      max=100,
                                                                      step = 0.01)),
                                                  column(6,
                                                         numericInput("yIn",
                                                                      "Width (y)",
                                                                      value = 0,
                                                                      min = 0,
                                                                      max=100,
                                                                      step = 0.01),
                                                         numericInput("zIn",
                                                                      "Depth (z)",
                                                                      value = 0,
                                                                      min = 0,
                                                                      max=100,
                                                                      step = 0.01),
                                                         numericInput("depthIn",
                                                                      "Depth % (depth)",
                                                                      value = 0,
                                                                      min = 0,
                                                                      max=100,
                                                                      step = 0.1),
                                                         numericInput("tableIn",
                                                                      "Width % (table)",
                                                                      value = 0,
                                                                      min = 0,
                                                                      max=100,
                                                                      step = 1))
                                              )),
                                       column(6,
                                              box(background="green",width=12, 
                                                  verbatimTextOutput("gemPred"))))
                                       
                                     ),
                            ##Tab: Pottery prediction##
                            tabPanel("Pottery",
                                     fluidRow(
                                       column(12,
                                              h3("Directions"),
                                              box(background="green",width=12, 
                                                  h4("Here, you will have the option of entering values to predict a response. You will be able to specify which model you want to predict with, be it the regression model, tree model, or random forest model. Note that you will need to specify training proportion and the variables for the model you wish to use in the Fitting section to be able to predict. Once you have fit and selected your model, you will be able to input values for your chosen predictors. When you are ready to predict, press the 'Predict' button.")))),
                                     fluidRow(
                                       column(6,
                                              box(background="green",width=12, 
                                                  #select model type
                                                  selectInput("potSelect",
                                                              "Select Model",
                                                              choices = c("Generalized Linear Regression"="glr",
                                                                          "Classification Tree"="classtr",
                                                                          "Random Forest"="rf")),
                                                  #variable inputs
                                                  column(6,
                                                         numericInput("alumtriIn",
                                                                      "Al2O3",
                                                                      value=0,
                                                                      min=0,
                                                                      max=22,
                                                                      step=0.1),
                                                         numericInput("irontriIn",
                                                                      "Fe2O3",
                                                                      value=0,
                                                                      min=0,
                                                                      max=10,
                                                                      step=0.01),
                                                         numericInput("magoxIn",
                                                                      "MgO",
                                                                      value=0,
                                                                      min=0,
                                                                      max=10,
                                                                      step=0.01),
                                                         numericInput("calcoxIn",
                                                                      "CaO",
                                                                      value=0,
                                                                      min=0,
                                                                      max=2,
                                                                      step=0.01),
                                                         numericInput("natoxIn",
                                                                      "Na2O",
                                                                      value=0,
                                                                      min=0,
                                                                      max=1,
                                                                      step=0.01)),
                                                  column(6,
                                                         numericInput("calioxIn",
                                                                      "K2O",
                                                                      value=0,
                                                                      min=0,
                                                                      max=5,
                                                                      step=0.01),
                                                         numericInput("titoxIn",
                                                                      "TiO2",
                                                                      value=0,
                                                                      min=0,
                                                                      max=1.5,
                                                                      step=0.01),
                                                         numericInput("manganoxIn",
                                                                      "MnO",
                                                                      value=0,
                                                                      min=0,
                                                                      max=0.25,
                                                                      step=0.01),
                                                         numericInput("baroxIn",
                                                                      "BaO",
                                                                      value=0,
                                                                      min=0,
                                                                      max=0.1,
                                                                      step=0.01))
                                                  )),
                                       column(6,
                                              box(background="green",width=12, 
                                                  verbatimTextOutput("potPred"))))
                                     ),
                            ##Tab: Kanga prediction##
                            tabPanel("Kanga",
                                     fluidRow(
                                       column(12,
                                              h3("Directions"),
                                              box(background="green",width=12, 
                                                  h4("Here, you will have the option of entering values to predict a response. You will be able to specify which model you want to predict with, be it the regression model, tree model, or random forest model. Note that you will need to specify training proportion and the variables for the model you wish to use in the Fitting section to be able to predict. Once you have fit and selected your model, you will be able to input values for your chosen predictors. When you are ready to predict, press the 'Predict' button.")))),
                                     fluidRow(
                                       column(6,
                                              box(background="green",width=12, 
                                                  #select model type
                                                  selectInput("rooSelect",
                                                              "Select Model",
                                                              choices = c("Generalized Linear Regression"="glr",
                                                                          "Classification Tree"="classtr",
                                                                          "Random Forest"="rf")),
                                                  #variable inputs
                                                  column(6,
                                                         selectInput("sexIn",
                                                                     "Sex",
                                                                     choices = c("Male",
                                                                                 "Female")),
                                                         numericInput("balIn",
                                                                      "Basilar Length",
                                                                      value=1000,
                                                                      min=1000,
                                                                      max=2000,
                                                                      step=1),
                                                         numericInput("oclIn",
                                                                      "Occiptonasal Length",
                                                                      value=1100,
                                                                      min=1100,
                                                                      max=2000,
                                                                      step=1),
                                                         numericInput("palIn",
                                                                      "Palate Length",
                                                                      value=650,
                                                                      min=650,
                                                                      max=1500,
                                                                      step=1),
                                                         numericInput("pawIn",
                                                                      "Palate Width",
                                                                      value=150,
                                                                      min=150,
                                                                      max=350,
                                                                      step=1),
                                                         numericInput("nalIn",
                                                                      "Nasal Length",
                                                                      value=425,
                                                                      min=425,
                                                                      max=900,
                                                                      step=1),
                                                         numericInput("nawIn",
                                                                      "Nasal Width",
                                                                      value=125,
                                                                      min=125,
                                                                      max=325,
                                                                      step=1),
                                                         numericInput("sqdIn",
                                                                      "Squamosal Depth",
                                                                      value=115,
                                                                      min=115,
                                                                      max=300,
                                                                      step=1),
                                                         numericInput("lawIn",
                                                                      "Lacrymal Width",
                                                                      value=300,
                                                                      min=300,
                                                                      max=550,
                                                                      step=1),
                                                         numericInput("zywIn",
                                                                      "Zygomatic Width",
                                                                      value=625,
                                                                      min=625,
                                                                      max=1100,
                                                                      step=1)),
                                                  column(6,
                                                         numericInput("orwIn",
                                                                      "Orbital Width",
                                                                      value=175,
                                                                      min=175,
                                                                      max=300,
                                                                      step=1),
                                                         numericInput("rowIn",
                                                                      "Rostral Width",
                                                                      value=150,
                                                                      min=150,
                                                                      max=375,
                                                                      step=1),
                                                         numericInput("ocdIn",
                                                                      "Occipital Depth",
                                                                      value=425,
                                                                      min=425,
                                                                      max=800,
                                                                      step=1),
                                                         numericInput("crwIn",
                                                                      "Crest Width",
                                                                      value=10,
                                                                      min=10,
                                                                      max=225,
                                                                      step=1),
                                                         numericInput("folIn",
                                                                      "Foramina Length",
                                                                      value=50,
                                                                      min=50,
                                                                      max=150,
                                                                      step=1),
                                                         numericInput("malIn",
                                                                      "Mandible Length",
                                                                      value=850,
                                                                      min=850,
                                                                      max=1600,
                                                                      step=1),
                                                         numericInput("mawIn",
                                                                      "Mandible Width",
                                                                      value=100,
                                                                      min=100,
                                                                      max=175,
                                                                      step=1),
                                                         numericInput("madIn",
                                                                      "Mandible Depth",
                                                                      value=125,
                                                                      min=125,
                                                                      max=275,
                                                                      step=1),
                                                         numericInput("rahIn",
                                                                      "Ramus Height",
                                                                      value=450,
                                                                      min=450,
                                                                      max=900,
                                                                      step=1))
                                                  )),
                                       column(6,
                                              box(background="green",width=12, 
                                                  verbatimTextOutput("rooPred"))))
                                     )
                            )
                          ),
                  tabItem(tabName = "data",
                          tabsetPanel(
                            ##Tab: Diamonds dataset##
                            tabPanel("Diamonds",
                                     fluidRow(
                                       column(3,
                                              h2("Subset Data"),
                                              box(background="green",width=12,
                                                  checkboxGroupInput(
                                                    "gemSubVars",
                                                    "Variable Selection",
                                                    choices = c("carat",
                                                                "cut",
                                                                "color",
                                                                "clarity",
                                                                "x",
                                                                "y",
                                                                "z",
                                                                "price",
                                                                "depth",
                                                                "table"),
                                                    selected = c("carat",
                                                                 "cut",
                                                                 "color",
                                                                 "clarity",
                                                                 "x",
                                                                 "y",
                                                                 "z",
                                                                 "price",
                                                                 "depth",
                                                                 "table")
                                                    ),
                                                  )),
                                       column(9,
                                              h2("Data Table"),
                                              box(background="green",width=12, 
                                                  dataTableOutput("gemSubDat")))
                                     ),
                                     fluidRow(
                                       column(4),
                                       column(4,
                                              align = "center",
                                              box(background="green",width=12,
                                                  h3("Save current data"),
                                                  downloadButton("gemSave",
                                                                 "Save Data"))),
                                       column(4))
                                     ),
                            ##Tab: Pottery dataset##
                            tabPanel("Pottery",
                                     fluidRow(
                                       column(3,
                                              h2("Subset Data"),
                                              box(background="green",width=12,
                                                  checkboxGroupInput(
                                                    "potSubVars",
                                                    "Variable Selection",
                                                    choices = c("Al2O3",
                                                                "Fe2O3",
                                                                "MgO",
                                                                "CaO",
                                                                "Na2O",
                                                                "K2O",
                                                                "TiO2",
                                                                "MnO",
                                                                "BaO",
                                                                "kiln"),
                                                    selected = c("Al2O3",
                                                                 "Fe2O3",
                                                                 "MgO",
                                                                 "CaO",
                                                                 "Na2O",
                                                                 "K2O",
                                                                 "TiO2",
                                                                 "MnO",
                                                                 "BaO",
                                                                 "kiln")
                                                  ))),
                                       column(9,
                                              h2("Data Table"),
                                              box(background="green",width=12, 
                                                  dataTableOutput("potSubDat")))
                                     ),
                                     fluidRow(
                                       column(4),
                                       column(4,
                                              align = "center",
                                              box(background="green",width=12,
                                                  h3("Save current data"),
                                                  downloadButton("potSave",
                                                                 "Save Data"))),
                                       column(4))
                                     ),
                            ##Tab: Kanga dataset##
                            tabPanel("Kanga",
                                     fluidRow(
                                       column(3,
                                              h2("Subset Data"),
                                              box(background="green",width=12,
                                                  h4("Note that this is the original, unfiltered data, not the data used for EDA and modeling."),
                                                  checkboxGroupInput(
                                                    "rooSubVars",
                                                    "Variable Selection",
                                                    choices = c("species"="species",
                                                                "sex"="sex",
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
                                                                "ramus.height"),
                                                    selected = c("species",
                                                                 "sex",
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
                                                                 "ramus.height"))
                                                  )),
                                       column(9,
                                              h2("Data Table"),
                                              box(background="green",width=12, 
                                                  dataTableOutput("rooSubDat")))
                                     ),
                                     fluidRow(
                                       column(4),
                                       column(4,
                                              align = "center",
                                              box(background="green",width=12,
                                                  h3("Save current data"),
                                                  downloadButton("rooSave",
                                                                 "Save Data"))),
                                       column(4))
                                     )
                            )
                          )
                  )
                )
              )