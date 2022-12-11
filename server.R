library(shiny)
library(shinydashboard)
library(DT)       #for displaying data tables
library(tidyverse)#for data manipulation, graphing, and diamonds data
library(corrplot) #for correlation plots
library(caret)    #for modeling
library(HSAUR3)   #for pottery data
library(faraway)  #for kanga data
#load datasets
data("diamonds")
data("pottery", package = "HSAUR3")
data("kanga")
roo <- na.omit(kanga)


shinyServer(function(input, output, session) {
  
  ##### Diamonds #####
  #Numerical Var summaries
  output$gemStats <- renderPrint({
    as.table(summary(diamonds[,input$gemNum]))
  })
  
  #Categorical Var Summaries
  output$gemLevels <-renderTable({
    table(diamonds[,input$gemCat])
  })
  
  #Graphs
  output$gemPlot <- renderPlot({
  #base plot
    gem <- ggplot(diamonds)

  #correlation plot
    if(input$gemGraphs=="corrMap"){corrplot(cor(diamonds[,c(1,5:10)]))} 
    
  #boxplots
    else if(input$gemGraphs=="priceBox"){
      if(input$gemBoxScat=="none"){
        gem+geom_boxplot(aes(price))}
      else if(input$gemBoxScat=="cut"){
        gem+geom_boxplot(aes(price, color=cut))}
      else if(input$gemBoxScat=="color"){
        gem+geom_boxplot(aes(price, color=color))}
      else if(input$gemBoxScat=="clarity"){
        gem+geom_boxplot(aes(price, color=clarity))}
      }
    else if(input$gemGraphs=="carBox"){
      if(input$gemBoxScat=="none"){
        gem+geom_boxplot(aes(carat))}
      else if(input$gemBoxScat=="cut"){
        gem+geom_boxplot(aes(carat, color=cut))}
      else if(input$gemBoxScat=="color"){
        gem+geom_boxplot(aes(carat, color=color))}
      else if(input$gemBoxScat=="clarity"){
        gem+geom_boxplot(aes(carat, color=clarity))}
    }
    else if(input$gemGraphs=="xBox"){
      if(input$gemBoxScat=="none"){
        gem+geom_boxplot(aes(x))}
      else if(input$gemBoxScat=="cut"){
        gem+geom_boxplot(aes(x, color=cut))}
      else if(input$gemBoxScat=="color"){
        gem+geom_boxplot(aes(x, color=color))}
      else if(input$gemBoxScat=="clarity"){
        gem+geom_boxplot(aes(x, color=clarity))}
    }
    else if(input$gemGraphs=="yBox"){
      if(input$gemBoxScat=="none"){
        gem+geom_boxplot(aes(y))}
      else if(input$gemBoxScat=="cut"){
        gem+geom_boxplot(aes(y, color=cut))}
      else if(input$gemBoxScat=="color"){
        gem+geom_boxplot(aes(y, color=color))}
      else if(input$gemBoxScat=="clarity"){
        gem+geom_boxplot(aes(y, color=clarity))}
    }
    else if(input$gemGraphs=="zBox"){
      if(input$gemBoxScat=="none"){
        gem+geom_boxplot(aes(z))}
      else if(input$gemBoxScat=="cut"){
        gem+geom_boxplot(aes(z, color=cut))}
      else if(input$gemBoxScat=="color"){
        gem+geom_boxplot(aes(z, color=color))}
      else if(input$gemBoxScat=="clarity"){
        gem+geom_boxplot(aes(z, color=clarity))}
    }
    
  #barplots
    else if(input$gemGraphs=="cutBar"){
      if(input$gemCut=="neither"){
        gem+geom_bar(aes(cut))}
      else if(input$gemCut=="color"){
        gem+geom_bar(aes(cut, fill=color))}
      else if(input$gemCut=="clarity"){
        gem+geom_bar(aes(cut, fill=clarity))}
    }
    else if(input$gemGraphs=="colBar"){
      if(input$gemCol=="neither"){
        gem+geom_bar(aes(color))}
      else if(input$gemCol=="cut"){
        gem+geom_bar(aes(color, fill=cut))}
      else if(input$gemCol=="clarity"){
        gem+geom_bar(aes(color, fill=clarity))}
    }
    else if(input$gemGraphs=="clrBar"){
      if(input$gemBoxScat=="neither"){
        gem+geom_bar(aes(clarity))}
      else if(input$gemBoxScat=="cut"){
        gem+geom_bar(aes(clarity, fill=cut))}
      else if(input$gemBoxScat=="color"){
        gem+geom_bar(aes(clarity, fill=color))}
    }
    
  #scatterplots
    else if(input$gemGraphs=="xyScat"){
      if(input$gemBoxScat=="none"){
        gem+geom_point(aes(x=x,y=y))}
      else if(input$gemBoxScat=="cut"){
        gem+geom_point(aes(x=x,y=y, color=cut))}
      else if(input$gemBoxScat=="color"){
        gem+geom_point(aes(x=x,y=y, color=color))}
      else if(input$gemBoxScat=="clarity"){
        gem+geom_point(aes(x=x,y=y, color=clarity))}
    }
    else if(input$gemGraphs=="xzScat"){
      if(input$gemBoxScat=="none"){
        gem+geom_point(aes(x=x,y=z))}
      else if(input$gemBoxScat=="cut"){
        gem+geom_point(aes(x=x,y=z, color=cut))}
      else if(input$gemBoxScat=="color"){
        gem+geom_point(aes(x=x,y=z, color=color))}
      else if(input$gemBoxScat=="clarity"){
        gem+geom_point(aes(x=x,y=z, color=clarity))}
    }
    else if(input$gemGraphs=="yzScat"){
      if(input$gemBoxScat=="none"){
        gem+geom_point(aes(x=y,y=z))}
      else if(input$gemBoxScat=="cut"){
        gem+geom_point(aes(x=y,y=z, color=cut))}
      else if(input$gemBoxScat=="color"){
        gem+geom_point(aes(x=y,y=z, color=color))}
      else if(input$gemBoxScat=="clarity"){
        gem+geom_point(aes(x=y,y=z, color=clarity))}
    }
    else if(input$gemGraphs=="zdepthScat"){
      if(input$gemBoxScat=="none"){
        gem+geom_point(aes(x=z,y=depth))}
      else if(input$gemBoxScat=="cut"){
        gem+geom_point(aes(x=z,y=depth, color=cut))}
      else if(input$gemBoxScat=="color"){
        gem+geom_point(aes(x=z,y=depth, color=color))}
      else if(input$gemBoxScat=="clarity"){
        gem+geom_point(aes(x=z,y=depth, color=clarity))}
    }
    else if(input$gemGraphs=="ytableScat"){
      if(input$gemBoxScat=="none"){
        gem+geom_point(aes(x=y,y=table))}
      else if(input$gemBoxScat=="cut"){
        gem+geom_point(aes(x=y,y=table, color=cut))}
      else if(input$gemBoxScat=="color"){
        gem+geom_point(aes(x=y,y=table, color=color))}
      else if(input$gemBoxScat=="clarity"){
        gem+geom_point(aes(x=y,y=table, color=clarity))}
    }
    else if(input$gemGraphs=="carpriceScat"){
      if(input$gemBoxScat=="none"){
        gem+geom_point(aes(x=carat,y=price))}
      else if(input$gemBoxScat=="cut"){
        gem+geom_point(aes(x=carat,y=price, color=cut))}
      else if(input$gemBoxScat=="color"){
        gem+geom_point(aes(x=carat,y=price, color=color))}
      else if(input$gemBoxScat=="clarity"){
        gem+geom_point(aes(x=carat,y=price, color=clarity))}
    }
    else if(input$gemGraphs=="xpriceScat"){
      if(input$gemBoxScat=="none"){
        gem+geom_point(aes(x=x,y=price))}
      else if(input$gemBoxScat=="cut"){
        gem+geom_point(aes(x=x,y=price, color=cut))}
      else if(input$gemBoxScat=="color"){
        gem+geom_point(aes(x=x,y=price, color=color))}
      else if(input$gemBoxScat=="clarity"){
        gem+geom_point(aes(x=x,y=price, color=clarity))}
    }
    else if(input$gemGraphs=="ypriceScat"){
      if(input$gemBoxScat=="none"){
        gem+geom_point(aes(x=y,y=price))}
      else if(input$gemBoxScat=="cut"){
        gem+geom_point(aes(x=y,y=price, color=cut))}
      else if(input$gemBoxScat=="color"){
        gem+geom_point(aes(x=y,y=price, color=color))}
      else if(input$gemBoxScat=="clarity"){
        gem+geom_point(aes(x=y,y=price, color=clarity))}
    }
    else if(input$gemGraphs=="zpriceScat"){
      if(input$gemBoxScat=="none"){
        gem+geom_point(aes(x=z,y=price))}
      else if(input$gemBoxScat=="cut"){
        gem+geom_point(aes(x=z,y=price, color=cut))}
      else if(input$gemBoxScat=="color"){
        gem+geom_point(aes(x=z,y=price, color=color))}
      else if(input$gemBoxScat=="clarity"){
        gem+geom_point(aes(x=z,y=price, color=clarity))}
    }
  })
  
  #
  
  
  
  
  
  ##### Pottery #####
  #Numerical Var summaries
  output$potStats <- renderPrint({
    as.table(summary(pottery[,input$potNum]))
  })
  
  #Categorical Var Summaries
  output$potLevels <- renderTable({
    table(pottery[,input$potCat])
  })
  
  #Graphs
  output$potPlot <- renderPlot({
    #base plot
    pot <- ggplot(pottery)
    
    #correlation plot
    if(input$potGraphs=="corrMap"){corrplot(cor(pottery[,1:9]))} 
    
    #boxplots
    else if(input$potGraphs=="Al2O3Box"){
      if(input$opt=="no"){
        pot+geom_boxplot(aes(Al2O3))}
      else if(input$opt=="yes"){
        pot+geom_boxplot(aes(Al2O3, color=kiln))}
    }
    else if(input$potGraphs=="Fe2O3Box"){
      if(input$opt=="no"){
        pot+geom_boxplot(aes(Fe2O3))}
      else if(input$opt=="yes"){
        pot+geom_boxplot(aes(Fe2O3, color=kiln))}
    }
    else if(input$potGraphs=="MgOBox"){
      if(input$opt=="no"){
        pot+geom_boxplot(aes(MgO))}
      else if(input$opt=="yes"){
        pot+geom_boxplot(aes(MgO, color=kiln))}
    }
    else if(input$potGraphs=="CaOBox"){
      if(input$opt=="no"){
        pot+geom_boxplot(aes(CaO))}
      else if(input$opt=="yes"){
        pot+geom_boxplot(aes(CaO, color=kiln))}
    }
    else if(input$potGraphs=="Na2OBox"){
      if(input$opt=="no"){
        pot+geom_boxplot(aes(Na2O))}
      else if(input$opt=="yes"){
        pot+geom_boxplot(aes(Na2O, color=kiln))}
    }
    else if(input$potGraphs=="K2OBox"){
      if(input$opt=="no"){
        pot+geom_boxplot(aes(K2O))}
      else if(input$opt=="yes"){
        pot+geom_boxplot(aes(K2O, color=kiln))}
    }
    else if(input$potGraphs=="TiO2Box"){
      if(input$opt=="no"){
        pot+geom_boxplot(aes(TiO2))}
      else if(input$opt=="yes"){
        pot+geom_boxplot(aes(TiO2, color=kiln))}
    }
    else if(input$potGraphs=="MnOBox"){
      if(input$opt=="no"){
        pot+geom_boxplot(aes(MnO))}
      else if(input$opt=="yes"){
        pot+geom_boxplot(aes(MnO, color=kiln))}
    }
    else if(input$potGraphs=="BaOBox"){
      if(input$opt=="no"){
        pot+geom_boxplot(aes(BaO))}
      else if(input$opt=="yes"){
        pot+geom_boxplot(aes(BaO, color=kiln))}
    }
    
    #histograms
    else if(input$potGraphs=="Al2O3Hist"){
      if(input$opt=="no"){
        pot+geom_histogram(aes(Al2O3))}
      else if(input$opt=="yes"){
        pot+geom_histogram(aes(Al2O3, fill=kiln))}
    }
    else if(input$potGraphs=="Fe2O3Hist"){
      if(input$opt=="no"){
        pot+geom_histogram(aes(Fe2O3))}
      else if(input$opt=="yes"){
        pot+geom_histogram(aes(Fe2O3, fill=kiln))}
    }
    else if(input$potGraphs=="MgOHist"){
      if(input$opt=="no"){
        pot+geom_histogram(aes(MgO))}
      else if(input$opt=="yes"){
        pot+geom_histogram(aes(MgO, fill=kiln))}
    }
    else if(input$potGraphs=="CaOHist"){
      if(input$opt=="no"){
        pot+geom_histogram(aes(CaO))}
      else if(input$opt=="yes"){
        pot+geom_histogram(aes(CaO, fill=kiln))}
    }
    else if(input$potGraphs=="Na2OHist"){
      if(input$opt=="no"){
        pot+geom_histogram(aes(Na2O))}
      else if(input$opt=="yes"){
        pot+geom_histogram(aes(Na2O, fill=kiln))}
    }
    else if(input$potGraphs=="K2OHist"){
      if(input$opt=="no"){
        pot+geom_histogram(aes(K2O))}
      else if(input$opt=="yes"){
        pot+geom_histogram(aes(K2O, fill=kiln))}
    }
    else if(input$potGraphs=="TiO2Hist"){
      if(input$opt=="no"){
        pot+geom_histogram(aes(TiO2))}
      else if(input$opt=="yes"){
        pot+geom_histogram(aes(TiO2, fill=kiln))}
    }
    else if(input$potGraphs=="MnOHist"){
      if(input$opt=="no"){
        pot+geom_histogram(aes(MnO))}
      else if(input$opt=="yes"){
        pot+geom_histogram(aes(MnO, fill=kiln))}
    }
    else if(input$potGraphs=="BaOHist"){
      if(input$opt=="no"){
        pot+geom_histogram(aes(BaO))}
      else if(input$opt=="yes"){
        pot+geom_histogram(aes(BaO, fill=kiln))}
    }
  })
  
  
  
  ##### Kanga #####
  #Numerical Var summaries
  output$rooStats <- renderPrint({
    as.table(summary(roo[,input$rooNum]))
  })
  
  #Categorical Var Summaries
  output$rooLevels <- renderTable({
    table(roo[,input$rooCat])
  })
  
  #Graphs
  output$rooPlot <- renderPlot({
    #base plot
    kan <- ggplot(roo)
    
    #correlation plot
    if(input$rooGraphs=="corrMap"){corrplot(cor(roo[,c(3:20)]))}
    
    #boxplots
    else if(input$rooGraphs=="baLBox"){
      if(input$opt=="neither"){
        kan+geom_boxplot(aes(basilar.length))}
      else if(input$opt=="sex"){
        kan+geom_boxplot(aes(basilar.length, color=sex))}
      else if(input$opt=="species"){
        kan+geom_boxplot(aes(basilar.length, color=species))}
    }
    else if(input$rooGraphs=="ocLBox"){
      if(input$opt=="neither"){
        kan+geom_boxplot(aes(occipitonasal.length))}
      else if(input$opt=="sex"){
        kan+geom_boxplot(aes(occipitonasal.length, color=sex))}
      else if(input$opt=="species"){
        kan+geom_boxplot(aes(occipitonasal.length, color=species))}
    }
    else if(input$rooGraphs=="paLBox"){
      if(input$opt=="neither"){
        kan+geom_boxplot(aes(palate.length))}
      else if(input$opt=="sex"){
        kan+geom_boxplot(aes(palate.length, color=sex))}
      else if(input$opt=="species"){
        kan+geom_boxplot(aes(palate.length, color=species))}
    }
    else if(input$rooGraphs=="paWBox"){
      if(input$opt=="neither"){
        kan+geom_boxplot(aes(palate.width))}
      else if(input$opt=="sex"){
        kan+geom_boxplot(aes(palate.width, color=sex))}
      else if(input$opt=="species"){
        kan+geom_boxplot(aes(palate.width, color=species))}
    }
    else if(input$rooGraphs=="naLBox"){
      if(input$opt=="neither"){
        kan+geom_boxplot(aes(nasal.length))}
      else if(input$opt=="sex"){
        kan+geom_boxplot(aes(nasal.length, color=sex))}
      else if(input$opt=="species"){
        kan+geom_boxplot(aes(nasal.length, color=species))}
    }
    else if(input$rooGraphs=="naWBox"){
      if(input$opt=="neither"){
        kan+geom_boxplot(aes(nasal.width))}
      else if(input$opt=="sex"){
        kan+geom_boxplot(aes(nasal.width, color=sex))}
      else if(input$opt=="species"){
        kan+geom_boxplot(aes(nasal.width, color=species))}
    }
    else if(input$rooGraphs=="sqDBox"){
      if(input$opt=="neither"){
        kan+geom_boxplot(aes(squamosal.depth))}
      else if(input$opt=="sex"){
        kan+geom_boxplot(aes(squamosal.depth, color=sex))}
      else if(input$opt=="species"){
        kan+geom_boxplot(aes(squamosal.depth, color=species))}
    }
    else if(input$rooGraphs=="laWBox"){
      if(input$opt=="neither"){
        kan+geom_boxplot(aes(lacrymal.width))}
      else if(input$opt=="sex"){
        kan+geom_boxplot(aes(lacrymal.width, color=sex))}
      else if(input$opt=="species"){
        kan+geom_boxplot(aes(lacrymal.width, color=species))}
    }
    else if(input$rooGraphs=="zyWBox"){
      if(input$opt=="neither"){
        kan+geom_boxplot(aes(zygomatic.width))}
      else if(input$opt=="sex"){
        kan+geom_boxplot(aes(zygomatic.width, color=sex))}
      else if(input$opt=="species"){
        kan+geom_boxplot(aes(zygomatic.width, color=species))}
    }
    else if(input$rooGraphs=="orWBox"){
      if(input$opt=="neither"){
        kan+geom_boxplot(aes(orbital.width))}
      else if(input$opt=="sex"){
        kan+geom_boxplot(aes(orbital.width, color=sex))}
      else if(input$opt=="species"){
        kan+geom_boxplot(aes(orbital.width, color=species))}
    }
    else if(input$rooGraphs=="roWBox"){
      if(input$opt=="neither"){
        kan+geom_boxplot(aes(.rostral.width))}
      else if(input$opt=="sex"){
        kan+geom_boxplot(aes(.rostral.width, color=sex))}
      else if(input$opt=="species"){
        kan+geom_boxplot(aes(.rostral.width, color=species))}
    }
    else if(input$rooGraphs=="ocDBox"){
      if(input$opt=="neither"){
        kan+geom_boxplot(aes(occipital.depth))}
      else if(input$opt=="sex"){
        kan+geom_boxplot(aes(occipital.depth, color=sex))}
      else if(input$opt=="species"){
        kan+geom_boxplot(aes(occipital.depth, color=species))}
    }
    else if(input$rooGraphs=="crWBox"){
      if(input$opt=="neither"){
        kan+geom_boxplot(aes(crest.width))}
      else if(input$opt=="sex"){
        kan+geom_boxplot(aes(crest.width, color=sex))}
      else if(input$opt=="species"){
        kan+geom_boxplot(aes(crest.width, color=species))}
    }
    else if(input$rooGraphs=="foLBox"){
      if(input$opt=="neither"){
        kan+geom_boxplot(aes(foramina.length))}
      else if(input$opt=="sex"){
        kan+geom_boxplot(aes(foramina.length, color=sex))}
      else if(input$opt=="species"){
        kan+geom_boxplot(aes(foramina.length, color=species))}
    }
    else if(input$rooGraphs=="maLBox"){
      if(input$opt=="neither"){
        kan+geom_boxplot(aes(mandible.length))}
      else if(input$opt=="sex"){
        kan+geom_boxplot(aes(mandible.length, color=sex))}
      else if(input$opt=="species"){
        kan+geom_boxplot(aes(mandible.length, color=species))}
    }
    else if(input$rooGraphs=="maWBox"){
      if(input$opt=="neither"){
        kan+geom_boxplot(aes(mandible.width))}
      else if(input$opt=="sex"){
        kan+geom_boxplot(aes(mandible.width, color=sex))}
      else if(input$opt=="species"){
        kan+geom_boxplot(aes(mandible.width, color=species))}
    }
    else if(input$rooGraphs=="maDBox"){
      if(input$opt=="neither"){
        kan+geom_boxplot(aes(mandible.depth))}
      else if(input$opt=="sex"){
        kan+geom_boxplot(aes(mandible.depth, color=sex))}
      else if(input$opt=="species"){
        kan+geom_boxplot(aes(mandible.depth, color=species))}
    }
    else if(input$rooGraphs=="raHBox"){
      if(input$opt=="neither"){
        kan+geom_boxplot(aes(ramus.height))}
      else if(input$opt=="sex"){
        kan+geom_boxplot(aes(ramus.height, color=sex))}
      else if(input$opt=="species"){
        kan+geom_boxplot(aes(ramus.height, color=species))}
    }
    
    #histograms
    else if(input$rooGraphs=="baLHist"){
      if(input$opt=="neither"){
        kan+geom_histogram(aes(basilar.length))}
      else if(input$opt=="sex"){
        kan+geom_histogram(aes(basilar.length, color=sex))}
      else if(input$opt=="species"){
        kan+geom_histogram(aes(basilar.length, color=species))}
    }
    else if(input$rooGraphs=="ocLHist"){
      if(input$opt=="neither"){
        kan+geom_histogram(aes(occipitonasal.length))}
      else if(input$opt=="sex"){
        kan+geom_histogram(aes(occipitonasal.length, color=sex))}
      else if(input$opt=="species"){
        kan+geom_histogram(aes(occipitonasal.length, color=species))}
    }
    else if(input$rooGraphs=="paLHist"){
      if(input$opt=="neither"){
        kan+geom_histogram(aes(palate.length))}
      else if(input$opt=="sex"){
        kan+geom_histogram(aes(palate.length, color=sex))}
      else if(input$opt=="species"){
        kan+geom_histogram(aes(palate.length, color=species))}
    }
    else if(input$rooGraphs=="paWHist"){
      if(input$opt=="neither"){
        kan+geom_histogram(aes(palate.width))}
      else if(input$opt=="sex"){
        kan+geom_histogram(aes(palate.width, color=sex))}
      else if(input$opt=="species"){
        kan+geom_histogram(aes(palate.width, color=species))}
    }
    else if(input$rooGraphs=="naLHist"){
      if(input$opt=="neither"){
        kan+geom_histogram(aes(nasal.length))}
      else if(input$opt=="sex"){
        kan+geom_histogram(aes(nasal.length, color=sex))}
      else if(input$opt=="species"){
        kan+geom_histogram(aes(nasal.length, color=species))}
    }
    else if(input$rooGraphs=="naWHist"){
      if(input$opt=="neither"){
        kan+geom_histogram(aes(nasal.width))}
      else if(input$opt=="sex"){
        kan+geom_histogram(aes(nasal.width, color=sex))}
      else if(input$opt=="species"){
        kan+geom_histogram(aes(nasal.width, color=species))}
    }
    else if(input$rooGraphs=="sqDHist"){
      if(input$opt=="neither"){
        kan+geom_histogram(aes(squamosal.depth))}
      else if(input$opt=="sex"){
        kan+geom_histogram(aes(squamosal.depth, color=sex))}
      else if(input$opt=="species"){
        kan+geom_histogram(aes(squamosal.depth, color=species))}
    }
    else if(input$rooGraphs=="laWHist"){
      if(input$opt=="neither"){
        kan+geom_histogram(aes(lacrymal.width))}
      else if(input$opt=="sex"){
        kan+geom_histogram(aes(lacrymal.width, color=sex))}
      else if(input$opt=="species"){
        kan+geom_histogram(aes(lacrymal.width, color=species))}
    }
    else if(input$rooGraphs=="zyWHist"){
      if(input$opt=="neither"){
        kan+geom_histogram(aes(zygomatic.width))}
      else if(input$opt=="sex"){
        kan+geom_histogram(aes(zygomatic.width, color=sex))}
      else if(input$opt=="species"){
        kan+geom_histogram(aes(zygomatic.width, color=species))}
    }
    else if(input$rooGraphs=="orWHist"){
      if(input$opt=="neither"){
        kan+geom_histogram(aes(orbital.width))}
      else if(input$opt=="sex"){
        kan+geom_histogram(aes(orbital.width, color=sex))}
      else if(input$opt=="species"){
        kan+geom_histogram(aes(orbital.width, color=species))}
    }
    else if(input$rooGraphs=="roWHist"){
      if(input$opt=="neither"){
        kan+geom_histogram(aes(.rostral.width))}
      else if(input$opt=="sex"){
        kan+geom_histogram(aes(.rostral.width, color=sex))}
      else if(input$opt=="species"){
        kan+geom_histogram(aes(.rostral.width, color=species))}
    }
    else if(input$rooGraphs=="ocDHist"){
      if(input$opt=="neither"){
        kan+geom_histogram(aes(occipital.depth))}
      else if(input$opt=="sex"){
        kan+geom_histogram(aes(occipital.depth, color=sex))}
      else if(input$opt=="species"){
        kan+geom_histogram(aes(occipital.depth, color=species))}
    }
    else if(input$rooGraphs=="crWHist"){
      if(input$opt=="neither"){
        kan+geom_histogram(aes(crest.width))}
      else if(input$opt=="sex"){
        kan+geom_histogram(aes(crest.width, color=sex))}
      else if(input$opt=="species"){
        kan+geom_histogram(aes(crest.width, color=species))}
    }
    else if(input$rooGraphs=="foLHist"){
      if(input$opt=="neither"){
        kan+geom_histogram(aes(foramina.length))}
      else if(input$opt=="sex"){
        kan+geom_histogram(aes(foramina.length, color=sex))}
      else if(input$opt=="species"){
        kan+geom_histogram(aes(foramina.length, color=species))}
    }
    else if(input$rooGraphs=="maLHist"){
      if(input$opt=="neither"){
        kan+geom_histogram(aes(mandible.length))}
      else if(input$opt=="sex"){
        kan+geom_histogram(aes(mandible.length, color=sex))}
      else if(input$opt=="species"){
        kan+geom_histogram(aes(mandible.length, color=species))}
    }
    else if(input$rooGraphs=="maWHist"){
      if(input$opt=="neither"){
        kan+geom_histogram(aes(mandible.width))}
      else if(input$opt=="sex"){
        kan+geom_histogram(aes(mandible.width, color=sex))}
      else if(input$opt=="species"){
        kan+geom_histogram(aes(mandible.width, color=species))}
    }
    else if(input$rooGraphs=="maDHist"){
      if(input$opt=="neither"){
        kan+geom_histogram(aes(mandible.depth))}
      else if(input$opt=="sex"){
        kan+geom_histogram(aes(mandible.depth, color=sex))}
      else if(input$opt=="species"){
        kan+geom_histogram(aes(mandible.depth, color=species))}
    }
    else if(input$rooGraphs=="raHHist"){
      if(input$opt=="neither"){
        kan+geom_histogram(aes(ramus.height))}
      else if(input$opt=="sex"){
        kan+geom_histogram(aes(ramus.height, color=sex))}
      else if(input$opt=="species"){
        kan+geom_histogram(aes(ramus.height, color=species))}
    }
    
    #barplots
    else if(input$rooGraphs=="spBar"){
      if(input$speciesBar=="no"){
        kan+geom_bar(aes(species))}
      else if(input$speciesBar=="yes"){
        kan+geom_bar(aes(species, fill=sex))}
    }
    else if(input$rooGraphs=="seBar"){
      if(input$sexBar=="no"){
        kan+geom_bar(aes(sex))}
      else if(input$sexBar=="yes"){
        kan+geom_bar(aes(sex, fill=species))}
    }
    
    #scatterplots
    else if(input$rooGraphs=="paLWScat"){
      if(input$opt=="neither"){
        kan+geom_point(aes(palate.length, palate.width))}
      else if(input$opt=="sex"){
        kan+geom_point(aes(palate.length, palate.width, color=sex))}
      else if(input$opt=="species"){
        kan+geom_point(aes(palate.length, palate.width, color=species))}
    }
    else if(input$rooGraphs=="naLWScat"){
      if(input$opt=="neither"){
        kan+geom_point(aes(nasal.length, nasal.width))}
      else if(input$opt=="sex"){
        kan+geom_point(aes(nasal.length, nasal.width, color=sex))}
      else if(input$opt=="species"){
        kan+geom_point(aes(nasal.length, nasal.width, color=species))}
    }
    else if(input$rooGraphs=="maLWScat"){
      if(input$opt=="neither"){
        kan+geom_point(aes(mandible.length, mandible.width))}
      else if(input$opt=="sex"){
        kan+geom_point(aes(mandible.length, mandible.width, color=sex))}
      else if(input$opt=="species"){
        kan+geom_point(aes(mandible.length, mandible.width, color=species))}
    }
    else if(input$rooGraphs=="maLDScat"){
      if(input$opt=="neither"){
        kan+geom_point(aes(mandible.length, mandible.depth))}
      else if(input$opt=="sex"){
        kan+geom_point(aes(mandible.length, mandible.depth, color=sex))}
      else if(input$opt=="species"){
        kan+geom_point(aes(mandible.length, mandible.depth, color=species))}
    }
    else if(input$rooGraphs=="maWDScat"){
      if(input$opt=="neither"){
        kan+geom_point(aes(mandible.width, mandible.depth))}
      else if(input$opt=="sex"){
        kan+geom_point(aes(mandible.width, mandible.depth, color=sex))}
      else if(input$opt=="species"){
        kan+geom_point(aes(mandible.width, mandible.depth, color=species))}
    }
    
  })
  

  
})