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

#load datasets
data("diamonds")
data("pottery", package = "HSAUR3")
data("kanga")
roo <- na.omit(kanga)


shinyServer(function(input, output, session) {
  
  ##### Diamonds #####
  ## Numerical Var summaries
  output$gemStats <- renderPrint({
    as.table(summary(diamonds[,input$gemNum]))
  })
  
  ## Categorical Var Summaries
  output$gemLevels <-renderTable({
    table(diamonds[,input$gemCat])
  })
  
  ## Graphs
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
  
  ## Model Fitting
  #MLR
  output$gemMLR <- renderPrint({
    #set seed for reproduction purposes
    set.seed(17)
    #Training/Testing Data
    train <- sample(1:nrow(diamonds), size = nrow(diamonds)*input$gemTrain)
    gemTrainDat <- diamonds[train,]
    gemTestDat <- diamonds[-train,]
    
    #fit
    lmod <- train(reformulate(input$gemMLRVars, "price"),
                  data=gemTrainDat,
                  method = "lm",
                  trControl = trainControl("cv",number=10))
    #predict
    lmodPred <- predict(lmod, newdata = gemTestDat)
    
    #output
    list(
      "Model_Summary"=summary(lmod),
      "RMSE"=RMSE(lmodPred, gemTestDat$price))
  })

  #Regression Tree
  output$gemTree <- renderPrint({
    #set seed for reproduction purposes
    set.seed(17)
    #Training/Testing Data
    train <- sample(1:nrow(diamonds), size = nrow(diamonds)*input$gemTrain)
    gemTrainDat <- diamonds[train,]
    gemTestDat <- diamonds[-train,]
    
    #fit
    treemod <- tree(reformulate(input$gemTreeVars, "price"),
                    data = gemTrainDat)
    #predict
    treePred <- predict(treemod, newdata = gemTestDat)
    
    #output
    list(
      "Model_Summary"=summary(treemod),
      "RMSE"=sqrt(mean((treePred-gemTestDat$price)^2)))
  })
  
  #Random Forest
  output$gemRand <- renderPrint({
    #set seed for reproduction purposes
    set.seed(17)
    #Training/Testing Data
    train <- sample(1:nrow(diamonds), size = nrow(diamonds)*input$gemTrain)
    gemTrainDat <- diamonds[train,]
    gemTestDat <- diamonds[-train,]
    
    #fit
    rfmod <- randomForest(reformulate(input$gemForestVars, "price"), 
                          data = gemTrainDat, 
                          mtry = 1,
                          ntree = 100, 
                          importance = TRUE)
    #predict
    rfPred <- predict(rfmod, newdata = gemTestDat)
    
    #output
    list(
      "Model_Summary"=rfmod,
      "RMSE"=RMSE(rfPred, gemTestDat$price))
  })
  
  
  ## Prediction
  #models
  output$gemPred <- renderPrint({
    #set seed for reproduction purposes
    set.seed(17)
    
    #Training/Testing Data
    train <- sample(1:nrow(diamonds), size = nrow(diamonds)*input$gemTrain)
    gemTrainDat <- diamonds[train,]
    gemTestDat <- diamonds[-train,]
    
    #fit linear mod
    lmod <- train(reformulate(input$gemMLRVars, "price"),
                  data=gemTrainDat,
                  method = "lm",
                  trControl = trainControl("cv",number=10))
    #fit tree
    treemod <- tree(reformulate(input$gemTreeVars, "price"),
                    data = gemTrainDat)
    #fit rand. forest
    rfmod <- randomForest(reformulate(input$gemForestVars, "price"), 
                          data = gemTrainDat, 
                          mtry = 1,
                          ntree = 100, 
                          importance = TRUE)
    
    #create prediction data from user input
    gemPredDat <- data.frame(carat=input$caratIn, cut=as.factor(input$cutIn), color=as.factor(input$colorIn), 
                             clarity=as.factor(input$clarIn), x=input$xIn, y=input$yIn, z=input$zIn, 
                             depth=input$depthIn, table=input$tableIn)
    
    
    #Output options - Predict with user input
    if(input$gemSelect=="mlr"){
      predict(lmod, newdata = gemPredDat)
    }
    else if(input$gemSelect=="regtr"){
      predict(treemod, newdata = gemPredDat)
    }
    else if(input$gemSelect=="rf"){
      predict(rfmod, newdata = gemPredDat)
    }
    
  })
  
  
  ## Download data
  output$gemSubDat <- renderDataTable(diamonds[ ,c(input$gemSubVars), drop=FALSE])
  
  
  output$gemSave <- downloadHandler(filename = "diamonds.csv",
                                    content = write.csv(diamonds[ ,c(input$gemSubVars), drop=FALSE], "diamonds.csv"))
  
  
  
  
  
  
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
  
  ## Model Fitting
  #GLR
  output$potGLR <- renderPrint({
    #set seed for reproduction purposes
    set.seed(17)
    #Training/Testing Data
    train <- sample(1:nrow(pottery), size = nrow(pottery)*input$potTrain)
    potTrainDat <- pottery[train,]
    potTestDat <- pottery[-train,]
    
    #fit
    lmod <- multinom(reformulate(input$potGLRVars, "kiln"),
                     data=potTrainDat)
    #predict
    lmodPred <- predict(lmod, newdata = potTestDat)
    
    #classification rate
    class <- table(potTestDat$kiln, lmodPred)
    misclassRate <- 1-(sum(diag(class))/sum(class))
    
    #output
    list(
      "Model_Summary"=summary(lmod),
      "Classification"=class,
      "Missclassification_Error_Rate"= misclassRate)
  })
  
  #Classification Tree
  output$potTree <- renderPrint({
    #set seed for reproduction purposes
    set.seed(17)
    #Training/Testing Data
    train <- sample(1:nrow(pottery), size = nrow(pottery)*input$potTrain)
    potTrainDat <- pottery[train,]
    potTestDat <- pottery[-train,]
    
    #fit
    treemod <- tree(reformulate(input$potTreeVars, "kiln"),
                    data = potTrainDat)
    #predict
    treePred <- predict(treemod, newdata = potTestDat)
    
    #output
    list(
      "Model_Summary"=summary(treemod))
  })
  
  #Random Forest
  output$potRand <- renderPrint({
    #set seed for reproduction purposes
    set.seed(17)
    #Training/Testing Data
    train <- sample(1:nrow(pottery), size = nrow(pottery)*input$potTrain)
    potTrainDat <- pottery[train,]
    potTestDat <- pottery[-train,]
    
    #fit
    rfmod <- randomForest(reformulate(input$potForestVars, "kiln"), 
                          data = potTrainDat, 
                          mtry = 1,
                          ntree = 100, 
                          importance = TRUE)
    #predict
    rfPred <- predict(rfmod, newdata = potTestDat)
    
    #classification rate
    class <- table(potTestDat$kiln, rfPred)
    missclassRate <- 1-(sum(diag(class))/sum(class))
    
    #output
    list(
      "Model_Summary"=summary(rfmod),
      "Classification"=class,
      "Missclassification_Error_Rate"=missclassRate
    )
  })
  
  

  ## Prediction
  #models
  output$potPred <- renderPrint({
    #set seed for reproduction purposes
    set.seed(17)
    
    #Training/Testing Data
    train <- sample(1:nrow(pottery), size = nrow(pottery)*input$potTrain)
    potTrainDat <- pottery[train,]
    potTestDat <- pottery[-train,]
    
    #fit
    lmod <- multinom(reformulate(input$potGLRVars, "kiln"),
                     data=potTrainDat)
    #fit tree
    treemod <- tree(reformulate(input$potTreeVars, "kiln"),
                    data = potTrainDat)
    #fit rand. forest
    rfmod <- randomForest(reformulate(input$potForestVars, "kiln"), 
                          data = potTrainDat, 
                          mtry = 1,
                          ntree = 100, 
                          importance = TRUE)
    
    #create prediction data from user input
    potPredDat <- data.frame(Al2O3=input$alumtriIn, Fe2O3=input$irontriIn, MgO=input$magoxIn, 
                             CaO=input$calcoxIn, Na2O=input$natoxIn, K2O=input$calioxIn, 
                             TiO2=input$titoxIn, MnO=input$manganoxIn, BaO=input$baroxIn)
    
    
    #Output options - Predict with user input
    if(input$potSelect=="glr"){
      predict(lmod, newdata = potPredDat)
    }
    else if(input$potSelect=="classtr"){
      predict(treemod, newdata = potPredDat)
    }
    else if(input$potSelect=="rf"){
      predict(rfmod, newdata = potPredDat)
    }
    
  })
  
  ## Download data
  output$potSubDat <- renderDataTable(pottery[ ,c(input$potSubVars), drop=FALSE])
  
  
  output$potSave <- downloadHandler(filename = "pottery.csv",
                                    content = write.csv(pottery[ ,c(input$potSubVars), drop=FALSE], "pottery.csv"))
  
  
  
  
  
  
  
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
      if(input$rooBHS=="neither"){
        kan+geom_boxplot(aes(basilar.length))}
      else if(input$rooBHS=="sex"){
        kan+geom_boxplot(aes(basilar.length, color=sex))}
      else if(input$rooBHS=="species"){
        kan+geom_boxplot(aes(basilar.length, color=species))}
    }
    else if(input$rooGraphs=="ocLBox"){
      if(input$rooBHS=="neither"){
        kan+geom_boxplot(aes(occipitonasal.length))}
      else if(input$rooBHS=="sex"){
        kan+geom_boxplot(aes(occipitonasal.length, color=sex))}
      else if(input$rooBHS=="species"){
        kan+geom_boxplot(aes(occipitonasal.length, color=species))}
    }
    else if(input$rooGraphs=="paLBox"){
      if(input$rooBHS=="neither"){
        kan+geom_boxplot(aes(palate.length))}
      else if(input$rooBHS=="sex"){
        kan+geom_boxplot(aes(palate.length, color=sex))}
      else if(input$rooBHS=="species"){
        kan+geom_boxplot(aes(palate.length, color=species))}
    }
    else if(input$rooGraphs=="paWBox"){
      if(input$rooBHS=="neither"){
        kan+geom_boxplot(aes(palate.width))}
      else if(input$rooBHS=="sex"){
        kan+geom_boxplot(aes(palate.width, color=sex))}
      else if(input$rooBHS=="species"){
        kan+geom_boxplot(aes(palate.width, color=species))}
    }
    else if(input$rooGraphs=="naLBox"){
      if(input$rooBHS=="neither"){
        kan+geom_boxplot(aes(nasal.length))}
      else if(input$rooBHS=="sex"){
        kan+geom_boxplot(aes(nasal.length, color=sex))}
      else if(input$rooBHS=="species"){
        kan+geom_boxplot(aes(nasal.length, color=species))}
    }
    else if(input$rooGraphs=="naWBox"){
      if(input$rooBHS=="neither"){
        kan+geom_boxplot(aes(nasal.width))}
      else if(input$rooBHS=="sex"){
        kan+geom_boxplot(aes(nasal.width, color=sex))}
      else if(input$rooBHS=="species"){
        kan+geom_boxplot(aes(nasal.width, color=species))}
    }
    else if(input$rooGraphs=="sqDBox"){
      if(input$rooBHS=="neither"){
        kan+geom_boxplot(aes(squamosal.depth))}
      else if(input$rooBHS=="sex"){
        kan+geom_boxplot(aes(squamosal.depth, color=sex))}
      else if(input$rooBHS=="species"){
        kan+geom_boxplot(aes(squamosal.depth, color=species))}
    }
    else if(input$rooGraphs=="laWBox"){
      if(input$rooBHS=="neither"){
        kan+geom_boxplot(aes(lacrymal.width))}
      else if(input$rooBHS=="sex"){
        kan+geom_boxplot(aes(lacrymal.width, color=sex))}
      else if(input$rooBHS=="species"){
        kan+geom_boxplot(aes(lacrymal.width, color=species))}
    }
    else if(input$rooGraphs=="zyWBox"){
      if(input$rooBHS=="neither"){
        kan+geom_boxplot(aes(zygomatic.width))}
      else if(input$rooBHS=="sex"){
        kan+geom_boxplot(aes(zygomatic.width, color=sex))}
      else if(input$rooBHS=="species"){
        kan+geom_boxplot(aes(zygomatic.width, color=species))}
    }
    else if(input$rooGraphs=="orWBox"){
      if(input$rooBHS=="neither"){
        kan+geom_boxplot(aes(orbital.width))}
      else if(input$rooBHS=="sex"){
        kan+geom_boxplot(aes(orbital.width, color=sex))}
      else if(input$rooBHS=="species"){
        kan+geom_boxplot(aes(orbital.width, color=species))}
    }
    else if(input$rooGraphs=="roWBox"){
      if(input$rooBHS=="neither"){
        kan+geom_boxplot(aes(.rostral.width))}
      else if(input$rooBHS=="sex"){
        kan+geom_boxplot(aes(.rostral.width, color=sex))}
      else if(input$rooBHS=="species"){
        kan+geom_boxplot(aes(.rostral.width, color=species))}
    }
    else if(input$rooGraphs=="ocDBox"){
      if(input$rooBHS=="neither"){
        kan+geom_boxplot(aes(occipital.depth))}
      else if(input$rooBHS=="sex"){
        kan+geom_boxplot(aes(occipital.depth, color=sex))}
      else if(input$rooBHS=="species"){
        kan+geom_boxplot(aes(occipital.depth, color=species))}
    }
    else if(input$rooGraphs=="crWBox"){
      if(input$rooBHS=="neither"){
        kan+geom_boxplot(aes(crest.width))}
      else if(input$rooBHS=="sex"){
        kan+geom_boxplot(aes(crest.width, color=sex))}
      else if(input$rooBHS=="species"){
        kan+geom_boxplot(aes(crest.width, color=species))}
    }
    else if(input$rooGraphs=="foLBox"){
      if(input$rooBHS=="neither"){
        kan+geom_boxplot(aes(foramina.length))}
      else if(input$rooBHS=="sex"){
        kan+geom_boxplot(aes(foramina.length, color=sex))}
      else if(input$rooBHS=="species"){
        kan+geom_boxplot(aes(foramina.length, color=species))}
    }
    else if(input$rooGraphs=="maLBox"){
      if(input$rooBHS=="neither"){
        kan+geom_boxplot(aes(mandible.length))}
      else if(input$rooBHS=="sex"){
        kan+geom_boxplot(aes(mandible.length, color=sex))}
      else if(input$rooBHS=="species"){
        kan+geom_boxplot(aes(mandible.length, color=species))}
    }
    else if(input$rooGraphs=="maWBox"){
      if(input$rooBHS=="neither"){
        kan+geom_boxplot(aes(mandible.width))}
      else if(input$rooBHS=="sex"){
        kan+geom_boxplot(aes(mandible.width, color=sex))}
      else if(input$rooBHS=="species"){
        kan+geom_boxplot(aes(mandible.width, color=species))}
    }
    else if(input$rooGraphs=="maDBox"){
      if(input$rooBHS=="neither"){
        kan+geom_boxplot(aes(mandible.depth))}
      else if(input$rooBHS=="sex"){
        kan+geom_boxplot(aes(mandible.depth, color=sex))}
      else if(input$rooBHS=="species"){
        kan+geom_boxplot(aes(mandible.depth, color=species))}
    }
    else if(input$rooGraphs=="raHBox"){
      if(input$rooBHS=="neither"){
        kan+geom_boxplot(aes(ramus.height))}
      else if(input$rooBHS=="sex"){
        kan+geom_boxplot(aes(ramus.height, color=sex))}
      else if(input$rooBHS=="species"){
        kan+geom_boxplot(aes(ramus.height, color=species))}
    }
    
    #histograms
    else if(input$rooGraphs=="baLHist"){
      if(input$rooBHS=="neither"){
        kan+geom_histogram(aes(basilar.length))}
      else if(input$rooBHS=="sex"){
        kan+geom_histogram(aes(basilar.length, fill=sex))}
      else if(input$rooBHS=="species"){
        kan+geom_histogram(aes(basilar.length, fill=species))}
    }
    else if(input$rooGraphs=="ocLHist"){
      if(input$rooBHS=="neither"){
        kan+geom_histogram(aes(occipitonasal.length))}
      else if(input$rooBHS=="sex"){
        kan+geom_histogram(aes(occipitonasal.length, fill=sex))}
      else if(input$rooBHS=="species"){
        kan+geom_histogram(aes(occipitonasal.length, fill=species))}
    }
    else if(input$rooGraphs=="paLHist"){
      if(input$rooBHS=="neither"){
        kan+geom_histogram(aes(palate.length))}
      else if(input$rooBHS=="sex"){
        kan+geom_histogram(aes(palate.length, fill=sex))}
      else if(input$rooBHS=="species"){
        kan+geom_histogram(aes(palate.length, fill=species))}
    }
    else if(input$rooGraphs=="paWHist"){
      if(input$rooBHS=="neither"){
        kan+geom_histogram(aes(palate.width))}
      else if(input$rooBHS=="sex"){
        kan+geom_histogram(aes(palate.width, fill=sex))}
      else if(input$rooBHS=="species"){
        kan+geom_histogram(aes(palate.width, fill=species))}
    }
    else if(input$rooGraphs=="naLHist"){
      if(input$rooBHS=="neither"){
        kan+geom_histogram(aes(nasal.length))}
      else if(input$rooBHS=="sex"){
        kan+geom_histogram(aes(nasal.length, fill=sex))}
      else if(input$rooBHS=="species"){
        kan+geom_histogram(aes(nasal.length, fill=species))}
    }
    else if(input$rooGraphs=="naWHist"){
      if(input$rooBHS=="neither"){
        kan+geom_histogram(aes(nasal.width))}
      else if(input$rooBHS=="sex"){
        kan+geom_histogram(aes(nasal.width, fill=sex))}
      else if(input$rooBHS=="species"){
        kan+geom_histogram(aes(nasal.width, fill=species))}
    }
    else if(input$rooGraphs=="sqDHist"){
      if(input$rooBHS=="neither"){
        kan+geom_histogram(aes(squamosal.depth))}
      else if(input$rooBHS=="sex"){
        kan+geom_histogram(aes(squamosal.depth, fill=sex))}
      else if(input$rooBHS=="species"){
        kan+geom_histogram(aes(squamosal.depth, fill=species))}
    }
    else if(input$rooGraphs=="laWHist"){
      if(input$rooBHS=="neither"){
        kan+geom_histogram(aes(lacrymal.width))}
      else if(input$rooBHS=="sex"){
        kan+geom_histogram(aes(lacrymal.width, fill=sex))}
      else if(input$rooBHS=="species"){
        kan+geom_histogram(aes(lacrymal.width, fill=species))}
    }
    else if(input$rooGraphs=="zyWHist"){
      if(input$rooBHS=="neither"){
        kan+geom_histogram(aes(zygomatic.width))}
      else if(input$rooBHS=="sex"){
        kan+geom_histogram(aes(zygomatic.width, fill=sex))}
      else if(input$rooBHS=="species"){
        kan+geom_histogram(aes(zygomatic.width, fill=species))}
    }
    else if(input$rooGraphs=="orWHist"){
      if(input$rooBHS=="neither"){
        kan+geom_histogram(aes(orbital.width))}
      else if(input$rooBHS=="sex"){
        kan+geom_histogram(aes(orbital.width, fill=sex))}
      else if(input$rooBHS=="species"){
        kan+geom_histogram(aes(orbital.width, fill=species))}
    }
    else if(input$rooGraphs=="roWHist"){
      if(input$rooBHS=="neither"){
        kan+geom_histogram(aes(.rostral.width))}
      else if(input$rooBHS=="sex"){
        kan+geom_histogram(aes(.rostral.width, fill=sex))}
      else if(input$rooBHS=="species"){
        kan+geom_histogram(aes(.rostral.width, fill=species))}
    }
    else if(input$rooGraphs=="ocDHist"){
      if(input$rooBHS=="neither"){
        kan+geom_histogram(aes(occipital.depth))}
      else if(input$rooBHS=="sex"){
        kan+geom_histogram(aes(occipital.depth, fill=sex))}
      else if(input$rooBHS=="species"){
        kan+geom_histogram(aes(occipital.depth, fill=species))}
    }
    else if(input$rooGraphs=="crWHist"){
      if(input$rooBHS=="neither"){
        kan+geom_histogram(aes(crest.width))}
      else if(input$rooBHS=="sex"){
        kan+geom_histogram(aes(crest.width, fill=sex))}
      else if(input$rooBHS=="species"){
        kan+geom_histogram(aes(crest.width, fill=species))}
    }
    else if(input$rooGraphs=="foLHist"){
      if(input$rooBHS=="neither"){
        kan+geom_histogram(aes(foramina.length))}
      else if(input$rooBHS=="sex"){
        kan+geom_histogram(aes(foramina.length, fill=sex))}
      else if(input$rooBHS=="species"){
        kan+geom_histogram(aes(foramina.length, fill=species))}
    }
    else if(input$rooGraphs=="maLHist"){
      if(input$rooBHS=="neither"){
        kan+geom_histogram(aes(mandible.length))}
      else if(input$rooBHS=="sex"){
        kan+geom_histogram(aes(mandible.length, fill=sex))}
      else if(input$rooBHS=="species"){
        kan+geom_histogram(aes(mandible.length, fill=species))}
    }
    else if(input$rooGraphs=="maWHist"){
      if(input$rooBHS=="neither"){
        kan+geom_histogram(aes(mandible.width))}
      else if(input$rooBHS=="sex"){
        kan+geom_histogram(aes(mandible.width, fill=sex))}
      else if(input$rooBHS=="species"){
        kan+geom_histogram(aes(mandible.width, fill=species))}
    }
    else if(input$rooGraphs=="maDHist"){
      if(input$rooBHS=="neither"){
        kan+geom_histogram(aes(mandible.depth))}
      else if(input$rooBHS=="sex"){
        kan+geom_histogram(aes(mandible.depth, fill=sex))}
      else if(input$rooBHS=="species"){
        kan+geom_histogram(aes(mandible.depth, fill=species))}
    }
    else if(input$rooGraphs=="raHHist"){
      if(input$rooBHS=="neither"){
        kan+geom_histogram(aes(ramus.height))}
      else if(input$rooBHS=="sex"){
        kan+geom_histogram(aes(ramus.height, fill=sex))}
      else if(input$rooBHS=="species"){
        kan+geom_histogram(aes(ramus.height, fill=species))}
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
      if(input$rooBHS=="neither"){
        kan+geom_point(aes(palate.length, palate.width))}
      else if(input$rooBHS=="sex"){
        kan+geom_point(aes(palate.length, palate.width, color=sex))}
      else if(input$rooBHS=="species"){
        kan+geom_point(aes(palate.length, palate.width, color=species))}
    }
    else if(input$rooGraphs=="naLWScat"){
      if(input$rooBHS=="neither"){
        kan+geom_point(aes(nasal.length, nasal.width))}
      else if(input$rooBHS=="sex"){
        kan+geom_point(aes(nasal.length, nasal.width, color=sex))}
      else if(input$rooBHS=="species"){
        kan+geom_point(aes(nasal.length, nasal.width, color=species))}
    }
    else if(input$rooGraphs=="maLWScat"){
      if(input$rooBHS=="neither"){
        kan+geom_point(aes(mandible.length, mandible.width))}
      else if(input$rooBHS=="sex"){
        kan+geom_point(aes(mandible.length, mandible.width, color=sex))}
      else if(input$rooBHS=="species"){
        kan+geom_point(aes(mandible.length, mandible.width, color=species))}
    }
    else if(input$rooGraphs=="maLDScat"){
      if(input$rooBHS=="neither"){
        kan+geom_point(aes(mandible.length, mandible.depth))}
      else if(input$rooBHS=="sex"){
        kan+geom_point(aes(mandible.length, mandible.depth, color=sex))}
      else if(input$rooBHS=="species"){
        kan+geom_point(aes(mandible.length, mandible.depth, color=species))}
    }
    else if(input$rooGraphs=="maWDScat"){
      if(input$rooBHS=="neither"){
        kan+geom_point(aes(mandible.width, mandible.depth))}
      else if(input$rooBHS=="sex"){
        kan+geom_point(aes(mandible.width, mandible.depth, color=sex))}
      else if(input$rooBHS=="species"){
        kan+geom_point(aes(mandible.width, mandible.depth, color=species))}
    }
  })

  ## Model Fitting
  #GLR
  output$rooGLR <- renderPrint({
    #set seed for reproduction purposes
    set.seed(17)
    #Training/Testing Data
    train <- sample(1:nrow(roo), size = nrow(roo)*input$rooTrain)
    rooTrainDat <- roo[train,]
    rooTestDat <- roo[-train,]
    
    #fit
    lmod <- multinom(reformulate(input$rooGLRVars, "species"),
                     data=rooTrainDat)
    #predict
    lmodPred <- predict(lmod, newdata = rooTestDat)
    
    #classification rate
    class <- table(rooTestDat$species, lmodPred)
    misclassRate <- 1-(sum(diag(class))/sum(class))
    
    #output
    list(
      "Model_Summary"=summary(lmod),
      "Classification"=class,
      "Missclassification_Error_Rate"= misclassRate)
  })
  
  #Classification Tree
  output$rooTree <- renderPrint({
    #set seed for reproduction purposes
    set.seed(17)
    #Training/Testing Data
    train <- sample(1:nrow(roo), size = nrow(roo)*input$rooTrain)
    rooTrainDat <- roo[train,]
    rooTestDat <- roo[-train,]
    
    #fit
    treemod <- tree(reformulate(input$rooTreeVars, "species"),
                    data = rooTrainDat)
    #predict
    treePred <- predict(treemod, newdata = rooTestDat)
    
    #output
    list(
      "Model_Summary"=summary(treemod))
  })
  
  #Random Forest
  output$rooRand <- renderPrint({
    #set seed for reproduction purposes
    set.seed(17)
    #Training/Testing Data
    train <- sample(1:nrow(roo), size = nrow(roo)*input$rooTrain)
    rooTrainDat <- roo[train,]
    rooTestDat <- roo[-train,]
    
    #fit
    rfmod <- randomForest(reformulate(input$rooForestVars, "species"), 
                          data = rooTrainDat, 
                          mtry = 1,
                          ntree = 100, 
                          importance = TRUE)
    #predict
    rfPred <- predict(rfmod, newdata = rooTestDat)
    
    #classification rate
    class <- table(rooTestDat$species, rfPred)
    missclassRate <- 1-(sum(diag(class))/sum(class))
    
    #output
    list(
      "Model_Summary"=summary(rfmod),
      "Classification"=class,
      "Missclassification_Error_Rate"=missclassRate
    )
  })
  
  ## Prediction
  #models
  output$rooPred <- renderPrint({
    #set seed for reproduction purposes
    set.seed(17)

    #Training/Testing Data
    train <- sample(1:nrow(roo), size = nrow(roo)*input$rooTrain)
    rooTrainDat <- roo[train,]
    rooTestDat <- roo[-train,]
    
    #fit linear mod
    lmod <- multinom(reformulate(input$rooGLRVars, "species"),
                     data=rooTrainDat)
    #fit tree
    treemod <- tree(reformulate(input$rooTreeVars, "species"),
                    data = rooTrainDat)
    #fit rand. forest
    rfmod <- randomForest(reformulate(input$rooForestVars, "species"), 
                          data = rooTrainDat, 
                          mtry = 1,
                          ntree = 100, 
                          importance = TRUE)
    
    #create prediction data from user input
    rooPredDat <- data.frame(sex=as.factor(input$sexIn), basilar.length=input$balIn, occipitonasal.length=input$oclIn,
                             palate.length=input$palIn, palate.width=input$pawIn, nasal.length=input$nalIn,
                             nasal.width=input$nawIn, squamosal.depth=input$sqdIn, lacrymal.width=input$lawIn,
                             zygomatic.width=input$zywIn, orbital.width=input$orwIn, .rostral.width=input$rowIn,
                             occipital.depth=input$ocdIn, crest.width=input$crwIn, foramina.length=input$folIn,
                             mandible.length=input$malIn, mandible.width=input$mawIn, mandible.depth=input$madIn,
                             ramus.height=input$rahIn)
    
    #Output options - Predict with user input
    if(input$rooSelect=="glr"){
      predict(lmod, newdata = rooPredDat)
    }
    else if(input$rooSelect=="classtr"){
      predict(treemod, newdata = rooPredDat)
    }
    else if(input$rooSelect=="rf"){
      predict(rfmod, newdata = rooPredDat)
    }
    
  })
  
  
  ## Download data
  output$rooSubDat <- renderDataTable(kanga[ ,c(input$rooSubVars), drop=FALSE])
  
  
  output$rooSave <- downloadHandler(filename = "kangaroo.csv",
                                    content = write.csv(kanga[ ,c(input$rooSubVars), drop=FALSE], "kanga.csv"))
  
})