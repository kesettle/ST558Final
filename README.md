# ST558Final

## About

The purpose of this application is to explore, model, and predict with using three different data sets that the user can choose from: `diamonds`, `pottery`, and `kanga`.  
Each data set has its own exploratory data analysis (EDA) tab, modeling tab, and prediction tab, as well as a tab to look at and explore the data itself.  
The EDA tab includes statistical summaries, variable frequencies, and various graphs to choose from.  
Each data set allows three different models to be fit: a linear regression type model, a tree-based model, and a random forest model. The user may pick one of these models (after fitting) to predict with, inputting or selecting values to use for the prediction.

## Packages Required

*packages required and code to install/load them*

The following packages are used to run the app: 
- `shiny` - used to create and run the app
- `shinydashboard` - used for app dashboard creation
- `DT` - used for displaying data tables
- `tidyverse` - used for data manipulation, graphing, and to access the `diamonds` data
- `corrplot` - used to create correlation plots
- `caret` - used for modeling and predicting
- `tree` - used for modeling
- `randomForest` - used for modeling
- `HSAUR3` - used to access the `pottery` data
- `faraway` - used to access the `kanga` data


Use the following code to install the packages if you do not have them or do not have all of them:

```{r}
install.packages(c("shiny", "shinydashboard", "DT", "tidyverse", "corrplot", "caret", "tree", "randomForest", "HSAUR3", "faraway"))
```

## Run Away!

Now that you know about the app, you can explore it. Run the app with the following code:

```{r}
shiny::runGitHub("kesettle/ST558Final")
```
