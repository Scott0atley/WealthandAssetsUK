# WealthandAssetsUK
Looking at the UK Wealth and Assets survey to assess wealth and inequality measures


## Setting up Data Structure
You can change directory/data routing at the top of the R code. However a basic directory structure such as: "G:\Stata data and do\UK Wealth and Assets\UKDA-7215-stata\stata\stata13_se\Data" will suffice. Of note, the way the code runs requires all data to be stored in the same local host folder. For this, move all household and indiviudal datasets into the same local folder. Currently this R code reads the data, strips it of required variables and writes .csv files at nearly every step of the analysis. This does create a non-small amount of files (this can be removed if required, it does however help in debugging). 

## Data
All code is commented upon. There is one master merged file that contains all data from household and indiviudal dataset waves of interest. However further datasets contain household and indiviudal data seperately. 

## Data Frames
Two 'master' dataframes have been created to store all important and relevant inequality measures. The first is a simple dataframe that includes mean, median houseprices, several Gini coefficients and the 90/10 ratio by wave. The second dataframe duplicates these statistics by region by wave. 

## Visualisations
There are several graphs and visualisations in this code. Some are 'test' graphs to make sure the dataframe is working as intended. These are labelled as such. Near the end of the code there are several graphs that look at the relationship of Gini and mean net wealth over each wave as well as producing Lorenz curves per wave and overall. Finally there are some graphs that look at the mean and median house prices per region by wave. 

## Next Steps
This code will be cleaned and some decomposition analysis will be conducted usign the Age variable. Weights have been included but as of yet the intergration is not perfect. Further work on this needed. 

