Food imports, international prices, and violence in Africa
================

Published in [Oxford Economics Papers, 2016](http://oep.oxfordjournals.org/content/early/2016/05/02/oep.gpw015.abstract).

**Summary of research:** 
This study examines the effect of food price fluctuations on violence in Africa, using international food prices as a source of exogenous shock weighted by a country’s import pattern of major food commodities to create a country-specific food price index. The regression analysis shows that between 1990 and 2011, food price increases are associated with higher levels of violence. Moving from low to high values in the price index corresponds, after controlling for economic, social, and political factors, to an increase in violence intensity of 1.3 incidents. This effect is predominantly driven by imports of low-value-added primary products. Despite the statistically significant results, the predictive power of food prices is relatively low, both in and out of sample. Using 2012 data for out-of-sample forecast shows that food prices are a relatively poor predictor of violence. 

**Data used:** [Social Conflict Analysis Database](https://www.strausscenter.org/scad.html), [Global Economic Monitor Commodities](http://databank.worldbank.org/data/reports.aspx?source=global-economic-monitor-%28gem%29-commodities), [FAO Trade data](http://faostat3.fao.org/home/E)

![](http://i.imgur.com/mfEDp2Q.png)

#### Replication material

Model estimation done in `R`.

If in trying to replicate the results you notice anything unusual, please contact me at weezel.van[at]gmail.com

The repository contains two folders with the following content:

`code` contains:

* `clean.R`, prepares the data for the regression analysis
* `functions.R`, contains all the functions used for the analysis
* `estimation.R`, is the main estimation as presented in the paper
* `cross_validation.R`, is the cross-validation exercise as presented in the paper
* `robustness.R`, are all the robustness checks as presented in the suporting material. 

`tidy_data` contains:

* `allData.Rdata`, which is the dataset used for the main estimation and the various robustness checks
* `ous.Rdata`, is the dataset for the cross-validation

