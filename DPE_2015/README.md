Economic Shocks and Civil Conflict Onset in Sub-Saharan Africa, 1981-2010
================

This repository contains the replication material for:

"Economic Shocks and Civil Conflict Onset in Sub-Saharan Africa, 1981-2010" ([Defence and Peace Economics](http://www.tandfonline.com/doi/full/10.1080/10242694.2014.887489))

The statistical analysis and creation of some of the figures was done using `Stata/IC 12.1`, the maps and aggregation of the rainfall data was carried out using `R`.

The repository contains three folders with the following content:

`code` contains:

* `precipitation.R` which aggregates the rainfall data   
* `replication_dpe.do` replicates the estimation results
* `figures_dpe.R` creates the figures used in the paper

`raw_data` contains:

* `NASA` a folder with the raw rainfall data in ASCII format    
* `shp` a folder  with the shapefiles needed for `precipitation.R` and `figures_dpe.R`       

`tidy_data` contains:

* `re-analysis_rain_shocks_conflict` which is the main dataset provided as a Stata `.dta` file and as a `.csv` file    
*  `map_data.csv` is the csv-file needed for `figures_dpe.R`        
*  `precipitation.csv` is a csv-file with rainfall data aggregated to country-year level, the product of `precipitation.R`. 

If in trying to replicate the results you notice anything unusual, please contact me at weezel.van[at]gmail.com
