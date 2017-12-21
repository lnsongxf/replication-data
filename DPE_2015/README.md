### Economic Shocks and Civil Conflict Onset in Sub-Saharan Africa, 1981-2010
[Defence and Peace Economics](http://www.tandfonline.com/doi/full/10.1080/10242694.2014.887489), 2015.

**Abstract:** 
A number of studies have examined the link between rainfall and conflict but results so far have been inconclusive.
This study examines the effect of rainfall on economic performance in different sectors and conflict onset. 
The empirical analysis finds no support for a strong relation between rainfall and conflict as most results are not robust to different model specifications. 
The results also do not provide conclusive evidence for a link between growth in specific economic sectors and civil conflict onset.

**Data used:** 
* [UCDP/PRIO Armed Conflict Dataset](http://www.pcr.uu.se/research/ucdp/datasets/ucdp_prio_armed_conflict_dataset/)
* [World Bank Development Indicators](http://databank.worldbank.org/data/reports.aspx?source=world-development-indicators)
* [NASA Global Precipitation Climatology Project](http://precip.gsfc.nasa.gov/)

![](http://i.imgur.com/0FjHa2G.png)

#### Replication material

Model estimation done with `Stata/IC 12.1`.
Preparation of precipitation data was done in `R`.
Figures made using `R` (note that some of the figures in the paper were made using `Stata`, but they can be replicated using the `R` code).

If in trying to replicate the results you notice anything unusual, please contact me at weezel.van[at]gmail.com

The repository contains three folders with the following content:

`code` contains:

* `precipitation.R`, which aggregates the rainfall data   
* `replication_dpe.do`, replicates the estimation results
* `figures_dpe.R`, creates the figures used in the paper

`raw_data` contains:

* `NASA`, a folder with the raw rainfall data in ASCII format    
* `shp`, a folder  with the shapefiles needed for `precipitation.R` and `figures_dpe.R`       

`tidy_data` contains:

* `dpe_2015`, which is the main dataset provided as a Stata `.dta` file and as a `.csv` file    
*  `map_data.csv`, is the csv-file needed for `figures_dpe.R`        
*  `precipitation.csv`, is a csv-file with rainfall data aggregated to country-year level, the product of `precipitation.R`. 

