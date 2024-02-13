# Living in the Mountains. Settlement patterns in Northwestern Iberia during the Palaeolithic period
This repository contains data and code to enable reproducibility of the paper: Díaz-Rodríguez, M.<a href="https://orcid.org/0000-0002-2703-1507">
<img alt="ORCID logo" src="https://info.orcid.org/wp-content/uploads/2019/11/orcid_16x16.png" width="14" height="14" />
</a>. (submitted). "Living in the mountains. Settlement patterns in Northwestern Iberia during Palaeolithic period". Archaeological and Anthropological Sciences.

## Repository structure
/csv/*.csv ... contains CSV files with the data to elaborate some statistical analysis.

/figures/paper ... folder with the figures used in the paper in PNG format.

/figures/sm ... folder with the figures used in the supplementary material file in PNG format.

/grids/ ... folder with the layers of the covariates used in the study.

/output/ ... folder with the output files in PNG format.

/shp/ ... folder with the layers of the sites, the random points and the study area in vector format.

/sm/ ... folder with supplementary information appendix with a detailed description of the obtention of each variable and some extra information.

Diaz_23.html ... html file to reproduce the analysis.

Diaz_23.Rmd ... Rmarkdown file to reproduce the analysis.

## R Packages used
[dismo](https://cran.r-project.org/web/packages/dismo/dismo.pdf) - Methods for species distribution modelling.

[geostatsp](https://rdrr.io/cran/geostatsp/) - Geostatistical Modelling with Likelihood and Bayes.

[GGally](https://cran.r-project.org/web/packages/GGally/index.html) - This package is a plotting system based on the grammar of graphics.

[ggplot2](https://cran.r-project.org/web/packages/ggplot2/index.html) - Package for creating graphics.

[maps](https://cran.r-project.org/web/packages/maps/index.html) - Display of maps.

[maptools](https://cran.r-project.org/web/packages/maptools/index.html) - Set of tools for manipulating geographic data.

[MASS](https://cran.r-project.org/web/packages/MASS/MASS.pdf) - Functions and datasets to support Venables and Ripley.

[patchwork](https://cran.r-project.org/web/packages/patchwork/index.html) - Package for combining multiple plots.

[plyr](https://cran.r-project.org/web/packages/plyr/index.html) - Set of tools that solves problems relates with applying or combining data.

[purrrlyr](https://cran.r-project.org/web/packages/purrrlyr/index.html) - This package allows some functions at the intersection of “dplyr”. and “purrr”.

[raster](https://cran.r-project.org/web/packages/raster/index.html) - Reading, writing, manipulating, analysing and modelling of spatial data.

[readxl](https://cran.r-project.org/web/packages/readxl/index.html) - Package for read excel files.

[Rcmdr](https://cran.r-project.org/web/packages/Rcmdr/index.html) - A platform-independent basic-statistics GUI (graphical user interface) for R.

[rgdal](https://cran.r-project.org/web/packages/rgdal/index.html) - Provides bindings to the “GDAL” and “PROJ” library.

[rgeos](https://cran.r-project.org/web/packages/rgeos/index.html) - Package for topology operations on geometries.

[sp](https://cran.r-project.org/web/packages/sp/index.html) - Classes and methods for spatial data.

[spatstat](https://cran.r-project.org/web/packages/spatstat/index.html) - Toolbox for analysing Spatial Point Patterns.

[tidyverse](https://cran.r-project.org/web/packages/tidyverse/index.html) - Data representations and API design.

### References about the packages

Baddeley, A., Turner, R., Rubak, E., 2020. spatstat: Spatial Point Pattern Analysis, Model-Fitting, Simulation, Tests.

Becker, R.A., Wilks, A.R., Brownrigg, R., Minka, T.P., Deckmyn, A., 2021. maps: Draw Geographical Maps.

Bivand, R., Lewin-Koh, N., Pebesma, E., Archer, E., Baddeley, A., Bearman, N., Bibiko, H.-J., Brey, S., Callahan, J., Carrillo, G., Dray, S., Forrest, D., Friendly, M., Giraudoux, P., Golicher, D., Gómez Rubio, V., Hausmann, P., Hufthammer, K.O., Jagger, T., Johnson, K., Lewis, M., Luque, S., MacQueen, D., Niccolai, A., Pebesma, E., Prepiñán Lamigueiro, O., Plunkett, E., Rubak, E., Short, T., Snow, G., Stabler, B., Stokely, M., Turner, R., 2020a. maptools: Tools for Handling Spatial Objects.

Bivand, R., Keitt, T., Rowlingson, B., Pebesma, E., Sumner, M., Hijmans, R., Rouault, E., Warmerdam, F., Ooms, J., Rundel, C., 2020b. rgdal: Bindings for the “Geospatial” Data Abstraction Library.

Bivand, R., Rundel, C., Pebesma, E., Stuetz, R., Hufthammer, K.O., Giraudoux, P., Davis, M., Santilli, S., 2020c. rgeos: Interface to Geometry Engine - Open Source ('GEOS’).

Brown, P.E., 2015. Model-Based Geostatistics the Easy Way. Journal of Statistical Software 63, 1–24. doi:10.18637/jss.v063.i12

Fox, J., Marquez, M.M., Bouchet-Valat, M., 2023. Rcmdr: R Commander.

Henry, L., 2022. Tools at the Intersection of ‘purrr’ and ‘dplyr’.

Hijmans, R.J., Phillips, S., Leathwick, J., Elith, J., 2017. dismo: Species Distribution Modeling.

Hijmans, R.J., Etten, J. van, Sumner, M., Cheng, J., Baston, D., Bevan, A., Bivand, R., Busetto, L., Canty, M., Forrest, D., Ghosh, A., Golicher, D., Gray, J., Greenberg, J.A., Hiemstra, P., Hingee, K., Karney, C., Mattiuzzi, M., Mosher, S., Nowosad, J., Pebesma, E., Perpinan Lamigueiro, O., Racine, E.B., Rowlingson, B., Shortridge, A., Venables, B., Wueest, R., 2020. raster: Geographic Data Analysis and Modeling.

Pebesma, E., Bivand, R., Rowlingson, B., Gomez-Rubio, V., Hijmans, R., Sumner, M., MacQueen, D., Lemon, J., Lindgren, F., O’Brien, J., O’Rourke, J., 2020. sp: Classes and Methods for Spatial Data.

Pedersen, T.L., 2022. patchwork: The Composer of Plots.

Ripley, B., Venables, B., Bates, D.M., Hornik, K., Gebhardt, A., Firth, D., 2020. MASS: Support Functions and Datasets for Venables and Ripley’s MASS.

Schloerke, B., Cook, D., Larmarange, J., Briatte, F., Marbach, M., Thoen, E., Elberg, A., Toomet, O., Crowley, J., Hofmann, H., Wickham, H., 2021. GGally: Extension to “ggplot2.”

Wickham, H., 2020. plyr: Tools for Splitting, Applying and Combining Data.

Wickham, H., Bryan, J., Kalicinski, M., Valery, K., Leitienne, C., Colbert, B., Hoerl, D., Miller, E., 2019a. readxl: Read Excel Files.

Wickham, H., Averick, M., Bryan, J., Chang, W., D’Agostino McGowan, L., François, R., Grolemund, G., Hayes, A., Henry, L., Hester, J., Kuhn, M., Lin Pedersen, T., Miller, E., Milton Bache, S., Müller, K., Ooms, J., Robinson, D., Piage Seidel, D., Spinu, V., Takahashi, K., Vaughan, D., Wilke, C., Woo, K., Yutani, H., 2019a. [Welcome to the tidyverse](https://joss.theoj.org/papers/10.21105/joss.01686). J. Open Source Softw. 4, 1686. doi:10.21105/joss.01686

Wickham, H., Chang, W., Henry, L., Pedersen, T.L., Takahashi, K., Wilke, C., Woo, K., Yutani, H., Dunnington, D., 2020. ggplot2: Create Elegant Data Visualisations Using the Grammar of Graphics.


## Source code and data reference
Mikel Díaz-Rodríguez<a href="https://orcid.org/0000-0002-2703-1507">
<img alt="ORCID logo" src="https://info.orcid.org/wp-content/uploads/2019/11/orcid_16x16.png" width="14" height="14" />
</a>. (2023, November 16). Living in the Mountains. Settlement patterns in Northwestern Iberia during the Palaeolithic period. Source code and data. Zenodo. https://zenodo.org/doi/10.5281/zenodo.10142401

[![DOI](https://zenodo.org/badge/610742714.svg)](https://zenodo.org/doi/10.5281/zenodo.10142401)

[Link in Zenodo](https://doi.org/10.5281/zenodo.10142401) 

## License
CC-BY 4.0
