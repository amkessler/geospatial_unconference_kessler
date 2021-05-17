# geospatial_unconference_kessler

This repo contains materials for my 30-minute presentation on GIS using R at the May 2021 Washington, DC Journalism R "Unconference" hosted by Andrew Ba Tran.

If you would like to clone this repo into an R project in one step to follow along, you can do so by running this line in the console:

`usethis::use_course("https://tinyurl.com/s7rz2233")`

The primary geospatial packages we'll be touching on will be:

-   [sf](https://r-spatial.github.io/sf/)

-   [tmap](https://github.com/mtennekes/tmap)

-   [tigris](https://github.com/walkerke/tigris)

-   [leaflet](https://rstudio.github.io/leaflet/)

-   [maps](https://www.rdocumentation.org/packages/maps/versions/3.3.0)

-   [geosphere](https://cran.r-project.org/web/packages/geosphere/index.html)

The packages you'll need to install to accompany this repo if you don't have them already can be done by running this code in R:

`install.packages(c("sf", "tmap", "tmaptools", "tigris", "leaflet", "albersusa", "rmapshaper", "maps", "geosphere", "htmltools"))`

Note that depending on your operating system, you may need to install several dependencies such as GDAL outside of R itself (i.e. on the computer separately). A helpful walkthrough of how to do this on Windows, Mac and Linux is available from the [Carpentries](https://datacarpentry.org/geospatial-workshop/setup.html).

We'll also be discussing the importance of coordinate reference systems, and a resource for more information on that topic can be found on the irreverent yet informative site [here](https://ihatecoordinatesystems.com/).
