# UFO Sightings around the world

The [National UFO Reporting Center](https://nuforc.org/) hosts reporting about UFO sightings.

I started this project at first with a dataset provided by [planetsig](https://github.com/planetsig/ufo-reports) which spans from 1900 to 2014. The problem with this data, even if geolocated, was that very few countries outside US were specified and should be sought with reversed geocoding or locality matching. Moreover, the full description included in the online reports was not there.

After some tinkering with this dataset, i resoIved myself to build a scraper on R to retrieve the original data. The data retrieval can be quite time consuming and is better done in steps or from the cloud (e.g. start the code in bundles, on differents jupyter notebooks, free on Kaggle) .

I include here all the datasets which include reports compiled until June 2022 . The first dataset is a mostly a quite funny florilegium of historical anecdotes. The records that follow are voluntary reports, sometimes lucid, sometimes unorthodox, submitted to the website.

Additional data cleaning is needed to normalize the toponyms, task which i plan to do next.

The dashboard is still in progress and is just there to give a rough idea of the phenomenon.
