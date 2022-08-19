# UFO Sightings all over the world

The [National UFO Reporting Center](https://nuforc.org/) hosts data about UFO sightings starting from the 15 century.

Even if most of the data is about this century, clearly.

I started this project at first with a dataset provided by [planetsig](https://github.com/planetsig/ufo-reports) which spans from 1900 to 2014. The problem with this data, even if geolocated, was that very few countries outside US were specified and should be sought with reversed geocoding or locality matching. Moreover, the full description included in the online reports was not there.

After some tinkering with this dataset, i resoIved myself to build a scraper on R to retrieve the original data. The data retrieval can be quite time consuming and is better done in steps or from the cloud (e.g. start the code in bundles, on differents jupyter notebooks, free on Kaggle) .

I include here as an example only the last dataset which spans from 2011 to June 2022. Additional data cleaning is needed to normalize the toponyms, task which i plan to do next.

The dashboard is still in progress and is just there to give a rough idea of the phenomenon.
