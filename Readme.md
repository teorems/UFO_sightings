# UFO Sightings around the world

**Dashboard: <https://teorems.github.io/UFO_sightings/>**, with maps, charts and tables of NUFORC reports from around the world and GEIPAN cases in France.

The [National UFO Reporting Center](https://nuforc.org/) hosts reporting about UFO sightings.

I started this project at first with a dataset provided by [planetsig](https://github.com/planetsig/ufo-reports) which spans from 1900 to 2014. The problem with this data, even if geolocated, was that very few countries outside US were specified and should be sought with reversed geocoding or locality matching. Moreover, the full description included in the online reports was not there.

After some tinkering with this dataset, i resolved myself to build a scraper on R to retrieve the original data. The data retrieval can be quite time consuming and is better done in steps or from the cloud (e.g. start the code in bundles, on different jupyter notebooks, free on Kaggle) .

I include here all the datasets which include reports compiled until October 2022 . The first dataset is a mostly a quite funny florilegium of historical anecdotes. The records that follow are voluntary reports, sometimes lucid, sometimes unorthodox, submitted to the website.

Country names are normalised when the data is loaded: spelling variants, typos, regions and non-answers are mapped to one name per country through `data/country_names.csv`, which can be edited directly. City names still need cleaning.

The dashboard merges every extract in `data/`, covering reports from the 1400s through October 2022, and is still just there to give a rough idea of the phenomenon.

## The dashboard

The dashboard at <https://teorems.github.io/UFO_sightings/> is served by GitHub Pages from the `docs/` folder. It needs no server: the page is plain HTML and JavaScript (Leaflet for the maps, Plotly for the charts) and reads its data from JSON files in `docs/data/`.

To keep it in sync after the data changes, regenerate those files with R from the repository root and commit them:

```
Rscript scripts/build_site_data.R
```

The dashboard shows each NUFORC report's short summary and links to the full report on nuforc.org; the full texts (~50 MB compressed) would make the page too heavy.

`app.R` is the original Shiny version of the dashboard. It is no longer deployed, but it still runs locally in R and shows the full report texts.

## France: GEIPAN cases

The dashboard also has a France page built on the case database of [GEIPAN](https://www.cnes-geipan.fr), the unit of the French space agency (CNES) that investigates unidentified aerospace phenomena. Each case is classified after investigation, from A (identified) to D (still unexplained). The data is GEIPAN's case export (`data/export_cas.xlsx`, downloaded from the case search page on their site); to update it, download the export again and replace the file. Case descriptions are in French, and GEIPAN rounds locations to 0.1° to protect witnesses.

## Status of the scraper

The NUFORC data stops in October 2022 and is no longer updated. The scraping scripts in `scripts/` and `funcs/` were written for NUFORC's old site (the `/webreports/` pages), which disappeared when the site was rebuilt, so they no longer work. They are kept for reference only. NUFORC's [terms of service](https://nuforc.org/terms/) also forbid scraping the site without written consent.

The report pages still exist on the new site under a new address. The dashboard rewrites the stored links to `https://nuforc.org/sighting/?id=<report number>`, so each report still opens from the map and the table.
