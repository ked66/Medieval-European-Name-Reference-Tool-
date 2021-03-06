# Medieval European Name Reference Tool -- README

## App Purpose

![Screenshot of App Homepage](https://github.com/ked66/Medieval-European-Name-Reference-Tool-/blob/master/images/Home.png)

The purpose of this app is to explore the usage of various forenames (AKA "given names" or "first names") in Medieval Europe, c. 500 - 1600 AD. All data is from the [Dictionary of Medieval Names from European Sources](http://dmnes.org/), reformatted for easy visualization. Detailed information regarding the research process can be found on their website. 

The app can be accessed [here](https://katherine-dillon15.shinyapps.io/Medieval-European-Name-Reference-Tool-/)

## App

The app has four main features:
*  A <b>Reactive Table</b>, which displays all names meeting criteria set by user
*  A <b>Summary of Etymology and Usage</b>, which displays a summary of the DMNES page for selected name or name variation
*  An <b>Interactive Line Graph</b>, which displays name usage over time for selected name
*  An <b>Interactive Chloropleth Map</b>, which displays name usage across Europe for selected name

### Reactive Table

![Screenshot of Reactive Table with example search](https://github.com/ked66/Medieval-European-Name-Reference-Tool-/blob/master/images/Search%20Example.png)

Reactive Data Table allows users to filter names and name variations by the following criteria:

*  Name Type - show all names & variation, only CNF names, or only variations of a selected CNF
*  Country of Use - option to show only names used in selected country/countries
*  First Letter - option to show only names with selected first letter(s)
*  Language of Origin - option to show only names with select language(s) or origin
*  Year Range of Use - option to show only names with recorded uses within specified range

Users can also search by name meaning, and sort by number of recorded uses (an imperfect surrogate measure of popularity).

<img src="https://github.com/ked66/Medieval-European-Name-Reference-Tool-/blob/master/images/Filter%20Options.png" alt="Screenshot of table filter options" width="33%"/>

### Summary of Etymology and Usage

![Screenshot of Additional Information Tab](https://github.com/ked66/Medieval-European-Name-Reference-Tool-/blob/master/images/Additional%20Information.png)

For a selected CNF, this Reactive User Interface displays the following information:

*  Etymology - language of origin, the name written in that language (if that language doesn't use the Latin alphabet), and meaning
*  List of notable Medieval people with the CNF (e.g. Biblical characters, Saints, monarchs)
*  List of Variations, with their grammatical case as appropriate
*  Citation and Link to DMNES page

If a user selects a name variation, the CNF for that variation is displayed.

### Interactive Line Graph

![Screenshot of Usage Over Time tab](https://github.com/ked66/Medieval-European-Name-Reference-Tool-/blob/master/images/Usage%20Over%20Time.png)

For each CNF, users can elect to graph usage over time in two ways:

*  Number of uses by century - graph CNF as a whole and up to 10 variations
*  Variations as percent of CNF uses by century - graph up to 10 variations

    - e.g. If a CNF has 3 recorded uses in the 12th century, and variation_A has 1 recorded use in the 12th century, then variation_A represents 33% of recorded uses in the 12th century  

Clicking on the graph displays uses by name variation by century - in numbers and percent of CNF uses.

This graph is generated via two Reactive Data Tables (one for CNF names, one for variations), which provide the data. The graph is stored as a Reactive Value that Renders when users click a "Graph Now" button. Clicking functionality comes from a Reactive User Interface connected with the Data Tables.

### Interactive Chloropleth Map

![Screenshot of Name Distribution tab](https://github.com/ked66/Medieval-European-Name-Reference-Tool-/blob/master/images/Name%20Distribution.png)

This interactive map is generated using Leaflet and a geojson map of Europe downloaded from https://geojson-maps.ash.ms/. 

For each CNF, users can elect to graph usage across Europe in two ways:

*  Number of uses by country
    - e.g. a country with 14 uses of "John" will be darker than a country with 5 uses of "John"
*  Percent of all uses by country
    - e.g. a country where 20% of all recorded name uses were "John" will be darker than a country where 10% of all recorded name uses were "John"

Clicking on a country in the graph will display all variations of the CNF with recorded uses in that country, with a year range of recorded use. This functionality is directly part of the Leaflet map.

### Images
Aside from graphs, all images in this app come from the Metropolitan Museum of Art and are designated as being in the Public Domain.

## Data

The code used to scrape data from the [Dictionary of Medieval Names from European Sources](http://dmnes.org/) is found in "Webscraping Functions.R" and "Variations Scrape.R". For each name, the following information was scraped:

*  Gender (male, female, or unclear)
*  Etymology (i.e. language of origin and meaning)
*  Notable people with the name
*  DMNES-preferred citation
*  Countries of use
*  Dates of recorded uses, grouped by country of use
*  Name variations (e.g. "Jean" for "John")
*  Dates of recorded uses, grouped by name variation

With light pre-processing (see "Data Pre-Processing.R"), two tables were created for the app: app_name_data.csv and app_variations.csv. Columns are listed below.

app_name_data.csv
*  NAME - canonical name form (i.e. main name, not a variation)
*  GENDER - gender of name (male, female, or unclear)
*  FIRST_LETTER - first letter of the name ("A", "B", etc.)
*  LENGTH - number of letters in name
*  ORIGIN - country of origin (e.g. England, Germany, Italy)
*  MEANING - meaning of name (e.g. "brave soldier")
*  NOTES - list of noteable people with that CNF
*  CITATION - DMNES-preferred citation
*  LINK - link to DMNES page for that CNF
*  AREA - country of usage
*  EARLIEST - year of earliest recorded use of name
*  LATEST - year of latest recorded use of name
*  MENTIONS - total number recorded uses in given area/country from 500 to 1600 AD
*  TOTAL_MENTIONS - total number of recorded uses in all countries from 500 to 1600 AD
*  SIXTH, SEVENTH . . . SIXTEENTH - number recorded uses in given area/country by century (SIXTH is number in sixth century, etc.)

variations_data.csv - same as app_name_data.csv except the following:
*  NAME - name variation
*  CNF - canonical name form of the variation (corresponds with NAME in app_name_data.csv)
