# NOAA earthquake tools

This package allows the cleaning up and visualization of [NOAA Significant Earthquake Database](https://www.ngdc.noaa.gov/nndc/struts/form?t=101650&s=1&d=1). NOAA stands for U.S. National Oceanographic and Atmospheric Administration. The whole dataset (downloadable from the previous link) contains more than 5,900 earthquackes worldwide. The goal of this package is to make the information more readable and visualizable.

## Reading and cleaning data
Once you read the data (note it's a tab separated file) you can use eq_cleab_data() to convert the data in a more usable format

## Visualizations
There are two kind of visualization:
* ggplot2 based:
+geom_timeline - creates a timeline of selected earthquakes (see function description for more details)
+geom_timeline_label - add top n labels by magnitude of the earthquakes to the previously created timeline
*leaflet based:
+eq_map - plots the selected earthquakes on a leaflet map
+eq_create_label - generates a more informative labels to be used in the leafletmap 
