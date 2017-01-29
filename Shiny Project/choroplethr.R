library(choroplethr)
library(choroplethrMaps)
library(mapproj)
library(ggplot2)
library(dplyr)
library(choroplethrZip)


#Pull in zip.regions metadata file for choroplethrZip
data(zip.regions)
head(zip.regions)
#FIPS codes for each county in NYC, helpful for making maps
nyc_fips = c(36005, 36047, 36061, 36081, 36085)
colnames(zip.regions)
unique(zip.regions[zip.regions$state.name == 'district of columbia', ]$county.fips.numeric)
#Test data file:A data.frame containing population estimates 
data(df_pop_zip)

# highlight a county
highlight_county = function(county_fips)
{
  library(choroplethrMaps)
  data(county.map, package="choroplethrMaps", envir=environment())
  df = county.map[county.map$region %in% county_fips, ]
  geom_polygon(data=df, aes(long, lat, group = group), color = "yellow", fill = NA, size = 0)
}

#Zoom County
dd_fips = c(36061, 36081, 36047, 36005, 36085)
zip_choropleth(df_pop_zip, 
               county_zoom=dd_fips,  
               title="New York Test",
               legend="Population",
               reference_map = TRUE) +
  highlight_county(dd_fips)


data(df_zip_demographics)

# FIPS codes for the 5 counties (boroughs) of New York City
nyc_fips = c(36005, 36047, 36061, 36081, 36085)

# print a map for each column of the demographic data.frame
for (i in 2:ncol(df_zip_demographics))
{
  # set the data and title
  df_zip_demographics$value = df_zip_demographics[,i]
  title = paste0("2013 New York City ZCTA Demographics:\n",
                 colnames(df_zip_demographics[i]))
  
  # print the map
  choro = zip_choropleth(df_zip_demographics, title=title, county_zoom=nyc_fips)
  print(choro)
}

