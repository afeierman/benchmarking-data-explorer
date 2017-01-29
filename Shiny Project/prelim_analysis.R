#Special thanks to Ari Lamstein for his excellent choropleth packages and documentation

#########################################START############################################
library(dplyr)
library(ggplot2)
library(ggthemes)

##############################################NYC########################################################
#Data is released on a year by year basis. Read each year in, then add to a central DF
nyc_bm_2014 <- tbl_df(read.csv("~/NYCDSA /Shiny Project/NYC/Energy_and_Water_Data_Disclosure_for_Local_Law__2014_NYC.csv"))
nyc_bm_2013 <- tbl_df(read.csv("~/NYCDSA /Shiny Project/NYC/Energy_and_Water_Data_Disclosure_for_Local_Law_84__2013_.csv"))
nyc_bm_2012 <- tbl_df(read.csv("~/NYCDSA /Shiny Project/NYC/Energy_and_Water_Data_Disclosure_for_Local_Law_84__2012_.csv"))
nyc_bm_2011 <- tbl_df(read.csv("~/NYCDSA /Shiny Project/NYC/Energy_and_Water_Data_Disclosure_for_Local_Law_84__2011_.csv"))

nyc_bm_2014$Year = 2014
nyc_bm_2013$Year = 2013
nyc_bm_2012$Year = 2012
nyc_bm_2011$Year = 2011

trim_2011 <- select(nyc_bm_2011, BBL, Street.Number, Street.Name, Borough, Zip.Code = Zip, Site.EUI.kBtu.ft2. = Site.EUI, Weather.Normalized.Source.EUI.kBtu.ft2. = Weather.Normalized.Source.EUI, ENERGY.STAR.Score, Reported.Building.SF = Reported.Building.Square.Footage, Property.Type = Reported.Facility.Type, Number.of.Buildings, Year)

#some issues with 2012: typo in Buildings, inconsistent periods at end of sentences, column names changed from 2011
trim_2012 <- select(nyc_bm_2012, BBL, Street.Number, Street.Name, Borough, Zip.Code = Zip, Site.EUI.kBtu.ft2., Weather.Normalized.Source.EUI.kBtu.ft2., ENERGY.STAR.Score, Reported.Building.SF =  Property.Floor.Area..Buildngs.and.Parking..ft2., Property.Type = Primary.Property.Type...Self.Selected, Number.of.Buildings, Year)

trim_2013 <- select(nyc_bm_2013, BBL = NYC.Borough..Block..and.Lot..BBL., Street.Number, Street.Name, Borough, Zip.Code, Site.EUI.kBtu.ft2., Weather.Normalized.Site.EUI.kBtu.ft2., Source.EUI.kBtu.ft2., Weather.Normalized.Source.EUI.kBtu.ft2., ENERGY.STAR.Score, Reported.Building.SF = Reported.Property.Floor.Area..Building.s....ft.., DOF.Property.Floor.Area = DOF.Property.Floor.Area..Buildngs.and.Parking..ft2., Property.Type = Primary.Property.Type...Self.Selected, Number.of.Buildings = DOF.Number.of.Buildings, Year)

trim_2014 <- select(nyc_bm_2014, BBL = NYC.Borough..Block..and.Lot..BBL., Street.Number, Street.Name, Borough, Zip.Code, Site.EUI.kBtu.ft2., Weather.Normalized.Site.EUI.kBtu.ft2., Source.EUI.kBtu.ft2., Weather.Normalized.Source.EUI.kBtu.ft2., ENERGY.STAR.Score, Reported.Building.SF = Reported.Property.Floor.Area..Building.s....ft.., DOF.Property.Floor.Area = DOF.Property.Floor.Area..Buildngs.and.Parking..ft2., Property.Type = Primary.Property.Type...Self.Selected, Number.of.Buildings = DOF.Number.of.Buildings, Year)

#This was necessary to combine the data sets, and I will put things into the correct datatype in a later step

trim_2011[] <- lapply(trim_2011, as.character)
trim_2012[] <- lapply(trim_2012, as.character)
trim_2013[] <- lapply(trim_2013, as.character)
trim_2014[] <- lapply(trim_2014, as.character)

nyc_bm <- full_join(trim_2014, trim_2013)
nyc_bm <- full_join(nyc_bm, trim_2012)
nyc_bm <- full_join(nyc_bm, trim_2011)

#Remove Observations that don't provide energy consumption values
nyc_bm <- filter(nyc_bm, ENERGY.STAR.Score != "See Primary BBL")


#Get columns in the right format
nyc_bm$Site.EUI.kBtu.ft2. <- as.numeric(nyc_bm$Site.EUI.kBtu.ft2.)
nyc_bm$Weather.Normalized.Site.EUI.kBtu.ft2.<- as.numeric(nyc_bm$Weather.Normalized.Site.EUI.kBtu.ft2.)
nyc_bm$Source.EUI.kBtu.ft2. <- as.numeric(nyc_bm$Source.EUI.kBtu.ft2.)
nyc_bm$Weather.Normalized.Source.EUI.kBtu.ft2. <- as.numeric(nyc_bm$Weather.Normalized.Source.EUI.kBtu.ft2.)
nyc_bm$ENERGY.STAR.Score <- as.numeric(nyc_bm$ENERGY.STAR.Score)
nyc_bm$Reported.Building.SF <- as.numeric(nyc_bm$Reported.Building.SF)
nyc_bm$Property.Type <- as.factor(nyc_bm$Property.Type)
nyc_bm$Borough <- as.factor(toupper(gsub("[ ]+", "", nyc_bm$Borough)))
nyc_bm$Zip.Code <- as.character(nyc_bm$Zip.Code)
nyc_bm$Year <- factor(nyc_bm$Year)

#We now have a basic NYC Benchmarking Data Set. Now we'll clean the data some more. 
#47k observations

#Since we're going to aggregate by Zip Code for the project, we filter out all properties w/o an entry for Zip Code

nyc_bm <- filter(nyc_bm, is.na(nyc_bm$Zip.Code) == F)

#43k observations

#Only take observations that have at least 1 energy data reading
nyc_bm <- filter(nyc_bm, (Weather.Normalized.Source.EUI.kBtu.ft2. > 0 | Source.EUI.kBtu.ft2. > 0 | Weather.Normalized.Site.EUI.kBtu.ft2. > 0 | Site.EUI.kBtu.ft2. > 0))

#34k observations
#Only take reasonable observations
nyc_bm <- filter(nyc_bm, (Weather.Normalized.Source.EUI.kBtu.ft2. < 1000 | Source.EUI.kBtu.ft2. < 1000 | Weather.Normalized.Site.EUI.kBtu.ft2. < 1000 | Site.EUI.kBtu.ft2. < 1000))

#34k observations

nyc_shiny <- group_by(nyc_bm, Year, Borough, Zip.Code) %>%
  summarize("Observations" = n(), "MedNormSourceEUI" = median(Weather.Normalized.Source.EUI.kBtu.ft2., na.rm=TRUE), "MedSiteEUI" = median(Site.EUI.kBtu.ft2.), "MedSourceEUI" = median(Source.EUI.kBtu.ft2., na.rm = T), "MedNormSiteEUI" = median(Weather.Normalized.Source.EUI.kBtu.ft2., na.rm = T))

#Only use Zip Codes with more than 1 observation
nyc_shiny <- filter(nyc_shiny, Observations > 1)

#write the data set to R, to be used by my Shiny app
write.csv(nyc_shiny, "~/NYCDSA /Shiny Project/nyc_shiny.csv")


remove(nyc_bm_2011, nyc_bm_2012, nyc_bm_2013, nyc_bm_2014, trim_2011, trim_2012, trim_2013, trim_2014)
###############################################DC##############################################

#xlsx library is necessary to read in DC benchmarking data
library(xlsx)
dc_bm_2011 <- tbl_df(read.xlsx("~/NYCDSA /Shiny Project/DC/BenchmarkDC_Disclosure_2011_final_022014.xlsx", 1))
dc_bm_2012 <- tbl_df(read.xlsx("~/NYCDSA /Shiny Project/DC/BenchmarkDC_Disclosure_2012_final_022014.xlsx", 1))
dc_bm_2013 <- tbl_df(read.csv("~/NYCDSA /Shiny Project/DC/2013 Energy and Water Performance Benchmarking Results 09-30-2016.csv"))
dc_bm_2014 <- tbl_df(read.csv("~/NYCDSA /Shiny Project/DC/2014 Energy and Water Performance Benchmarking Results 09-30-2016.csv"))
dc_bm_2015 <- tbl_df(read.csv("~/NYCDSA /Shiny Project/DC/2015 Energy and Water Performance Benchmarking Results 09-30-2016.csv"))
#I would like to use this, but unsure how to assign each converted data frame to the right variable
#sapply(c(dc_bm_2011, dc_bm_2012, dc_bm_2013, dc_bm_2014, dc_bm_2015), tbl_df)

dc_bm_2011$Year <- 2011
dc_bm_2012$Year <- 2012
dc_bm_2013$Year <- 2013
dc_bm_2014$Year <- 2014
dc_bm_2015$Year <- 2015

#Useful to add this now, so that join goes better later
dc_bm_2012$TaxGFA = as.character(NA)

#I found it was easiest to get the joins (later on) to cooperate if I made everything a character, and making every field a character was the easiest way to do so. I'll set datatypes after combining everything.
dc_bm_2011[] <- lapply(dc_bm_2011, as.character)
dc_bm_2012[] <- lapply(dc_bm_2012, as.character)
dc_bm_2013[] <- lapply(dc_bm_2013, as.character)
dc_bm_2014[] <- lapply(dc_bm_2014, as.character)
dc_bm_2015[] <- lapply(dc_bm_2015, as.character)

trim_dc_2011 <- select(dc_bm_2011, PropertyID = DC.Real.Property.ID, Address = Address.of.Record, Ward, City, State, Zip.Code = Postal.Code, PropType = Primary.Property.Type, Year.Built, ReportedGFA = Reported.Building.Gross.Floor.Area..ft.., TaxGFA = Tax.Record.Building.Gross.Floor.Area..ft.., SiteEUI = Site.EUI..kBtu.ft.., NormSourceEUI = Weather.Normalized.Source.EUI..kBtu.ft.., ENERGY.STAR.Score, ElectricUse = Electricity.Use...Grid.Purchase.and.Onsite..kWh., NatGas = Natural.Gas.Use..therms., Year)

trim_dc_2012 <- select(dc_bm_2012,  PropertyID = DC.Real.Property.ID, Address = Address.of.Record, Ward, City, State, Zip.Code = Postal.Code, PropType = Primary.Property.Type, Year.Built, ReportedGFA = Reported.Building.Gross.Floor.Area..ft.., TaxGFA, SiteEUI = Site.EUI..kBtu.ft.., NormSourceEUI = Weather.Normalized.Source.EUI..kBtu.ft.., ENERGY.STAR.Score, ElectricUse = Electricity.Use...Grid.Purchase.and.Onsite..kWh., NatGas = Natural.Gas.Use..therms., Year)

trim_dc_2013 <- select(dc_bm_2013,  PropertyID = DC.Real.Property.ID, Address = Address.of.Record, Ward, City, State, Zip.Code = Postal.Code, PropType = Primary.Property.Type...Calculated, Year.Built, ReportedGFA = Reported.Building.Gross.Floor.Area, TaxGFA = Tax.Record.Floor.Area, SiteEUI = Site.EUI..kBtu.ft.., NormSiteEUI = Weather.Normalized.Site.EUI..kBtu.ft.., SourceEUI = Source.EUI..kBtu.ft.., NormSourceEUI = Weather.Normalized.Source.EUI..kBtu.ft.., ENERGY.STAR.Score, ElectricUse = Electricity.Use..kWh., NatGas = Natural.Gas.Use..therms., Year)

trim_dc_2014 <- select(dc_bm_2014, PropertyID = DC.Real.Property.ID, Address = Address.of.Record, Ward, City, State, Zip.Code = Postal.Code, PropType = Primary.Property.Type...Calculated, Year.Built, ReportedGFA = Reported.Building.Gross.Floor.Area, TaxGFA = Tax.Record.Floor.Area, SiteEUI = Site.EUI..kBtu.ft.., NormSiteEUI = Weather.Normalized.Site.EUI..kBtu.ft.., SourceEUI = Source.EUI..kBtu.ft.., NormSourceEUI = Weather.Normalized.Source.EUI..kBtu.ft.., ENERGY.STAR.Score, ElectricUse = Electricity.Use..kWh., NatGas = Natural.Gas.Use..therms., Year)

trim_dc_2015 <- select(dc_bm_2015,  PropertyID = DC.Real.Property.ID, Address = Address.of.Record, Ward, City, State, Zip.Code = Postal.Code, PropType = Primary.Property.Type...Calculated, Year.Built, ReportedGFA = Reported.Building.Gross.Floor.Area, TaxGFA = Tax.Record.Floor.Area, SiteEUI = Site.EUI..kBtu.ft.., NormSiteEUI = Weather.Normalized.Site.EUI..kBtu.ft.., SourceEUI = Source.EUI..kBtu.ft.., NormSourceEUI = Weather.Normalized.Source.EUI..kBtu.ft.., ENERGY.STAR.Score, ElectricUse = Electricity.Use..kWh., NatGas = Natural.Gas.Use..therms., Year)

dc_bm <- full_join(trim_dc_2011, trim_dc_2012)
dc_bm <- full_join(dc_bm, trim_dc_2013)
dc_bm <- full_join(dc_bm, trim_dc_2014)
dc_bm <- full_join(dc_bm, trim_dc_2015)

#Set empty strings as NAs. Solution from Stack Overflow
## define a helper function
empty_as_na <- function(x){
  if("factor" %in% class(x)) x <- as.character(x) ## since ifelse wont work with factors
  ifelse(as.character(x)!="", x, NA)
}

## transform all columns
dc_bm <- dc_bm %>% mutate_each(funs(empty_as_na)) 


#The DC join created a lot of NAs. I don't know why, but now I'm deleting them:
dc_bm <- dc_bm[is.na(dc_bm$Address) == F, ]
#Cleaning out bad data Zip Code information not filled in)
dc_bm <- dc_bm[is.na(dc_bm$Zip.Code) == F,]
#Removing Zip Code information beyond the standard 5 digits
dc_bm$Zip.Code <- substr(dc_bm$Zip.Code, 1, 5)
#Only take observations that have at least 1 reasonable energy data reading
dc_bm <- filter(dc_bm, (NormSourceEUI > 1 | SourceEUI > 1 | NormSiteEUI > 1 | SiteEUI > 1 | ENERGY.STAR.Score <= 100))
#4.5k observations
#Fix some weird Year Built variables... assume typo
dc_bm$Year.Built <- gsub(1000, 2000, dc_bm$Year.Built)
#only use buildings that have either an Electricity Use value OR an Energy Star Score
dc_bm <- filter(dc_bm, (is.na(ElectricUse) == F & is.na(ENERGY.STAR.Score) == F) )
#3.5k observations
#note: add Ward later
dc_bm$PropertyID <- gsub("\\D*", "", dc_bm$PropertyID)
dc_bm$PropertyID <- substr(test, 1, 8)
dc_bm$PropertyID

dc_shiny <- group_by(dc_bm, Year, Zip.Code) %>%
  summarize("Count" = n(), "MedNormSourceEUI" = median(as.numeric(NormSourceEUI), na.rm=TRUE), 
            "MedSiteEUI" = median(as.numeric(SiteEUI)), "MedSourceEUI" = median(as.numeric(SourceEUI), na.rm = T),
            "MedNormSiteEUI" = median(as.numeric(NormSiteEUI), na.rm = T))

#Only use Zip Codes with more than 1 submission
dc_shiny <- filter(dc_shiny, Count > 1)

write.csv(dc_shiny, "~/NYCDSA /Shiny Project/dc_shiny.csv")

remove(dc_bm_2011, dc_bm_2012, dc_bm_2013, dc_bm_2014, dc_bm_2015, trim_dc_2011, trim_dc_2012, trim_dc_2013, trim_dc_2014, trim_dc_2015)
###################################CREATEMASTERLIST##############################
#combine the existing datasets!
nyc_bm$city <- c("New York City")

nyc_bm2 <- select(nyc_bm, ID = BBL, city, Borough, Zip.Code, SiteEUI = Site.EUI.kBtu.ft2., NormSiteEUI = Weather.Normalized.Site.EUI.kBtu.ft2., SourceEUI = Source.EUI.kBtu.ft2., NormSourceEUI = Weather.Normalized.Source.EUI.kBtu.ft2., ENERGY.STAR.Score, ReportedGFA = Reported.Building.SF, GovGFA = DOF.Property.Floor.Area, PropType = Property.Type, Year)
dc_bm$city <- c("DC")

dc_bm2 <- select(dc_bm, ID = PropertyID, city, Ward, Zip.Code, SiteEUI, NormSiteEUI, SourceEUI, NormSourceEUI, ENERGY.STAR.Score, ReportedGFA, GovGFA = TaxGFA, PropType, Year)


nyc_bm2[] <- lapply(nyc_bm2, as.character)
dc_bm2[] <- lapply(dc_bm2, as.character)
full_data <- tbl_df(full_join(nyc_bm2, dc_bm2))

dc_shiny$city <- c('DC')
nyc_shiny$city <- c('NYC')

bm_full <- tbl_df(full_join(dc_shiny, nyc_shiny))

bm_full$city <- as.factor(bm_full$city)
bm_full$Year <- as.factor(bm_full$Year)
bm_full$region <- bm_full$Zip.Code


#write it out so it's accessible for my shiny project
write.csv(bm_full, "~/NYCDSA /Shiny Project/bm_full.csv")
write.csv(full_data, "~/NYCDSA /Shiny Project/full_data.csv")

##########################ZIP CODE CONNECTION#########################################
nyc_zips <- bm_full[bm_full$city == "NYC", ]
nyc_zips <- unique(nyc_zips)
nyc_zips <- rename(nyc_zips, zip = Zip.Code)

dc_zips <- unique(bm_full[bm_full$city == "DC", ])
dc_zips <- rename(dc_zips, zip = Zip.Code)
head(dc_zips)

library(zipcode)
data(zipcode)
nyc_data <- merge(nyc_zips, zipcode, by.x="zip",by.y="zip")
dc_data <- merge(dc_zips, zipcode, by.x="zip",by.y="zip" )
full_zips <- full_join(nyc_data, dc_data)
head(full_zips)
full_zips$city.x
full_zips %>%
  filter(city.x %in% "DC")
full_zips
write.csv(full_zips, "~/NYCDSA /Shiny Project/full_zips.csv")

ggmap(get_map("New York, New York",zoom=11,color = "bw")) + geom_point(data=filter(full_zips, city.x %in% "NYC"), aes(x=longitude,y=latitude, size = MedNormSourceEUI), color='red') + scale_color_gradient(low = 'green', high = 'orange')

ggmap(get_map("Washington, DC",zoom=12,color = "bw")) + geom_point(data=dc_data, aes(x=longitude,y=latitude, size = MedNormSourceEUI), color='green') + scale_color_gradient(low = 'green', high = 'orange')
######################################PLOTTING#####################################

#xploratory plotting
nyc_plot <- ggplot(nyc_bm, aes())
nyc_plot + geom_point(aes(x = Reported.Building.SF, y = Weather.Normalized.Source.EUI.kBtu.ft2.)) + xlim(0, 1e+06) + ylim(0 , 1e+05)

nyc_plot + geom_point(aes(x = Reported.Building.SF, y = Weather.Normalized.Source.EUI.kBtu.ft2.)) + xlim(0, 1e+06) + ylim(0 , 1000)

#Histograms of NYC energy consumption
ggplot(nyc_bm, aes(x = Source.EUI.kBtu.ft2., fill = Borough)) + geom_histogram(bins = 100) + xlim(0, 1000) + ggtitle("Source EUI")
ggplot(nyc_bm, aes(x = Weather.Normalized.Source.EUI.kBtu.ft2., fill = Year)) + geom_histogram(bins = 100) + xlim(0, 1000) + ggtitle("Weather Normalized Source EUI")
ggplot(nyc_bm, aes(x = Site.EUI.kBtu.ft2., fill = Borough)) + geom_histogram(bins = 100) + xlim(0, 1000) + ggtitle("Site EUI")
ggplot(nyc_bm, aes(x = Weather.Normalized.Site.EUI.kBtu.ft2., fill = Borough)) + geom_histogram(bins = 100) + xlim(0, 1000) + ggtitle("Weather Normalized Site EUI")

#Same thing, but for DC
ggplot(dc_bm, aes(x = SourceEUI, fill = Ward)) + geom_histogram(bins = 100) + xlim(0, 1000) + ggtitle("Source EUI")
ggplot(dc_bm, aes(x = NormSourceEUI, fill = Year)) + geom_histogram(bins = 100) + xlim(0, 1000) + ggtitle("Weather Normalized Source EUI")
ggplot(dc_bm, aes(x = SiteEUI, fill = Ward)) + geom_histogram(bins = 100) + xlim(0, 1000) + ggtitle("Site EUI")
ggplot(dc_bm, aes(x = NormSiteEUI, fill = Ward)) + geom_histogram(bins = 100) + xlim(0, 1000) + ggtitle("Weather Normalized Site EUI")

#Combining the two
ggplot(bm_full, aes(x = MedNormSourceEUI, fill = state)) + geom_histogram(bins = 100) + xlim(0, 1000) + ggtitle("Median Weather Normalized Source EUI by Zip Code")

ggplot(nyc_shiny, aes(x = MedSourceEUI, fill = Borough)) + geom_histogram(bins = 100) + xlim(0, 1000) + ggtitle("Median Source EUI by Zip Code")
ggplot(nyc_shiny, aes(x = MedNormSourceEUI, fill = Borough)) + geom_histogram(bins = 100) + xlim(0, 1000) + ggtitle("Median Weather Normalized Source EUI by Zip Code") +theme_economist()
ggplot(nyc_shiny, aes(x = MedSiteEUI, fill = Borough)) + geom_histogram(bins = 100) + xlim(0, 1000) + theme_gdocs() + ggtitle("Median Site EUI by Zip Code")
ggplot(nyc_shiny, aes(x = MedNormSiteEUI, fill = Borough)) + geom_histogram(bins = 100) + xlim(0, 1000) + ggtitle("Median Weather Normalized Source EUI by Zip Code") + theme_fivethirtyeight()

#switching map



################################MAPS##################################################
library(choroplethr)
library(choroplethrMaps)
library(mapproj)
library(choroplethrZip)

ny2014 <- nyc_shiny[nyc_shiny$Year == 2014,]
ny2014$region <- ny2014$Zip.Code
ny2014
# FIPS codes for the 5 counties (boroughs) of New York City
nyc_fips = c(36005, 36047, 36061, 36081, 36085)

zip_choropleth(ny2014, title=title, county_zoom=nyc_fips)
# print a map for each column of the demographic data.frame
for (i in 4:8) {
  # set the data and title
  ny2014$value <- ny2014[[i]]
  title = paste0("New York City Benchmarking Stats by Zip Code:\n", colnames(ny2014[i]))
  # print the map
  choro = zip_choropleth(ny2014, title=title, county_zoom=nyc_fips)
  print(choro)
}

