library(dplyr)
library(tidyr)
clean_bm <- tbl_df(read.csv("~/NYCDSA /Shiny Project/full_data.csv"))

clean_bm2 <- tbl_df(read.csv("~/NYCDSA /Shiny Project/clean_bm.csv"))
clean_bm$PropType
sf_data <- tbl_df(read.csv("~/NYCDSA /Shiny Project/San Francisco/Existing_Commercial_Buildings_Energy_Performance_Ordinance_Report_Clean.csv"))

sf_data <- rename(sf_data, X2012.Weather.Normalized.Source.EUI..kBtu.ft2. = X2012.Weather.Normalized.Source.EUI..kBtu.sq.ft.)
sf_data$city = c("San Francisco")
sf_tidy <- sf_data %>%
  gather(key, value, X2011.ENERGY.STAR.Score:X2015.Weather.Normalized.Source.EUI..kBtu.ft2., -Building.Name, -Building.Address, -Postal.Code, -Floor.Area, -Property.Type...Self.Selected) %>%
  extract(key, c("Year", "Measure"), "X(201.).(.+)") %>%
  spread(Measure, value) %>%
  #Only take observations that have at least 1 energy data reading
  filter((Weather.Normalized.Source.EUI..kBtu.ft2. > 1 | Source.EUI..kBtu.ft2. > 1 | Weather.Normalized.Site.EUI..kBtu.ft2. > 1 | Site.EUI..kBtu.ft2. > 1)) %>%
  #only take observations with realistic energy data readings
  filter(Weather.Normalized.Source.EUI..kBtu.ft2. < 5000) %>%
  select(city, Building.Name, Building.Address, Zip.Code = Postal.Code, ReportedGFA = Floor.Area, PropType = Property.Type...Self.Selected, SiteEUI = Site.EUI..kBtu.ft2., NormSiteEUI = Weather.Normalized.Site.EUI..kBtu.ft2., SourceEUI = Source.EUI..kBtu.ft2., NormSourceEUI = Weather.Normalized.Source.EUI..kBtu.ft2., ENERGY.STAR.Score, Year)

clean_bm[] <- lapply(clean_bm, as.character)
sf_tidy[] <- lapply(sf_tidy, as.character)

clean_bm <- full_join(clean_bm, sf_tidy)

clean_bm$X <- NULL
clean_bm$city <- as.factor(clean_bm$city)
clean_bm$Borough <- as.factor(clean_bm$Borough)
clean_bm$Zip.Code <- as.integer(clean_bm$Zip.Code)
clean_bm$SiteEUI <- as.numeric(clean_bm$SiteEUI)
clean_bm$NormSiteEUI <- as.numeric(clean_bm$NormSiteEUI)
clean_bm$SourceEUI <- as.numeric(clean_bm$SourceEUI)
clean_bm$NormSourceEUI <- as.numeric(clean_bm$NormSourceEUI)
clean_bm$ENERGY.STAR.Score <- as.integer(clean_bm$ENERGY.STAR.Score)
clean_bm$ReportedGFA <- as.integer(clean_bm$ReportedGFA)
clean_bm$GovGFA <- as.integer(clean_bm$GovGFA)
clean_bm$PropType <- as.factor(clean_bm$PropType)
clean_bm$Year <- as.factor(clean_bm$Year)
clean_bm$Ward <- as.factor(clean_bm$Ward)
clean_bm$Building.Name <- as.character(clean_bm$Building.Name)
clean_bm$Building.Address <- as.character(clean_bm$Building.Address)

clean_bm <- clean_bm %>%
  filter(NormSourceEUI > 1) 
clean_bm %>% 
  filter(city == "New York City") %>%
  group_by(Year) %>% summarise(n())


######################PROPERTY TYPE###############################
clean_bm$PropType <- as.character(clean_bm$PropType)
clean_bm$DirtyPropType <- clean_bm$PropType
clean_bm$CleanPropType <- NULL
clean_bm$PropType <- clean_bm2$PropType
head(clean_bm)

saveRDS(clean_bm, file = "~/NYCDSA /Shiny Project/clean_bm.rdata")
write.csv(clean_bm, file = "~/NYCDSA /Shiny Project/clean_bm.csv")

test<-clean_bm %>%
  mutate('yoy_energy' = (NormSourceEUI - lag(NormSourceEUI, order_by = Year))/lag(NormSourceEUI, order_by = Year))
test$yoy_energy

write.csv(clean_bm$PropType, file = "~/NYCDSA /Shiny Project/cleanproptype.csv")
