#Forecasting solar energy, wind energy, and total electricity generation with an ARIMA model
#This script contains the data preparation we use for the analysis. (replicated in the notebook)
library(tidyverse)
library(lubridate)

#obtain data from eia.gov website. This dataset contains electricity generation
#monthly for the total U.S., categorized by source.

elec.raw <- read.csv(file = "https://www.eia.gov/totalenergy/data/browser/csv.php?tbl=T01.02",
                     stringsAsFactors=FALSE)


str(elec.raw) #look at structure of elec.raw dataframe

#examine energy sources
(elec.sources <- unique(elec.raw$Description))


#look at the range of values for generation
range(elec.raw$Value) 

#some values are "Not Available", so we examine which
which(elec.raw$Value == "Not Available") #find the indices for "Not Available" values, yields [4194:5545]

#indices corresponding to "Not Available" belong to wind, solar, and geothermal
elec.raw[which(elec.raw$Value == "Not Available"),5]

#mutate(elec.raw$Values, 
#replace(elec.raw$Value, which(elec.raw$Value == "Not Available"), NA)
#as.numeric(elec.raw$Value) #

#We need to convert btus to gigawatt hours, so we use the ratio gigawatthours/quadrillion btus

qbtu.to.gigawatthours <- 293071.070172222215  # ratio of gigawatthours per quadrillion btus

#create dataframe for total energy generation

elec.total <-   mutate(
  elec.raw, gwh = as.numeric(Value)*qbtu.to.gigawatthours,  #convert btus to gigawatthours
  YYYYMMDD = ymd(paste(YYYYMM, "01",sep = ""))  #convert YYYYMM to date + first day of the month
  ) %>% 
  filter(Description == "Total Primary Energy Production", 
    is.na(YYYYMMDD) == FALSE) %>%
  select(gwh,Description, YYYYMMDD)

str(elec.total)
range(elec.total$YYYYMMDD)

#look at unique sources of energy (totals of groups removed):
(sources <- unique(elec.raw$Description)[-c(5,12,13)])

#create dataframe for seperate energy sources

elec.sources <-   mutate(
  elec.raw, gwh = as.numeric(Value)*qbtu.to.gigawatthours,  #convert btus to gigawatthours
  YYYYMMDD = ymd(paste(YYYYMM, "01",sep = ""))  #convert YYYYMM to date + first day of the month
  ) %>% 
  filter(Description %in% sources,  
    is.na(YYYYMMDD) == FALSE) %>%
  select(gwh,Description, YYYYMMDD)



#plot generation

ggplot(data = elec.total) + 
  geom_path(mapping = aes(x = elec.total$YYYYMMDD, y = elec.total$gwh),
            na.rm = TRUE) +
  ggtitle(label = "Total U.S. Electricity Generation 1973-2017") +
  xlab("Year") +
  ylab("Generation (gigawatt-hours)")

ggplot(data = elec.sources) + 
  geom_path(mapping = aes(x = elec.sources$YYYYMMDD,
                          y = elec.sources$gwh,
                          color = elec.sources$Description),
            na.rm = TRUE) +
  ggtitle(label = "U.S. Electricity Generation 1973-2017 by Source") +
  xlab("Year") +
  ylab("Generation (gigawatt-hours)")
  
