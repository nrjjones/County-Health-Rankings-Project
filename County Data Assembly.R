################################################################################
### Environment setup
################################################################################

library(httr)          # cURL-like tools, command line web interface
library(jsonlite)      # Parses output from APIs
library(ggmap)         # Mapping and geocoding tools
library(tidyverse)     # Lots of data tools, https://www.tidyverse.org/packages/
library(rvest)         # Scrape web pages
library(tidycensus)    # Access to Census data

source("./secrets.R")          # Loads my Google API Key
source("./fun/get20results.R") # Loads function to get first 20 Google results

################################################################################
### Load Data from Rankings
################################################################################

# Obesity, smoking

# Read data
CHR <- read_csv("./raw/Smoking and Obesity.csv")

# Rename columns, convert from character to numeric and divide by 100
CHR <- CHR %>%
  transmute(
    county=county,
    smkpreg=`% Births for Which Mothers Smoked During Pregnancy`,
    obesity_women=`% of women w obesity`
  )

CHR$county_raw <- CHR$county
CHR$county <- paste0(CHR$county, " County, Wisconsin")

# Need some more details for some counties

CHR$county <- case_when(
  CHR$county_raw=="Ashland" ~ "Ashland, Ashland County, Wisconsin",
  CHR$county_raw=="Kenosha" ~ "Kenosha, Kenosha County, Wisconsin",
  CHR$county_raw=="Kewaunee" ~ "Kewaunee, Kewaunee County, Wisconsin",
  CHR$county_raw=="Manitowoc" ~ "Manitowoc, Manitowoc County, Wisconsin",
  CHR$county_raw=="Ozaukee" ~ "Port Washington, Ozaukee County, Wisconsin",
  CHR$county_raw=="Racine" ~ "Racine, Racine County, Wisconsin",
  CHR$county_raw=="Sheboygan" ~ "Sheboygan, Sheboygan County, Wisconsin",
  TRUE~CHR$county
)


CHR$smkpreg <- str_remove(CHR$smkpreg, "%")
CHR$obesity_women <- str_remove(CHR$obesity_women, "%")

CHR$smkpreg <- as.numeric(CHR$smkpreg)/100
CHR$obesity_women <- as.numeric(CHR$obesity_women)/100

# Remove Statewide
CHR <- CHR[-1, ]

# Clinic data

clinic <- read_csv("./raw/fpclinicwisc.csv")
names(clinic)[2] <- "county_raw"

CHR <- left_join(CHR, clinic)

# Medicaid, poverty, HS education

rankings <- read_csv("./raw/rankingsmeasures.csv")
rankings <- select(rankings, -c("State", "County"))
CHR <- left_join(CHR, rankings)

################################################################################
### NICU list
################################################################################

# From Jenna, source?

NICU <- c(
  "St Joseph's Hospital, Milwaukee",
  "Aurora Sinai, Milwaukee",
  "St. Joseph's Hospital, Marshfield",
  "Meriter Hospital, Madison",
  "Children Hospital of Wisconsin, Milwaukee",
  "St. Mary's Hospital, Madison",
  "St. Vincent Hospital, Green Bay",
  "Columbia-St. Mary's Hospital, Milwaukee",
  "St. Luke's Hospital, Racine",
  "Children's Hospital of Wisconsin- Fox Valley, Neenah",
  "Waukesha Memorial Hospital, Waukesha",
  "Aurora Women's Pavilion, West Allis",
  "Gunderson Lutheran Hospital, LaCrosse",
  "Aurora Bay Care, Green Bay",
  "St. Elizabeth Hospital, Affinity Health System, Appleton",
  "Franciscan Skemp Hospital, La Crosse"
)

################################################################################
### Planned Parenthood list
################################################################################

# Original search

# PP <- get20results("planned parenthood health center wisconsin")
# pp <- PP$name
# pp[21] <- "Planned Parenthood - Milwaukee-Water Street Health Center"
# PP <- as.data.frame(pp)
# write_csv(PP, "./raw/PP List.csv")

pp <- read_csv("./raw/PP List.csv")
pp <- as.character(unlist(pp))

################################################################################
### Distance to NICUs
################################################################################

# Set FROM to sample addresses, TO to first in list

# Initialize
# from <- CHR$county
# to <- NICU[1]
#
# # Mapdist to calculate distance and estimated duration
# # First search hit
# dist1 <- mapdist(
#   from,
#   to,
#   mode="driving",
#   output="simple"
# )
#
# dist <- dist1
#
# # Mapdist to calculate distance and estimated duration
# # Loop through 2 to N search hits
# # Appends to end of the existing file to create a dataset
#
# # Test
# # for(i in 2:3) {
#
# for(i in 2:length(NICU)) {
#
#   to <- NICU[i]
#
#   disti <- mapdist(
#     from,
#     to,
#     mode="driving",
#     output="simple"
#   )
#   dist <- bind_rows(dist, disti)
# }
#
# write_csv(dist, "./out/NICU_distances.csv")
# nicd <- dist

nicd <- read_csv("./out/NICU_distances.csv")

################################################################################
### Distance to Planned Parenthoods
################################################################################

# Set FROM to sample addresses, TO to first in list

# # Initialize
#  from <- CHR$county
#  to <- pp[1]
#
# # Mapdist to calculate distance and estimated duration
# # First search hit
#  dist1 <- mapdist(
#    from,
#    to,
#    mode="driving",
#    output="simple"
#  )
#
#  dist <- dist1
#
# # Mapdist to calculate distance and estimated duration
# # Loop through 2 to N search hits
# # Appends to end of the existing file to create a dataset
#
# # Test
# # for(i in 2:3) {
#
# for(i in 2:length(pp)) {
#   to <- pp[i]
#
#    disti <- mapdist(
#     from,
#     to,
#     mode="driving",
#     output="simple"
#   )
#   dist <- bind_rows(dist, disti)
#  }
#
# write_csv(dist, "./out/pp_distances.csv")
# ppd <- dist

ppd <- read_csv("./out/pp_distances.csv")

################################################################################
### Nearest NICU and Planned Parenthood
################################################################################

ppn <- ppd %>%
  group_by(from) %>%
  arrange(minutes) %>%
  filter(row_number()==1)

NICUn <- nicd %>%
  group_by(from) %>%
  arrange(minutes) %>%
  filter(row_number()==1)

################################################################################
### Merge nearest
################################################################################

ppn <- select(ppn, c("from", "to", "minutes"))
names(ppn) <- c("county", "pp_nearest", "pp_minutes")

NICUn <- select(NICUn, c("from", "to", "minutes"))
names(NICUn) <- c("county", "nicu_nearest", "nicu_minutes")

CHR <- left_join(CHR, ppn)
CHR <- left_join(CHR, NICUn)

# Write files

write_csv(CHR, "./out/Courtney.csv")
write_csv(ppn, "./out/Nearest PP.csv")
write_csv(NICUn, "./out/Nearest NICU.csv")

write_csv(ppd, "./out/Distances from County to all PP.csv")
write_csv(nicd, "./out/Distances from County to all NICU.csv")



