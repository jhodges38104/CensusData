


############### SET UP ##################

library(tidycensus)
library(tigris)
library(maps)
library(purrr)

# READ CENSUS API KEY
Sys.getenv("CENSUS_API_KEY")

# allow cache
options(tigris_use_cache = TRUE)




############### GET DATA ##################
# 2008-2012 5-year ACS estimates at the census tract level for all US states

acs_vars <- load_variables(2012, dataset = "acs5", cache = TRUE)

#### VARIABLE FOR ICE RACE+INC ####
# B19001_001: total household income
# B19001H_014: 100-125k, NH White
# B19001H_015: 125-150k, NH White
# B19001H_016: 150-200k, NH White
# B19001H_017: 200k+, NH White
# B19001B_002: < 10k, Black
# B19001B_003: 10-15k, Black
# B19001B_004: 15-20k, Black


#### VARIABLES FOR ICE OWNER V RENTER ####
# B25003_001 total tenure
# B25003_002 owner occupied
# B25003_003 renter occupied
# B25003H_002E white owner occupied
# B25003B_003E black renter occupied


#### VARIABLES FOR ICE INCOME ####
# B19001_014 100-125k
# B19001_015 125-150k
# B19001_016 150-200k
# B19001_017 200k+
# B19001_002 < 10k
# B19001_003 15-20k
# B19001_004


#### VARIABLES FOR ICE RACE ####
# B03002_001 total pop
# B03002_003 white nh alone
# B03002_004 black nh alone


# Get FIPS codes for all states and DC
us <- unique(fips_codes$state)[1:51]


# Download data
USacs2012_CT <- map_dfr(us, function(x) {
  get_acs(geography = "tract", variables = c("B19001_001", 
                                             "B19001H_014", "B19001H_015", "B19001H_016", "B19001H_017", 
                                             "B19001B_002", "B19001B_003", "B19001B_004", 
                                             "B25003_001", 
                                             "B25003_002", 
                                             "B25003_003",
                                             "B25003H_002",
                                             "B25003B_003",
                                             "B19001_014", "B19001_015", "B19001_016", "B19001_017", 
                                             "B19001_002", "B19001_003", "B19001_004", 
                                             "B03002_001", 
                                             "B03002_003", 
                                             "B03002_004"), 
          state = x, geometry = FALSE, output = "wide", year = 2012,
          keep_geo_vars = FALSE, moe_level = 95, survey = "acs5")
})




################ CLEAN DATA #################

# Omit the margin of error variables
USdf <- subset(USacs2012_CT, select = -c(B19001_001M,
                                         B19001H_014M, B19001H_015M, B19001H_016M, B19001H_017M, 
                                         B19001B_002M, B19001B_003M, B19001B_004M, 
                                         B25003_001M, B25003_002M, B25003_003M, 
                                         B25003H_002M, B25003B_003M,
                                         B19001_014M, B19001_015M, B19001_016M, B19001_017M, 
                                         B19001_002M, B19001_003M, B19001_004M,
                                         B03002_001M, B03002_003M, B03002_004M))

# Reformat as data frame
USdf <- as.data.frame(USdf)

# Rename variables
names(USdf) <- c("GEOID", "NAME", 
                 "totpop_income", 
                 "NHW100_125", "NHW125_150", "NHW150_200", "NHW200up", 
                 "Bund10", "B10_15", "B15_20", 
                 "totpop_tenure", 
                 "owner", "renter",  
                 "white_owner", "black_renter",
                 "all100_125", "all125_150", "all150_200", "all200up", 
                 "allund10", "all10_15", "all15_20",
                 "totpop_race", 
                 "NHW", "NHB")



################ CONSTRUCT ICE - CT #################


USdf$ICEraceinc <- ((USdf$NHW100_125 + USdf$NHW125_150 + USdf$NHW150_200 + USdf$NHW200up) - 
                    (USdf$B10_15 + USdf$B15_20 + USdf$Bund10))/ USdf$totpop_income

USdf$ICErace <- (USdf$NHW - USdf$NHB)/USdf$totpop_race


USdf$ICEinc <- ((USdf$all100_125 + USdf$all125_150 + USdf$all150_200 + USdf$all200up) - 
                  (USdf$all10_15 + USdf$all15_20 + USdf$allund10))/ USdf$totpop_income

USdf$ICEown <- (USdf$owner- USdf$renter)/USdf$totpop_tenure

USdf$ICEraceown <- (USdf$white_owner- USdf$black_renter)/USdf$totpop_tenure


summary(USdf$ICErace)
summary(USdf$ICEinc)
summary(USdf$ICEraceinc)
summary(USdf$ICEown)
summary(USdf$ICEraceown)
# All values between -1 and 1
# NAs for each variable are all for cases in which the denominator == 0


################ CREATE VAR FOR STATE #################


USdf$geoid_char <- as.character(USdf$GEOID)


USdf$state[startsWith(USdf$geoid, "01")] <- "AL"
USdf$state[startsWith(USdf$geoid, "02")] <- "AK"
USdf$state[startsWith(USdf$geoid, "04")] <- "AZ"
USdf$state[startsWith(USdf$geoid, "05")] <- "AR"
USdf$state[startsWith(USdf$geoid, "06")] <- "CA"
USdf$state[startsWith(USdf$geoid, "08")] <- "CO"
USdf$state[startsWith(USdf$geoid, "09")] <- "CT"
USdf$state[startsWith(USdf$geoid, "10")] <- "DE"
USdf$state[startsWith(USdf$geoid, "12")] <- "FL"
USdf$state[startsWith(USdf$geoid, "13")] <- "GA"
USdf$state[startsWith(USdf$geoid, "15")] <- "HI"
USdf$state[startsWith(USdf$geoid, "19")] <- "IA"
USdf$state[startsWith(USdf$geoid, "16")] <- "ID"
USdf$state[startsWith(USdf$geoid, "17")] <- "IL"
USdf$state[startsWith(USdf$geoid, "18")] <- "IN"
USdf$state[startsWith(USdf$geoid, "20")] <- "KS"
USdf$state[startsWith(USdf$geoid, "21")] <- "KY"
USdf$state[startsWith(USdf$geoid, "22")] <- "LA"
USdf$state[startsWith(USdf$geoid, "23")] <- "ME"
USdf$state[startsWith(USdf$geoid, "24")] <- "MD"
USdf$state[startsWith(USdf$geoid, "25")] <- "MA"
USdf$state[startsWith(USdf$geoid, "26")] <- "MI"
USdf$state[startsWith(USdf$geoid, "27")] <- "MN"
USdf$state[startsWith(USdf$geoid, "28")] <- "MS"
USdf$state[startsWith(USdf$geoid, "29")] <- "MO"
USdf$state[startsWith(USdf$geoid, "30")] <- "MT"
USdf$state[startsWith(USdf$geoid, "31")] <- "NE"
USdf$state[startsWith(USdf$geoid, "32")] <- "NV"
USdf$state[startsWith(USdf$geoid, "33")] <- "NH"
USdf$state[startsWith(USdf$geoid, "34")] <- "NJ"
USdf$state[startsWith(USdf$geoid, "35")] <- "NM"
USdf$state[startsWith(USdf$geoid, "36")] <- "NY"
USdf$state[startsWith(USdf$geoid, "37")] <- "NC"
USdf$state[startsWith(USdf$geoid, "38")] <- "ND"
USdf$state[startsWith(USdf$geoid, "39")] <- "OH"
USdf$state[startsWith(USdf$geoid, "40")] <- "OK"
USdf$state[startsWith(USdf$geoid, "41")] <- "OR"
USdf$state[startsWith(USdf$geoid, "42")] <- "PA"
USdf$state[startsWith(USdf$geoid, "44")] <- "RI"
USdf$state[startsWith(USdf$geoid, "45")] <- "SC"
USdf$state[startsWith(USdf$geoid, "46")] <- "SD"
USdf$state[startsWith(USdf$geoid, "47")] <- "TN"
USdf$state[startsWith(USdf$geoid, "48")] <- "TX"
USdf$state[startsWith(USdf$geoid, "49")] <- "UT"
USdf$state[startsWith(USdf$geoid, "50")] <- "VT"
USdf$state[startsWith(USdf$geoid, "51")] <- "VA"
USdf$state[startsWith(USdf$geoid, "53")] <- "WA"
USdf$state[startsWith(USdf$geoid, "54")] <- "WV"
USdf$state[startsWith(USdf$geoid, "55")] <- "WI"
USdf$state[startsWith(USdf$geoid, "56")] <- "WY"
USdf$state[startsWith(USdf$geoid, "11")] <- "DC"
  
                      
USdf <- subset(USdf, select = -geoid_char)

USdf_final <- subset(USdf, select = c(GEOID, NAME, state, ICErace, ICEinc, ICEraceinc, ICEown, ICEraceown))



################# WRITE CSV ###################

setwd("~/Harvard PhD in PHS/2019_R01_epigenetics/Data")

write.csv(USdf, "ICE_USCTs_ACS08_12_allvar.csv", row.names = FALSE)
write.csv(USdf_final, "ICE_USCTs_ACS08_12.csv", row.names = FALSE)                          



