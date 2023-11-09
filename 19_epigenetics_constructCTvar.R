

library(tidycensus)
library(tigris)
library(dplyr)

# READ CENSUS API KEY
Sys.getenv("CENSUS_API_KEY")

# allow cache
options(tigris_use_cache = TRUE)



############### GET DATA ##################
# 2006-2010 5-year ACS estimates at the census tract level for MA

acs_vars <- load_variables(2010, dataset = "acs5", cache = TRUE)

# B03002_001 Estimate!!Total
# B03002_003 Estimate!!Total!!Not Hispanic or Latino!!White alone
# B03002_004 Estimate!!Total!!Not Hispanic or Latino!!Black or African Am

# B02001_001 Total
# B02001_002 White
# B02001_003 Black


# B19001_001: total household income
# B19001H_014: 100-125k, NH White
# B19001H_015: 125-150k, NH White
# B19001H_016: 150-200k, NH White
# B19001H_017: 200k+, NH White

# B19001B_002: < 10k, Black
# B19001B_003: 10-15k, Black
# B19001B_004: 15-20k, Black


# B19001_014: 100-125k
# B19001_015: 125-150k
# B19001_016: 150-200k
# B19001_017: 200k+

# B19001_002: < 10k
# B19001_003: 10-15k
# B19001_004: 15-20k


# B17001_001: total poverty status in the last 12 months by sex and age
# B17001_002: income in the past 12 months below the poverty level


MAacs2015_CT <- get_acs(geography = "tract", state = "MA",
                        variables = c("B03002_001", "B03002_003", "B03002_004", 
                                      "B02001_001", "B02001_002", "B02001_003", 
                                      "B19001_001", "B19001H_014", "B19001H_015", "B19001H_016", "B19001H_017", 
                                      "B19001B_002", "B19001B_003", "B19001B_004", 
                                      "B17001_001", "B17001_002", 
                                      "B19001_014", "B19001_015", "B19001_016", "B19001_017", 
                                      "B19001_002", "B19001_003", "B19001_004"),
                        year = 2015, geometry = FALSE, output = "wide", 
                        keep_geo_vars = FALSE, moe_level = 95, survey = "acs5", 
                        key = "fc8a0d82b8eb4b03d8061799b8d967c7411dba56")


################ CONSTRUCT ICE - CT #################

df <- subset(MAacs2015_CT, select = -c(NAME, 
                                       B03002_001M, B03002_003M, B03002_004M, 
                                       B02001_001M, B02001_002M, B02001_003M, 
                                       B19001_001M, B19001H_014M, B19001H_015M, B19001H_016M, B19001H_017M, 
                                       B19001B_002M, B19001B_003M, B19001B_004M, 
                                       B17001_001M, B17001_002M,
                                       B19001_014M, B19001_015M, B19001_016M, B19001_017M, 
                                       B19001_002M, B19001_003M, B19001_004M))
df <- as.data.frame(df)
head(df)

names(df) <- c("geoid", "totpopraceeth", "WNH", "BNH", "totpoprace", "W", "B", 
               "totpopraceinc", "NHW100_125", "NHW125_150", "NHW150_200", "NHW200up", 
               "Bund10", "B10_15", "B15_20",
               "totpoppov", "belowpov", 
               "inc100_125", "inc125_150", "inc150_200", "inc200up", 
               "incund10", "inc10_15", "inc15_20")




# ICE WNH vs BNH (racial/ethnic segregation)
df$ICErace <- NA
df$ICErace <- ((df$WNH - df$BNH)/ df$totpopraceeth)

summary(df$ICErace)

# ICE WNH high income vs B low income (racialized economic segregation)
df$ICEraceinc <- NA
df$ICEraceinc <- ((df$NHW100_125 + df$NHW125_150 + df$NHW150_200 + df$NHW200up) - 
                    (df$B10_15 + df$B15_20 + df$Bund10))/ df$totpopraceinc

summary(df$ICEraceinc)


# construct percent WNH
df$percWNH <- df$WNH / df$totpopraceeth


# construct percent BNH
df$percBNH <- df$BNH / df$totpopraceeth


summary(df$percBNH)
summary(df$percWNH)


# construct percent below poverty
df$percpov <- df$belowpov/df$totpoppov


# construct ICE for household income
df$ICEinc <- ((df$inc100_125 + df$inc125_150 + df$inc150_200 + df$inc200up) - 
                (df$inc10_15 + df$inc15_20 + df$incund10))/ df$totpopraceinc


summary(df$percpov)
summary(df$ICEinc)



setwd("~/Harvard PhD in PHS/2019_R01_epigenetics/Data")
write.csv(df, "19_mepigen_CTmeasures.csv", row.names = FALSE)
