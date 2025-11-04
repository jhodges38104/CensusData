# CDC PLACES Census Tract Data Download Scripts

This directory contains R scripts for downloading census tract-level health data from the CDC PLACES dataset for 2024.

## Overview

The CDC PLACES (Population Level Analysis and Community Estimates) provides health data at multiple geographic levels, including census tracts. These scripts allow you to download census tract-level health indicators for any U.S. state or territory.

## Scripts

### 1. `pull_cdc_places_census_tract_2024.R`

**Interactive script with user prompts**

This script provides an interactive interface where you are prompted to enter a state abbreviation.

**Usage:**
```r
source("pull_cdc_places_census_tract_2024.R")
```

**Features:**
- Interactive state selection with validation
- Detailed progress output
- Automatic pagination for large datasets
- Data summary statistics
- Outputs CSV file named: `cdc_places_census_tract_[STATE]_2024.csv`

**Example:**
```r
source("pull_cdc_places_census_tract_2024.R")
# When prompted, enter: MA
# Output: cdc_places_census_tract_MA_2024.csv
```

### 2. `pull_cdc_places_census_tract_2024_batch.R`

**Command-line script for batch processing**

This script accepts state abbreviations as command-line arguments, making it ideal for batch processing or automated workflows.

**Usage:**
```bash
# Single state
Rscript pull_cdc_places_census_tract_2024_batch.R MA

# Multiple states
Rscript pull_cdc_places_census_tract_2024_batch.R MA CA NY TX

# All New England states
Rscript pull_cdc_places_census_tract_2024_batch.R ME NH VT MA RI CT
```

**Features:**
- Process multiple states in one command
- Progress tracking for each state
- Summary report at completion
- Automatic error handling and retry logic
- Outputs separate CSV files for each state

## Required R Packages

Install the required packages before running these scripts:

```r
install.packages("httr")
install.packages("jsonlite")
install.packages("dplyr")
install.packages("tidyr")
```

## Valid State Abbreviations

The scripts accept the following state and territory abbreviations:

```
AL, AK, AZ, AR, CA, CO, CT, DE, FL, GA, HI, ID, IL, IN, IA, KS, KY, LA,
ME, MD, MA, MI, MN, MS, MO, MT, NE, NV, NH, NJ, NM, NY, NC, ND, OH, OK,
OR, PA, RI, SC, SD, TN, TX, UT, VT, VA, WA, WV, WI, WY, DC, PR
```

## Output Data Structure

The downloaded data includes the following key fields:

- **locationname**: Census tract identifier
- **stateabbr**: State abbreviation
- **measureid**: Health measure identifier (e.g., BPHIGH, DIABETES)
- **measure**: Full name of the health measure
- **data_value**: Estimate value for the measure
- **data_value_type**: Type of estimate (e.g., crude prevalence, age-adjusted prevalence)
- **low_confidence_limit**: Lower bound of 95% confidence interval
- **high_confidence_limit**: Upper bound of 95% confidence interval
- **totalpopulation**: Total population of the census tract
- **geolocation**: Geographic coordinates

## Common Health Measures in CDC PLACES

The dataset includes measures such as:

- **Chronic Conditions**: Diabetes, high blood pressure, coronary heart disease, stroke, asthma, COPD
- **Prevention**: Routine checkup, dental visit, cholesterol screening, cancer screenings
- **Health Behaviors**: Current smoking, obesity, physical inactivity, sleep <7 hours
- **Mental Health**: Depression, mental health not good
- **Access to Care**: Health insurance coverage, cost barriers to care

## Data Size Considerations

Census tract-level data can be quite large:
- Small states (e.g., Rhode Island): ~5-10 MB
- Medium states (e.g., Massachusetts): ~20-40 MB
- Large states (e.g., California, Texas): 100+ MB

Processing time varies by state size and internet connection speed.

## API Information

These scripts use the CDC's Socrata Open Data API (SODA).

- **Endpoint**: https://data.cdc.gov/resource/cwsq-ngmh.json
- **Dataset ID**: cwsq-ngmh (2024 census tract data)
- **Rate Limits**: The script includes built-in delays to respect API rate limits
- **Pagination**: Automatically handles datasets larger than 50,000 records

**Note**: If you encounter a 404 error, the dataset ID may have been updated. Check the CDC PLACES website for the latest identifier: https://data.cdc.gov/browse?category=500+Cities+%26+Places

## Troubleshooting

### Error: "Failed to fetch data from CDC PLACES API"

**Possible causes:**
1. Internet connection issue
2. CDC API is temporarily unavailable
3. Dataset ID has been updated by CDC

**Solutions:**
- Check your internet connection
- Wait a few minutes and try again
- Verify the dataset ID on the CDC website

### Error: "No data found for state: XX"

**Possible causes:**
1. Invalid state abbreviation
2. No data available for that state/territory

**Solutions:**
- Verify the state abbreviation is valid
- Check CDC PLACES documentation for data availability

### Package Installation Issues

If you encounter issues installing packages:

```r
# Try installing from different mirror
install.packages("httr", repos = "https://cloud.r-project.org/")

# Or use pak for faster installation
install.packages("pak")
pak::pkg_install(c("httr", "jsonlite", "dplyr", "tidyr"))
```

## Integration with Existing Census Data

These scripts are designed to complement the existing census tract analysis workflow in this repository. You can merge CDC PLACES health data with census demographic data using the census tract GEOID as a common key.

### Example Merge:

```r
# Load census data (from existing scripts)
census_data <- read.csv("19_mepigen_CTmeasures.csv")

# Load CDC PLACES data
places_data <- read.csv("cdc_places_census_tract_MA_2024.csv")

# Prepare PLACES data (pivot wider if needed)
places_wide <- places_data %>%
  select(locationname, measureid, data_value) %>%
  pivot_wider(names_from = measureid, values_from = data_value)

# Extract GEOID from locationname (format: "Census Tract XXXXXX, State")
places_wide$geoid <- sub("Census Tract ([0-9.]+),.*", "\\1", places_wide$locationname)

# Merge datasets
combined_data <- left_join(census_data, places_wide, by = "geoid")
```

## Citation

If you use CDC PLACES data in your research, please cite:

> Centers for Disease Control and Prevention. PLACES: Local Data for Better Health. Available at: https://www.cdc.gov/places

## Additional Resources

- **CDC PLACES Homepage**: https://www.cdc.gov/places
- **Data Dictionary**: https://www.cdc.gov/places/measure-definitions/index.html
- **API Documentation**: https://dev.socrata.com/foundry/data.cdc.gov/cwsq-ngmh
- **Technical Documentation**: https://www.cdc.gov/places/methodology/index.html

## Questions or Issues

For questions about:
- **Script functionality**: Contact the project team
- **CDC PLACES data**: Visit https://www.cdc.gov/places or email places@cdc.gov
- **API issues**: Check https://dev.socrata.com/

---

**Last Updated**: November 4, 2024
**Project**: DNA methylation & adversity (R01MD014304-04)
