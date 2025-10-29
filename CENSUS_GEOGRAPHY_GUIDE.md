# Census Geography Levels Guide

## Understanding Census Geography Hierarchy

The US Census uses a hierarchical geography system. Understanding these levels is crucial when collecting census data:

### Geography Hierarchy (from largest to smallest)

1. **Nation**
2. **Region** (e.g., Northeast, South, Midwest, West)
3. **Division** (sub-regions within regions)
4. **State**
5. **County**
6. **Census Tract** (~4,000 people)
7. **Block Group** (~1,500 people)
8. **Census Block** (~100 people)

## Important Distinction: Decennial Census vs ACS

### Decennial Census
- Conducted every 10 years (2010, 2020, 2030, etc.)
- **Complete enumeration** of all households
- Available at **ALL geography levels** including census blocks
- Limited variables (basic demographics, housing)
- Last available: 2020

### American Community Survey (ACS)
- Conducted annually
- **Sample-based survey** (~3.5 million addresses per year)
- Available at **limited geography levels**
- Rich set of social, economic, housing, and demographic variables
- Available as:
  - **1-year estimates** (for areas with 65,000+ population)
  - **5-year estimates** (for all geographic levels where sample size permits)

## ACS Data Availability by Geography

### Available for ACS (Most Variables)
- ✅ State
- ✅ County
- ✅ Census Tract
- ✅ Block Group (smallest available for most variables)
- ✅ Place (city/town)
- ✅ Congressional District
- ✅ ZIP Code Tabulation Area (ZCTA)

### NOT Available for ACS
- ❌ **Census Block** - Sample size too small for reliable estimates
- ❌ Blocks are too small for sample-based surveys
- ❌ Privacy concerns at very small geographies

## Why Census Blocks Don't Have ACS Data

1. **Sample Size**: ACS surveys only ~3.5 million addresses annually out of 140+ million housing units. Census blocks average only ~100 people, meaning most blocks would have zero or very few sampled households.

2. **Statistical Reliability**: Small sample sizes lead to unreliable estimates with large margins of error.

3. **Privacy Concerns**: At very small geographies, survey responses could potentially be identifiable, violating confidentiality protections.

4. **Cost**: It would be prohibitively expensive to achieve the sample sizes needed for block-level estimates.

## Recommended Approaches

### If You Need Block-Level Data
- Use **Decennial Census** data (2010, 2020)
- Limited to basic variables: population, race, Hispanic origin, housing occupancy
- Available via Census API: `https://api.census.gov/data/2020/dec/pl`

### If You Need Rich Socioeconomic Data
- Use **ACS at Block Group level** (smallest available)
- Contains income, education, employment, housing characteristics, etc.
- This repository's script: `collect_acs_blockgroup_data.py`

### Hybrid Approach
- Use Decennial Census for block-level population/race
- Use ACS block group data for socioeconomic characteristics
- Join data using geographic relationships (blocks nest within block groups)

## Geographic Identifiers (GEOIDs)

Census geographies use standardized GEOID codes:

```
Census Tract GEOID:   [State][County][Tract]
                      25      025     010100
                      (MA)    (Suffolk) (tract 101)

Block Group GEOID:    [State][County][Tract][Block Group]
                      25      025     010100  1
                      (MA)    (Suffolk)(tract 101)(BG 1)

Census Block GEOID:   [State][County][Tract][Block]
                      25      025     010100  1001
                      (MA)    (Suffolk)(tract 101)(Block 1001)
```

## Using the Block Group Data Collection Script

### Prerequisites

1. Get a Census API key (free):
   ```bash
   # Visit: https://api.census.gov/data/key_signup.html
   ```

2. Set your API key as an environment variable:
   ```bash
   export CENSUS_API_KEY="your_key_here"
   ```

3. Install dependencies:
   ```bash
   pip install -r requirements.txt
   ```

### Basic Usage

```python
from collect_acs_blockgroup_data import ACSBlockGroupCollector

# Initialize collector
collector = ACSBlockGroupCollector()

# Collect data for a single state (Massachusetts = FIPS 25)
ma_data = collector.collect_blockgroup_data(state='25', year=2021)

# Calculate ICE measures
ma_data = collector.calculate_ice_measures(ma_data)

# Save to CSV
ma_data.to_csv('ma_blockgroups_2021.csv', index=False)
```

### Collect Data for Specific County

```python
# Suffolk County, MA (FIPS: 025)
suffolk_data = collector.collect_blockgroup_data(
    state='25',
    county='025',
    year=2021
)
```

### Collect Data for All US States

```python
# Warning: This will take significant time and make many API calls
us_data = collector.collect_all_states(year=2021)
us_data = collector.calculate_ice_measures(us_data)
us_data.to_csv('us_blockgroups_2021.csv', index=False)
```

## Variables Collected

The script collects variables needed to calculate ICE measures:

### For ICE Race
- Total population by race/ethnicity
- Non-Hispanic White alone
- Non-Hispanic Black alone

### For ICE Income
- Total households by income brackets
- High income (≥$100k): 100-125k, 125-150k, 150-200k, 200k+
- Low income (<$20k): <10k, 10-15k, 15-20k

### For ICE Race + Income
- Non-Hispanic White households by high income brackets
- Black households by low income brackets

### For ICE Tenure
- Total housing units by tenure
- Owner-occupied vs renter-occupied
- White owner-occupied vs Black renter-occupied

## ICE Measures Calculated

The script calculates five ICE (Index of Concentration at the Extremes) measures:

1. **ICE_race**: White NH vs Black NH
2. **ICE_income**: High income vs low income
3. **ICE_race_income**: White NH high income vs Black low income
4. **ICE_owner_renter**: Owner occupied vs renter occupied
5. **ICE_race_tenure**: White owner vs Black renter

All ICE measures range from -1 to +1:
- **-1**: Complete concentration of disadvantaged group
- **0**: No concentration (equality)
- **+1**: Complete concentration of privileged group

## References

- Census Geography: https://www.census.gov/programs-surveys/geography/about/glossary.html
- ACS Data: https://www.census.gov/programs-surveys/acs/data.html
- Census API: https://www.census.gov/data/developers/data-sets.html
- ICE Measures: Krieger N, et al. "Public Health Monitoring of Privilege and Deprivation With the Index of Concentration at the Extremes." Am J Public Health. 2016;106(2):256-63.
