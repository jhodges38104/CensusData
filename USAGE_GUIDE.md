# ACS Block Group Data Collection - Usage Guide

## Quick Start

### 1. Setup

```bash
# Install dependencies
pip install -r requirements.txt

# Set your Census API key
export CENSUS_API_KEY="4d5e7ded000067ff443e2f90683ce53bcf660392"
```

### 2. Basic Usage Examples

#### Example 1: Collect Data for a Single County

```python
from collect_acs_blockgroup_data import ACSBlockGroupCollector

# Initialize collector
collector = ACSBlockGroupCollector()

# Collect data for Suffolk County, MA (Boston area)
# State FIPS: 25 (Massachusetts), County FIPS: 025 (Suffolk)
data = collector.collect_blockgroup_data(
    state='25',
    county='025',
    year=2021
)

# Calculate ICE measures
data = collector.calculate_ice_measures(data)

# Save to CSV
data.to_csv('suffolk_blockgroups_2021.csv', index=False)

print(f"Collected data for {len(data)} block groups")
print(data[['GEOID', 'NAME', 'ICE_race', 'ICE_income']].head())
```

#### Example 2: Collect Data for an Entire State

```python
from collect_acs_blockgroup_data import ACSBlockGroupCollector

collector = ACSBlockGroupCollector()

# Collect all block groups in Massachusetts
ma_data = collector.collect_blockgroup_data(state='25', year=2021)
ma_data = collector.calculate_ice_measures(ma_data)

ma_data.to_csv('massachusetts_blockgroups_2021.csv', index=False)

print(f"Total block groups in MA: {len(ma_data)}")
```

#### Example 3: Collect Data for Multiple States

```python
from collect_acs_blockgroup_data import ACSBlockGroupCollector
import pandas as pd

collector = ACSBlockGroupCollector()

# List of states to collect
states = {
    '25': 'MA',  # Massachusetts
    '09': 'CT',  # Connecticut
    '44': 'RI'   # Rhode Island
}

all_data = []
for fips, state_abbr in states.items():
    print(f"Collecting data for {state_abbr}...")
    data = collector.collect_blockgroup_data(state=fips, year=2021)
    data['state_abbr'] = state_abbr
    all_data.append(data)

# Combine all states
combined = pd.concat(all_data, ignore_index=True)
combined = collector.calculate_ice_measures(combined)

combined.to_csv('new_england_blockgroups_2021.csv', index=False)
```

#### Example 4: Collect Data for All US States (WARNING: Large Dataset)

```python
from collect_acs_blockgroup_data import ACSBlockGroupCollector

collector = ACSBlockGroupCollector()

# This will take significant time (could be hours) and make many API calls
# The resulting dataset will be very large (millions of rows)
us_data = collector.collect_all_states(year=2021)
us_data = collector.calculate_ice_measures(us_data)

# Save in chunks to avoid memory issues
us_data.to_csv('us_blockgroups_2021.csv', index=False, chunksize=100000)

print(f"Total block groups collected: {len(us_data)}")
```

## State and County FIPS Codes

### Common State FIPS Codes

| State | FIPS | State | FIPS |
|-------|------|-------|------|
| Alabama | 01 | Montana | 30 |
| Alaska | 02 | Nebraska | 31 |
| Arizona | 04 | Nevada | 32 |
| Arkansas | 05 | New Hampshire | 33 |
| California | 06 | New Jersey | 34 |
| Colorado | 08 | New Mexico | 35 |
| Connecticut | 09 | New York | 36 |
| Delaware | 10 | North Carolina | 37 |
| DC | 11 | North Dakota | 38 |
| Florida | 12 | Ohio | 39 |
| Georgia | 13 | Oklahoma | 40 |
| Hawaii | 15 | Oregon | 41 |
| Idaho | 16 | Pennsylvania | 42 |
| Illinois | 17 | Rhode Island | 44 |
| Indiana | 18 | South Carolina | 45 |
| Iowa | 19 | South Dakota | 46 |
| Kansas | 20 | Tennessee | 47 |
| Kentucky | 21 | Texas | 48 |
| Louisiana | 22 | Utah | 49 |
| Maine | 23 | Vermont | 50 |
| Maryland | 24 | Virginia | 51 |
| Massachusetts | 25 | Washington | 53 |
| Michigan | 26 | West Virginia | 54 |
| Minnesota | 27 | Wisconsin | 55 |
| Mississippi | 28 | Wyoming | 56 |
| Missouri | 29 | | |

### Finding County FIPS Codes

County FIPS codes are 3 digits. You can find them at:
- https://www.census.gov/library/reference/code-lists/ansi.html
- Or use the script to collect all counties in a state, then filter

Example Massachusetts counties:
- 025 - Suffolk (Boston)
- 021 - Norfolk
- 017 - Middlesex (Cambridge)
- 027 - Worcester

## Understanding the Output

### Columns in the Output Dataset

#### Geographic Identifiers
- `GEOID` - Unique identifier for the block group (15 digits)
  - Format: [State(2)][County(3)][Tract(6)][Block Group(1)]
  - Example: `250250101001` = MA (25), Suffolk (025), Tract 010100, Block Group 1
- `NAME` - Human-readable name of the block group
- `state` - State FIPS code
- `county` - County FIPS code
- `tract` - Census tract code
- `block group` - Block group number
- `state_abbr` - State abbreviation (if added)

#### Population & Race Variables
- `total_pop_race_eth` - Total population (B03002_001E)
- `white_NH_alone` - Non-Hispanic White alone (B03002_003E)
- `black_NH_alone` - Non-Hispanic Black alone (B03002_004E)

#### Income Variables
- `total_household_income` - Total households (B19001_001E)
- `all_100_125k` through `all_200k_plus` - Income brackets for all races
- `NHW_100_125k` through `NHW_200k_plus` - Income brackets for NH White
- `Black_under_10k` through `Black_15_20k` - Income brackets for Black

#### Housing Tenure Variables
- `total_tenure` - Total housing units (B25003_001E)
- `owner_occupied` - Owner-occupied units (B25003_002E)
- `renter_occupied` - Renter-occupied units (B25003_003E)
- `white_owner_occupied` - White owner-occupied (B25003H_002E)
- `black_renter_occupied` - Black renter-occupied (B25003B_003E)

#### ICE Measures (Calculated)
- `ICE_race` - White NH vs Black NH (-1 to +1)
- `ICE_income` - High income vs low income (-1 to +1)
- `ICE_race_income` - White NH high income vs Black low income (-1 to +1)
- `ICE_owner_renter` - Owner occupied vs renter occupied (-1 to +1)
- `ICE_race_tenure` - White owner vs Black renter (-1 to +1)

### Interpreting ICE Measures

All ICE measures range from -1 to +1:

- **-1.0**: Complete concentration of disadvantaged group
  - For ICE_race: All residents are Black NH
  - For ICE_income: All households are low income

- **0.0**: No concentration (equality)
  - Equal representation of privileged and disadvantaged groups

- **+1.0**: Complete concentration of privileged group
  - For ICE_race: All residents are White NH
  - For ICE_income: All households are high income

- **Missing (NaN)**: Occurs when denominator is 0 (no population in that category)

### Example Output

```
GEOID          NAME                          ICE_race  ICE_income  ICE_race_income
250250101001   Block Group 1, Census...      0.532     0.156       0.412
250250101002   Block Group 2, Census...     -0.234    -0.523      -0.678
250250102001   Block Group 1, Census...      0.821     0.734       0.892
```

## Troubleshooting

### "Access denied" or 403 errors
- **Cause**: Either the API key is invalid, not activated yet, or there are network restrictions
- **Solution**:
  - Verify your API key at: https://api.census.gov/data/key_signup.html
  - Wait 5-10 minutes after registration for activation
  - Check if your network/firewall blocks Census API access

### "Too many requests" or 429 errors
- **Cause**: Census API rate limiting
- **Solution**:
  - Add delays between requests (the script includes automatic delays)
  - Collect data for smaller geographic areas at a time
  - Run during off-peak hours

### Missing values (NaN) in ICE measures
- **Cause**: Division by zero when population/household counts are 0
- **Solution**: This is expected for block groups with no population in certain categories
- **Handling**: Filter out NaN values or replace with 0 depending on your analysis needs

### Large memory usage when collecting all US states
- **Cause**: Millions of block groups across all states
- **Solution**:
  - Collect states one at a time and save separately
  - Use chunked CSV writing
  - Process data in batches

## Data Years Available

ACS 5-year estimates are released annually with a 1-year lag:
- 2021 ACS = 2017-2021 estimates (released December 2022)
- 2020 ACS = 2016-2020 estimates (released March 2022)
- 2019 ACS = 2015-2019 estimates
- And so on...

For most recent data, use year=2021 (as of 2025).

## Matching with Your Cohort Data

If you have participant addresses and need to link them to block groups:

1. **Geocode addresses to get coordinates** (lat/lon)
2. **Use Census geocoder API or TIGER/Line shapefiles** to determine block group
3. **Join your cohort data with collected ACS data** using GEOID

Example workflow:
```python
import pandas as pd

# Your cohort data with GEOIDs
cohort = pd.read_csv('my_cohort_with_geoids.csv')

# Collected ACS data
acs = pd.read_csv('massachusetts_blockgroups_2021.csv')

# Merge on GEOID
merged = cohort.merge(acs[['GEOID', 'ICE_race', 'ICE_income', 'ICE_race_income']],
                      on='GEOID', how='left')

merged.to_csv('cohort_with_ice_measures.csv', index=False)
```

## API Rate Limits

The Census API has rate limits:
- **500 queries per IP per day** (for most users)
- **No more than 50 queries per 10 seconds**

The collector script includes automatic delays to respect these limits.

## References

- Census API Documentation: https://www.census.gov/data/developers/data-sets/acs-5year.html
- ACS Variables: https://api.census.gov/data/2021/acs/acs5/variables.html
- Geography Reference: https://www.census.gov/programs-surveys/geography/about/glossary.html
- ICE Measures: Krieger N, et al. "Public Health Monitoring of Privilege and Deprivation With the Index of Concentration at the Extremes." Am J Public Health. 2016;106(2):256-63.
