# Area Deprivation Index (ADI) for Memphis Radius - Google Colab Guide

This guide shows how to get Area Deprivation Index (ADI) data for block groups within 30 miles of Memphis, TN.

## What is ADI?

The **Area Deprivation Index (ADI)** is a composite measure of neighborhood socioeconomic disadvantage developed by the University of Wisconsin. It ranks neighborhoods from 1 (least deprived) to 100 (most deprived) based on:
- Income
- Education
- Employment
- Housing quality

ADI is available at:
- **Block group level** (most granular)
- **Census tract level**
- **ZIP code level**

---

## OPTION 1: Use Official ADI Data (Recommended)

The official ADI is available from the University of Wisconsin Neighborhood Atlas.

### Step 1: Download ADI Data

1. **Visit:** https://www.neighborhoodatlas.medicine.wisc.edu/
2. **Click:** "Download Data"
3. **Register** for a free account (required)
4. **Download** the 2021 ADI data for:
   - Tennessee
   - Arkansas
   - Mississippi
5. **Files you'll get:** `US_2021_ADI_Census Block Group_v4.0_TN.txt` (and AR, MS)

### Step 2: Google Colab Code

Use these cells after downloading the ADI files:

---

#### **Cell 1: Setup (if not already done)**
```python
!git clone https://github.com/jhodges38104/CensusData.git
%cd CensusData
!git checkout claude/collect-acs-census-data-011CUc2zqWZm3HPihktsDGut
!pip install -r requirements.txt
```

---

#### **Cell 2: Upload ADI Files**
```python
from google.colab import files
import pandas as pd

print("Upload the ADI .txt files you downloaded from Neighborhood Atlas")
print("(TN, AR, and MS)")

uploaded = files.upload()

# You should see file upload buttons
# Upload these 3 files:
# - US_2021_ADI_Census Block Group_v4.0_TN.txt
# - US_2021_ADI_Census Block Group_v4.0_AR.txt
# - US_2021_ADI_Census Block Group_v4.0_MS.txt
```

---

#### **Cell 3: Load ADI Data**
```python
# Read ADI files
adi_files = [f for f in uploaded.keys() if 'ADI' in f]

adi_data = []
for file in adi_files:
    df = pd.read_csv(file, sep='\t', dtype={'FIPS': str})
    adi_data.append(df)

# Combine all states
adi_combined = pd.concat(adi_data, ignore_index=True)

# Rename FIPS to GEOID for merging
adi_combined.rename(columns={'FIPS': 'GEOID'}, inplace=True)

print(f"✓ Loaded ADI data for {len(adi_combined)} block groups")
print(f"\nColumns: {list(adi_combined.columns)}")
print(f"\nSample data:")
print(adi_combined[['GEOID', 'ADI_NATRANK', 'ADI_STATERNK']].head())
```

---

#### **Cell 4: Load Memphis Radius Block Groups**
```python
# Option A: If you already have the Memphis CSV saved
memphis_data = pd.read_csv('memphis_30mile_radius_2021.csv', dtype={'GEOID': str})

# Option B: If you need to collect it again (run Memphis collection code from previous guide)
# ... (use the Memphis collection code from earlier)

print(f"✓ Loaded {len(memphis_data)} Memphis area block groups")
```

---

#### **Cell 5: Merge ADI with Memphis Data**
```python
# Ensure GEOID is string in both datasets
memphis_data['GEOID'] = memphis_data['GEOID'].astype(str)
adi_combined['GEOID'] = adi_combined['GEOID'].astype(str)

# Merge ADI data with Memphis radius data
memphis_with_adi = memphis_data.merge(
    adi_combined[['GEOID', 'ADI_NATRANK', 'ADI_STATERNK']],
    on='GEOID',
    how='left'
)

print("=" * 70)
print("MEMPHIS AREA WITH ADI DATA")
print("=" * 70)
print(f"Total block groups: {len(memphis_with_adi)}")
print(f"Block groups with ADI data: {memphis_with_adi['ADI_NATRANK'].notna().sum()}")
print(f"Block groups missing ADI: {memphis_with_adi['ADI_NATRANK'].isna().sum()}")

# Show sample
print("\n--- Sample: Closest 10 Block Groups with ADI ---")
cols = ['GEOID', 'state_abbr', 'distance_from_memphis_miles',
        'ADI_NATRANK', 'ADI_STATERNK', 'ICE_race', 'ICE_income']
print(memphis_with_adi[cols].head(10))
```

---

#### **Cell 6: ADI Statistics**
```python
# Summary statistics
print("\n--- ADI National Rank Summary ---")
print(memphis_with_adi['ADI_NATRANK'].describe())

print("\n--- ADI by State ---")
for state in ['TN', 'AR', 'MS']:
    state_data = memphis_with_adi[memphis_with_adi['state_abbr'] == state]
    print(f"\n{state}:")
    print(f"  Count: {len(state_data)}")
    print(f"  Mean ADI: {state_data['ADI_NATRANK'].mean():.1f}")
    print(f"  Median ADI: {state_data['ADI_NATRANK'].median():.1f}")
    print(f"  Min ADI: {state_data['ADI_NATRANK'].min():.0f}")
    print(f"  Max ADI: {state_data['ADI_NATRANK'].max():.0f}")

# Most deprived block groups
print("\n--- Most Deprived Block Groups (Highest ADI) ---")
most_deprived = memphis_with_adi.nlargest(10, 'ADI_NATRANK')
print(most_deprived[['GEOID', 'state_abbr', 'distance_from_memphis_miles', 'ADI_NATRANK']])

# Least deprived block groups
print("\n--- Least Deprived Block Groups (Lowest ADI) ---")
least_deprived = memphis_with_adi.nsmallest(10, 'ADI_NATRANK')
print(least_deprived[['GEOID', 'state_abbr', 'distance_from_memphis_miles', 'ADI_NATRANK']])
```

---

#### **Cell 7: Visualize ADI Distribution**
```python
import matplotlib.pyplot as plt

# Create histogram
fig, axes = plt.subplots(1, 2, figsize=(14, 5))

# National rank
axes[0].hist(memphis_with_adi['ADI_NATRANK'].dropna(), bins=20, edgecolor='black')
axes[0].set_xlabel('ADI National Rank (1=Least Deprived, 100=Most Deprived)')
axes[0].set_ylabel('Number of Block Groups')
axes[0].set_title('ADI Distribution - Memphis 30-Mile Radius')
axes[0].axvline(memphis_with_adi['ADI_NATRANK'].median(),
                color='red', linestyle='--', label=f"Median: {memphis_with_adi['ADI_NATRANK'].median():.1f}")
axes[0].legend()

# By state
state_colors = {'TN': 'blue', 'AR': 'green', 'MS': 'orange'}
for state, color in state_colors.items():
    state_data = memphis_with_adi[memphis_with_adi['state_abbr'] == state]
    axes[1].hist(state_data['ADI_NATRANK'].dropna(), alpha=0.6,
                label=f"{state} (n={len(state_data)})", color=color, bins=20)

axes[1].set_xlabel('ADI National Rank')
axes[1].set_ylabel('Number of Block Groups')
axes[1].set_title('ADI Distribution by State')
axes[1].legend()

plt.tight_layout()
plt.show()
```

---

#### **Cell 8: Compare ADI with ICE Measures**
```python
# Correlation between ADI and ICE measures
print("=" * 70)
print("CORRELATION: ADI vs ICE Measures")
print("=" * 70)

ice_cols = [col for col in memphis_with_adi.columns if col.startswith('ICE_')]

for ice_col in ice_cols:
    corr = memphis_with_adi[['ADI_NATRANK', ice_col]].corr().iloc[0, 1]
    print(f"{ice_col:25} r = {corr:.3f}")

print("\nNote: ADI increases with deprivation (higher = more deprived)")
print("      ICE increases with privilege (higher = more privileged)")
print("      So we expect negative correlations")

# Scatter plot: ADI vs ICE_race_income
import matplotlib.pyplot as plt

plt.figure(figsize=(10, 6))
plt.scatter(memphis_with_adi['ADI_NATRANK'],
           memphis_with_adi['ICE_race_income'],
           alpha=0.5, s=20)
plt.xlabel('ADI National Rank (Higher = More Deprived)')
plt.ylabel('ICE Race+Income (Higher = More Privileged)')
plt.title('ADI vs ICE Race+Income - Memphis 30-Mile Radius')
plt.grid(True, alpha=0.3)
plt.show()
```

---

#### **Cell 9: Save and Download**
```python
# Save combined data
filename = 'memphis_30mile_adi_ice_2021.csv'
memphis_with_adi.to_csv(filename, index=False)

print(f"✓ Saved: {filename}")
print(f"  Rows: {len(memphis_with_adi)}")
print(f"  Columns: {len(memphis_with_adi.columns)}")

# Download
from google.colab import files
files.download(filename)
print("✓ Download started!")
```

---

## OPTION 2: Calculate Custom Deprivation Index

If you don't want to register for ADI, you can calculate a similar composite deprivation index from the Census variables we already have.

---

#### **Cell: Calculate Custom Deprivation Index**
```python
import pandas as pd
import numpy as np
from sklearn.preprocessing import StandardScaler
from sklearn.decomposition import PCA

# Load Memphis data (should already have from earlier)
# memphis_data should be loaded

# Select deprivation indicators (higher = more deprived)
# We'll reverse ICE measures since higher ICE = less deprived

deprivation_vars = {
    'pct_poverty': None,  # Will calculate
    'pct_no_hs': None,
    'pct_unemployed': None,
    'pct_low_income': None,
    'pct_renter': None,
    'pct_crowded': None,
    'ice_race_reversed': None,
    'ice_income_reversed': None,
}

# Calculate percentage measures
# First, we need to get additional variables if not already collected

print("Calculating custom deprivation index from Census variables...")

# Using what we have:
# 1. Reverse ICE measures (multiply by -1 so higher = more deprived)
memphis_data['ice_race_reversed'] = -1 * memphis_data['ICE_race']
memphis_data['ice_income_reversed'] = -1 * memphis_data['ICE_income']
memphis_data['ice_raceinc_reversed'] = -1 * memphis_data['ICE_race_income']

# 2. Calculate percentages from existing variables
memphis_data['pct_low_income'] = (
    (memphis_data['all_under_10k'] + memphis_data['all_10_15k'] + memphis_data['all_15_20k']) /
    memphis_data['total_household_income']
)

memphis_data['pct_renter'] = (
    memphis_data['renter_occupied'] / memphis_data['total_tenure']
)

# 3. Create composite score using available variables
deprivation_indicators = [
    'ice_race_reversed',
    'ice_income_reversed',
    'ice_raceinc_reversed',
    'pct_low_income',
    'pct_renter'
]

# Remove NaN values for scoring
scoring_data = memphis_data[deprivation_indicators].copy()
scoring_data = scoring_data.fillna(scoring_data.mean())

# Standardize variables
scaler = StandardScaler()
scaled_data = scaler.fit_transform(scoring_data)

# Create composite score (simple average of standardized variables)
memphis_data['custom_deprivation_score'] = scaled_data.mean(axis=1)

# Convert to percentile rank (0-100, higher = more deprived)
from scipy.stats import rankdata
memphis_data['custom_deprivation_percentile'] = (
    rankdata(memphis_data['custom_deprivation_score']) / len(memphis_data) * 100
)

print("✓ Custom deprivation index calculated")
print("\n--- Custom Deprivation Percentile Summary ---")
print(memphis_data['custom_deprivation_percentile'].describe())

print("\n--- Most Deprived (Custom Index) ---")
print(memphis_data.nlargest(10, 'custom_deprivation_percentile')[
    ['GEOID', 'state_abbr', 'custom_deprivation_percentile', 'ICE_race', 'ICE_income']
])

# Save
memphis_data.to_csv('memphis_30mile_custom_deprivation_2021.csv', index=False)
files.download('memphis_30mile_custom_deprivation_2021.csv')
```

---

## Summary of Variables You'll Have

### Option 1 (Official ADI):
- `ADI_NATRANK` - National ADI percentile (1-100)
- `ADI_STATERNK` - State-specific ADI percentile (1-10)
- All ICE measures
- All demographic variables
- Distance from Memphis

### Option 2 (Custom Index):
- `custom_deprivation_score` - Standardized composite score
- `custom_deprivation_percentile` - Percentile rank (0-100)
- All ICE measures
- All demographic variables
- Distance from Memphis

---

## Which Option to Choose?

**Choose Option 1 (Official ADI) if:**
- You want to use the validated, published ADI
- You're okay with registering for Neighborhood Atlas
- You want comparability with other studies using ADI

**Choose Option 2 (Custom Index) if:**
- You want a quick solution without registration
- You want to customize which variables to include
- You're okay with a less validated measure

---

## Key Differences

| Aspect | Official ADI | Custom Index |
|--------|-------------|--------------|
| Source | UW Neighborhood Atlas | Calculated from Census ACS |
| Variables | 17 indicators | 5 indicators (based on what we collected) |
| Validation | Extensively validated | Not validated |
| Comparability | High (widely used) | Low (custom) |
| Effort | Requires download/registration | No extra downloads |
| Time Period | Matches Census year | Matches Census year |

---

## References

- **Neighborhood Atlas**: https://www.neighborhoodatlas.medicine.wisc.edu/
- **ADI Citation**: Kind AJH, Buckingham W. Making neighborhood disadvantage metrics accessible: The Neighborhood Atlas. N Engl J Med. 2018;378:2456-2458.
- **ADI Methodology**: Singh GK. Area deprivation and widening inequalities in US mortality, 1969-1998. Am J Public Health. 2003;93:1137-1143.
