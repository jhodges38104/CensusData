# Memphis Area: Official ADI Data Collection - Google Colab Guide

This guide walks you through collecting **official Area Deprivation Index (ADI)** data for the Memphis 30-mile radius area.

---

## PART 1: Download ADI Data (Do This First)

### Step 1: Register for Neighborhood Atlas

1. **Go to:** https://www.neighborhoodatlas.medicine.wisc.edu/
2. **Click:** "Download Data" button (top right)
3. **Register:** Create a free account
   - Provide your name, email, institution
   - Agree to terms of use
4. **Verify:** Check your email for verification link

### Step 2: Download ADI Files

Once logged in:

1. **Navigate to:** "Download Data" section
2. **Select:**
   - **Dataset:** "2021 ADI by Census Block Group"
   - **Version:** v4.0 (or latest)
   - **Geography:** Block Group
3. **Download these states:**
   - ✅ Tennessee (TN) - File: `US_2021_ADI_Census Block Group_v4.0_TN.txt`
   - ✅ Arkansas (AR) - File: `US_2021_ADI_Census Block Group_v4.0_AR.txt`
   - ✅ Mississippi (MS) - File: `US_2021_ADI_Census Block Group_v4.0_MS.txt`

4. **Save files** to your computer (you'll upload them to Colab)

**Note:** Each file is ~1-5 MB (small and fast to download)

---

## PART 2: Google Colab - Complete Workflow

Now open Google Colab and run these cells in order:

### **Cell 1: Setup Repository**
```python
# Clone repository and install dependencies
!git clone https://github.com/jhodges38104/CensusData.git
%cd CensusData
!git checkout claude/collect-acs-census-data-011CUc2zqWZm3HPihktsDGut
!pip install -r requirements.txt -q

print("✓ Setup complete!")
```

---

### **Cell 2: Import Libraries**
```python
import os
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from collect_acs_blockgroup_data import ACSBlockGroupCollector

# Set API key
os.environ['CENSUS_API_KEY'] = '4d5e7ded000067ff443e2f90683ce53bcf660392'

print("✓ Libraries loaded")
```

---

### **Cell 3: Distance Calculation Function**
```python
def haversine_distance(lat1, lon1, lat2, lon2):
    """Calculate distance in miles between two lat/lon points."""
    lat1, lon1, lat2, lon2 = map(np.radians, [lat1, lon1, lat2, lon2])
    dlat = lat2 - lat1
    dlon = lon2 - lon1
    a = np.sin(dlat/2)**2 + np.cos(lat1) * np.cos(lat2) * np.sin(dlon/2)**2
    c = 2 * np.arcsin(np.sqrt(a))
    return 3959 * c  # Earth radius in miles

def get_tract_centroids(state_fips):
    """Get coordinates using census tract centroids."""
    import requests
    url = "https://tigerweb.geo.census.gov/arcgis/rest/services/TIGERweb/tigerWMS_ACS2021/MapServer/8/query"

    params = {
        'where': f"STATE='{state_fips}'",
        'outFields': 'STATE,COUNTY,TRACT,CENTLAT,CENTLON',
        'returnGeometry': 'false',
        'f': 'json',
        'resultRecordCount': 5000
    }

    coords = {}
    offset = 0

    while True:
        params['resultOffset'] = offset
        response = requests.get(url, params=params, timeout=30)
        if response.status_code != 200:
            break

        result = response.json()
        features = result.get('features', [])
        if not features:
            break

        for feature in features:
            attrs = feature['attributes']
            tract_geoid = f"{attrs['STATE']}{attrs['COUNTY']}{attrs['TRACT']}"
            coords[tract_geoid] = {
                'latitude': float(attrs['CENTLAT']),
                'longitude': float(attrs['CENTLON'])
            }

        offset += len(features)
        if len(features) < 5000:
            break

    return coords

print("✓ Functions loaded")
```

---

### **Cell 4: Collect Memphis Area Census Data (Takes 5-10 minutes)**
```python
# Memphis center point and radius
MEMPHIS_LAT = 35.1495
MEMPHIS_LON = -90.0490
RADIUS_MILES = 30

print("=" * 70)
print(f"COLLECTING CENSUS DATA - {RADIUS_MILES} MILES FROM MEMPHIS")
print("=" * 70)

# Initialize collector
collector = ACSBlockGroupCollector()

# States (Memphis is on the border)
states = {'47': 'TN', '05': 'AR', '28': 'MS'}

all_data = []
for state_fips, state_abbr in states.items():
    print(f"\n--- {state_abbr} ---")

    # Get ACS data
    data = collector.collect_blockgroup_data(state=state_fips, year=2021)
    data['state_abbr'] = state_abbr

    # Get coordinates
    print(f"  Getting coordinates...")
    tract_coords = get_tract_centroids(state_fips)
    data['tract_geoid'] = data['GEOID'].str[:11]
    data['latitude'] = data['tract_geoid'].map(lambda x: tract_coords.get(x, {}).get('latitude'))
    data['longitude'] = data['tract_geoid'].map(lambda x: tract_coords.get(x, {}).get('longitude'))

    print(f"  ✓ {len(data)} block groups")
    all_data.append(data)

# Combine and filter
combined = pd.concat(all_data, ignore_index=True)
combined = combined.dropna(subset=['latitude', 'longitude'])

# Calculate distances
combined['distance_from_memphis_miles'] = combined.apply(
    lambda row: haversine_distance(MEMPHIS_LAT, MEMPHIS_LON, row['latitude'], row['longitude']),
    axis=1
)

# Filter to radius
memphis_data = combined[combined['distance_from_memphis_miles'] <= RADIUS_MILES].copy()

# Calculate ICE measures
memphis_data = collector.calculate_ice_measures(memphis_data)
memphis_data = memphis_data.sort_values('distance_from_memphis_miles')

print("\n" + "=" * 70)
print("CENSUS DATA COLLECTION COMPLETE")
print("=" * 70)
print(f"✓ Total block groups: {len(memphis_data)}")
print(f"✓ Distance range: {memphis_data['distance_from_memphis_miles'].min():.1f} - {memphis_data['distance_from_memphis_miles'].max():.1f} miles")

print("\n✓ By state:")
for state in ['TN', 'AR', 'MS']:
    count = len(memphis_data[memphis_data['state_abbr'] == state])
    print(f"  {state}: {count}")
```

---

### **Cell 5: Upload ADI Files** ⬆️
```python
from google.colab import files

print("=" * 70)
print("UPLOAD ADI FILES")
print("=" * 70)
print("\nClick 'Choose Files' and upload the 3 ADI .txt files you downloaded:")
print("  1. US_2021_ADI_Census Block Group_v4.0_TN.txt")
print("  2. US_2021_ADI_Census Block Group_v4.0_AR.txt")
print("  3. US_2021_ADI_Census Block Group_v4.0_MS.txt")
print()

uploaded = files.upload()

print(f"\n✓ Uploaded {len(uploaded)} file(s)")
for filename in uploaded.keys():
    print(f"  - {filename}")
```

---

### **Cell 6: Load and Merge ADI Data**
```python
print("\n" + "=" * 70)
print("LOADING ADI DATA")
print("=" * 70)

# Find ADI files
adi_files = [f for f in uploaded.keys() if 'ADI' in f and '.txt' in f]
print(f"\nFound {len(adi_files)} ADI files")

# Load all ADI files
adi_data_list = []
for file in adi_files:
    print(f"  Reading {file}...")
    df = pd.read_csv(file, sep='\t', dtype={'FIPS': str})
    adi_data_list.append(df)
    print(f"    ✓ {len(df)} block groups")

# Combine all states
adi_combined = pd.concat(adi_data_list, ignore_index=True)
print(f"\n✓ Total ADI records: {len(adi_combined)}")

# Rename FIPS to GEOID for merging
adi_combined.rename(columns={'FIPS': 'GEOID'}, inplace=True)

# Ensure GEOID is string and properly formatted (15 digits)
adi_combined['GEOID'] = adi_combined['GEOID'].astype(str).str.zfill(12)
memphis_data['GEOID'] = memphis_data['GEOID'].astype(str)

print("\nADI columns available:")
print(list(adi_combined.columns))

# Merge ADI with Memphis census data
print("\n" + "=" * 70)
print("MERGING ADI WITH CENSUS DATA")
print("=" * 70)

memphis_with_adi = memphis_data.merge(
    adi_combined[['GEOID', 'ADI_NATRANK', 'ADI_STATERNK']],
    on='GEOID',
    how='left'
)

print(f"\n✓ Merge complete!")
print(f"  Total block groups: {len(memphis_with_adi)}")
print(f"  With ADI data: {memphis_with_adi['ADI_NATRANK'].notna().sum()}")
print(f"  Missing ADI: {memphis_with_adi['ADI_NATRANK'].isna().sum()}")

if memphis_with_adi['ADI_NATRANK'].isna().sum() > 0:
    print("\n  Note: Some block groups may not have ADI due to insufficient population")
```

---

### **Cell 7: ADI Summary Statistics**
```python
print("=" * 70)
print("ADI SUMMARY - MEMPHIS 30-MILE RADIUS")
print("=" * 70)

# Overall statistics
print("\n--- Overall ADI Distribution ---")
print(memphis_with_adi['ADI_NATRANK'].describe().round(2))

# By state
print("\n--- ADI by State ---")
for state in ['TN', 'AR', 'MS']:
    state_data = memphis_with_adi[memphis_with_adi['state_abbr'] == state]
    adi_values = state_data['ADI_NATRANK'].dropna()

    if len(adi_values) > 0:
        print(f"\n{state} (n={len(state_data)}):")
        print(f"  Mean ADI:   {adi_values.mean():.1f}")
        print(f"  Median ADI: {adi_values.median():.1f}")
        print(f"  Min ADI:    {adi_values.min():.0f}")
        print(f"  Max ADI:    {adi_values.max():.0f}")

# Most deprived areas
print("\n" + "=" * 70)
print("MOST DEPRIVED BLOCK GROUPS (Highest ADI)")
print("=" * 70)
cols = ['GEOID', 'state_abbr', 'distance_from_memphis_miles', 'ADI_NATRANK', 'ICE_race', 'ICE_income']
print(memphis_with_adi.nlargest(10, 'ADI_NATRANK')[cols].to_string(index=False))

# Least deprived areas
print("\n" + "=" * 70)
print("LEAST DEPRIVED BLOCK GROUPS (Lowest ADI)")
print("=" * 70)
print(memphis_with_adi.nsmallest(10, 'ADI_NATRANK')[cols].to_string(index=False))
```

---

### **Cell 8: Compare ADI with ICE Measures**
```python
print("=" * 70)
print("CORRELATION: ADI vs ICE MEASURES")
print("=" * 70)

ice_cols = [col for col in memphis_with_adi.columns if col.startswith('ICE_')]

correlations = []
for ice_col in ice_cols:
    # Calculate correlation (only for rows with both values)
    subset = memphis_with_adi[['ADI_NATRANK', ice_col]].dropna()
    if len(subset) > 0:
        corr = subset.corr().iloc[0, 1]
        correlations.append({'ICE_Measure': ice_col, 'Correlation': corr})
        print(f"{ice_col:25} r = {corr:6.3f}")

print("\n📊 Interpretation:")
print("  • ADI: Higher = More Deprived")
print("  • ICE: Higher = More Privileged")
print("  • Expected: Negative correlations (confirmed above)")
print("  • Stronger negative r = Stronger inverse relationship")

# Create correlation dataframe
corr_df = pd.DataFrame(correlations)
print("\n" + corr_df.to_string(index=False))
```

---

### **Cell 9: Visualize ADI Distribution**
```python
import matplotlib.pyplot as plt

fig, axes = plt.subplots(2, 2, figsize=(14, 10))

# 1. ADI Distribution - Overall
axes[0, 0].hist(memphis_with_adi['ADI_NATRANK'].dropna(), bins=25, edgecolor='black', color='steelblue')
axes[0, 0].set_xlabel('ADI National Rank', fontsize=11)
axes[0, 0].set_ylabel('Number of Block Groups', fontsize=11)
axes[0, 0].set_title('ADI Distribution - Memphis 30-Mile Radius', fontsize=12, fontweight='bold')
median_adi = memphis_with_adi['ADI_NATRANK'].median()
axes[0, 0].axvline(median_adi, color='red', linestyle='--', linewidth=2, label=f'Median: {median_adi:.1f}')
axes[0, 0].legend()
axes[0, 0].grid(True, alpha=0.3)

# 2. ADI by State
state_colors = {'TN': 'blue', 'AR': 'green', 'MS': 'orange'}
for state, color in state_colors.items():
    state_data = memphis_with_adi[memphis_with_adi['state_abbr'] == state]
    axes[0, 1].hist(state_data['ADI_NATRANK'].dropna(), alpha=0.6,
                    label=f"{state} (n={len(state_data)})", color=color, bins=20)

axes[0, 1].set_xlabel('ADI National Rank', fontsize=11)
axes[0, 1].set_ylabel('Number of Block Groups', fontsize=11)
axes[0, 1].set_title('ADI Distribution by State', fontsize=12, fontweight='bold')
axes[0, 1].legend()
axes[0, 1].grid(True, alpha=0.3)

# 3. ADI vs ICE Race+Income
axes[1, 0].scatter(memphis_with_adi['ADI_NATRANK'],
                   memphis_with_adi['ICE_race_income'],
                   alpha=0.5, s=30, c='purple')
axes[1, 0].set_xlabel('ADI National Rank (Higher = More Deprived)', fontsize=11)
axes[1, 0].set_ylabel('ICE Race+Income (Higher = More Privileged)', fontsize=11)
axes[1, 0].set_title('ADI vs ICE Race+Income', fontsize=12, fontweight='bold')
axes[1, 0].grid(True, alpha=0.3)

# Add correlation to plot
subset = memphis_with_adi[['ADI_NATRANK', 'ICE_race_income']].dropna()
if len(subset) > 0:
    corr = subset.corr().iloc[0, 1]
    axes[1, 0].text(0.05, 0.95, f'r = {corr:.3f}', transform=axes[1, 0].transAxes,
                    fontsize=12, verticalalignment='top',
                    bbox=dict(boxstyle='round', facecolor='white', alpha=0.8))

# 4. ADI vs Distance from Memphis
axes[1, 1].scatter(memphis_with_adi['distance_from_memphis_miles'],
                   memphis_with_adi['ADI_NATRANK'],
                   alpha=0.5, s=30, c='coral')
axes[1, 1].set_xlabel('Distance from Memphis (miles)', fontsize=11)
axes[1, 1].set_ylabel('ADI National Rank', fontsize=11)
axes[1, 1].set_title('ADI by Distance from Memphis', fontsize=12, fontweight='bold')
axes[1, 1].grid(True, alpha=0.3)

plt.tight_layout()
plt.show()

print("\n✓ Visualizations complete!")
```

---

### **Cell 10: Create Deprivation Categories**
```python
# Create categorical ADI groups for analysis
memphis_with_adi['adi_category'] = pd.cut(
    memphis_with_adi['ADI_NATRANK'],
    bins=[0, 20, 40, 60, 80, 100],
    labels=['Least Deprived (1-20)', 'Low (21-40)', 'Moderate (41-60)',
            'High (61-80)', 'Most Deprived (81-100)']
)

print("=" * 70)
print("ADI CATEGORIES")
print("=" * 70)

category_summary = memphis_with_adi.groupby('adi_category', observed=True).agg({
    'GEOID': 'count',
    'total_pop_race_eth': 'sum',
    'ADI_NATRANK': 'mean',
    'ICE_race': 'mean',
    'ICE_income': 'mean',
    'ICE_race_income': 'mean'
}).round(2)

category_summary.columns = ['Count', 'Total_Pop', 'Mean_ADI', 'Mean_ICE_Race',
                            'Mean_ICE_Income', 'Mean_ICE_RaceIncome']

print(category_summary)

# Visualize
fig, ax = plt.subplots(figsize=(10, 6))
category_counts = memphis_with_adi['adi_category'].value_counts().sort_index()
colors = ['green', 'yellowgreen', 'yellow', 'orange', 'red']
bars = ax.bar(range(len(category_counts)), category_counts.values, color=colors, edgecolor='black')
ax.set_xticks(range(len(category_counts)))
ax.set_xticklabels(category_counts.index, rotation=15, ha='right')
ax.set_ylabel('Number of Block Groups', fontsize=12)
ax.set_title('Distribution Across ADI Deprivation Categories', fontsize=13, fontweight='bold')
ax.grid(True, alpha=0.3, axis='y')

# Add counts on bars
for i, bar in enumerate(bars):
    height = bar.get_height()
    ax.text(bar.get_x() + bar.get_width()/2., height,
            f'{int(height)}',
            ha='center', va='bottom', fontsize=10, fontweight='bold')

plt.tight_layout()
plt.show()
```

---

### **Cell 11: Save and Download Final Dataset**
```python
# Save complete dataset
filename = 'memphis_30mile_adi_ice_complete_2021.csv'
memphis_with_adi.to_csv(filename, index=False)

print("=" * 70)
print("FINAL DATASET SAVED")
print("=" * 70)
print(f"\n✓ Filename: {filename}")
print(f"✓ Total rows: {len(memphis_with_adi):,}")
print(f"✓ Total columns: {len(memphis_with_adi.columns)}")

print("\n📊 Dataset includes:")
print("  • Geographic identifiers (GEOID, state, county, tract, block group)")
print("  • Distance from Memphis center")
print("  • Coordinates (latitude, longitude)")
print("  • ADI measures (national rank, state rank)")
print("  • 5 ICE measures (race, income, race+income, tenure, race+tenure)")
print("  • All Census demographic variables")
print("  • Population, race, income, housing tenure")

print("\n📥 Starting download...")
from google.colab import files
files.download(filename)
print("✓ Download complete!")

print("\n" + "=" * 70)
print("SUCCESS! You now have Memphis area data with ADI + ICE measures")
print("=" * 70)
```

---

### **Cell 12 (Optional): Quick Data Check**
```python
# View sample of final data
print("Sample of final dataset (first 5 rows):")
display_cols = ['GEOID', 'state_abbr', 'distance_from_memphis_miles',
                'ADI_NATRANK', 'adi_category', 'ICE_race', 'ICE_income',
                'ICE_race_income', 'total_pop_race_eth']

available_cols = [col for col in display_cols if col in memphis_with_adi.columns]
print(memphis_with_adi[available_cols].head(5).to_string(index=False))

# Check for missing data
print("\n\nMissing data summary:")
missing = memphis_with_adi[['ADI_NATRANK', 'ICE_race', 'ICE_income', 'ICE_race_income']].isnull().sum()
print(missing)

print(f"\n✓ Dataset ready for analysis!")
```

---

## 📊 What You'll Have

Your final CSV will contain **500-800 block groups** with:

### Geographic Variables
- `GEOID` - 12-digit block group identifier
- `state_abbr` - TN, AR, or MS
- `distance_from_memphis_miles` - Distance from Memphis center
- `latitude`, `longitude` - Coordinates

### ADI Variables
- `ADI_NATRANK` - National ADI (1-100, higher = more deprived)
- `ADI_STATERNK` - State ADI decile (1-10)
- `adi_category` - Categorical deprivation level

### ICE Variables (All range -1 to +1)
- `ICE_race` - White NH vs Black NH segregation
- `ICE_income` - Income segregation
- `ICE_race_income` - Racialized economic segregation
- `ICE_owner_renter` - Housing tenure
- `ICE_race_tenure` - Racialized housing tenure

### Demographics
- Population by race/ethnicity
- Household income distributions
- Housing tenure variables
- All raw Census variables

---

## ⏱️ Total Time

- **Cell 4** (Census collection): ~5-10 minutes
- **Cell 5** (Upload): ~1 minute
- **All other cells**: <1 minute each
- **Total**: ~15 minutes

---

## 💡 Pro Tips

1. **Save your Colab notebook**: File → Save a copy in Drive
2. **If interrupted**: You can re-run just from Cell 5 onward if you already collected census data
3. **Customize radius**: Change `RADIUS_MILES = 30` to any value in Cell 4
4. **Different center**: Change `MEMPHIS_LAT` and `MEMPHIS_LON` in Cell 4

---

## 🎯 Ready to Use for Research

This dataset is publication-ready and includes:
- ✅ Validated ADI from official source
- ✅ ICE measures following Krieger et al. methodology
- ✅ Proper geographic identifiers
- ✅ Complete demographic context

You can now analyze neighborhood deprivation patterns in the Memphis area! 🚀
