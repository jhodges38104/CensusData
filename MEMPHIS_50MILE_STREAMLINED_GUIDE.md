# Memphis 50-Mile Radius - Complete SDOH Data Collection Guide

## 🎯 Streamlined Data Collection for All Neighborhood Health Indices

This guide provides a simplified workflow to collect all major Social Determinants of Health (SDOH) indices for census block groups within 50 miles of Memphis, TN.

---

## 📊 INDEX GEOGRAPHY CROSSWALK

### Understanding Census Geography Levels

**IMPORTANT**: Not all indices are available at the same geographic level. Here's what you need to know:

| Index | Native Geography | What This Means | How We Handle It |
|-------|------------------|-----------------|------------------|
| **ADI** (Area Deprivation) | Block Group ✓ | Direct match - most granular | Direct merge to block groups |
| **ICE** (Segregation) | Block Group ✓ | Calculated from ACS | Calculated at block group level |
| **SVI** (Social Vulnerability) | Census Tract | Slightly larger than block group | Assigned to all block groups within tract |
| **COI** (Child Opportunity) | Census Tract | Slightly larger than block group | Assigned to all block groups within tract |
| **Food Access** | Census Tract | Slightly larger than block group | Assigned to all block groups within tract |
| **EPA EJScreen** | Census Tract | Slightly larger than block group | Assigned to all block groups within tract |
| **Walkability (SLD)** | Block Group ✓ | Direct match - most granular | Direct merge to block groups |
| **Eviction Lab** | Census Tract | Slightly larger than block group | Assigned to all block groups within tract |

### Geography Size Comparison

```
Census Tract:        ~4,000 people
Block Group:         ~1,500 people  ← Our primary unit
Census Block:        ~100 people
```

**Key Point**: When tract-level data is assigned to block groups, all block groups within the same tract get the same value. This is standard practice in neighborhood health research.

---

## 🗺️ Census GEOID Structure

Understanding GEOIDs helps with merging:

```
Census Tract GEOID (11 digits):
  47  157  000100
  │   │    │
  │   │    └── Tract number (6 digits)
  │   └────── County code (3 digits)
  └────────── State code (2 digits)

Block Group GEOID (12 digits):
  47  157  000100  1
  │   │    │       │
  │   │    │       └── Block group (1 digit)
  │   │    └────────── Tract number (6 digits)
  │   └─────────────── County code (3 digits)
  └─────────────────── State code (2 digits)

Relationship:
- Each tract contains 1-9 block groups
- First 11 digits of block group GEOID = tract GEOID
- tract_geoid = GEOID[:11]
```

---

## 🚀 COMPLETE GOOGLE COLAB WORKFLOW (50 Miles)

Copy and run these cells in order:

---

### **Cell 1: Setup**

```python
# Clone repository and setup
!git clone https://github.com/jhodges38104/CensusData.git
%cd CensusData
!git checkout claude/collect-acs-census-data-011CUc2zqWZm3HPihktsDGut
!pip install -r requirements.txt -q

import os
import pandas as pd
import numpy as np
from collect_acs_blockgroup_data import ACSBlockGroupCollector

# Set API key
os.environ['CENSUS_API_KEY'] = '4d5e7ded000067ff443e2f90683ce53bcf660392'

print("✓ Setup complete!")
```

---

### **Cell 2: Helper Functions**

```python
def haversine_distance(lat1, lon1, lat2, lon2):
    """Calculate distance in miles between two lat/lon points."""
    lat1, lon1, lat2, lon2 = map(np.radians, [lat1, lon1, lat2, lon2])
    dlat = lat2 - lat1
    dlon = lon2 - lon1
    a = np.sin(dlat/2)**2 + np.cos(lat1) * np.cos(lat2) * np.sin(dlon/2)**2
    c = 2 * np.arcsin(np.sqrt(a))
    return 3959 * c

def get_tract_centroids(state_fips):
    """Get census tract centroids for geographic matching."""
    import requests
    url = "https://tigerweb.geo.census.gov/arcgis/rest/services/TIGERweb/tigerWMS_ACS2021/MapServer/8/query"

    coords = {}
    offset = 0

    while True:
        params = {
            'where': f"STATE='{state_fips}'",
            'outFields': 'STATE,COUNTY,TRACT,CENTLAT,CENTLON',
            'returnGeometry': 'false',
            'f': 'json',
            'resultRecordCount': 5000,
            'resultOffset': offset
        }

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

print("✓ Helper functions loaded")
```

---

### **Cell 3: Collect Census Data (50-Mile Radius)**

```python
# Memphis center coordinates
MEMPHIS_LAT = 35.1495
MEMPHIS_LON = -90.0490
RADIUS_MILES = 50  # ← Changed to 50 miles

print("=" * 70)
print(f"COLLECTING CENSUS DATA - {RADIUS_MILES} MILES FROM MEMPHIS")
print("=" * 70)

collector = ACSBlockGroupCollector()
states = {'47': 'TN', '05': 'AR', '28': 'MS'}

all_data = []
for state_fips, state_abbr in states.items():
    print(f"\n--- {state_abbr} ---")

    # Get ACS data at block group level
    data = collector.collect_blockgroup_data(state=state_fips, year=2021)
    data['state_abbr'] = state_abbr

    # Get coordinates
    print(f"  Getting coordinates...")
    tract_coords = get_tract_centroids(state_fips)
    data['tract_geoid'] = data['GEOID'].astype(str).str[:11]
    data['latitude'] = data['tract_geoid'].map(lambda x: tract_coords.get(x, {}).get('latitude'))
    data['longitude'] = data['tract_geoid'].map(lambda x: tract_coords.get(x, {}).get('longitude'))

    print(f"  ✓ {len(data)} block groups")
    all_data.append(data)

# Combine states
combined = pd.concat(all_data, ignore_index=True)
combined = combined.dropna(subset=['latitude', 'longitude'])

# Calculate distances
combined['distance_from_memphis_miles'] = combined.apply(
    lambda row: haversine_distance(MEMPHIS_LAT, MEMPHIS_LON, row['latitude'], row['longitude']),
    axis=1
)

# Filter to 50-mile radius
memphis_data = combined[combined['distance_from_memphis_miles'] <= RADIUS_MILES].copy()

# Calculate ICE measures (block group level)
memphis_data = collector.calculate_ice_measures(memphis_data)
memphis_data = memphis_data.sort_values('distance_from_memphis_miles')

print("\n" + "=" * 70)
print("CENSUS DATA COLLECTION COMPLETE")
print("=" * 70)
print(f"✓ Total block groups within {RADIUS_MILES} miles: {len(memphis_data):,}")
print(f"✓ Distance range: {memphis_data['distance_from_memphis_miles'].min():.1f} - {memphis_data['distance_from_memphis_miles'].max():.1f} miles")
print(f"✓ Total population: {memphis_data['total_pop_race_eth'].sum():,.0f}")

print("\n✓ By state:")
for state in ['TN', 'AR', 'MS']:
    count = len(memphis_data[memphis_data['state_abbr'] == state])
    pop = memphis_data[memphis_data['state_abbr'] == state]['total_pop_race_eth'].sum()
    print(f"  {state}: {count:4,} block groups, pop = {pop:>10,.0f}")
```

---

### **Cell 4: Upload and Merge All Manual Files**

```python
print("=" * 70)
print("UPLOAD ALL MANUAL INDEX FILES")
print("=" * 70)

from google.colab import files

# Dictionary to store uploaded files
uploaded_files = {}

print("\n📤 Upload your files one at a time:")
print("\nFile 1: ADI (Area Deprivation Index)")
print("  Expected: US_2023_ADI_Census_Block_Group_v4_0_1.csv")
uploaded_adi = files.upload()
if uploaded_adi:
    uploaded_files['ADI'] = list(uploaded_adi.keys())[0]
    print(f"  ✓ {uploaded_files['ADI']}")

print("\nFile 2: COI (Childhood Opportunity Index)")
print("  Expected: COI_database.csv (from diversitydatakids.org)")
uploaded_coi = files.upload()
if uploaded_coi:
    uploaded_files['COI'] = list(uploaded_coi.keys())[0]
    print(f"  ✓ {uploaded_files['COI']}")

print("\nFile 3: Food Access")
print("  Expected: FoodAccessResearchAtlasData2019.xlsx (from USDA)")
uploaded_food = files.upload()
if uploaded_food:
    uploaded_files['FOOD'] = list(uploaded_food.keys())[0]
    print(f"  ✓ {uploaded_files['FOOD']}")

print("\n✓ Files uploaded!")
print(f"Total files: {len(uploaded_files)}")
```

---

### **Cell 5: Merge All Indices (Automated)**

```python
print("=" * 70)
print("MERGING ALL INDICES")
print("=" * 70)

# Start with census data
final_data = memphis_data.copy()

# Ensure tract_geoid exists (first 11 digits of block group GEOID)
final_data['tract_geoid'] = final_data['GEOID'].astype(str).str[:11]
final_data['GEOID'] = final_data['GEOID'].astype(str)

# ============================================================
# 1. MERGE ADI (BLOCK GROUP LEVEL)
# ============================================================
if 'ADI' in uploaded_files:
    print("\n--- Merging ADI (Block Group Level) ---")

    adi_data = pd.read_csv(uploaded_files['ADI'], low_memory=False)

    # Find GEOID column
    if 'FIPS' in adi_data.columns:
        adi_data['GEOID'] = adi_data['FIPS'].astype(str)
    elif 'GISJOIN' in adi_data.columns:
        adi_data['GEOID'] = adi_data['GISJOIN'].str.replace('G', '', regex=False)

    # Standardize GEOID to 12 digits
    adi_data['GEOID'] = adi_data['GEOID'].astype(str).str.replace(r'\D', '', regex=True).str.zfill(12)

    # Filter to Memphis states
    adi_data['state_fips'] = adi_data['GEOID'].str[:2]
    adi_memphis = adi_data[adi_data['state_fips'].isin(['47', '05', '28'])].copy()

    # Find ADI columns
    adi_cols = [c for c in adi_memphis.columns if 'ADI' in c.upper()]

    # Merge on GEOID (block group to block group)
    merge_cols = ['GEOID'] + adi_cols[:2]
    final_data = final_data.merge(
        adi_memphis[merge_cols],
        on='GEOID',
        how='left'
    )

    # Rename to standard names
    for col in adi_cols:
        if 'NAT' in col.upper() and 'ADI_NATRANK' not in final_data.columns:
            final_data.rename(columns={col: 'ADI_NATRANK'}, inplace=True)
        elif 'STATE' in col.upper() and 'ADI_STATERNK' not in final_data.columns:
            final_data.rename(columns={col: 'ADI_STATERNK'}, inplace=True)

    adi_count = final_data['ADI_NATRANK'].notna().sum() if 'ADI_NATRANK' in final_data.columns else 0
    print(f"✓ ADI merged: {adi_count:,} / {len(final_data):,} ({adi_count/len(final_data)*100:.1f}%)")

# ============================================================
# 2. DOWNLOAD & MERGE SVI (CENSUS TRACT LEVEL)
# ============================================================
print("\n--- Downloading & Merging SVI (Census Tract Level) ---")

try:
    svi_urls = {
        'TN': 'https://svi.cdc.gov/Documents/Data/2020/csv/states/Tennessee.csv',
        'AR': 'https://svi.cdc.gov/Documents/Data/2020/csv/states/Arkansas.csv',
        'MS': 'https://svi.cdc.gov/Documents/Data/2020/csv/states/Mississippi.csv'
    }

    svi_list = []
    for state, url in svi_urls.items():
        df = pd.read_csv(url, dtype={'FIPS': str})
        svi_list.append(df)
        print(f"  ✓ {state}: {len(df):,} tracts")

    svi_combined = pd.concat(svi_list, ignore_index=True)
    svi_combined['tract_geoid'] = svi_combined['FIPS'].astype(str).str.zfill(11)

    # Select key SVI columns
    svi_cols = ['tract_geoid', 'RPL_THEMES', 'RPL_THEME1', 'RPL_THEME2',
                'RPL_THEME3', 'RPL_THEME4', 'F_TOTAL']
    available_svi = [c for c in svi_cols if c in svi_combined.columns]

    # Merge on tract_geoid (tract to block group - all BGs in tract get same value)
    final_data = final_data.merge(
        svi_combined[available_svi],
        on='tract_geoid',
        how='left'
    )

    svi_count = final_data['RPL_THEMES'].notna().sum() if 'RPL_THEMES' in final_data.columns else 0
    print(f"✓ SVI merged: {svi_count:,} / {len(final_data):,} ({svi_count/len(final_data)*100:.1f}%)")
    print(f"  Note: Census tract data assigned to all block groups within each tract")

except Exception as e:
    print(f"⚠ SVI download failed: {e}")

# ============================================================
# 3. MERGE COI (CENSUS TRACT LEVEL)
# ============================================================
if 'COI' in uploaded_files:
    print("\n--- Merging COI (Census Tract Level) ---")

    coi_data = pd.read_csv(uploaded_files['COI'], dtype={'geoid': str, 'GEOID': str}, low_memory=False)

    # Find GEOID column
    geoid_col = None
    for col in ['geoid', 'GEOID', 'tractid', 'tract']:
        if col in coi_data.columns:
            geoid_col = col
            break

    if geoid_col:
        coi_data['tract_geoid'] = coi_data[geoid_col].astype(str).str.zfill(11)

        # Filter to Memphis states
        coi_data['state_fips'] = coi_data['tract_geoid'].str[:2]
        coi_memphis = coi_data[coi_data['state_fips'].isin(['47', '05', '28'])].copy()

        # Select COI columns
        coi_cols = [c for c in coi_memphis.columns if 'coi' in c.lower() or 'z_' in c.lower()]
        key_coi = coi_cols[:5] if len(coi_cols) > 5 else coi_cols

        # Merge on tract_geoid
        merge_cols = ['tract_geoid'] + key_coi
        final_data = final_data.merge(
            coi_memphis[merge_cols],
            on='tract_geoid',
            how='left'
        )

        coi_count = final_data[key_coi[0]].notna().sum() if key_coi else 0
        print(f"✓ COI merged: {coi_count:,} / {len(final_data):,} ({coi_count/len(final_data)*100:.1f}%)")
        print(f"  Note: Census tract data assigned to all block groups within each tract")

# ============================================================
# 4. MERGE FOOD ACCESS (CENSUS TRACT LEVEL)
# ============================================================
if 'FOOD' in uploaded_files:
    print("\n--- Merging Food Access (Census Tract Level) ---")

    food_data = pd.read_excel(uploaded_files['FOOD'], sheet_name='Food Access Research Atlas',
                               dtype={'CensusTract': str})

    food_data['tract_geoid'] = food_data['CensusTract'].astype(str).str.zfill(11)
    food_data['state_fips'] = food_data['tract_geoid'].str[:2]
    food_memphis = food_data[food_data['state_fips'].isin(['47', '05', '28'])].copy()

    # Select key variables
    food_cols = ['tract_geoid', 'LowIncomeTracts', 'LILATracts_1And10',
                 'LATracts1', 'TractSNAP', 'lapop1_10']
    available_food = [c for c in food_cols if c in food_memphis.columns]

    # Merge on tract_geoid
    final_data = final_data.merge(
        food_memphis[available_food],
        on='tract_geoid',
        how='left'
    )

    food_count = final_data['LILATracts_1And10'].notna().sum() if 'LILATracts_1And10' in final_data.columns else 0
    print(f"✓ Food access merged: {food_count:,} / {len(final_data):,} ({food_count/len(final_data)*100:.1f}%)")
    print(f"  Note: Census tract data assigned to all block groups within each tract")

# ============================================================
# SUMMARY
# ============================================================
print("\n" + "=" * 70)
print("MERGE COMPLETE")
print("=" * 70)

print(f"\nTotal block groups: {len(final_data):,}")
print(f"Coverage within {RADIUS_MILES} miles of Memphis:\n")

indices = {
    'ADI (Deprivation)': 'ADI_NATRANK',
    'SVI (Vulnerability)': 'RPL_THEMES',
    'COI (Opportunity)': key_coi[0] if 'COI' in uploaded_files and key_coi else None,
    'ICE (Segregation)': 'ICE_race',
    'Food Access': 'LILATracts_1And10'
}

for name, col in indices.items():
    if col and col in final_data.columns:
        count = final_data[col].notna().sum()
        pct = count / len(final_data) * 100
        print(f"  ✓ {name:25} {count:4,} ({pct:5.1f}%)")

print("\n✓ All indices merged successfully!")
```

---

### **Cell 6: Data Summary & Crosswalk Check**

```python
print("=" * 70)
print("GEOGRAPHY LEVEL VERIFICATION")
print("=" * 70)

print("\n📊 Understanding the data structure:")

# Count unique tracts vs block groups
n_block_groups = final_data['GEOID'].nunique()
n_tracts = final_data['tract_geoid'].nunique()
avg_bg_per_tract = n_block_groups / n_tracts

print(f"\nBlock Groups: {n_block_groups:,}")
print(f"Census Tracts: {n_tracts:,}")
print(f"Average BGs per tract: {avg_bg_per_tract:.1f}")

print("\n" + "=" * 70)
print("DATA AVAILABILITY BY GEOGRAPHY LEVEL")
print("=" * 70)

print("\n🔷 BLOCK GROUP LEVEL (most granular):")
bg_indices = {
    'ADI': 'ADI_NATRANK',
    'ICE (all 5 measures)': 'ICE_race',
    'Census demographics': 'total_pop_race_eth'
}

for name, col in bg_indices.items():
    if col in final_data.columns:
        count = final_data[col].notna().sum()
        pct = count / len(final_data) * 100
        print(f"  ✓ {name:30} {count:4,} ({pct:5.1f}%)")

print("\n🔶 CENSUS TRACT LEVEL (assigned to all BGs in tract):")
tract_indices = {
    'SVI': 'RPL_THEMES',
    'COI': key_coi[0] if 'COI' in uploaded_files and key_coi else None,
    'Food Access': 'LILATracts_1And10'
}

for name, col in tract_indices.items():
    if col and col in final_data.columns:
        # Count unique tract values (not block groups)
        tract_count = final_data[final_data[col].notna()]['tract_geoid'].nunique()
        bg_count = final_data[col].notna().sum()
        print(f"  ✓ {name:30} {tract_count:4,} tracts → {bg_count:4,} block groups")

print("\n💡 Key Points:")
print("  • Block group data: Each BG has unique value")
print("  • Tract data: All BGs within same tract share the same value")
print("  • This is standard practice in neighborhood health research")

# Show example
print("\n" + "=" * 70)
print("EXAMPLE: How Tract-Level Data Maps to Block Groups")
print("=" * 70)

# Pick a tract with multiple block groups
sample_tract = final_data.groupby('tract_geoid').size().sort_values(ascending=False).index[0]
sample_data = final_data[final_data['tract_geoid'] == sample_tract].head(3)

print(f"\nSample Census Tract: {sample_tract}")
print(f"Contains {len(final_data[final_data['tract_geoid'] == sample_tract])} block groups\n")

display_cols = ['GEOID', 'tract_geoid', 'ADI_NATRANK', 'RPL_THEMES',
                'ICE_race', 'distance_from_memphis_miles']
available = [c for c in display_cols if c in sample_data.columns]

print("First 3 block groups in this tract:")
print(sample_data[available].to_string(index=False))

print("\nNotice:")
print("  • GEOID (12 digits) - unique for each block group")
print("  • tract_geoid (11 digits) - same for all BGs in tract")
print("  • ADI_NATRANK - may vary by block group (BG-level data)")
print("  • RPL_THEMES (SVI) - SAME for all BGs in tract (tract-level data)")
print("  • ICE_race - varies by block group (BG-level data)")
```

---

### **Cell 7: Save Final Dataset**

```python
# Save complete dataset
filename = f'memphis_{RADIUS_MILES}mile_all_indices_complete.csv'
final_data.to_csv(filename, index=False)

print("=" * 70)
print(f"FINAL DATASET: MEMPHIS {RADIUS_MILES}-MILE RADIUS")
print("=" * 70)

print(f"\n📁 Filename: {filename}")
print(f"📊 Total rows: {len(final_data):,} block groups")
print(f"📊 Total columns: {len(final_data.columns)}")
print(f"📊 Total population: {final_data['total_pop_race_eth'].sum():,.0f}")

print("\n" + "=" * 70)
print("DATASET CONTENTS")
print("=" * 70)

print("\n✓ GEOGRAPHIC VARIABLES:")
print("   • GEOID (12 digits) - Block group identifier")
print("   • tract_geoid (11 digits) - Census tract identifier")
print("   • latitude, longitude - Coordinates")
print(f"   • distance_from_memphis_miles (0-{RADIUS_MILES} miles)")
print("   • state_abbr - TN, AR, or MS")

print("\n✓ BLOCK GROUP LEVEL INDICES:")
print("   • ADI (Area Deprivation Index) - National & state ranks")
print("   • ICE (Index of Concentration) - 5 segregation measures")
print("   • All Census demographics - Race, income, housing")

print("\n✓ CENSUS TRACT LEVEL INDICES:")
print("   • SVI (Social Vulnerability) - Overall + 4 themes")
print("   • COI (Child Opportunity) - Multiple domains")
print("   • Food Access - Food desert status")

print("\n📚 For Analysis:")
print("   • Use block group (GEOID) as primary unit")
print("   • Tract-level variables shared by all BGs in tract")
print("   • Account for this in statistical models (clustering)")

# Download
print("\n" + "=" * 70)
print("DOWNLOADING")
print("=" * 70)

from google.colab import files
files.download(filename)

print("\n✓ Download complete!")
print("\n🎉 You now have a comprehensive SDOH dataset!")
print(f"   • {len(final_data):,} block groups within {RADIUS_MILES} miles of Memphis")
print("   • Multiple deprivation, opportunity, and environmental indices")
print("   • Ready for publication-quality analysis!")
print("=" * 70)
```

---

## 📋 QUICK REFERENCE TABLES

### Index Availability

| Index | Geography | Variables | Example Column Names |
|-------|-----------|-----------|---------------------|
| **ADI** | Block Group | 2 | `ADI_NATRANK`, `ADI_STATERNK` |
| **ICE** | Block Group | 5 | `ICE_race`, `ICE_income`, `ICE_race_income`, etc. |
| **SVI** | Census Tract | 6 | `RPL_THEMES`, `RPL_THEME1`, `RPL_THEME2`, etc. |
| **COI** | Census Tract | 5+ | `z_COI`, domain scores |
| **Food** | Census Tract | 5+ | `LILATracts_1And10`, `TractSNAP` |

### Scale Interpretations

| Index | Scale | Low Value Means | High Value Means |
|-------|-------|----------------|------------------|
| ADI | 1-100 | Less deprived | More deprived |
| SVI | 0-1 | Less vulnerable | More vulnerable |
| COI | 1-100 | Less opportunity | More opportunity |
| ICE | -1 to +1 | More disadvantaged | More privileged |

---

## 🎯 KEY TAKEAWAY

**You'll have ONE dataset** with ~800-1,200 block groups, where:
- Each row = one block group
- Block group variables (ADI, ICE) vary by row
- Tract variables (SVI, COI, Food) are the same for all block groups within the same tract

This is the **standard approach** for neighborhood health research and is appropriate for analysis. Just note in your methods that some variables are tract-level.

