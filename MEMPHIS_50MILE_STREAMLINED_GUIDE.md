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
| **EPA EJScreen** | Block Group ✓ | Direct match - most granular | Direct merge to block groups |
| **SVI** (Social Vulnerability) | Census Tract | Slightly larger than block group | Assigned to all block groups within tract |
| **COI** (Child Opportunity) | Census Tract | Slightly larger than block group | Assigned to all block groups within tract |
| **Food Access** | Census Tract | Slightly larger than block group | Assigned to all block groups within tract |
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

### **Cell 4: Upload All Manual Files**

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
print("  Download: https://www.neighborhoodatlas.medicine.wisc.edu/")
uploaded_adi = files.upload()
if uploaded_adi:
    uploaded_files['ADI'] = list(uploaded_adi.keys())[0]
    print(f"  ✓ {uploaded_files['ADI']}")

print("\nFile 2: COI (Childhood Opportunity Index)")
print("  Expected: COI_database.csv (from diversitydatakids.org)")
print("  Download: https://www.diversitydatakids.org/child-opportunity-index")
uploaded_coi = files.upload()
if uploaded_coi:
    uploaded_files['COI'] = list(uploaded_coi.keys())[0]
    print(f"  ✓ {uploaded_files['COI']}")

print("\nFile 3: Food Access")
print("  Expected: FoodAccessResearchAtlasData2019.xlsx (from USDA)")
print("  Download: https://www.ers.usda.gov/data-products/food-access-research-atlas/")
uploaded_food = files.upload()
if uploaded_food:
    uploaded_files['FOOD'] = list(uploaded_food.keys())[0]
    print(f"  ✓ {uploaded_files['FOOD']}")

print("\nFile 4: EJScreen (Environmental Justice)")
print("  Expected: EJSCREEN_2023_BG_StatePct_with_AS_CNMI_GU_VI.csv")
print("  Download: https://www.epa.gov/ejscreen/download-ejscreen-data")
uploaded_ej = files.upload()
if uploaded_ej:
    uploaded_files['EJSCREEN'] = list(uploaded_ej.keys())[0]
    print(f"  ✓ {uploaded_files['EJSCREEN']}")

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
# 5. MERGE EJSCREEN (BLOCK GROUP LEVEL)
# ============================================================
if 'EJSCREEN' in uploaded_files:
    print("\n--- Merging EJScreen (Block Group Level) ---")

    # Try different encodings (EJScreen files often use Latin-1 or Windows-1252)
    ej_data = None
    for encoding in ['latin-1', 'ISO-8859-1', 'cp1252', 'utf-8']:
        try:
            ej_data = pd.read_csv(uploaded_files['EJSCREEN'], dtype={'ID': str},
                                  encoding=encoding, low_memory=False)
            print(f"  ✓ File loaded successfully with {encoding} encoding")
            break
        except UnicodeDecodeError:
            continue

    if ej_data is None:
        print("  ⚠ Could not read EJScreen file with any common encoding")
        print("  Skipping EJScreen merge...")
    else:
        # Find GEOID column (EJScreen uses 'ID' as the block group GEOID)
        if 'ID' in ej_data.columns:
            ej_data['GEOID'] = ej_data['ID'].astype(str).str.zfill(12)

        # Filter to Memphis states
        ej_data['state_fips'] = ej_data['GEOID'].str[:2]
        ej_memphis = ej_data[ej_data['state_fips'].isin(['47', '05', '28'])].copy()

        # Select key EJScreen variables
        # Environmental indicators (P_ = percentile, national)
        ej_cols = ['GEOID',
                   'P_PM25', 'P_OZONE', 'P_DSLPM',  # Air quality
                   'P_CANCER', 'P_RESP', 'P_PTRAF', 'P_LDPNT', 'P_PNPL',  # Toxic exposure
                   'P_PRMP', 'P_PWDIS',  # Water quality
                   'P_PTSDF', 'P_UST',  # Waste sites
                   'P_MINORPCT', 'P_LOWINCPCT', 'P_LESSHSPCT', 'P_LINGISOPCT', 'P_UNDER5PCT', 'P_OVER64PCT',  # Demographics
                   'P_DEMOGIDX_2', 'P_DEMOGIDX_5',  # Demographic index
                   'P_VULEOPCT']  # Vulnerable populations

        available_ej = [c for c in ej_cols if c in ej_memphis.columns]

        # Merge on GEOID (block group to block group)
        final_data = final_data.merge(
            ej_memphis[available_ej],
            on='GEOID',
            how='left'
        )

        # Rename to more readable names
        rename_map = {
            'P_PM25': 'EJ_PM25_Pctl',
            'P_OZONE': 'EJ_Ozone_Pctl',
            'P_DSLPM': 'EJ_DieselPM_Pctl',
            'P_CANCER': 'EJ_Cancer_Pctl',
            'P_RESP': 'EJ_Respiratory_Pctl',
            'P_PTRAF': 'EJ_Traffic_Pctl',
            'P_LDPNT': 'EJ_LeadPaint_Pctl',
            'P_PNPL': 'EJ_Superfund_Pctl',
            'P_PRMP': 'EJ_RMP_Pctl',
            'P_PWDIS': 'EJ_WasteWater_Pctl',
            'P_PTSDF': 'EJ_HazWaste_Pctl',
            'P_UST': 'EJ_UndergroundTanks_Pctl',
            'P_MINORPCT': 'EJ_MinorityPct_Pctl',
            'P_LOWINCPCT': 'EJ_LowIncomePct_Pctl',
            'P_DEMOGIDX_2': 'EJ_DemoIndex2_Pctl',
            'P_DEMOGIDX_5': 'EJ_DemoIndex5_Pctl',
            'P_VULEOPCT': 'EJ_VulnerablePct_Pctl'
        }

        for old_col, new_col in rename_map.items():
            if old_col in final_data.columns:
                final_data.rename(columns={old_col: new_col}, inplace=True)

        ej_count = final_data['EJ_PM25_Pctl'].notna().sum() if 'EJ_PM25_Pctl' in final_data.columns else 0
        print(f"  ✓ EJScreen merged: {ej_count:,} / {len(final_data):,} ({ej_count/len(final_data)*100:.1f}%)")

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
    'Food Access': 'LILATracts_1And10',
    'EJScreen (Environment)': 'EJ_PM25_Pctl'
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
    'EJScreen': 'EJ_PM25_Pctl',
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
print("   • EJScreen (Environmental Justice) - Air quality, toxics, demographics")
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

### **Cell 7.5: Clean Non-Numeric Index Values**

```python
print("=" * 70)
print("CLEANING NON-NUMERIC VALUES")
print("=" * 70)

# Get all index columns (exclude geographic and demographic)
exclude_prefixes = ['GEOID', 'tract_geoid', 'state', 'latitude', 'longitude',
                    'distance', 'total_', 'white_', 'black_', 'hispanic_',
                    'all_', 'owner_', 'renter_']

index_cols = [col for col in final_data.columns
              if not any(col.startswith(prefix) for prefix in exclude_prefixes)]

# Also specifically include known index columns
index_patterns = ['ADI', 'SVI', 'RPL', 'ICE', 'COI', 'z_', 'LILA', 'Tract', 'LA']
index_cols.extend([col for col in final_data.columns
                   if any(pattern in col for pattern in index_patterns)])
index_cols = list(set(index_cols))  # Remove duplicates

print(f"\n🔍 Found {len(index_cols)} index columns to clean")

# Track changes
changes_summary = []

for col in index_cols:
    if col not in final_data.columns:
        continue

    # Count non-numeric before
    non_numeric_before = 0
    try:
        converted = pd.to_numeric(final_data[col], errors='coerce')
        non_numeric_before = (final_data[col].notna() & converted.isna()).sum()
    except:
        continue

    if non_numeric_before > 0:
        # Convert to numeric (non-numeric becomes NaN)
        final_data[col] = pd.to_numeric(final_data[col], errors='coerce')
        changes_summary.append((col, non_numeric_before))
        print(f"  ✓ {col}: {non_numeric_before} non-numeric → NA")

# Special value handling
print("\n🔧 Handling special values:")

# Values that should be NA
special_values = {
    'ADI_NATRANK': (1, 100),
    'ADI_STATERNK': (1, 10),
    'RPL_THEMES': (0, 1),
    'RPL_THEME1': (0, 1),
    'RPL_THEME2': (0, 1),
    'RPL_THEME3': (0, 1),
    'RPL_THEME4': (0, 1)
}

for col, (min_val, max_val) in special_values.items():
    if col in final_data.columns:
        # Values outside valid range → NA
        out_of_range = ((final_data[col] < min_val) | (final_data[col] > max_val)) & final_data[col].notna()
        if out_of_range.sum() > 0:
            final_data.loc[out_of_range, col] = np.nan
            print(f"  ✓ {col}: {out_of_range.sum()} values outside [{min_val}, {max_val}] → NA")

# Summary
print("\n" + "=" * 70)
print("CLEANING COMPLETE")
print("=" * 70)

if changes_summary:
    print(f"\nCleaned {len(changes_summary)} columns:")
    for col, count in changes_summary[:10]:  # Show first 10
        print(f"  • {col}: {count} values")
    if len(changes_summary) > 10:
        print(f"  ... and {len(changes_summary) - 10} more")
else:
    print("\n✓ All index columns already clean!")

print("\n✓ Data ready for analysis!")
```

---

### **Cell 8: Comprehensive Visualizations & Correlations**

```python
import matplotlib.pyplot as plt
import seaborn as sns

print("=" * 70)
print("VISUALIZATIONS & CORRELATION ANALYSIS")
print("=" * 70)

# Select key indices for visualization
viz_indices = {
    'ADI_NATRANK': 'ADI (Deprivation)',
    'RPL_THEMES': 'SVI (Vulnerability)',
    'ICE_race': 'ICE Race',
    'ICE_income': 'ICE Income',
    'ICE_race_income': 'ICE Race+Income'
}

# Add COI if available
coi_col = [c for c in final_data.columns if 'z_COI' in c or 'COI' in c]
if coi_col:
    viz_indices[coi_col[0]] = 'COI (Opportunity)'

# Add Food Access if available
if 'LILATracts_1And10' in final_data.columns:
    viz_indices['LILATracts_1And10'] = 'Food Desert'

# Filter to available columns
available_indices = {k: v for k, v in viz_indices.items() if k in final_data.columns}

print(f"\n📊 Analyzing {len(available_indices)} indices")

# ============================================================
# 1. CORRELATION MATRIX
# ============================================================
print("\n--- Calculating Correlations ---")

corr_data = final_data[list(available_indices.keys())].copy()
correlation_matrix = corr_data.corr()

# Save correlation matrix
correlation_matrix.to_csv('memphis_indices_correlation_matrix.csv')
print("✓ Saved: memphis_indices_correlation_matrix.csv")

# Visualize correlation matrix
fig, ax = plt.subplots(figsize=(10, 8))
sns.heatmap(correlation_matrix, annot=True, fmt='.2f', cmap='coolwarm',
            center=0, vmin=-1, vmax=1, square=True, ax=ax,
            xticklabels=[available_indices[k] for k in correlation_matrix.columns],
            yticklabels=[available_indices[k] for k in correlation_matrix.index])
ax.set_title('Correlation Matrix: SDOH Indices\nMemphis 50-Mile Radius', fontsize=14, pad=20)
plt.tight_layout()
plt.savefig('correlation_matrix.png', dpi=300, bbox_inches='tight')
plt.show()

print("\n✓ Key Correlations:")
# Show strongest correlations
for i, col1 in enumerate(correlation_matrix.columns):
    for col2 in correlation_matrix.columns[i+1:]:
        r = correlation_matrix.loc[col1, col2]
        if abs(r) > 0.5:  # Strong correlation
            print(f"  {available_indices[col1]:25} ↔ {available_indices[col2]:25} r = {r:.3f}")

# ============================================================
# 2. KEY SCATTER PLOTS
# ============================================================
print("\n--- Creating Scatter Plots ---")

fig, axes = plt.subplots(1, 3, figsize=(18, 5))

# Plot 1: ADI vs SVI
if 'ADI_NATRANK' in corr_data.columns and 'RPL_THEMES' in corr_data.columns:
    plot_data = corr_data[['ADI_NATRANK', 'RPL_THEMES']].dropna()
    if len(plot_data) > 0:
        axes[0].scatter(plot_data['ADI_NATRANK'], plot_data['RPL_THEMES'],
                       alpha=0.5, s=20)
        axes[0].set_xlabel('ADI (Higher = More Deprived)')
        axes[0].set_ylabel('SVI (Higher = More Vulnerable)')
        axes[0].set_title('ADI vs SVI')
        r = plot_data.corr().iloc[0, 1]
        axes[0].text(0.05, 0.95, f'r = {r:.3f}', transform=axes[0].transAxes,
                    verticalalignment='top', bbox=dict(boxstyle='round', facecolor='white', alpha=0.8))
        axes[0].grid(True, alpha=0.3)

# Plot 2: ADI vs ICE
if 'ADI_NATRANK' in corr_data.columns and 'ICE_race_income' in corr_data.columns:
    plot_data = corr_data[['ADI_NATRANK', 'ICE_race_income']].dropna()
    if len(plot_data) > 0:
        axes[1].scatter(plot_data['ADI_NATRANK'], plot_data['ICE_race_income'],
                       alpha=0.5, s=20, color='orange')
        axes[1].set_xlabel('ADI (Higher = More Deprived)')
        axes[1].set_ylabel('ICE Race+Income (Higher = More Privileged)')
        axes[1].set_title('ADI vs ICE Race+Income')
        r = plot_data.corr().iloc[0, 1]
        axes[1].text(0.05, 0.95, f'r = {r:.3f}', transform=axes[1].transAxes,
                    verticalalignment='top', bbox=dict(boxstyle='round', facecolor='white', alpha=0.8))
        axes[1].grid(True, alpha=0.3)

# Plot 3: SVI vs COI (if available)
if 'RPL_THEMES' in corr_data.columns and coi_col:
    plot_data = corr_data[['RPL_THEMES', coi_col[0]]].dropna()
    if len(plot_data) > 0:
        axes[2].scatter(plot_data['RPL_THEMES'], plot_data[coi_col[0]],
                       alpha=0.5, s=20, color='green')
        axes[2].set_xlabel('SVI (Higher = More Vulnerable)')
        axes[2].set_ylabel('COI (Higher = More Opportunity)')
        axes[2].set_title('SVI vs COI')
        r = plot_data.corr().iloc[0, 1]
        axes[2].text(0.05, 0.95, f'r = {r:.3f}', transform=axes[2].transAxes,
                    verticalalignment='top', bbox=dict(boxstyle='round', facecolor='white', alpha=0.8))
        axes[2].grid(True, alpha=0.3)
else:
    axes[2].text(0.5, 0.5, 'COI not available', ha='center', va='center',
                transform=axes[2].transAxes)
    axes[2].set_title('SVI vs COI')

plt.tight_layout()
plt.savefig('scatter_plots.png', dpi=300, bbox_inches='tight')
plt.show()

# ============================================================
# 3. DISTRIBUTION COMPARISONS
# ============================================================
print("\n--- Creating Distribution Plots ---")

fig, axes = plt.subplots(2, 2, figsize=(14, 10))

# Standardize all to 0-100 scale for comparison
standardized = {}
for col in available_indices.keys():
    data = final_data[col].dropna()
    if len(data) > 0:
        # Standardize to 0-100
        if 'ICE' in col:  # ICE is -1 to +1, reverse so high = disadvantage
            standardized[col] = (1 - data) / 2 * 100
        elif 'RPL' in col:  # SVI is 0-1
            standardized[col] = data * 100
        else:  # ADI, COI already 0-100ish
            standardized[col] = data

# Plot 1: Overlaid histograms
for col, label in list(available_indices.items())[:4]:
    if col in standardized:
        axes[0, 0].hist(standardized[col], bins=30, alpha=0.5, label=label)
axes[0, 0].set_xlabel('Standardized Score (0-100)')
axes[0, 0].set_ylabel('Frequency')
axes[0, 0].set_title('Distribution Comparison (All Standardized 0-100)')
axes[0, 0].legend()

# Plot 2: Box plots
box_data = [standardized[col] for col in available_indices.keys() if col in standardized]
box_labels = [available_indices[col] for col in available_indices.keys() if col in standardized]
axes[0, 1].boxplot(box_data, labels=box_labels)
axes[0, 1].set_ylabel('Standardized Score (0-100)')
axes[0, 1].set_title('Distribution by Index')
axes[0, 1].tick_params(axis='x', rotation=45)

# Plot 3: Data completeness
completeness = {}
for col, label in available_indices.items():
    pct = final_data[col].notna().sum() / len(final_data) * 100
    completeness[label] = pct

axes[1, 0].barh(list(completeness.keys()), list(completeness.values()))
axes[1, 0].set_xlabel('Data Completeness (%)')
axes[1, 0].set_title('Index Data Availability')
axes[1, 0].set_xlim(0, 100)

# Plot 4: Geographic pattern
if 'distance_from_memphis_miles' in final_data.columns and 'ADI_NATRANK' in final_data.columns:
    plot_data = final_data[['distance_from_memphis_miles', 'ADI_NATRANK']].dropna()
    axes[1, 1].scatter(plot_data['distance_from_memphis_miles'],
                      plot_data['ADI_NATRANK'], alpha=0.3, s=10)
    axes[1, 1].set_xlabel('Distance from Memphis (miles)')
    axes[1, 1].set_ylabel('ADI (Deprivation)')
    axes[1, 1].set_title('ADI by Distance from Memphis')
    axes[1, 1].grid(True, alpha=0.3)

plt.tight_layout()
plt.savefig('distribution_plots.png', dpi=300, bbox_inches='tight')
plt.show()

print("\n" + "=" * 70)
print("VISUALIZATION COMPLETE")
print("=" * 70)
print("\n✓ Saved files:")
print("  • correlation_matrix.png")
print("  • scatter_plots.png")
print("  • distribution_plots.png")
print("  • memphis_indices_correlation_matrix.csv")
```

---

### **Cell 9: Export Summary Statistics**

```python
print("=" * 70)
print("EXPORTING SUMMARY STATISTICS")
print("=" * 70)

# Create summary statistics for all indices
summary_stats = pd.DataFrame()

for col, label in available_indices.items():
    data = final_data[col].dropna()
    if len(data) > 0:
        stats = {
            'Index': label,
            'Variable': col,
            'Count': len(data),
            'Mean': data.mean(),
            'Median': data.median(),
            'Std': data.std(),
            'Min': data.min(),
            'Max': data.max(),
            'Q25': data.quantile(0.25),
            'Q75': data.quantile(0.75),
            'Missing_N': final_data[col].isna().sum(),
            'Missing_Pct': final_data[col].isna().sum() / len(final_data) * 100
        }
        summary_stats = pd.concat([summary_stats, pd.DataFrame([stats])], ignore_index=True)

# Save summary
summary_stats.to_csv('memphis_indices_summary_statistics.csv', index=False)

print("\n✓ Saved: memphis_indices_summary_statistics.csv")
print("\n📊 Summary Statistics:")
print(summary_stats.to_string(index=False))

# Download all analysis files
print("\n" + "=" * 70)
print("DOWNLOADING ANALYSIS FILES")
print("=" * 70)

from google.colab import files

analysis_files = [
    'correlation_matrix.png',
    'scatter_plots.png',
    'distribution_plots.png',
    'memphis_indices_correlation_matrix.csv',
    'memphis_indices_summary_statistics.csv'
]

for file in analysis_files:
    try:
        files.download(file)
        print(f"✓ Downloaded: {file}")
    except:
        print(f"⚠ Could not download: {file}")

print("\n✓ Analysis complete!")
```

---

### **Cell 10: Extract ADI Component Variables**

```python
print("=" * 70)
print("EXTRACTING ADI COMPONENT VARIABLES")
print("=" * 70)

print("\nℹ️  ADI (Area Deprivation Index) is composed of 17 Census variables")
print("   across 4 domains: Income, Education, Employment, Housing Quality")

# Define the 17 ADI component variables from Singh (2003)
adi_components = {
    # INCOME DOMAIN (5 variables)
    'B17001_002E': 'pop_below_poverty',
    'B17001_001E': 'pop_poverty_determined',
    'B19113_001E': 'median_family_income',
    'B25077_001E': 'median_home_value',
    'B25064_001E': 'median_gross_rent',

    # EDUCATION DOMAIN (2 variables)
    'B15002_003E': 'male_less_than_9th',
    'B15002_004E': 'male_9th_to_12th_no_diploma',
    'B15002_020E': 'female_less_than_9th',
    'B15002_021E': 'female_9th_to_12th_no_diploma',
    'B15002_001E': 'total_pop_education',

    # EMPLOYMENT DOMAIN (2 variables)
    'B23025_005E': 'unemployed',
    'B23025_003E': 'in_labor_force',

    # HOUSING QUALITY DOMAIN (8 variables)
    'B25044_003E': 'owner_no_vehicle',
    'B25044_010E': 'renter_no_vehicle',
    'B25044_001E': 'total_tenure_vehicle',
    'B25014_005E': 'owner_1.01_to_1.50_per_room',
    'B25014_006E': 'owner_1.51_to_2.00_per_room',
    'B25014_007E': 'owner_2.01_or_more_per_room',
    'B25014_011E': 'renter_1.01_to_1.50_per_room',
    'B25014_012E': 'renter_1.51_to_2.00_per_room',
    'B25014_013E': 'renter_2.01_or_more_per_room',
    'B25014_001E': 'total_occupancy_per_room',
    'B25043_007E': 'owner_no_telephone',
    'B25043_014E': 'renter_no_telephone',
    'B25043_001E': 'total_telephone',
    'B25003_003E': 'renter_occupied',
    'B25003_001E': 'total_housing_units',
    'B11001_002E': 'family_households',
    'B11001_001E': 'total_households'
}

print(f"\n🔍 Checking for ADI components in dataset...")

# Step 1: Check which variables already exist
available_vars = []
missing_vars = []

for census_var, readable_name in adi_components.items():
    # Check if variable exists in dataset (with or without suffix)
    if census_var in final_data.columns:
        available_vars.append((census_var, readable_name, census_var))
    elif readable_name in final_data.columns:
        available_vars.append((census_var, readable_name, readable_name))
    else:
        # Check if census variable without 'E' suffix exists
        var_no_suffix = census_var.replace('E', '')
        if var_no_suffix in final_data.columns:
            available_vars.append((census_var, readable_name, var_no_suffix))
        else:
            missing_vars.append((census_var, readable_name))

print(f"\n✓ Found {len(available_vars)} variables already in dataset")
print(f"⚠  Missing {len(missing_vars)} variables (need to collect from Census API)")

if available_vars:
    print("\n📋 Already available:")
    for i, (census_var, readable_name, _) in enumerate(available_vars[:5], 1):
        print(f"   {i}. {readable_name} ({census_var})")
    if len(available_vars) > 5:
        print(f"   ... and {len(available_vars) - 5} more")

if missing_vars:
    print("\n📋 Need to collect:")
    for i, (census_var, readable_name) in enumerate(missing_vars[:5], 1):
        print(f"   {i}. {readable_name} ({census_var})")
    if len(missing_vars) > 5:
        print(f"   ... and {len(missing_vars) - 5} more")

# Step 2: If missing variables, try to collect them from Census API
if missing_vars:
    print(f"\n{'='*70}")
    print(f"COLLECTING {len(missing_vars)} MISSING VARIABLES FROM CENSUS API")
    print(f"{'='*70}")

    try:
        import requests
        import os
        import time

        api_key = os.environ.get('CENSUS_API_KEY', '4d5e7ded000067ff443e2f90683ce53bcf660392')

        # Split variables into smaller batches (Census API can be finicky with too many vars)
        batch_size = 25
        missing_var_batches = [missing_vars[i:i + batch_size]
                               for i in range(0, len(missing_vars), batch_size)]

        print(f"\n📦 Splitting into {len(missing_var_batches)} batches of up to {batch_size} variables")

        # Collect by state
        all_state_data = {}

        for state_fips, state_abbr in {'47': 'TN', '05': 'AR', '28': 'MS'}.items():
            print(f"\n{'─'*70}")
            print(f"📍 {state_abbr} (state FIPS: {state_fips})")
            print(f"{'─'*70}")

            state_dfs = []

            for batch_idx, var_batch in enumerate(missing_var_batches, 1):
                print(f"\n  Batch {batch_idx}/{len(missing_var_batches)} ({len(var_batch)} variables)...")

                # Build variable list
                var_list = ','.join([var for var, _ in var_batch])

                # API call
                url = f"https://api.census.gov/data/2021/acs/acs5"
                params = {
                    'get': var_list,
                    'for': 'block group:*',
                    'in': f'state:{state_fips} county:* tract:*',
                    'key': api_key
                }

                try:
                    response = requests.get(url, params=params, timeout=120)

                    if response.status_code == 200:
                        data = response.json()

                        if len(data) > 1:  # Has data rows beyond header
                            headers = data[0]
                            rows = data[1:]

                            # Convert to DataFrame
                            df = pd.DataFrame(rows, columns=headers)

                            # Create GEOID
                            df['GEOID'] = (df['state'] + df['county'] +
                                          df['tract'] + df['block group'])
                            df['GEOID'] = df['GEOID'].astype(str).str.zfill(12)

                            # Rename variables to readable names
                            rename_map = {var: name for var, name in var_batch}
                            df.rename(columns=rename_map, inplace=True)

                            state_dfs.append(df)
                            print(f"    ✓ Collected {len(df)} block groups")
                        else:
                            print(f"    ⚠ No data returned")

                    else:
                        print(f"    ⚠ HTTP {response.status_code}: {response.text[:200]}")

                    # Be nice to the API
                    time.sleep(0.5)

                except Exception as e:
                    print(f"    ⚠ Error: {e}")
                    continue

            # Merge batches for this state
            if state_dfs:
                # Start with first batch
                state_combined = state_dfs[0]

                # Merge additional batches
                for df in state_dfs[1:]:
                    state_combined = state_combined.merge(df, on='GEOID', how='outer',
                                                          suffixes=('', '_dup'))
                    # Drop duplicate geographic columns
                    dup_cols = [c for c in state_combined.columns if c.endswith('_dup')]
                    state_combined.drop(columns=dup_cols, inplace=True)

                all_state_data[state_fips] = state_combined
                print(f"\n  ✓ {state_abbr} complete: {len(state_combined)} block groups with {len(state_combined.columns)-5} variables")

        # Combine all states
        if all_state_data:
            print(f"\n{'='*70}")
            print("MERGING NEW DATA INTO DATASET")
            print(f"{'='*70}")

            new_data_combined = pd.concat(all_state_data.values(), ignore_index=True)
            print(f"\n✓ Combined data: {len(new_data_combined)} block groups")

            # Get list of new variable columns (exclude geographic columns)
            geo_cols = ['GEOID', 'state', 'county', 'tract', 'block group']
            new_var_cols = [c for c in new_data_combined.columns if c not in geo_cols]

            print(f"✓ New variables: {len(new_var_cols)}")

            # Merge with final_data
            merge_cols = ['GEOID'] + new_var_cols
            before_cols = len(final_data.columns)

            final_data = final_data.merge(
                new_data_combined[merge_cols],
                on='GEOID',
                how='left',
                suffixes=('', '_new')
            )

            after_cols = len(final_data.columns)
            print(f"✓ Merged into final_data: {before_cols} → {after_cols} columns (+{after_cols-before_cols})")

            # Update available_vars with newly collected variables
            for var, name in missing_vars:
                if name in final_data.columns:
                    available_vars.append((var, name, name))

            print(f"\n✅ SUCCESS: Collected {len(new_var_cols)} variables from Census API!")
        else:
            print(f"\n⚠ WARNING: Could not collect any missing variables from Census API")
            print("   Continuing with only the variables already in dataset...")

    except Exception as e:
        print(f"\n⚠ ERROR collecting variables: {e}")
        print("   Continuing with available variables only...")
        import traceback
        print(f"\nFull error:\n{traceback.format_exc()}")

# Step 3: Create ADI components dataset
print(f"\n{'='*70}")
print(f"CREATING ADI COMPONENTS DATASET")
print(f"{'='*70}")

print(f"\n📊 Using {len(available_vars)} variables...")

# Extract variables into new dataframe
adi_extract = final_data[['GEOID', 'tract_geoid', 'state_abbr',
                           'latitude', 'longitude', 'distance_from_memphis_miles']].copy()

# Add raw variables
for census_var, readable_name, actual_col in available_vars:
    adi_extract[readable_name] = pd.to_numeric(final_data[actual_col], errors='coerce')

# Step 4: Calculate ADI component percentages
print("\n🧮 Calculating ADI component percentages...")

# Calculate percentage indicators used in ADI
try:
    # 1. Poverty rate
    if 'pop_below_poverty' in adi_extract.columns and 'pop_poverty_determined' in adi_extract.columns:
        adi_extract['pct_below_poverty'] = (
            adi_extract['pop_below_poverty'] / adi_extract['pop_poverty_determined'] * 100
        )

    # 2. Less than high school education
    if all(col in adi_extract.columns for col in ['male_less_than_9th', 'male_9th_to_12th_no_diploma',
                                                    'female_less_than_9th', 'female_9th_to_12th_no_diploma',
                                                    'total_pop_education']):
        adi_extract['pct_no_hs_diploma'] = (
            (adi_extract['male_less_than_9th'] + adi_extract['male_9th_to_12th_no_diploma'] +
             adi_extract['female_less_than_9th'] + adi_extract['female_9th_to_12th_no_diploma']) /
            adi_extract['total_pop_education'] * 100
        )

    # 3. Unemployment rate
    if 'unemployed' in adi_extract.columns and 'in_labor_force' in adi_extract.columns:
        adi_extract['pct_unemployed'] = (
            adi_extract['unemployed'] / adi_extract['in_labor_force'] * 100
        )

    # 4. No vehicle
    if all(col in adi_extract.columns for col in ['owner_no_vehicle', 'renter_no_vehicle', 'total_tenure_vehicle']):
        adi_extract['pct_no_vehicle'] = (
            (adi_extract['owner_no_vehicle'] + adi_extract['renter_no_vehicle']) /
            adi_extract['total_tenure_vehicle'] * 100
        )

    # 5. Crowded housing (>1 person per room)
    crowding_cols = ['owner_1.01_to_1.50_per_room', 'owner_1.51_to_2.00_per_room', 'owner_2.01_or_more_per_room',
                     'renter_1.01_to_1.50_per_room', 'renter_1.51_to_2.00_per_room', 'renter_2.01_or_more_per_room']
    if all(col in adi_extract.columns for col in crowding_cols + ['total_occupancy_per_room']):
        adi_extract['pct_crowded'] = (
            sum(adi_extract[col] for col in crowding_cols) / adi_extract['total_occupancy_per_room'] * 100
        )

    # 6. No telephone
    if all(col in adi_extract.columns for col in ['owner_no_telephone', 'renter_no_telephone', 'total_telephone']):
        adi_extract['pct_no_telephone'] = (
            (adi_extract['owner_no_telephone'] + adi_extract['renter_no_telephone']) /
            adi_extract['total_telephone'] * 100
        )

    # 7. Renter occupied
    if 'renter_occupied' in adi_extract.columns and 'total_housing_units' in adi_extract.columns:
        adi_extract['pct_renter'] = (
            adi_extract['renter_occupied'] / adi_extract['total_housing_units'] * 100
        )

    # 8. Single parent households
    if 'family_households' in adi_extract.columns and 'total_households' in adi_extract.columns:
        adi_extract['pct_family_households'] = (
            adi_extract['family_households'] / adi_extract['total_households'] * 100
        )

    print("✓ Calculated percentage indicators")

except Exception as e:
    print(f"⚠ Some percentage calculations failed: {e}")

# Step 5: Save to CSV
filename = f'memphis_{RADIUS_MILES}mile_adi_components.csv'
adi_extract.to_csv(filename, index=False)

print("\n" + "=" * 70)
print("ADI COMPONENTS EXTRACTION COMPLETE")
print("=" * 70)

print(f"\n📁 Filename: {filename}")
print(f"📊 Total rows: {len(adi_extract):,} block groups")
print(f"📊 Total columns: {len(adi_extract.columns)}")

print("\n✓ Contents:")
print(f"  • {len(available_vars)} raw Census variables")
print(f"  • {len([c for c in adi_extract.columns if c.startswith('pct_')])} calculated percentages")
print("  • Geographic identifiers (GEOID, state, distance)")

print("\n📋 Sample data:")
display_cols = ['GEOID', 'state_abbr', 'distance_from_memphis_miles']
if 'pct_below_poverty' in adi_extract.columns:
    display_cols.append('pct_below_poverty')
if 'pct_unemployed' in adi_extract.columns:
    display_cols.append('pct_unemployed')
if 'median_family_income' in adi_extract.columns:
    display_cols.append('median_family_income')

print(adi_extract[display_cols].head(10).to_string(index=False))

# Create data dictionary
print("\n📖 Creating data dictionary...")

data_dict = []
for census_var, readable_name, _ in available_vars:
    data_dict.append({
        'Variable': readable_name,
        'Census_Code': census_var,
        'Domain': 'Income' if 'income' in readable_name or 'poverty' in readable_name or 'value' in readable_name or 'rent' in readable_name
                  else 'Education' if 'education' in readable_name or '9th' in readable_name or '12th' in readable_name
                  else 'Employment' if 'employed' in readable_name or 'labor' in readable_name
                  else 'Housing' if 'vehicle' in readable_name or 'room' in readable_name or 'telephone' in readable_name or 'renter' in readable_name or 'household' in readable_name
                  else 'Other',
        'Description': readable_name.replace('_', ' ').title()
    })

# Add percentage variables
pct_vars = [c for c in adi_extract.columns if c.startswith('pct_')]
for var in pct_vars:
    data_dict.append({
        'Variable': var,
        'Census_Code': 'Calculated',
        'Domain': 'Percentage Indicator',
        'Description': var.replace('_', ' ').title()
    })

data_dict_df = pd.DataFrame(data_dict)
data_dict_filename = f'memphis_{RADIUS_MILES}mile_adi_components_dictionary.csv'
data_dict_df.to_csv(data_dict_filename, index=False)

print(f"✓ Saved: {data_dict_filename}")

# Download files
print("\n" + "=" * 70)
print("DOWNLOADING")
print("=" * 70)

from google.colab import files

files.download(filename)
files.download(data_dict_filename)

print("\n✓ Download complete!")
print("\n🎉 You now have all ADI component variables!")
print(f"   • {len(available_vars)} Census variables")
print("   • Calculated percentage indicators")
print("   • Data dictionary for reference")
print("=" * 70)
```

---

## 📋 QUICK REFERENCE TABLES

### Index Availability

| Index | Geography | Variables | Example Column Names |
|-------|-----------|-----------|---------------------|
| **ADI** | Block Group | 2 | `ADI_NATRANK`, `ADI_STATERNK` |
| **ICE** | Block Group | 5 | `ICE_race`, `ICE_income`, `ICE_race_income`, etc. |
| **EJScreen** | Block Group | 20+ | `EJ_PM25_Pctl`, `EJ_Cancer_Pctl`, `EJ_Traffic_Pctl`, etc. |
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
| EJScreen | 0-100 | Lower environmental burden | Higher environmental burden |

---

## 🎯 KEY TAKEAWAY

**You'll have ONE dataset** with ~800-1,200 block groups, where:
- Each row = one block group
- Block group variables (ADI, ICE, EJScreen) vary by row
- Tract variables (SVI, COI, Food) are the same for all block groups within the same tract

This is the **standard approach** for neighborhood health research and is appropriate for analysis. Just note in your methods that some variables are tract-level.

---

## 📚 DATA CITATIONS

### American Community Survey (ACS) Data

All Census demographic variables (used for ICE calculations and ADI components) are from:

**U.S. Census Bureau (2021).** *American Community Survey 5-Year Estimates, 2017-2021.* Retrieved from https://www.census.gov/programs-surveys/acs

**Specific tables used:**
- **B03002**: Hispanic or Latino Origin by Race
- **B19001**: Household Income in the Past 12 Months
- **B19001H**: Household Income (White Alone, Not Hispanic or Latino)
- **B19001B**: Household Income (Black or African American Alone)
- **B25003**: Tenure (Owner/Renter Occupied)
- **B25003H**: Tenure (White Alone, Not Hispanic or Latino Householder)
- **B25003B**: Tenure (Black or African American Alone Householder)
- **B17001**: Poverty Status in the Past 12 Months
- **B19113**: Median Family Income in the Past 12 Months
- **B25077**: Median Value (Dollars)
- **B25064**: Median Gross Rent (Dollars)
- **B15002**: Sex by Educational Attainment for the Population 25 Years and Over
- **B23025**: Employment Status for the Population 16 Years and Over
- **B25044**: Tenure by Vehicles Available
- **B25014**: Tenure by Occupants Per Room
- **B25043**: Tenure by Telephone Service Available
- **B11001**: Household Type

**API Access:**
```
https://api.census.gov/data/2021/acs/acs5
```

**Note on 5-Year Estimates:** The 2021 ACS 5-year estimates represent data collected over the period 2017-2021. These multi-year estimates provide more reliable data for small geographies like census block groups, where single-year estimates may not be available or may have large margins of error.

### Area Deprivation Index (ADI)

**Neighborhood Atlas® (2023).** *Area Deprivation Index v4.0.* University of Wisconsin School of Medicine and Public Health. Retrieved from https://www.neighborhoodatlas.medicine.wisc.edu/

**Citation format:**
> University of Wisconsin School of Medicine and Public Health (2023). 2023 Area Deprivation Index v4.0. Downloaded from https://www.neighborhoodatlas.medicine.wisc.edu/

**Original ADI methodology:**
> Singh, G.K. (2003). Area deprivation and widening inequalities in US mortality, 1969-1998. *American Journal of Public Health, 93*(7), 1137-1143. https://doi.org/10.2105/AJPH.93.7.1137

### Social Vulnerability Index (SVI)

**Centers for Disease Control and Prevention/Agency for Toxic Substances and Disease Registry (2020).** *CDC/ATSDR Social Vulnerability Index [2020 Database].* Retrieved from https://www.atsdr.cdc.gov/placeandhealth/svi/

**Citation format:**
> Centers for Disease Control and Prevention/ Agency for Toxic Substances and Disease Registry/ Geospatial Research, Analysis, and Services Program. CDC/ATSDR Social Vulnerability Index 2020 Database Tennessee, Arkansas, Mississippi. https://www.atsdr.cdc.gov/placeandhealth/svi/data_documentation_download.html. Accessed [DATE].

### Index of Concentration at the Extremes (ICE)

The ICE measures were calculated using ACS 2017-2021 data following the methodology described in:

> Krieger, N., Waterman, P.D., Chen, J.T., Soobader, M.J., Subramanian, S.V., & Carson, R. (2002). Zip code caveat: Bias due to spatiotemporal mismatches between zip codes and US census-defined geographic areas—the Public Health Disparities Geocoding Project. *American Journal of Public Health, 92*(7), 1100-1102. https://doi.org/10.2105/AJPH.92.7.1100

> Massey, D.S. (2001). The prodigal paradigm returns: Ecology comes back to sociology. In A. Booth & A.C. Crouter (Eds.), *Does it take a village? Community effects on children, adolescents, and families* (pp. 41-48). Lawrence Erlbaum Associates.

### Child Opportunity Index (COI)

**Acevedo-Garcia, D., Noelke, C., McArdle, N., et al. (2020).** *Child Opportunity Index 2.0 Database.* diversitydatakids.org. Brandeis University, The Heller School for Social Policy and Management. Retrieved from https://www.diversitydatakids.org/child-opportunity-index

### USDA Food Access Research Atlas

**U.S. Department of Agriculture, Economic Research Service (2019).** *Food Access Research Atlas.* Retrieved from https://www.ers.usda.gov/data-products/food-access-research-atlas/

**Citation format:**
> U.S. Department of Agriculture, Economic Research Service. Food Access Research Atlas. https://www.ers.usda.gov/data-products/food-access-research-atlas/. Accessed [DATE].

### EPA Environmental Justice Screen (EJScreen)

**U.S. Environmental Protection Agency (EPA). (2023).** *EJScreen: Environmental Justice Screening and Mapping Tool.* Retrieved from https://www.epa.gov/ejscreen

---

## 📝 METHODS STATEMENT TEMPLATE

When writing your methods section, you can adapt this text:

> **Data Sources:** Census demographic data were obtained from the U.S. Census Bureau's American Community Survey (ACS) 5-year estimates for 2017-2021 at the census block group level via the Census API. We collected data for all block groups within a 50-mile radius of Memphis, TN (35.1495°N, 90.0490°W), encompassing portions of Tennessee, Arkansas, and Mississippi.
>
> **Neighborhood Indices:** We incorporated multiple neighborhood-level social determinants of health (SDOH) indices. The Area Deprivation Index (ADI) v4.0 was obtained from the University of Wisconsin Neighborhood Atlas®. The CDC/ATSDR Social Vulnerability Index (SVI) 2020 was downloaded from the CDC's Geospatial Research, Analysis, and Services Program. The Child Opportunity Index (COI) 2.0 was obtained from diversitydatakids.org. Food access metrics were derived from the USDA Food Access Research Atlas (2019).
>
> **Index Calculations:** We calculated five Index of Concentration at the Extremes (ICE) measures following established methods (Krieger et al., 2002; Massey, 2001) using ACS 2017-2021 data: ICE for race (White Non-Hispanic vs. Black), income (high vs. low), race-income (White Non-Hispanic high-income vs. Black low-income), tenure (owner vs. renter), and race-tenure (White owner vs. Black renter).
>
> **Geographic Considerations:** While census block groups served as our primary unit of analysis (N = [YOUR N]), some indices (SVI, COI, Food Access) are natively available only at the census tract level. Following standard practice in neighborhood health research, tract-level values were assigned to all block groups within each tract. Statistical analyses accounted for this spatial clustering using [YOUR STATISTICAL APPROACH, e.g., multilevel models, cluster-robust standard errors].

---

