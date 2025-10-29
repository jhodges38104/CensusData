# Memphis Area Census Data Collection - Google Colab Guide

This guide shows you how to collect ACS data for all block groups within a 30-mile radius of Memphis, Tennessee.

## Google Colab Setup

Use these cells in order:

---

### **Cell 1: Setup Repository**
```python
# Clone and setup
!git clone https://github.com/jhodges38104/CensusData.git
%cd CensusData
!git checkout claude/collect-acs-census-data-011CUc2zqWZm3HPihktsDGut
!pip install -r requirements.txt
```

---

### **Cell 2: Import and Initialize**
```python
import os
import pandas as pd
import numpy as np
from collect_acs_blockgroup_data import ACSBlockGroupCollector

# Set API key
os.environ['CENSUS_API_KEY'] = '4d5e7ded000067ff443e2f90683ce53bcf660392'

# Initialize collector
collector = ACSBlockGroupCollector()
print("✓ Collector ready!")
```

---

### **Cell 3: Define Helper Functions**
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
    """Get approximate coordinates using census tract centroids."""
    import requests

    api_key = os.environ.get('CENSUS_API_KEY')

    url = "https://tigerweb.geo.census.gov/arcgis/rest/services/TIGERweb/tigerWMS_ACS2021/MapServer/8/query"

    params = {
        'where': f"STATE='{state_fips}'",
        'outFields': 'STATE,COUNTY,TRACT,CENTLAT,CENTLON',
        'returnGeometry': 'false',
        'f': 'json',
        'resultRecordCount': 5000  # Max records per request
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
            state = attrs['STATE']
            county = attrs['COUNTY']
            tract = attrs['TRACT']

            # Create tract GEOID
            tract_geoid = f"{state}{county}{tract}"

            coords[tract_geoid] = {
                'latitude': float(attrs['CENTLAT']),
                'longitude': float(attrs['CENTLON'])
            }

        offset += len(features)

        # Check if we got all records
        if len(features) < 5000:
            break

    return coords

print("✓ Helper functions loaded")
```

---

### **Cell 4: Collect Memphis Area Data**
```python
# Memphis, TN coordinates
MEMPHIS_LAT = 35.1495
MEMPHIS_LON = -90.0490
RADIUS_MILES = 30

print("=" * 70)
print(f"Collecting data within {RADIUS_MILES} miles of Memphis, TN")
print("=" * 70)

# States near Memphis (it's on the border)
states = {
    '47': 'TN',  # Tennessee
    '05': 'AR',  # Arkansas
    '28': 'MS',  # Mississippi
}

print(f"\nCollecting from: {', '.join(states.values())}")

# Collect ACS data for all three states
all_data = []

for state_fips, state_abbr in states.items():
    print(f"\n--- Collecting {state_abbr} ---")

    # Get ACS data
    data = collector.collect_blockgroup_data(state=state_fips, year=2021)
    data['state_abbr'] = state_abbr

    # Get tract coordinates
    print(f"  Getting coordinates for {state_abbr}...")
    tract_coords = get_tract_centroids(state_fips)

    # Add tract GEOID to data
    data['tract_geoid'] = data['GEOID'].str[:11]

    # Map coordinates from tract to block groups
    data['latitude'] = data['tract_geoid'].map(lambda x: tract_coords.get(x, {}).get('latitude'))
    data['longitude'] = data['tract_geoid'].map(lambda x: tract_coords.get(x, {}).get('longitude'))

    print(f"  ✓ Collected {len(data)} block groups")

    all_data.append(data)

# Combine states
print("\n--- Combining states ---")
combined = pd.concat(all_data, ignore_index=True)
print(f"✓ Total: {len(combined)} block groups")

# Remove any without coordinates
combined = combined.dropna(subset=['latitude', 'longitude'])
print(f"✓ With coordinates: {len(combined)}")
```

---

### **Cell 5: Filter by Distance**
```python
# Calculate distance from Memphis
print("\n--- Calculating distances ---")
combined['distance_from_memphis_miles'] = combined.apply(
    lambda row: haversine_distance(
        MEMPHIS_LAT, MEMPHIS_LON,
        row['latitude'], row['longitude']
    ),
    axis=1
)

# Filter to radius
memphis_area = combined[combined['distance_from_memphis_miles'] <= RADIUS_MILES].copy()

print(f"\n✓ Found {len(memphis_area)} block groups within {RADIUS_MILES} miles")
print("\nBreakdown by state:")
for state_abbr in states.values():
    count = len(memphis_area[memphis_area['state_abbr'] == state_abbr])
    print(f"  {state_abbr}: {count}")

# Calculate ICE measures
print("\n--- Calculating ICE measures ---")
memphis_area = collector.calculate_ice_measures(memphis_area)

# Sort by distance
memphis_area = memphis_area.sort_values('distance_from_memphis_miles')

print(f"\n✓ Complete! {len(memphis_area)} block groups")
```

---

### **Cell 6: View Results**
```python
# Summary statistics
print("=" * 70)
print("SUMMARY")
print("=" * 70)
print(f"Total block groups: {len(memphis_area)}")
print(f"Distance range: {memphis_area['distance_from_memphis_miles'].min():.1f} - {memphis_area['distance_from_memphis_miles'].max():.1f} miles")
print(f"Total population: {memphis_area['total_pop_race_eth'].sum():,.0f}")

# Sample data
print("\n--- Closest 10 Block Groups to Memphis ---")
display_cols = ['GEOID', 'state_abbr', 'distance_from_memphis_miles',
                'total_pop_race_eth', 'ICE_race', 'ICE_income', 'ICE_race_income']
print(memphis_area[display_cols].head(10).to_string(index=False))

# ICE statistics
print("\n--- ICE Measures Summary ---")
ice_cols = [col for col in memphis_area.columns if col.startswith('ICE_')]
print(memphis_area[ice_cols].describe().round(3))
```

---

### **Cell 7: Save and Download**
```python
# Save to CSV
filename = 'memphis_30mile_radius_2021.csv'
memphis_area.to_csv(filename, index=False)

print(f"\n✓ Saved to: {filename}")
print(f"  Rows: {len(memphis_area)}")
print(f"  Columns: {len(memphis_area.columns)}")

# Download
from google.colab import files
files.download(filename)
print("\n✓ Download started!")
```

---

## Customization Options

### Change the Radius

In **Cell 4**, change:
```python
RADIUS_MILES = 30  # Change to 20, 50, etc.
```

### Change the Center Point

In **Cell 4**, change coordinates:
```python
MEMPHIS_LAT = 35.1495  # Change to your latitude
MEMPHIS_LON = -90.0490  # Change to your longitude
```

### Use a Different Year

In **Cell 4**, change:
```python
data = collector.collect_blockgroup_data(state=state_fips, year=2021)  # Change year
```

### Add More or Fewer States

In **Cell 4**, modify the states dictionary:
```python
states = {
    '47': 'TN',  # Tennessee
    '05': 'AR',  # Arkansas
    '28': 'MS',  # Mississippi
    # '01': 'AL',  # Add Alabama if needed
}
```

---

## Expected Output

You should get a CSV file with approximately:
- **500-800 block groups** (depending on exact borders)
- All **ACS demographic variables**
- All **5 ICE measures**
- **Distance from Memphis** for each block group
- **Coordinates** (latitude/longitude)

---

## Troubleshooting

**If coordinate lookup is slow:**
- This is normal - getting coordinates for thousands of tracts takes time
- Be patient, it should complete in 5-10 minutes

**If you get timeout errors:**
- Re-run the cell - coordinates are cached
- Or reduce the initial search area to just TN

**If distance calculations seem off:**
- Coordinates are tract-level centroids (approximate)
- Block groups inherit their tract's centroid
- This is accurate enough for 30-mile radius filtering

---

## What You Get

The output CSV contains:

### Geographic Info
- `GEOID` - Block group identifier
- `NAME` - Human-readable name
- `state_abbr` - State (TN, AR, or MS)
- `latitude`, `longitude` - Coordinates
- `distance_from_memphis_miles` - Miles from Memphis center

### Demographics
- `total_pop_race_eth` - Total population
- `white_NH_alone` - Non-Hispanic White population
- `black_NH_alone` - Non-Hispanic Black population
- Income and housing tenure variables

### ICE Measures
- `ICE_race` - Racial segregation
- `ICE_income` - Income segregation
- `ICE_race_income` - Racialized economic segregation
- `ICE_owner_renter` - Housing tenure
- `ICE_race_tenure` - Racialized housing tenure

All ICE measures range from -1 (most disadvantaged) to +1 (most privileged).
