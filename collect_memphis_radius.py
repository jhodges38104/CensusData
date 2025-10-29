"""
Collect ACS data for all block groups within a 30-mile radius of Memphis, TN.

This script:
1. Defines Memphis, TN center point
2. Collects block group data for TN, AR, and MS (Memphis is near borders)
3. Calculates distance from Memphis for each block group
4. Filters to block groups within 30-mile radius
5. Outputs data with ICE measures

Usage in Google Colab:
- Run after setting up the collector (previous cells)
- Adjust radius_miles variable if needed
"""

import pandas as pd
import numpy as np
from collect_acs_blockgroup_data import ACSBlockGroupCollector
import os

# Set API key
os.environ['CENSUS_API_KEY'] = '4d5e7ded000067ff443e2f90683ce53bcf660392'


def haversine_distance(lat1, lon1, lat2, lon2):
    """
    Calculate the great circle distance between two points on Earth.

    Args:
        lat1, lon1: Latitude and longitude of point 1 (in decimal degrees)
        lat2, lon2: Latitude and longitude of point 2 (in decimal degrees)

    Returns:
        Distance in miles
    """
    # Convert decimal degrees to radians
    lat1, lon1, lat2, lon2 = map(np.radians, [lat1, lon1, lat2, lon2])

    # Haversine formula
    dlat = lat2 - lat1
    dlon = lon2 - lon1
    a = np.sin(dlat/2)**2 + np.cos(lat1) * np.cos(lat2) * np.sin(dlon/2)**2
    c = 2 * np.arcsin(np.sqrt(a))

    # Radius of Earth in miles
    radius_earth_miles = 3959

    return radius_earth_miles * c


def get_blockgroup_centroids(state_fips, year=2021):
    """
    Get block group centroids (center points) from Census API.

    Args:
        state_fips: State FIPS code
        year: Year for ACS data

    Returns:
        DataFrame with GEOID, latitude, longitude
    """
    import requests

    api_key = os.environ.get('CENSUS_API_KEY')

    # Get internal point (centroid) coordinates for block groups
    url = f"https://api.census.gov/data/{year}/acs/acs5"
    params = {
        'get': 'NAME,B01001_001E',  # Get name and total population
        'for': 'block group:*',
        'in': f'state:{state_fips} county:* tract:*',
        'key': api_key
    }

    response = requests.get(url, params=params)

    if response.status_code != 200:
        raise Exception(f"Failed to get block groups for state {state_fips}: {response.text}")

    data = response.json()
    headers = data[0]
    rows = data[1:]

    df = pd.DataFrame(rows, columns=headers)

    # Create GEOID
    df['GEOID'] = df['state'] + df['county'] + df['tract'] + df['block group']

    return df[['GEOID', 'NAME']]


def get_blockgroup_centroids_with_tigerweb(geoids):
    """
    Get centroids for block groups using Census TIGERweb service.
    This is more reliable than trying to calculate from boundaries.

    Args:
        geoids: List of GEOIDs

    Returns:
        DataFrame with GEOID, latitude, longitude
    """
    import requests
    import time

    centroids = []

    print(f"Getting coordinates for {len(geoids)} block groups...")
    print("This may take a few minutes...")

    # Process in batches to show progress
    batch_size = 100
    for i in range(0, len(geoids), batch_size):
        batch = geoids[i:i+batch_size]

        for geoid in batch:
            # Parse GEOID: state(2) + county(3) + tract(6) + blockgroup(1)
            state = geoid[:2]
            county = geoid[2:5]
            tract = geoid[5:11]
            bg = geoid[11:12]

            # TIGERweb REST API for block groups
            url = "https://tigerweb.geo.census.gov/arcgis/rest/services/TIGERweb/tigerWMS_ACS2021/MapServer/14/query"

            params = {
                'where': f"STATE='{state}' AND COUNTY='{county}' AND TRACT='{tract}' AND BLKGRP='{bg}'",
                'outFields': 'STATE,COUNTY,TRACT,BLKGRP,CENTLAT,CENTLON',
                'returnGeometry': 'false',
                'f': 'json'
            }

            try:
                response = requests.get(url, params=params, timeout=10)
                if response.status_code == 200:
                    result = response.json()
                    if 'features' in result and len(result['features']) > 0:
                        attrs = result['features'][0]['attributes']
                        centroids.append({
                            'GEOID': geoid,
                            'latitude': float(attrs.get('CENTLAT', 0)),
                            'longitude': float(attrs.get('CENTLON', 0))
                        })
                time.sleep(0.1)  # Rate limiting
            except Exception as e:
                print(f"Warning: Could not get centroid for {geoid}: {e}")
                continue

        # Progress update
        print(f"  Processed {min(i+batch_size, len(geoids))}/{len(geoids)} block groups...")

    return pd.DataFrame(centroids)


def estimate_centroids_from_geoid(df):
    """
    Estimate approximate centroids based on tract-level data.
    This is a fallback method that's faster but less accurate.

    Args:
        df: DataFrame with GEOID column

    Returns:
        DataFrame with estimated latitude and longitude
    """
    import requests

    api_key = os.environ.get('CENSUS_API_KEY')

    # Get unique tracts
    df['tract_geoid'] = df['GEOID'].str[:11]
    unique_tracts = df['tract_geoid'].unique()

    print(f"Estimating centroids for {len(df)} block groups using {len(unique_tracts)} tract centroids...")

    tract_coords = {}

    for tract_geoid in unique_tracts:
        state = tract_geoid[:2]
        county = tract_geoid[2:5]
        tract = tract_geoid[5:11]

        url = "https://tigerweb.geo.census.gov/arcgis/rest/services/TIGERweb/tigerWMS_ACS2021/MapServer/8/query"

        params = {
            'where': f"STATE='{state}' AND COUNTY='{county}' AND TRACT='{tract}'",
            'outFields': 'CENTLAT,CENTLON',
            'returnGeometry': 'false',
            'f': 'json'
        }

        try:
            response = requests.get(url, params=params, timeout=10)
            if response.status_code == 200:
                result = response.json()
                if 'features' in result and len(result['features']) > 0:
                    attrs = result['features'][0]['attributes']
                    tract_coords[tract_geoid] = {
                        'latitude': float(attrs.get('CENTLAT', 0)),
                        'longitude': float(attrs.get('CENTLON', 0))
                    }
        except:
            pass

    # Assign tract coordinates to all block groups in that tract
    df['latitude'] = df['tract_geoid'].map(lambda x: tract_coords.get(x, {}).get('latitude', None))
    df['longitude'] = df['tract_geoid'].map(lambda x: tract_coords.get(x, {}).get('longitude', None))

    return df


def collect_memphis_area_data(radius_miles=30, year=2021):
    """
    Collect ACS data for all block groups within radius of Memphis, TN.

    Args:
        radius_miles: Radius in miles from Memphis center
        year: ACS year

    Returns:
        DataFrame with ACS data and ICE measures for block groups within radius
    """

    # Memphis, TN coordinates (downtown)
    memphis_lat = 35.1495
    memphis_lon = -90.0490

    print("=" * 70)
    print(f"Collecting Census Data within {radius_miles} miles of Memphis, TN")
    print("=" * 70)
    print(f"\nMemphis center point: {memphis_lat}°N, {memphis_lon}°W")

    # States to check (Memphis is near TN/AR/MS border)
    states = {
        '47': 'TN',  # Tennessee
        '05': 'AR',  # Arkansas
        '28': 'MS',  # Mississippi
    }

    print(f"\nChecking states: {', '.join(states.values())} (Memphis is near state borders)")

    # Initialize collector
    collector = ACSBlockGroupCollector()

    # Collect data for all three states
    all_data = []

    for state_fips, state_abbr in states.items():
        print(f"\n--- Collecting data for {state_abbr} ---")

        # Get ACS data
        state_data = collector.collect_blockgroup_data(
            state=state_fips,
            year=year
        )
        state_data['state_abbr'] = state_abbr

        print(f"✓ Collected {len(state_data)} block groups")

        all_data.append(state_data)

    # Combine all states
    print("\n--- Combining data from all states ---")
    combined_data = pd.concat(all_data, ignore_index=True)
    print(f"✓ Total block groups: {len(combined_data)}")

    # Get coordinates using tract-level approximation (faster method)
    print("\n--- Getting geographic coordinates ---")
    combined_data = estimate_centroids_from_geoid(combined_data)

    # Remove any without coordinates
    before_count = len(combined_data)
    combined_data = combined_data.dropna(subset=['latitude', 'longitude'])
    after_count = len(combined_data)

    if before_count > after_count:
        print(f"Note: Removed {before_count - after_count} block groups without coordinates")

    # Calculate distance from Memphis
    print("\n--- Calculating distances from Memphis ---")
    combined_data['distance_from_memphis_miles'] = combined_data.apply(
        lambda row: haversine_distance(
            memphis_lat, memphis_lon,
            row['latitude'], row['longitude']
        ),
        axis=1
    )

    # Filter to radius
    print(f"\n--- Filtering to {radius_miles}-mile radius ---")
    memphis_area = combined_data[
        combined_data['distance_from_memphis_miles'] <= radius_miles
    ].copy()

    print(f"✓ Found {len(memphis_area)} block groups within {radius_miles} miles")

    # Show breakdown by state
    print("\nBlock groups by state:")
    for state_abbr in states.values():
        count = len(memphis_area[memphis_area['state_abbr'] == state_abbr])
        print(f"  {state_abbr}: {count}")

    # Calculate ICE measures
    print("\n--- Calculating ICE measures ---")
    memphis_area = collector.calculate_ice_measures(memphis_area)
    print("✓ ICE measures calculated")

    # Sort by distance
    memphis_area = memphis_area.sort_values('distance_from_memphis_miles')

    print("\n" + "=" * 70)
    print("SUMMARY")
    print("=" * 70)
    print(f"Total block groups within {radius_miles} miles: {len(memphis_area)}")
    print(f"Distance range: {memphis_area['distance_from_memphis_miles'].min():.1f} - {memphis_area['distance_from_memphis_miles'].max():.1f} miles")
    print(f"Total population: {memphis_area['total_pop_race_eth'].sum():,.0f}")

    return memphis_area


# Main execution
if __name__ == "__main__":
    # Collect data for 30-mile radius
    memphis_data = collect_memphis_area_data(radius_miles=30, year=2021)

    # Display sample
    print("\n--- Sample Data (closest 5 block groups) ---")
    display_cols = ['GEOID', 'state_abbr', 'distance_from_memphis_miles',
                   'total_pop_race_eth', 'ICE_race', 'ICE_income']
    available_cols = [col for col in display_cols if col in memphis_data.columns]
    print(memphis_data[available_cols].head())

    # Display ICE statistics
    print("\n--- ICE Measures Summary ---")
    ice_cols = [col for col in memphis_data.columns if col.startswith('ICE_')]
    print(memphis_data[ice_cols].describe().round(3))

    # Save to CSV
    output_file = 'memphis_30mile_radius_blockgroups_2021.csv'
    memphis_data.to_csv(output_file, index=False)
    print(f"\n✓ Data saved to: {output_file}")
    print(f"✓ Total rows: {len(memphis_data)}")
    print(f"✓ Total columns: {len(memphis_data.columns)}")
