"""
Test script to verify ACS data collection is working correctly.
"""

import os
from collect_acs_blockgroup_data import ACSBlockGroupCollector

# Set API key
os.environ['CENSUS_API_KEY'] = '4d5e7ded000067ff443e2f90683ce53bcf660392'

print("=" * 70)
print("Testing ACS Block Group Data Collection")
print("=" * 70)

# Initialize collector
print("\n1. Initializing collector...")
collector = ACSBlockGroupCollector()
print("   ✓ Collector initialized successfully")

# Test 1: Collect data for Suffolk County, MA (FIPS: 25-025)
print("\n2. Testing data collection for Suffolk County, MA...")
print("   (This includes Boston and surrounding areas)")
suffolk_data = collector.collect_blockgroup_data(
    state='25',
    county='025',
    year=2021
)
print(f"   ✓ Successfully collected data for {len(suffolk_data)} block groups")

# Test 2: Calculate ICE measures
print("\n3. Calculating ICE measures...")
suffolk_data = collector.calculate_ice_measures(suffolk_data)
print("   ✓ ICE measures calculated successfully")

# Display basic info
print("\n" + "=" * 70)
print("DATA SUMMARY")
print("=" * 70)
print(f"\nTotal block groups: {len(suffolk_data)}")
print(f"\nColumns in dataset: {len(suffolk_data.columns)}")
print(f"\nSample GEOIDs (first 5):")
for geoid in suffolk_data['GEOID'].head(5):
    print(f"  - {geoid}")

# Display ICE statistics
print("\n" + "=" * 70)
print("ICE MEASURES - SUMMARY STATISTICS")
print("=" * 70)
ice_columns = [col for col in suffolk_data.columns if col.startswith('ICE_')]
print(f"\nICE measures calculated: {', '.join(ice_columns)}")
print("\nStatistics (ICE measures range from -1 to +1):")
print(suffolk_data[ice_columns].describe().round(3))

# Save the data
output_file = 'test_suffolk_blockgroups_2021.csv'
suffolk_data.to_csv(output_file, index=False)
print(f"\n✓ Data saved to: {output_file}")

# Display sample of the data
print("\n" + "=" * 70)
print("SAMPLE DATA (first 3 block groups)")
print("=" * 70)
# Display key columns
display_cols = ['GEOID', 'NAME', 'total_pop_race_eth', 'white_NH_alone',
                'black_NH_alone', 'ICE_race', 'ICE_income', 'ICE_race_income']
available_cols = [col for col in display_cols if col in suffolk_data.columns]
print(suffolk_data[available_cols].head(3).to_string(index=False))

print("\n" + "=" * 70)
print("TEST COMPLETED SUCCESSFULLY!")
print("=" * 70)
print("\nYou can now use the collector to gather data for:")
print("  - Other counties: collector.collect_blockgroup_data(state='XX', county='YYY')")
print("  - Entire states: collector.collect_blockgroup_data(state='XX')")
print("  - All US states: collector.collect_all_states()")
