"""
Example usage of the ACS Block Group Data Collector.

This script demonstrates how to collect ACS data at the block group level
and calculate ICE measures.

Usage:
    1. Set your Census API key as an environment variable:
       export CENSUS_API_KEY="4d5e7ded000067ff443e2f90683ce53bcf660392"

    2. Run the script:
       python example_usage.py
"""

from collect_acs_blockgroup_data import ACSBlockGroupCollector
import pandas as pd

def main():
    print("=" * 70)
    print("ACS Block Group Data Collection - Example Usage")
    print("=" * 70)

    # Initialize the collector
    # API key will be read from CENSUS_API_KEY environment variable
    print("\nInitializing collector...")
    collector = ACSBlockGroupCollector()
    print("✓ Collector initialized")

    # EXAMPLE 1: Collect data for Suffolk County, MA (Boston area)
    # This is a manageable size for testing
    print("\n" + "=" * 70)
    print("EXAMPLE 1: Suffolk County, MA (Boston area)")
    print("=" * 70)

    print("\nCollecting data...")
    suffolk_data = collector.collect_blockgroup_data(
        state='25',      # Massachusetts
        county='025',    # Suffolk County
        year=2021        # 2017-2021 5-year estimates
    )
    print(f"✓ Collected {len(suffolk_data)} block groups")

    print("\nCalculating ICE measures...")
    suffolk_data = collector.calculate_ice_measures(suffolk_data)
    print("✓ ICE measures calculated")

    print("\nSaving to CSV...")
    suffolk_data.to_csv('example_suffolk_county_2021.csv', index=False)
    print("✓ Saved to: example_suffolk_county_2021.csv")

    # Display summary statistics
    print("\n--- Summary Statistics ---")
    ice_cols = [col for col in suffolk_data.columns if col.startswith('ICE_')]
    print(suffolk_data[ice_cols].describe().round(3))

    # Display sample records
    print("\n--- Sample Data (first 3 block groups) ---")
    display_cols = ['GEOID', 'NAME', 'total_pop_race_eth',
                   'ICE_race', 'ICE_income', 'ICE_race_income']
    print(suffolk_data[display_cols].head(3).to_string(index=False))

    # EXAMPLE 2: Collect data for entire state (commented out - uncomment to use)
    print("\n" + "=" * 70)
    print("EXAMPLE 2: Entire State (Commented Out)")
    print("=" * 70)
    print("\nTo collect all block groups in Massachusetts, uncomment this code:")
    print("""
    ma_data = collector.collect_blockgroup_data(state='25', year=2021)
    ma_data = collector.calculate_ice_measures(ma_data)
    ma_data.to_csv('example_massachusetts_all_2021.csv', index=False)
    print(f"✓ Collected {len(ma_data)} block groups for Massachusetts")
    """)

    # EXAMPLE 3: Multiple counties
    print("\n" + "=" * 70)
    print("EXAMPLE 3: Multiple Counties (Commented Out)")
    print("=" * 70)
    print("\nTo collect data for multiple counties:")
    print("""
    counties = {
        '025': 'Suffolk',
        '021': 'Norfolk',
        '017': 'Middlesex'
    }

    all_data = []
    for county_fips, county_name in counties.items():
        print(f"Collecting {county_name} County...")
        data = collector.collect_blockgroup_data(
            state='25',
            county=county_fips,
            year=2021
        )
        data['county_name'] = county_name
        all_data.append(data)

    combined = pd.concat(all_data, ignore_index=True)
    combined = collector.calculate_ice_measures(combined)
    combined.to_csv('example_multiple_counties_2021.csv', index=False)
    """)

    # Tips
    print("\n" + "=" * 70)
    print("TIPS")
    print("=" * 70)
    print("""
1. Start small: Test with a single county before collecting entire states

2. State and County FIPS codes:
   - Find at: https://www.census.gov/library/reference/code-lists/ansi.html
   - Massachusetts = 25
   - Suffolk County = 025
   - California = 06
   - New York = 36

3. Data years:
   - Use year=2021 for most recent (2017-2021 estimates)
   - Use year=2020 for 2016-2020 estimates
   - Etc.

4. ICE measures range from -1 to +1:
   - -1 = complete concentration of disadvantaged group
   -  0 = no concentration (equality)
   - +1 = complete concentration of privileged group

5. For all US states (WARNING: takes hours, very large file):
   us_data = collector.collect_all_states(year=2021)
    """)

    print("\n" + "=" * 70)
    print("✓ Example completed successfully!")
    print("=" * 70)


if __name__ == "__main__":
    try:
        main()
    except ValueError as e:
        print(f"\n✗ Error: {e}")
        print("\nMake sure to set your Census API key:")
        print('export CENSUS_API_KEY="4d5e7ded000067ff443e2f90683ce53bcf660392"')
    except Exception as e:
        print(f"\n✗ Unexpected error: {e}")
        print("\nPlease check:")
        print("1. Your Census API key is valid and activated")
        print("2. You have internet connection")
        print("3. The Census API is not experiencing downtime")
