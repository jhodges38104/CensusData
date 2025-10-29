"""
Collect ACS data at the census block group level.

Note: ACS data is NOT available at the census block level. Census blocks are
the smallest geographic unit in the US Census hierarchy, but ACS (American
Community Survey) data is sample-based and not available at this granular level
due to sample size and confidentiality concerns.

This script collects data at the BLOCK GROUP level, which is the smallest
geography typically available for most ACS variables.

Geography Hierarchy (largest to smallest):
- State
- County
- Census Tract
- Block Group (smallest for ACS)
- Block (only available for Decennial Census)
"""

import os
import pandas as pd
import requests
from typing import List, Dict, Optional
import logging

# Set up logging
logging.basicConfig(level=logging.INFO, format='%(asctime)s - %(levelname)s - %(message)s')
logger = logging.getLogger(__name__)


class ACSBlockGroupCollector:
    """Collect ACS data at the block group level."""

    # ACS variables for ICE calculations (matching the R scripts)
    VARIABLES = {
        # Variables for ICE race + income
        'B19001_001E': 'total_household_income',
        'B19001H_014E': 'NHW_100_125k',
        'B19001H_015E': 'NHW_125_150k',
        'B19001H_016E': 'NHW_150_200k',
        'B19001H_017E': 'NHW_200k_plus',
        'B19001B_002E': 'Black_under_10k',
        'B19001B_003E': 'Black_10_15k',
        'B19001B_004E': 'Black_15_20k',

        # Variables for ICE owner vs renter
        'B25003_001E': 'total_tenure',
        'B25003_002E': 'owner_occupied',
        'B25003_003E': 'renter_occupied',
        'B25003H_002E': 'white_owner_occupied',
        'B25003B_003E': 'black_renter_occupied',

        # Variables for ICE income
        'B19001_014E': 'all_100_125k',
        'B19001_015E': 'all_125_150k',
        'B19001_016E': 'all_150_200k',
        'B19001_017E': 'all_200k_plus',
        'B19001_002E': 'all_under_10k',
        'B19001_003E': 'all_10_15k',
        'B19001_004E': 'all_15_20k',

        # Variables for ICE race
        'B03002_001E': 'total_pop_race_eth',
        'B03002_003E': 'white_NH_alone',
        'B03002_004E': 'black_NH_alone',
    }

    BASE_URL = "https://api.census.gov/data"

    def __init__(self, api_key: Optional[str] = None):
        """
        Initialize the collector.

        Args:
            api_key: Census API key. If not provided, will try to read from
                    environment variable CENSUS_API_KEY.
        """
        self.api_key = api_key or os.environ.get('CENSUS_API_KEY')
        if not self.api_key:
            raise ValueError(
                "Census API key required. Set CENSUS_API_KEY environment variable "
                "or pass api_key parameter. Get a key at: "
                "https://api.census.gov/data/key_signup.html"
            )

    def collect_blockgroup_data(
        self,
        state: str,
        county: Optional[str] = None,
        year: int = 2021,
        dataset: str = "acs5"
    ) -> pd.DataFrame:
        """
        Collect ACS data at the block group level.

        Args:
            state: State FIPS code (e.g., "25" for Massachusetts) or abbreviation
            county: County FIPS code (e.g., "025" for Suffolk County).
                   If None, collects for all counties in state.
            year: ACS year (e.g., 2021 for 2017-2021 5-year estimates)
            dataset: ACS dataset ("acs5" for 5-year, "acs1" for 1-year)

        Returns:
            DataFrame with ACS data at block group level
        """
        # Build API URL
        url = f"{self.BASE_URL}/{year}/acs/{dataset}"

        # Get variable list
        variables = ','.join(self.VARIABLES.keys())

        # Build geographic parameters
        # Block groups are specified as: block group:* in tract:* in county:* in state:*
        if county:
            geo_clause = f"block group:*"
            in_clause = f"state:{state} county:{county} tract:*"
        else:
            geo_clause = f"block group:*"
            in_clause = f"state:{state} county:* tract:*"

        # Build request parameters
        params = {
            'get': f"NAME,{variables}",
            'for': geo_clause,
            'in': in_clause,
            'key': self.api_key
        }

        logger.info(f"Requesting ACS {dataset} data for year {year}, state {state}" +
                   (f", county {county}" if county else ""))

        try:
            response = requests.get(url, params=params)
            response.raise_for_status()

            # Parse response
            data = response.json()

            # First row is headers, rest is data
            headers = data[0]
            rows = data[1:]

            # Create DataFrame
            df = pd.DataFrame(rows, columns=headers)

            # Create GEOID (concatenation of state, county, tract, block group)
            df['GEOID'] = (df['state'] + df['county'] + df['tract'] +
                          df['block group'])

            # Rename columns to friendly names
            rename_map = {k: v for k, v in self.VARIABLES.items()
                         if k.replace('E', '') in df.columns or k in df.columns}

            # Handle the 'E' suffix (estimates)
            for census_var, friendly_name in self.VARIABLES.items():
                if census_var in df.columns:
                    df.rename(columns={census_var: friendly_name}, inplace=True)

            # Convert numeric columns to appropriate types
            numeric_cols = [v for v in self.VARIABLES.values() if v in df.columns]
            for col in numeric_cols:
                df[col] = pd.to_numeric(df[col], errors='coerce')

            logger.info(f"Successfully collected data for {len(df)} block groups")

            return df

        except requests.exceptions.RequestException as e:
            logger.error(f"Error fetching data from Census API: {e}")
            raise

    def collect_all_states(
        self,
        year: int = 2021,
        dataset: str = "acs5"
    ) -> pd.DataFrame:
        """
        Collect ACS data at block group level for all US states.

        Args:
            year: ACS year
            dataset: ACS dataset ("acs5" or "acs1")

        Returns:
            DataFrame with data for all states
        """
        # State FIPS codes for all 50 states + DC
        state_fips = {
            '01': 'AL', '02': 'AK', '04': 'AZ', '05': 'AR', '06': 'CA',
            '08': 'CO', '09': 'CT', '10': 'DE', '11': 'DC', '12': 'FL',
            '13': 'GA', '15': 'HI', '16': 'ID', '17': 'IL', '18': 'IN',
            '19': 'IA', '20': 'KS', '21': 'KY', '22': 'LA', '23': 'ME',
            '24': 'MD', '25': 'MA', '26': 'MI', '27': 'MN', '28': 'MS',
            '29': 'MO', '30': 'MT', '31': 'NE', '32': 'NV', '33': 'NH',
            '34': 'NJ', '35': 'NM', '36': 'NY', '37': 'NC', '38': 'ND',
            '39': 'OH', '40': 'OK', '41': 'OR', '42': 'PA', '44': 'RI',
            '45': 'SC', '46': 'SD', '47': 'TN', '48': 'TX', '49': 'UT',
            '50': 'VT', '51': 'VA', '53': 'WA', '54': 'WV', '55': 'WI',
            '56': 'WY'
        }

        all_data = []

        for fips, state_abbr in state_fips.items():
            try:
                logger.info(f"Collecting data for {state_abbr}...")
                df = self.collect_blockgroup_data(
                    state=fips,
                    year=year,
                    dataset=dataset
                )
                df['state_abbr'] = state_abbr
                all_data.append(df)
            except Exception as e:
                logger.error(f"Failed to collect data for {state_abbr}: {e}")
                continue

        # Combine all states
        combined_df = pd.concat(all_data, ignore_index=True)
        logger.info(f"Total block groups collected: {len(combined_df)}")

        return combined_df

    def calculate_ice_measures(self, df: pd.DataFrame) -> pd.DataFrame:
        """
        Calculate ICE (Index of Concentration at the Extremes) measures.

        ICE measures range from -1 to 1:
        - -1: complete concentration of disadvantaged group
        - 0: no concentration
        - +1: complete concentration of privileged group

        Args:
            df: DataFrame with ACS variables

        Returns:
            DataFrame with added ICE measures
        """
        df = df.copy()

        # ICE for race (White NH vs Black NH)
        df['ICE_race'] = (
            (df['white_NH_alone'] - df['black_NH_alone']) /
            df['total_pop_race_eth']
        )

        # ICE for income (high income vs low income)
        df['ICE_income'] = (
            (df['all_100_125k'] + df['all_125_150k'] +
             df['all_150_200k'] + df['all_200k_plus']) -
            (df['all_under_10k'] + df['all_10_15k'] + df['all_15_20k'])
        ) / df['total_household_income']

        # ICE for race + income (White NH high income vs Black low income)
        df['ICE_race_income'] = (
            (df['NHW_100_125k'] + df['NHW_125_150k'] +
             df['NHW_150_200k'] + df['NHW_200k_plus']) -
            (df['Black_under_10k'] + df['Black_10_15k'] + df['Black_15_20k'])
        ) / df['total_household_income']

        # ICE for tenure (owner vs renter)
        df['ICE_owner_renter'] = (
            (df['owner_occupied'] - df['renter_occupied']) /
            df['total_tenure']
        )

        # ICE for race + tenure (White owner vs Black renter)
        df['ICE_race_tenure'] = (
            (df['white_owner_occupied'] - df['black_renter_occupied']) /
            df['total_tenure']
        )

        return df


def main():
    """Example usage of the ACS Block Group Collector."""

    # Initialize collector
    collector = ACSBlockGroupCollector()

    # Example 1: Collect data for Massachusetts (state FIPS: 25)
    logger.info("Example 1: Collecting data for Massachusetts...")
    ma_data = collector.collect_blockgroup_data(state='25', year=2021)

    # Calculate ICE measures
    ma_data = collector.calculate_ice_measures(ma_data)

    # Save to CSV
    output_file = 'acs_blockgroup_MA_2021.csv'
    ma_data.to_csv(output_file, index=False)
    logger.info(f"Saved Massachusetts data to {output_file}")

    # Display summary statistics
    print("\n=== ICE Measures Summary Statistics ===")
    ice_columns = [col for col in ma_data.columns if col.startswith('ICE_')]
    print(ma_data[ice_columns].describe())

    # Example 2: Collect data for a specific county (Suffolk County, MA: FIPS 025)
    logger.info("\nExample 2: Collecting data for Suffolk County, MA...")
    suffolk_data = collector.collect_blockgroup_data(
        state='25',
        county='025',
        year=2021
    )
    suffolk_data = collector.calculate_ice_measures(suffolk_data)
    suffolk_data.to_csv('acs_blockgroup_Suffolk_MA_2021.csv', index=False)

    # Example 3: Collect data for all US states (commented out - takes a while!)
    # logger.info("\nExample 3: Collecting data for all US states...")
    # us_data = collector.collect_all_states(year=2021)
    # us_data = collector.calculate_ice_measures(us_data)
    # us_data.to_csv('acs_blockgroup_US_2021.csv', index=False)


if __name__ == "__main__":
    main()
