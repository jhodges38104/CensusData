"""
Merge Area Deprivation Index (ADI) data with Memphis radius census data.

This script shows two approaches:
1. Merge official ADI data from UW Neighborhood Atlas
2. Calculate a custom deprivation index from Census variables

Usage:
    Option 1: Download ADI files from https://www.neighborhoodatlas.medicine.wisc.edu/
    Option 2: Use custom calculation (no download needed)
"""

import pandas as pd
import numpy as np
from scipy.stats import rankdata
from sklearn.preprocessing import StandardScaler


def load_adi_files(file_paths):
    """
    Load official ADI data files from Neighborhood Atlas.

    Args:
        file_paths: List of paths to ADI .txt files

    Returns:
        DataFrame with combined ADI data
    """
    adi_data = []

    for file_path in file_paths:
        df = pd.read_csv(file_path, sep='\t', dtype={'FIPS': str})
        adi_data.append(df)

    # Combine all states
    adi_combined = pd.concat(adi_data, ignore_index=True)

    # Rename FIPS to GEOID for consistency
    adi_combined.rename(columns={'FIPS': 'GEOID'}, inplace=True)

    print(f"✓ Loaded ADI data for {len(adi_combined)} block groups")

    return adi_combined


def merge_adi_with_census(census_data, adi_data):
    """
    Merge ADI data with census block group data.

    Args:
        census_data: DataFrame with census data (must have GEOID column)
        adi_data: DataFrame with ADI data (must have GEOID column)

    Returns:
        Merged DataFrame
    """
    # Ensure GEOID is string
    census_data['GEOID'] = census_data['GEOID'].astype(str)
    adi_data['GEOID'] = adi_data['GEOID'].astype(str)

    # Merge
    merged = census_data.merge(
        adi_data[['GEOID', 'ADI_NATRANK', 'ADI_STATERNK']],
        on='GEOID',
        how='left'
    )

    print(f"\n✓ Merged data:")
    print(f"  Total block groups: {len(merged)}")
    print(f"  With ADI data: {merged['ADI_NATRANK'].notna().sum()}")
    print(f"  Missing ADI: {merged['ADI_NATRANK'].isna().sum()}")

    return merged


def calculate_custom_deprivation_index(census_data):
    """
    Calculate a custom deprivation index from census variables.

    This creates a composite index similar to ADI but using only the
    variables we collected from the Census API.

    Args:
        census_data: DataFrame with census variables

    Returns:
        DataFrame with added deprivation index columns
    """
    data = census_data.copy()

    print("\nCalculating custom deprivation index...")

    # Calculate component indicators
    # 1. Reverse ICE measures (so higher = more deprived)
    data['ice_race_reversed'] = -1 * data['ICE_race']
    data['ice_income_reversed'] = -1 * data['ICE_income']
    data['ice_raceinc_reversed'] = -1 * data['ICE_race_income']
    data['ice_tenure_reversed'] = -1 * data['ICE_owner_renter']

    # 2. Calculate percentage indicators
    # Low income percentage
    data['pct_low_income'] = (
        (data['all_under_10k'] + data['all_10_15k'] + data['all_15_20k']) /
        data['total_household_income']
    )

    # High income percentage (reversed - lower is more deprived)
    data['pct_high_income'] = (
        (data['all_100_125k'] + data['all_125_150k'] +
         data['all_150_200k'] + data['all_200k_plus']) /
        data['total_household_income']
    )
    data['pct_high_income_reversed'] = 1 - data['pct_high_income']

    # Renter percentage
    data['pct_renter'] = data['renter_occupied'] / data['total_tenure']

    # 3. Select indicators for composite
    deprivation_indicators = [
        'ice_race_reversed',
        'ice_income_reversed',
        'ice_raceinc_reversed',
        'ice_tenure_reversed',
        'pct_low_income',
        'pct_high_income_reversed',
        'pct_renter'
    ]

    # 4. Handle missing values
    scoring_data = data[deprivation_indicators].copy()
    scoring_data = scoring_data.fillna(scoring_data.mean())

    # 5. Standardize all indicators (z-scores)
    scaler = StandardScaler()
    scaled_data = scaler.fit_transform(scoring_data)

    # 6. Create composite score (average of standardized indicators)
    data['custom_deprivation_score'] = scaled_data.mean(axis=1)

    # 7. Convert to percentile rank (0-100, like ADI)
    data['custom_deprivation_percentile'] = (
        rankdata(data['custom_deprivation_score']) / len(data) * 100
    )

    # 8. Create decile groups (1-10, like ADI state rank)
    data['custom_deprivation_decile'] = pd.qcut(
        data['custom_deprivation_percentile'],
        q=10,
        labels=range(1, 11),
        duplicates='drop'
    )

    print("✓ Custom deprivation index calculated")
    print(f"\n  Components used: {len(deprivation_indicators)}")
    print(f"  Mean percentile: {data['custom_deprivation_percentile'].mean():.1f}")
    print(f"  Std percentile: {data['custom_deprivation_percentile'].std():.1f}")

    return data


def compare_adi_ice(data):
    """
    Compare ADI with ICE measures.

    Args:
        data: DataFrame with both ADI and ICE measures

    Returns:
        DataFrame with correlations
    """
    print("\n" + "=" * 70)
    print("CORRELATION: ADI vs ICE Measures")
    print("=" * 70)

    ice_cols = [col for col in data.columns if col.startswith('ICE_')]

    correlations = {}
    for ice_col in ice_cols:
        corr = data[['ADI_NATRANK', ice_col]].corr().iloc[0, 1]
        correlations[ice_col] = corr
        print(f"{ice_col:25} r = {corr:.3f}")

    print("\nNote: ADI increases with deprivation (higher = more deprived)")
    print("      ICE increases with privilege (higher = more privileged)")
    print("      Therefore, negative correlations are expected")

    return pd.DataFrame(correlations.items(), columns=['ICE_Measure', 'Correlation'])


def summarize_deprivation_data(data, deprivation_col='ADI_NATRANK'):
    """
    Summarize deprivation data by state and distance.

    Args:
        data: DataFrame with deprivation and geographic data
        deprivation_col: Name of deprivation column to summarize

    Returns:
        Summary statistics
    """
    print("\n" + "=" * 70)
    print(f"SUMMARY: {deprivation_col}")
    print("=" * 70)

    # Overall statistics
    print(f"\nOverall:")
    print(data[deprivation_col].describe())

    # By state
    if 'state_abbr' in data.columns:
        print(f"\nBy State:")
        for state in data['state_abbr'].unique():
            state_data = data[data['state_abbr'] == state]
            print(f"\n  {state}:")
            print(f"    Count: {len(state_data)}")
            print(f"    Mean: {state_data[deprivation_col].mean():.2f}")
            print(f"    Median: {state_data[deprivation_col].median():.2f}")
            print(f"    Min: {state_data[deprivation_col].min():.2f}")
            print(f"    Max: {state_data[deprivation_col].max():.2f}")

    # Most and least deprived
    print(f"\n--- Most Deprived (Highest {deprivation_col}) ---")
    cols = ['GEOID', 'state_abbr', deprivation_col]
    if 'distance_from_memphis_miles' in data.columns:
        cols.insert(2, 'distance_from_memphis_miles')
    available_cols = [c for c in cols if c in data.columns]
    print(data.nlargest(5, deprivation_col)[available_cols].to_string(index=False))

    print(f"\n--- Least Deprived (Lowest {deprivation_col}) ---")
    print(data.nsmallest(5, deprivation_col)[available_cols].to_string(index=False))


# Example usage for Google Colab or standalone script
if __name__ == "__main__":

    print("=" * 70)
    print("MEMPHIS AREA - DEPRIVATION INDEX ANALYSIS")
    print("=" * 70)

    # OPTION 1: With official ADI data
    print("\n--- OPTION 1: Official ADI Data ---")
    print("To use this option:")
    print("1. Download ADI files from https://www.neighborhoodatlas.medicine.wisc.edu/")
    print("2. Load them and merge with your census data")
    print("\nExample code:")
    print("""
    # Load census data
    census_data = pd.read_csv('memphis_30mile_radius_2021.csv')

    # Load ADI files
    adi_files = [
        'US_2021_ADI_Census Block Group_v4.0_TN.txt',
        'US_2021_ADI_Census Block Group_v4.0_AR.txt',
        'US_2021_ADI_Census Block Group_v4.0_MS.txt'
    ]
    adi_data = load_adi_files(adi_files)

    # Merge
    merged_data = merge_adi_with_census(census_data, adi_data)

    # Analyze
    summarize_deprivation_data(merged_data, 'ADI_NATRANK')
    compare_adi_ice(merged_data)

    # Save
    merged_data.to_csv('memphis_with_adi.csv', index=False)
    """)

    # OPTION 2: Custom deprivation index
    print("\n\n--- OPTION 2: Custom Deprivation Index ---")
    print("Calculate from census variables without needing ADI download")
    print("\nExample code:")
    print("""
    # Load census data
    census_data = pd.read_csv('memphis_30mile_radius_2021.csv')

    # Calculate custom index
    data_with_custom = calculate_custom_deprivation_index(census_data)

    # Analyze
    summarize_deprivation_data(data_with_custom, 'custom_deprivation_percentile')

    # Save
    data_with_custom.to_csv('memphis_with_custom_deprivation.csv', index=False)
    """)

    print("\n" + "=" * 70)
    print("See MEMPHIS_ADI_GUIDE.md for full Google Colab implementation")
    print("=" * 70)
