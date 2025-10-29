"""
Test if the Census API key works and check variable availability.
"""

import requests
import os

os.environ['CENSUS_API_KEY'] = '4d5e7ded000067ff443e2f90683ce53bcf660392'
api_key = os.environ['CENSUS_API_KEY']

print("=" * 70)
print("Testing Census API Key")
print("=" * 70)

# Test 1: Simple request for census tract (should definitely work)
print("\n1. Testing API key with census tract level data...")
url = "https://api.census.gov/data/2021/acs/acs5"
params = {
    'get': 'NAME,B03002_001E,B03002_003E,B03002_004E',
    'for': 'tract:*',
    'in': 'state:25 county:025',
    'key': api_key
}

response = requests.get(url, params=params)
print(f"   Status Code: {response.status_code}")
if response.status_code == 200:
    print("   ✓ API key works for census tract data!")
    data = response.json()
    print(f"   ✓ Received data for {len(data)-1} census tracts")
else:
    print(f"   ✗ Error: {response.text}")

# Test 2: Try block group with basic variables
print("\n2. Testing block group level with basic variables...")
params = {
    'get': 'NAME,B03002_001E,B03002_003E,B03002_004E',
    'for': 'block group:*',
    'in': 'state:25 county:025 tract:*',
    'key': api_key
}

response = requests.get(url, params=params)
print(f"   Status Code: {response.status_code}")
if response.status_code == 200:
    print("   ✓ Block group data works for basic race variables!")
    data = response.json()
    print(f"   ✓ Received data for {len(data)-1} block groups")
else:
    print(f"   ✗ Error: {response.text}")

# Test 3: Check if race by income variables are available at block group level
print("\n3. Testing race by income variables at block group level...")
test_vars = {
    'B19001H_014E': 'NH White 100-125k income',
    'B19001B_002E': 'Black under 10k income',
    'B25003H_002E': 'White owner occupied',
    'B25003B_003E': 'Black renter occupied'
}

for var, description in test_vars.items():
    params = {
        'get': f'NAME,{var}',
        'for': 'block group:1',
        'in': 'state:25 county:025 tract:010100',
        'key': api_key
    }
    response = requests.get(url, params=params)
    if response.status_code == 200:
        print(f"   ✓ {var} ({description}) - AVAILABLE")
    else:
        print(f"   ✗ {var} ({description}) - NOT AVAILABLE")

print("\n" + "=" * 70)
