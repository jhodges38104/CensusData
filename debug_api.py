"""
Debug Census API access.
"""

import requests

api_key = '4d5e7ded000067ff443e2f90683ce53bcf660392'

print("=" * 70)
print("Debugging Census API Access")
print("=" * 70)

# Test 1: Try without API key (some endpoints allow limited access)
print("\n1. Testing WITHOUT API key (public access)...")
url = "https://api.census.gov/data/2021/acs/acs5"
params = {
    'get': 'NAME,B03002_001E',
    'for': 'state:25'
}

response = requests.get(url, params=params)
print(f"   Status Code: {response.status_code}")
print(f"   Response: {response.text[:200]}")

# Test 2: Try WITH API key
print("\n2. Testing WITH provided API key...")
params['key'] = api_key
response = requests.get(url, params=params)
print(f"   Status Code: {response.status_code}")
print(f"   Response: {response.text[:200]}")

# Test 3: Check if the API endpoint is even accessible
print("\n3. Testing basic Census API endpoint connectivity...")
test_url = "https://api.census.gov/data.json"
response = requests.get(test_url)
print(f"   Status Code: {response.status_code}")
if response.status_code == 200:
    print("   ✓ Census API is accessible")
else:
    print(f"   ✗ Cannot reach Census API: {response.text[:200]}")

# Test 4: Try 2020 ACS instead of 2021
print("\n4. Testing with 2020 ACS data...")
url = "https://api.census.gov/data/2020/acs/acs5"
params = {
    'get': 'NAME,B03002_001E',
    'for': 'state:25',
    'key': api_key
}
response = requests.get(url, params=params)
print(f"   Status Code: {response.status_code}")
print(f"   Response: {response.text[:200]}")

print("\n" + "=" * 70)
print("Note: If you just registered for the API key, it may take")
print("a few minutes to activate. Please try again in 5-10 minutes.")
print("=" * 70)
