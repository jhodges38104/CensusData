################################################################################
# Script: pull_cdc_places_census_tract_2024.R
# Purpose: Pull Census Tract level CDC PLACES data for 2024 with state prompts
# Date: 2024-11-04
# Project: DNA methylation & adversity
################################################################################

# Load required libraries
library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)

# Function to validate state input
validate_state <- function(state_input) {
  # List of valid state abbreviations
  valid_states <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA",
                    "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD",
                    "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ",
                    "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC",
                    "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY",
                    "DC", "PR")

  state_upper <- toupper(trimws(state_input))

  if (state_upper %in% valid_states) {
    return(state_upper)
  } else {
    return(NULL)
  }
}

# Function to get state name from abbreviation
get_state_name <- function(state_abbr) {
  state_mapping <- data.frame(
    abbr = c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA",
             "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD",
             "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ",
             "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC",
             "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY",
             "DC", "PR"),
    name = c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado",
             "Connecticut", "Delaware", "Florida", "Georgia", "Hawaii", "Idaho",
             "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana",
             "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota",
             "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada",
             "New Hampshire", "New Jersey", "New Mexico", "New York",
             "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon",
             "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota",
             "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington",
             "West Virginia", "Wisconsin", "Wyoming", "District of Columbia",
             "Puerto Rico"),
    stringsAsFactors = FALSE
  )

  result <- state_mapping$name[state_mapping$abbr == state_abbr]
  return(result)
}

# Prompt for state
cat("\n=================================================================\n")
cat("  CDC PLACES Census Tract Data Download - 2024 Release\n")
cat("=================================================================\n\n")
cat("This script downloads census tract-level health indicators from\n")
cat("the CDC PLACES dataset for a specified state.\n\n")
cat("Please enter a state abbreviation (e.g., MA, CA, NY):\n")

# Get state input
state_input <- readline(prompt = "State: ")
state_abbr <- validate_state(state_input)

# Validate state input
while (is.null(state_abbr)) {
  cat("\nInvalid state abbreviation. Please try again.\n")
  state_input <- readline(prompt = "State: ")
  state_abbr <- validate_state(state_input)
}

state_name <- get_state_name(state_abbr)
cat(sprintf("\nState selected: %s (%s)\n", state_name, state_abbr))

# CDC PLACES API endpoint for census tract level data (2024 release)
# Note: The dataset ID 'cwsq-ngmh' is for the 2024 census tract level data
# This may need to be updated based on CDC's latest release
base_url <- "https://data.cdc.gov/resource/cwsq-ngmh.json"

cat("\n-----------------------------------------------------------------\n")
cat("Fetching data from CDC PLACES API...\n")
cat("-----------------------------------------------------------------\n")

# Initialize variables for pagination
offset <- 0
limit <- 50000  # Maximum allowed by Socrata API
all_data <- list()
record_count <- 0

# Fetch data with pagination
repeat {
  cat(sprintf("Fetching records %d to %d...\n", offset + 1, offset + limit))

  # Build query with filters
  query_params <- list(
    `$where` = sprintf("stateabbr='%s'", state_abbr),
    `$limit` = limit,
    `$offset` = offset,
    `$order` = ":id"
  )

  # Make API request
  response <- tryCatch({
    GET(base_url, query = query_params, timeout(120))
  }, error = function(e) {
    cat(sprintf("\nError fetching data: %s\n", e$message))
    return(NULL)
  })

  # Check if request was successful
  if (is.null(response) || http_error(response)) {
    if (!is.null(response)) {
      cat(sprintf("\nHTTP Error: %d\n", status_code(response)))
      cat("Response content:\n")
      print(content(response, "text"))
    }
    cat("\nNote: If you receive a 404 error, the dataset ID may need to be updated.\n")
    cat("Please check the CDC PLACES website for the latest dataset identifier:\n")
    cat("https://data.cdc.gov/browse?category=500+Cities+%26+Places\n")
    stop("Failed to fetch data from CDC PLACES API")
  }

  # Parse JSON response
  batch_data <- fromJSON(content(response, "text", encoding = "UTF-8"),
                         flatten = TRUE)

  # Check if we got any data
  if (length(batch_data) == 0) {
    break
  }

  # Add to our collection
  all_data[[length(all_data) + 1]] <- batch_data
  record_count <- record_count + nrow(batch_data)

  cat(sprintf("Retrieved %d records (total so far: %d)\n",
              nrow(batch_data), record_count))

  # If we got fewer records than the limit, we're done
  if (nrow(batch_data) < limit) {
    break
  }

  # Move to next batch
  offset <- offset + limit

  # Small delay to be respectful of API rate limits
  Sys.sleep(1)
}

# Check if we got any data
if (length(all_data) == 0) {
  stop(sprintf("No data found for state: %s", state_abbr))
}

# Combine all batches
cat("\nCombining all data batches...\n")
places_data <- bind_rows(all_data)

cat(sprintf("\nTotal records retrieved: %s\n", format(nrow(places_data), big.mark = ",")))
cat(sprintf("Total census tracts: %s\n",
            format(length(unique(places_data$locationname)), big.mark = ",")))

# Display available measures
if ("measureid" %in% names(places_data)) {
  cat("\nAvailable health measures:\n")
  measures <- places_data %>%
    select(measureid, measure) %>%
    distinct() %>%
    arrange(measureid)
  print(measures, n = nrow(measures))
}

# Create output filename
output_filename <- sprintf("cdc_places_census_tract_%s_2024.csv", state_abbr)

# Save data
cat(sprintf("\nSaving data to: %s\n", output_filename))
write.csv(places_data, output_filename, row.names = FALSE)

# Print summary statistics
cat("\n=================================================================\n")
cat("  Data Summary\n")
cat("=================================================================\n")
cat(sprintf("State: %s (%s)\n", state_name, state_abbr))
cat(sprintf("Total records: %s\n", format(nrow(places_data), big.mark = ",")))
cat(sprintf("Output file: %s\n", output_filename))
cat(sprintf("File size: %.2f MB\n", file.size(output_filename) / 1024^2))

# Display column names
cat("\nColumn names in dataset:\n")
cat(paste(names(places_data), collapse = ", "))
cat("\n")

# Display first few rows
cat("\nFirst 5 rows of data:\n")
print(head(places_data, 5))

cat("\n=================================================================\n")
cat("  Download Complete!\n")
cat("=================================================================\n\n")
