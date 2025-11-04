################################################################################
# Script: pull_cdc_places_census_tract_2024_batch.R
# Purpose: Pull Census Tract level CDC PLACES data for 2024 (batch version)
# Usage: Rscript pull_cdc_places_census_tract_2024_batch.R MA
#        or Rscript pull_cdc_places_census_tract_2024_batch.R MA CA NY
# Date: 2024-11-04
# Project: DNA methylation & adversity
################################################################################

# Load required libraries
suppressPackageStartupMessages({
  library(httr)
  library(jsonlite)
  library(dplyr)
  library(tidyr)
})

# Function to validate state input
validate_state <- function(state_input) {
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

# Function to download data for a single state
download_state_data <- function(state_abbr) {
  cat(sprintf("\n=== Processing %s ===\n", state_abbr))

  # CDC PLACES API endpoint for census tract level data (2024 release)
  base_url <- "https://data.cdc.gov/resource/cwsq-ngmh.json"

  # Initialize variables for pagination
  offset <- 0
  limit <- 50000
  all_data <- list()
  record_count <- 0

  # Fetch data with pagination
  repeat {
    cat(sprintf("  Fetching records %d to %d...\n", offset + 1, offset + limit))

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
      cat(sprintf("  Error: %s\n", e$message))
      return(NULL)
    })

    # Check if request was successful
    if (is.null(response) || http_error(response)) {
      if (!is.null(response)) {
        cat(sprintf("  HTTP Error: %d\n", status_code(response)))
      }
      return(NULL)
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

    cat(sprintf("  Retrieved %d records (total: %d)\n",
                nrow(batch_data), record_count))

    # If we got fewer records than the limit, we're done
    if (nrow(batch_data) < limit) {
      break
    }

    # Move to next batch
    offset <- offset + limit
    Sys.sleep(1)
  }

  # Check if we got any data
  if (length(all_data) == 0) {
    cat(sprintf("  No data found for %s\n", state_abbr))
    return(NULL)
  }

  # Combine all batches
  places_data <- bind_rows(all_data)

  # Create output filename
  output_filename <- sprintf("cdc_places_census_tract_%s_2024.csv", state_abbr)

  # Save data
  cat(sprintf("  Saving to: %s\n", output_filename))
  write.csv(places_data, output_filename, row.names = FALSE)

  cat(sprintf("  Complete! Total records: %s\n",
              format(nrow(places_data), big.mark = ",")))

  return(nrow(places_data))
}

# Main execution
cat("\n=================================================================\n")
cat("  CDC PLACES Census Tract Data Download - Batch Mode\n")
cat("=================================================================\n")

# Get command line arguments
args <- commandArgs(trailingOnly = TRUE)

# Check if states were provided
if (length(args) == 0) {
  cat("\nUsage: Rscript pull_cdc_places_census_tract_2024_batch.R STATE1 [STATE2 ...]\n")
  cat("Example: Rscript pull_cdc_places_census_tract_2024_batch.R MA\n")
  cat("Example: Rscript pull_cdc_places_census_tract_2024_batch.R MA CA NY\n\n")
  cat("Valid state abbreviations: AL, AK, AZ, AR, CA, CO, CT, DE, FL, GA,\n")
  cat("  HI, ID, IL, IN, IA, KS, KY, LA, ME, MD, MA, MI, MN, MS, MO, MT,\n")
  cat("  NE, NV, NH, NJ, NM, NY, NC, ND, OH, OK, OR, PA, RI, SC, SD, TN,\n")
  cat("  TX, UT, VT, VA, WA, WV, WI, WY, DC, PR\n")
  quit(status = 1)
}

# Validate all state inputs
states_to_process <- c()
for (state_input in args) {
  state_abbr <- validate_state(state_input)
  if (is.null(state_abbr)) {
    cat(sprintf("\nWarning: Invalid state abbreviation '%s' - skipping\n", state_input))
  } else {
    states_to_process <- c(states_to_process, state_abbr)
  }
}

# Check if we have any valid states
if (length(states_to_process) == 0) {
  cat("\nError: No valid state abbreviations provided\n")
  quit(status = 1)
}

# Display states to process
cat(sprintf("\nStates to process: %s\n", paste(states_to_process, collapse = ", ")))
cat(sprintf("Total states: %d\n", length(states_to_process)))

# Process each state
results <- list()
start_time <- Sys.time()

for (state in states_to_process) {
  result <- tryCatch({
    download_state_data(state)
  }, error = function(e) {
    cat(sprintf("  Error processing %s: %s\n", state, e$message))
    return(NULL)
  })

  results[[state]] <- result
}

end_time <- Sys.time()
elapsed_time <- difftime(end_time, start_time, units = "mins")

# Print summary
cat("\n=================================================================\n")
cat("  Summary\n")
cat("=================================================================\n")
cat(sprintf("Total states processed: %d\n", length(states_to_process)))
cat(sprintf("Elapsed time: %.1f minutes\n", as.numeric(elapsed_time)))
cat("\nResults:\n")

for (state in names(results)) {
  if (is.null(results[[state]])) {
    cat(sprintf("  %s: FAILED\n", state))
  } else {
    cat(sprintf("  %s: %s records\n", state, format(results[[state]], big.mark = ",")))
  }
}

cat("\n=================================================================\n")
cat("  Batch download complete!\n")
cat("=================================================================\n\n")
