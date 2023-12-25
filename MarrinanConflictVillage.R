# This code imports conflict data and calculates the number of conflicts within 20km of each village

# Install and load required packages
library(geosphere)

# Reads the villages dataset
villages <- read.csv("/Users/liam/Downloads/villages.csv")

# Reads the conflicts dataset
conflicts <- read.csv("/Users/liam/Downloads/conflicts.csv")

# Convert 'month' to numeric in conflicts dataset
conflicts$month <- as.numeric(conflicts$month)

# Creates a sequence of monthly dates from 2007-01 to 2013-12
monthly_dates <- seq(as.Date("2007-01-01"), as.Date("2013-12-31"), by = "month")

# Creates all combinations of village codes and monthly dates
villages_expanded <- expand.grid(village_code = villages$village_code, date = monthly_dates)

# Merges the expanded villages data with the original villages data
villages_expanded <- merge(villages_expanded, villages, by = "village_code", all.x = TRUE)

# Create an empty column 'conflict_count' in the 'villages_expanded' dataset to store the conflict count
villages_expanded$conflict_count <- 0

# Loop through each row in the 'villages_expanded' dataset
for (i in 1:nrow(villages_expanded)) {
  # Extract latitude and longitude of the current village
  village_lat <- villages_expanded$latitude[i]
  village_lon <- villages_expanded$longitude[i]

  # Subset conflicts for the current village and date
  subset_conflicts <- conflicts[conflicts$month == as.numeric(format(villages_expanded$date[i], "%m")) & conflicts$year == as.numeric(format(villages_expanded$date[i], "%Y")), ]
  
  # Loops through each row
  for (j in 1:nrow(subset_conflicts)) {
    # Extract latitude and longitude of the current conflict event
    conflict_lat <- subset_conflicts$latitude[j]
    conflict_lon <- subset_conflicts$longitude[j]
    
    # Checks if the distance is within 20km
    distance <- distVincentySphere(c(village_lon, village_lat), c(conflict_lon, conflict_lat))
    
    if (distance <= 20000) {
      # Increment the 'conflict_count' for the village
      villages_expanded$conflict_count[i] <- villages_expanded$conflict_count[i] + 1
    }
  }
}

# Saves the updated dataset as a CSV file
write.csv(villages_expanded, file = "villages_expanded_with_conflict_count.csv", row.names = FALSE)


