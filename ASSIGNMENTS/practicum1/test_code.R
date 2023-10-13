
# remove military flights
# harmonize flight phases 

birdstrikes <- readr::read_csv("BirdStrikesData-V2.csv", 
                               col_types = readr::cols())

relevant_columns <- c("rid", "flight_date", "airline", "aircraft",
                      "flight_phase", "altitude_ft", "pilot_warned_flag",
                      "airport", "origin", # for airports
                      "sky_conditions") # for conditions 

# Omitting null values because any row with a null value 
# birdstrikes[rowSums(is.na(birdstrikes)) > 0,]

birdstrikes <- birdstrikes[, relevant_columns] |>
  na.omit()

colnames(birdstrikes) <- c("rid", "idate", "airline", "aircraft", 
                           "flightPhase", "altitude", "warning", # incidents
                           "airportName", "state", # airport
                           "condition") # conditions

# Converting date 
birdstrikes$idate <- as.Date(birdstrikes$idate, "%m/%d/%Y")

# Making warning true or false
birdstrikes$warning <- birdstrikes$warning == "Y"

# Remove military flights
birdstrikes <- birdstrikes[birdstrikes$airline != "MILITARY",]

# Harmonize flight phases
birdstrikes$flightPhase <- birdstrikes$flightPhase |>
  sapply(function(phase) switch(phase, 
                                "Take-off run" = "takeoff",
                                "Climb" = "inflight",
                                "Descent" = "inflight",
                                "Landing Roll" = "landing",
                                "Approach" = "landing",
                                "Taxi" = "unknown",
                                "Parked" = "unknown"))

# airports table
airport_df <- birdstrikes[,c("airportName", "state")] |>
  group_by(airportName, state) |>
  summarize(.groups = "keep") |>
  ungroup()

airport_df$aid <- 1:nrow(airport_df)
airport_df$airportCode <- NA

birdstrikes <- merge(birdstrikes, airport_df, 
                     by = c("airportName", "state"),
                     all = TRUE)

# conditions 
conditions_df <- data.frame(condition = unique(birdstrikes$condition),
                            explanation = NA)

conditions_df$cid <- 1:nrow(conditions_df)

birdstrikes <- merge(birdstrikes, conditions_df, 
      by = c("condition"),
      all = TRUE)

# incidents table
incidents_df <- birdstrikes[,c("rid", "idate", "airline", "aircraft", 
                               "flightPhase", "altitude", "warning",
                               "aid", "cid")] |>
  dplyr::rename(origin = aid, 
                conditions = cid)

