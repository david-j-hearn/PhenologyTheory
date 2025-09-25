#install.packages(c("ncdf4", "raster", "dplyr", "tidyr", "stringr"))
library(ncdf4)
#library(terra)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(purrr)


cat("Reading climate data.\n")
#read climate data
nc_file = "../Data/North_America_Land_TAVG_Gridded_0p25deg.nc"
nc <- nc_open(nc_file)
lon = ncvar_get(nc, "longitude")
lat = ncvar_get(nc, "latitude")
time_vals = ncvar_get(nc, "time")  # year as decimal
climatology = ncvar_get(nc, "climatology")
temp_anomaly = ncvar_get(nc, "temperature")

decimal_years <- ncvar_get(nc, "time")
layer_years = floor(decimal_years)

get_month_from_decimal_year <- function(dec_year) {
  year <- floor(dec_year)
  remainder <- dec_year - year
  day_of_year <- round(remainder * 365.25)
  as.integer(format(as.Date(paste0(year, "-01-01")) + day_of_year, "%m"))
}

months_vec <- sapply(time_vals, get_month_from_decimal_year)

# Initialize output array
absolute_temp <- array(NA, dim = dim(temp_anomaly))

# Add climatology for each time slice to get actual temperature
cat("\tExtracting absolute temperatures.\n")
for (t in seq_along(months_vec)) {
  m <- months_vec[t]
  absolute_temp[,,t] <- temp_anomaly[,,t] + climatology[,,m]
}

#example
#target_lat = 39
#target_lon = -76
#target_lon = -80.056447
#target_lat = 41.063951
# Find nearest grid cell
#lat_idx = which.min(abs(lat - target_lat))
#lon_idx = which.min(abs(lon - target_lon))
#ts = absolute_temp[lon_idx, lat_idx, ]
#annual_means = aggregate(ts ~ round(time), FUN = mean)

#read data and extract year and day of year from each date

decimal_year <- function(year, doy) {
  days_in_year <- ifelse((year %% 4 == 0 & year %% 100 != 0) | (year %% 400 == 0), 366, 365)
  year + (doy - 1) / days_in_year
}

cat("Processing species data.\n")
spp = c("Anemone_quinquefolia", "Camassia_scilloides", "Cardamine_concatenata", "Claytonia_virginica", "Collinsia_verna", "Dicentra_cucullaria", "Enemion_biternatum", "Erythronium_americanum", "Mertensia_virginica", "Podophyllum_peltatum", "Primula_meadia", "Sanguinaria_canadensis", "Thalictrum_thalictroides")

for(species in spp) {
	print(species)

data_raw = read.csv(paste0("/home/david/Documents/Projects/HerbariumConfounding/FinalAttempt/Analyses/A7_Empirical/Data/", species, ".csv"))
	bad_dates <- data_raw$eventdate[is.na(as.Date(data_raw$eventdate, format = "%Y-%m-%d"))]
#print(bad_dates)
data_raw = data_raw %>% 
	mutate(across(c(geopoint.lon, geopoint.lat), as.numeric))
data_raw <- data_raw %>%
	filter(!eventdate %in% bad_dates) %>%
	#filter(complete.cases(.)) %>%
	mutate(
		   eventdate = as.Date(eventdate),
		   Year = as.integer(format(eventdate, "%Y")),
		   DOY = as.integer(format(eventdate, "%j"))
	)

#Process the temperature data
for(i in 1:nrow(data_raw)) {
	#cat(paste0("\t", i, "\n"))
	target_lon = data_raw$geopoint.lon[i]
	target_lat = data_raw$geopoint.lat[i]
	target_year = data_raw$Year[i]
	lat_idx = which.min(abs(lat - target_lat))
	lon_idx = which.min(abs(lon - target_lon))
	ts = absolute_temp[lon_idx, lat_idx, ]
	matching_idx <- which(floor(time_vals) == target_year)
	if (length(matching_idx) > 0) {
		annual_temp <- mean(ts[matching_idx], na.rm = TRUE)
		firstThree_temp = mean(ts[matching_idx][1:3], na.rm = TRUE)
		spring_temp = mean(ts[matching_idx][3:5], na.rm = TRUE)
		data_raw$mean_temp[i] <- annual_temp
		data_raw$spring_temp[i] = spring_temp
		data_raw$firstThree_temp[i] = firstThree_temp
	} else {
		data_raw$mean_temp[i] <- NA  # Optional: flag missing year
		data_raw$spring_temp[i] <- NA  # Optional: flag missing year
		data_raw$firstThree_temp[i] = NA 
	}
	#annual_means = aggregate(ts ~ round(time), FUN = mean)
}

climate_df = data_raw

#get the rows that correspond to a flowering specimen
#meta_df = read.table(paste0("/home/david/Documents/Projects/HerbariumConfounding/FinalAttempt/Analyses/A7_Empirical/Data_Flowering/NoOutliers/", species, ".noOutliers.txt"), header=T, sep='\t')
meta_df = read.table(paste0("/home/david/Documents/Projects/HerbariumConfounding/FinalAttempt/Analyses/A7_Empirical/Data_Flowering/SpeciesData/", species, ".txt"), header=T, sep='\t')

matched_rows <- climate_df %>%
  mutate(match_row = map(uuid, ~ meta_df %>%
                           filter(str_detect(Image, fixed(.x))) %>%
                           slice(1))) %>%  # slice(1) picks first match if there are multiple
  filter(map_lgl(match_row, ~ nrow(.x) > 0)) %>%  # keep only successful matches
  unnest(match_row, names_sep = "meta")

  #matched_rows_unique <- matched_rows[, !duplicated(as.list(matched_rows))]

  finalData = data.frame(iDigBio_uuid = matched_rows$uuid,
						 Image = matched_rows$match_rowmetaImage,
						 Species = matched_rows$match_rowmetaSpecies,
						 Latitude = matched_rows$geopoint.lat,
						 Longitude = matched_rows$geopoint.lon,
						 #Year = matched_rows$Year,
						 Year = decimal_year(matched_rows$Year, matched_rows$DOY),
						 DOY = matched_rows$DOY,
						 Elevation = matched_rows$elevation,
						 AnnualMonthlyAverageTemp = matched_rows$mean_temp,
						 SpringMonthlyAverageTemp = matched_rows$spring_temp,
						 FirstQuarterMonthlyAverageTemp = matched_rows$firstThree_temp)
finalData = finalData %>% 
	filter(complete.cases(.)) 

  write.table(finalData, file = paste0(species, ".Full.txt"), sep = "\t", row.names = FALSE, quote = FALSE)
}



