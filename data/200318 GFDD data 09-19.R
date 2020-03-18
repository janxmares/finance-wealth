# Revision for phd dissertation
# GFDD FIE components estimate

# 200317, IES FSV UK, Jan Mares

library(data.table)
library(here)
library(countrycode)
library(stringr)
library(XLConnect)

# Read BIS data
gfdd <- data.table(read.csv(here("data/GFDDData.csv"), stringsAsFactors = F, header = T))

# Subset the columns
total.credit <- total.credit[, j = .(country = BORROWERS_CTY.Borrowers..country,
                                     yearquarter = TIME_PERIOD.Time.period.or.range,
                                     totalcredit = OBS_VALUE.Observation.Value)]

# create iso2 code
total.credit[, iso2c := substr(country, 1, 2)]
 
# year column
total.credit[, year := as.numeric(substr(yearquarter, 1, 4))]

# total credit yearly
total.credit <- total.credit[, j = .(totalcredit = mean(totalcredit, na.rm = T)), by = c("iso2c", "year")]

# total credit averages, -2011
total.credit <- total.credit[, j = .(totalcredit = mean(totalcredit)), by = c("iso2c")]

# assign iso3 codes
total.credit[, iso3c := countrycode(iso2c, "iso2c", "iso3c")]
total.credit[, iso2c := NULL]

# drop the aggregate aresa (NAs for iso3c)
total.credit <- total.credit[!(is.na(iso3c)), ]


# how many observations do we have?
# nrow(total.credit)

# save final BIS data to file
write.csv(total.credit, file = "data/BIS total credit final.csv", row.names = F)