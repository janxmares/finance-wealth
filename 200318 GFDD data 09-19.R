# Revision for phd dissertation
# GFDD FIE components estimate

# 200318, IES FSV UK, Jan Mares

library(data.table)
library(here)
library(countrycode)
library(stringr)

# Read BIS data
gfdd <- data.table(read.csv(here("data/GFDDData.csv"), stringsAsFactors = F, header = T))

# Drop obsolete columns
gfdd[, c(names(gfdd)[1],'X') := NULL]

# Keep only the efficiency indicators
ei <- c("GFDD.EI.01","GFDD.EI.02","GFDD.EI.03","GFDD.EI.04","GFDD.EI.05",
        "GFDD.EI.06", "GFDD.EI.07", "GFDD.EI.08", "GFDD.EI.09", "GFDD.EI.10")

gfdd.ei <- gfdd[Indicator.Code %in% ei,]

# reshape to long format
gfdd.ei.long <- data.table(melt(gfdd.ei, id.vars = c('Country.Code',"Indicator.Name","Indicator.Code"),
                                variable.name = 'year', value.name='value'))

# adjust the year column
gfdd.ei.long[, year := as.numeric(substr(year,2,5))]

# rename columns
setnames(gfdd.ei.long, c("iso3c",'indicator.name','indicator.code','year','value'))

# limit to 1980-2009 and take averages
gfdd.ei.long <- gfdd.ei.long[year %in% c(1980:2009), j = .(value = mean(value, na.rm = T)), by = c('iso3c','indicator.code','indicator.name')]

# select components of FIE
fie_components <- c('GFDD.EI.01','GFDD.EI.02','GFDD.EI.03','GFDD.EI.04',
                    'GFDD.EI.09','GFDD.EI.10') 

gfdd.ei.long <- gfdd.ei.long[indicator.code %in% fie_components, ]

# rename the indicators
gfdd.ei.long[indicator.code == 'GFDD.EI.01', indicator.code := 'NetInterestMargin']
gfdd.ei.long[indicator.code == 'GFDD.EI.02', indicator.code := 'LendDepoSpread']
gfdd.ei.long[indicator.code == 'GFDD.EI.03', indicator.code := 'NonIntIncomeToIncome']
gfdd.ei.long[indicator.code == 'GFDD.EI.04', indicator.code := 'OverheadCostsToAssets']
gfdd.ei.long[indicator.code == 'GFDD.EI.09', indicator.code := 'ROAbeforeTax']
gfdd.ei.long[indicator.code == 'GFDD.EI.10', indicator.code := 'ROEbeforeTax']

# drop indicator names
gfdd.ei.long[, indicator.name := NULL]

# reshape to wide
gfdd.ei.final <- dcast(gfdd.ei.long, iso3c ~ indicator.code, value.var = 'value')

# write into file
write.csv(gfdd.ei.final, file = here('data/fie components.csv'), row.names = F)