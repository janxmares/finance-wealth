# Jan Mares, Finance and Wealth Inequality, revision for dissertation
# March 2020
# Jan Mares

# Estimates - FIE components

# Packages 
library('data.table')
library('xtable')
library('ggplot2')
library('stargazer')
#library('dilutBMS2')
library('BMS')
library('RColorBrewer')
library('countrycode')
library('WDI')
library('here')

#
#

# Function to standardize variables
stz <- function(x){
  (x-mean(x, na.rm=T))/sd(x, na.rm=T)
}

#
#

# Original dataset
hhm_data <- data.table(read.table(file=here('data/data_hhm_mean_2016.csv'), sep=',', header=T, stringsAsFactors=F))

# Rename the financial liberalization variable
setnames(hhm_data, c("FinDev"),c("FinLib"))

# drop the outdated data
hhm_data[, c("GiniWealth","GiniMarket","GiniNet","Redist","FST","LendDepoSpread","NetInterestMargin") := NULL]

# 

# Standardize business variables
hhm_data[, StartBussT:=stz(StartBussT)]
hhm_data[, StartBussC:=stz(StartBussC)]

# Aggregate the variable on bussiness conditions
hhm_data[, BussCond:=(StartBussT+StartBussC)/2]

# Share of religious
hhm_data[,ShareReligous:=Muslim+Catholic+Protestant]

# BASELINE

#
#

# ADDITIONAL DATA

# Wealth inequality indicators from Credit Suisse wealth databook
wid <- data.table(read.table(file=here('data/cswd mean.csv'), sep=',', header = T, stringsAsFactors=F))
wid[, GiniWealth := NULL] # drop gini 2010-2018 before merging

hhm_data <- merge(hhm_data, wid, by = c('iso2c'), all.x = T)

# Compute the averaged GiniWealth index 2010-2016
hhm_data[, GiniWealth := round(mean(c(GiniWealth_2010, GiniWealth_2011, GiniWealth_2012, 
                                      GiniWealth_2013, GiniWealth_2014, GiniWealth_2015, 
                                      GiniWealth_2016), na.rm = T), 1), by = c('iso3c')]

# Read FIE components
fie_components <- data.table(read.csv(file = here('data/fie components.csv'), stringsAsFactors = F, header = T))

# Merge with the rest of the dataset
hhm_data <- merge(hhm_data, fie_components, by=c("iso3c"), all.x = T)

# Merge with data on income gini index
gini <- data.table(read.csv(file=here('data/gini swiid 2010-2018.csv'), header=T))
hhm_data <- merge(hhm_data, gini, by=c("iso2c"), all.x=T, all.y=F)

# Merge data on redistribution
redist <- data.table(read.csv(file=here('data/redist swiid 1980-2009.csv'), header=T))
hhm_data <- merge(hhm_data, redist, by=c("iso2c"), all.x=T, all.y=F)

#
#

# Drop Namibia as Redistribution/Income inequality data is now available
hhm_data <- hhm_data[!(Country %in% c("Namibia")),]

#
#

# Set order of columns (GiniWealth must be 1)
order <- c("iso2c", "iso3c", "Country","GiniWealth","GiniMarket","GiniNet","IPL","Top1","Top10","Top10income") # columns
setcolorder(hhm_data, c(order, names(hhm_data)[!(names(hhm_data) %in% order)]))


#
#

# Removing text columns & variables not used in the analysis
hhm_data <- hhm_data[, c("Country","iso3c","iso2c",
# hhm_data <- hhm_data[, c("iso3c","iso2c",
                         "Confucian","GiniNet","GiniMarket","Top10income",
                         "Top10","Top1","IPL","PrEdu","SecEdu","TerEdu",
                         "Glob_social","Glob_financial","Glob_political","Glob_restrictions",
                         "PrivatecreditVol","Yvolatility","GFCF",#):=NULL]
                         "Deposits2Branches","LendDepoSpread","BussDens","StartBussC","StartBussT",#):=NULL]
                         "TotRev","DemAcc","InstQuality","EquipI","NonequipI",
                         "ATMs1000","TaxRev","Trust","MalIncidence",
                         "ETHFRAC","ETHPOL","RELPOL","RELFRAC","EthnoLfrac","FinReform",
                         "TradeNeigh","ShareReligous",#):=NULL]
                         "Jewish","Buddha","Hindu","Muslim",#"MarketCap","MarketTurn",
                         "KLTaxSpread","logMort",#"TradeNeigh","Latitude",
                         "British","Spanish","French","Catholic","Protestant",#):=NULL]
                         "Legor_UK","Legor_FR","Legor_SO","Legor_GE","Legor_SC",
                         "statehist","Latitude","RuleOfLaw","OECD",#"FST","FST_1500",
                         #"FIA","FMD","FID","FIE","FME"):=NULL]#,"MarketCap","MarketTurn"):=NULL]
                         "BankBranches","MarketCap",
                         "MarketTurn","Loan2Deposits","Privatecredit","BankZscore",
                         "GiniWealth_2010","GiniWealth_2011","GiniWealth_2012","GiniWealth_2013",
                         "GiniWealth_2014","GiniWealth_2015","GiniWealth_2016","GiniWealth_2017",
                         "GiniWealth_2018","Redist_relative","FIE"):=NULL]



# Drop variables because of high collinearity
hhm_data <- hhm_data[, c("InflVol","Age90","SubSahara"):=NULL]
#
#

# Only keep the complete cases
hhm_data <- hhm_data[complete.cases(hhm_data[, !(names(hhm_data) %in% c('ROEbeforeTax','ROAbeforeTax','NonIntIncomeToIncome','NetInterestMargin','LendDepoSpread','OverheadCostsToAssets')), with = F]),]
# hhm_data <- hhm_data[complete.cases(hhm_data[, names(hhm_data) != "GiniWealth_2018", with = F]),]
# hhm_data <- hhm_data[order(Country),]

# nostd <- c("LatAm","Legor_UK","Legor_UK","Legor_FR","Legor_SO","Legor_GE","Legor_SC")
nostd <- c("LatAm")

#
#

# country list

#
#

# xtable(as.data.table(hhm_data$Country[order(hhm_data$Country)]))

#
#

# Standardize the variables

# hhm_data <- data.table(hhm_data[,1], sapply(hhm_data[,2:ncol(hhm_data)], function(x) stz(x)))
hhm_data_std <- data.table(sapply(hhm_data[j=names(hhm_data)[!(names(hhm_data) %in% nostd)], with=F],function(x) stz(x)))
hhm_data <- data.table(hhm_data_std, hhm_data[j=names(hhm_data) %in% nostd, with=F])

# Simple standardization case (standardize all)
# hhm_data <- data.table(sapply(hhm_data, function(x) stz(x)))

#write.csv(hhm_data_std, file="hhm_data_standardized_clean.csv", row.names = F)

#
#

# Run BMA, baseline
bma_finind <- bms(hhm_data, iter=5000000, burn=1000000, mprior = "uniform", g = "hyper",
                  nmodel=5000, mcmc="bd", user.int= F)

# Show summaries of the results
summary(bma_finind)
coef(bma_finind, exact=T)

# save the BMA environment variable
save(bma_finind, file = here('results/results_fie_components.Rdata'))

# Load the estimates if available
load("results/results_fie_components.Rdata")

# Write results into a file
results <- as.data.table(cbind(variable=row.names(coef(bma_finind, exact = T)), as.data.table(coef(bma_finind, exact = T))))
write.csv(results, file=here('results/results_fie_components.csv'), row.names = F)

#
#
# OUTPUT TO LATEX
#
#

# Variable names setup
explvarshort <- c("NatRes","PopGrowth","GovExp","NNSavings","EducExp","Infl","InflVol","VAI","VAA",
                  "StartBussC","StartBussT","BussCond","GFCF","NetFDI","GDP90","Ygrowth","Age90","LifeExp90",
                  "LabForce90","PopDens90","Buddha","Catholic","Confucian","Hindu","Jewish",
                  "Muslim","Protestant","RevCoups","PrEdu","SecEdu","TerEdu","EthnoLfrac","WarYears",
                  "RuleOfLaw","EcoFreedom","CivLib","PolRights","OutwardO",
                  "SubSahara","LatAm","ChinnIto","LeftWing","ActivRestrict","CapitalReg","DiversIndex",
                  "NetInterestMargin","BankZscore","Privatecredit","BankBranches","ATMs1000",
                  "RuleOfLaw#BankBranches","RuleOfLaw#ATMs1000","RuleOfLaw#Privatecredit",
                  "RuleOfLaw#BankZscore","RuleOfLaw#NetInterestMargin","RuleOfLaw#Loan2Deposits",
                  "RuleOfLaw#MarketCap","RuleOfLaw#MarketTurn","CLandPR","Redist","LAMRIG","Tech",
                  "TerEdu#TerEdu","EducIndex","LFPart90","RuleWB","FinLib","FinDev",
                  # Following variables left out from the analysis
                  "Area","PrimExp70","FractionEnglish","FracForeign","EcoOrg","Landlocked","YrsOpen","British","French","Spanish",
                  "Latitude","EquipI","NonequipI","BMP","sdBMP","ERdistort","FinReform","MarketCap","MarketTurn","LendDepoSpread",
                  "Deposits2Branches","AccountsFormalInst","Loan2Deposits","RD","WorkPop90",
                  "FIA","FMD","FID","FIE","FME",
                  "Glob_financial","Glob_social","Glob_political","Glob_restrictions","FST",
                  "debt_ch","y_ch","Redist_relative","EduQuality",
                  "d_bottom_inequality","d_bottom_inequality#FIA",
                  "d_bottom_inequality#FIE","d_bottom_inequality#FID","d_bottom_inequality#FMD",
                  "OverheadCostsToAssets","ROAbeforeTax","ROEbeforeTax","NonIntIncomeToIncome")

explvarnames <- c("Natural resources rents","Population growth","Government expenditures","Net national savings",
                  "Public education expenditures","Inflation","Inflation volatility","Value added in industry",
                  "Value added in agriculture","Costs of starting business","Time to start business","Business conditions",
                  "Gross fixed capital formation","Net foreign direct investment","GDP level in 1990","Average GDP growth",
                  "Median age of population","Life expectancy","Size of labour force","Population density",
                  "Fraction Buddhist","Fraction Catholic","Fraction Confucian","Fraction Hindu","Fraction Jewish",
                  "Fraction Muslim","Fraction Protestant","Revolutions and coups","Primary school attainment",
                  "High school attainment","Tertiary education attainment","Ethnolinguistic fractionalization",
                  "Number of war years","Rule of law",
                  "Economic freedom index (adjusted)","Civil liberties","Political rights","Outward orientation",
                  "Sub-Sahara dummy","Latin America dummy","Financial openness (Chinn-Ito)","Leftwing orientation",
                  "Active banking restrictions","Bank capital regulations","Banking diversification",
                  "Net interest margin","Bank Z-score","Private credit","Bank branches/1000 inh.","ATMs/1000 inh.",
                  "RuleOfLaw#BankBranches","RuleOfLaw#ATMs1000","RuleOfLaw#Privatecredit","RuleOfLaw#BankZscore",
                  "RuleOfLaw#NetInterestMargin","RuleOfLaw#Loan2Deposits",
                  "RuleOfLaw#MarketCap","RuleOfLaw#MarketTurn","Civ. liberties and Pol. rights",
                  "Redistribution","Labour market regulation","Technological progress","TerEdu#TerEdu",
                  "Education index (UN)","Labour force participation","Rule of law","Financial liberalization (EFW)",
                  "Financial liberalization (EFW)",
                  # Following variables left out from the analysis
                  "Area","Primary exports","Fraction of pop. speaking English","Fraction speaking foreign language",
                  "Degree of capitalism","Landlocked dummy","Number of years of open economy","British colony dummy",
                  "French colony dummy","Spanish colony dummy","Absolute latitude","Equipment investment",
                  "Non-equipment investment","Black market premium","SD of black market premium",
                  "Exchange rate distortions","Financial reform index (Abiad)","Market capitalization","Market turnover",
                  "Lending-deposit spread","Deposits to branches ratio","Accounts at formal institutions","Loan-to-deposits",
                  "Rule of law median dummy","Ratio of workers to population",
                  "Access to financial institutions","Financial market depth","Financial institutions depth",
                  "Financial institutions efficiency","Financial markets efficiency",
                  "Financial globalizaion (KOF)", "Social globalization (KOF)","Political globalization (KOF)",
                  "Trade restrictions (KOF)","Genetic difference from the US",
                  "Crisis severity (change in debt)","Crisis severity (change in GDP growth)","Redistribution (rel.)",
                  "Education quality (WB)","50% quantile WG","50% quantile WG#FIA",
                  "50% quantile WG#FIE","50% quantile WG#FID","50% quantile WG#FMD",
                  "Overhead costs / Assets","Pre-tax ROA", "Pre-tax ROE","Non-interest income")

# Bind short and long variables names together
varnames <- data.frame(cbind(explvarshort=as.character(explvarshort), explvarnames=as.character(explvarnames)))

# Creating results table - without economic variables
results_eco <- merge(results, varnames, by.x="variable", by.y="explvarshort", sort=F)

# reorder columns and drop redundant columns
results_eco[, c("variable","Cond.Pos.Sign","Idx") := NULL]
setnames(results_eco, c("explvarnames"), c("Variable"))
setcolorder(results_eco, c("Variable","PIP","Post Mean","Post SD"))

# LaTeX output with proper varnames
print(xtable(results_eco, digits=c(0,0,2,5,5)), include.rownames = F)

# Creating results table - without political variables
results_pol <- merge(results_pol, varnames, by.x="variable", by.y="explvarshort", sort=F)

# reorder columns and drop redundant columns
results_pol[, c("variable","Cond.Pos.Sign","Idx") := NULL]
setnames(results_pol, c("explvarnames"), c("Variable"))
setcolorder(results_pol, c("Variable","PIP","Post Mean","Post SD"))

# LaTeX output with proper varnames
print(xtable(results_pol, digits=c(0,0,2,5,5)), include.rownames = F)