# What happened around 1984? Uses Eurostat stats.





###==================================================================
### Head 
###  -- packages, directories, function definitions
###==================================================================


rm(list=ls()) # remove any objects from workspace
graphics.off() # Clear graphics device (i.e. any plots)


# Uncomment to download packages containing the functions
#install.packages("rdatamarket")
#install.packages("data.table")
#install.packages("gridExtra")


# Load required packages ( library() is a synonym for require() )
library(rdatamarket) # for dmlist() function
library(data.table)  # for data.table() -- enhanced data.frame obj.
library(ggplot2)     # for ggplot() & ggsave() i.e. plots
library(gridExtra)   # for putting "Source: " in plots
library(scales)      # if there are any monthly plots


# Save some directory info ( getwd() returns current working dir)
startdir  <- getwd()

dataroot  <- "D://Data//data_EUROSTAT"
project   <- "Quota"

scriptdir <- file.path(dataroot, "code_R"   , project, fsep="//")
origdata  <- file.path(dataroot, "OrigData"          , fsep="//")
outdata   <- file.path(dataroot, "OutData"  , project, fsep="//")
outgraph  <- file.path(           outdata   ,"graphs", fsep="//")


# Attempt to create OutData directories -- suppress any warnings
dir.create(outdata , showWarnings=F)
dir.create(outgraph, showWarnings=F)


# Change to script directory
setwd(scriptdir)



#-------------------------------------------------------------------
# Define importing function import.dm() 
#-------------------------------------------------------------------

source("../functions/import.dm.R")

#-------------------------------------------------------------------



#-------------------------------------------------------------------
# Define plotter functions. Includes:
#   - line.ESTAT()
#   - multi.line.ESTAT()
#   - monthly.points.ESTAT()
#-------------------------------------------------------------------

source("../functions/plot.ESTAT.R")

#-------------------------------------------------------------------



#------------------------------------------------------------------- 
# Define date conversion function: 
#  - Month.dates()
#
# Converts YYYY-MM month variable from factor 
#  to a Date class variable. Requires that a 
#  day of the month, so arbitrarily using first
#  of the month.
#------------------------------------------------------------------- 

Month.dates <- function(data){
  
  data$Month <- 
    as.Date(paste(as.character(data$Month),
                  "-01",
                  sep=""
                 ),
            format="%Y-%m-%d"
           )
  return(data)
}

#-------------------------------------------------------------------


###==================================================================
### End of Head
###==================================================================






###==================================================================
### Body 
### -- Downloading, importing, saving, and plotting
###==================================================================


# Set some import arguments globally since they won't change
geo.select    <- "Ireland"
import        <- F
time.UB       <- 2013
for.file.type <- "ppt"


##-------------------------------------------------------------------
## Agricultural Production
##-------------------------------------------------------------------

#Cows'milk collection and products obtained - annual data
# Units: Thousand tonnes
# NON-GEO DATA DIMENSIONS:
#        * Milk product
#        * Year

DScode         <- "1brq"
geo.dim        <- "Geopolitical.entity..reporting."
key.dims       <- c("Milk.product", "Year")

data <- NULL # empty data object first
data           <- import.dm(DScode=DScode,
                            geo.dim=geo.dim,
                            geo.select=geo.select,
                            key.dims=key.dims,
                            origdata=origdata,
                            import=import
                 )
data[, min(Year)] 
#  Start of series: 1968 

source("subscript//plots.1brq.R")


## Do year selection first -- common superset for following plots
#superset.for.plots <- NULL # empty data object first
#superset.for.plots <- data[Year<time.UB,list(Year,Value,Milk.product)]
#
#
## Any subsequent selections are made on the basis of Milk.product
#setkey(superset.for.plots,"Milk.product") 
#
#
#
## Generate a plot
#subset.for.plot <- NULL # empty data object first 
#subset.for.plot <- 
#  superset.for.plots[c("Cows' milk collected","Drinking milk")]
#
#full.filepath <- file.path(outgraph,"milk_prod.wmf", fsep="//")  
#
#multi.line.ESTAT(
#            subset.for.plot=subset.for.plot,
#            x="Year", y="Value",group="Milk.product",
#            this.plot.title="Milk production",
#            ylab="Thousand tonnes",
#            legend.position=c(1,0.1),
#            full.filepath=full.filepath,
#            for.file.type=for.file.type
#                )
#
# 
#
## Generate a plot
#subset.for.plot <- NULL # empty data object first 
#subset.for.plot <- 
#  superset.for.plots[c("Butter","Cheese")]
#
#full.filepath <- file.path(outgraph,"but_chs_prod.wmf", fsep="//")  
#
#multi.line.ESTAT(
#            subset.for.plot=subset.for.plot,
#            x="Year", y="Value", group="Milk.product", 
#            this.plot.title="Butter and Cheese production",
#            ylab="Thousand tonnes",
#            full.filepath=full.filepath,
#            for.file.type=for.file.type
#              )
#  
#
#
## Generate a plot
#subset.for.plot <- NULL # empty data object first 
#subset.for.plot <- 
#  superset.for.plots["Skimmed-milk powder"]
#
#full.filepath <- file.path(outgraph,"smp_prod.wmf", fsep="//")  
#
#line.ESTAT(
#            subset.for.plot=subset.for.plot,
#            x="Year", y="Value", 
#            this.plot.title="Skimmed-milk powder production",
#            ylab="Thousand tonnes",
#            full.filepath=full.filepath,
#            for.file.type=for.file.type
#          )
# 
#
#
## Generate a plot
#subset.for.plot <- NULL # empty data object first 
#subset.for.plot <- 
#  data[c("Stock of butter",
#         "Stock of skimmed-milk powder")]
#
#full.filepath <- file.path(outgraph,"but_smp_stocks.wmf", fsep="//")
#
#multi.line.ESTAT(
#            subset.for.plot=subset.for.plot,
#            x="Year", y="Value", group="Milk.product", 
#            this.plot.title="Stocks of Butter and SMP",
#            ylab="Thousand tonnes",
#            legend.position="top",
#            full.filepath=full.filepath,
#            for.file.type=for.file.type
#                )



#Production and utilization of milk on the farm - annual data
# NOTE: for Milk Yield, Milk Feed
# NON-GEO DATA DIMENSIONS:
#        * Milk product
#        * Item of milk
#        * Year
DScode         <- "1be7"
geo.dim        <- "Geopolitical.entity..reporting."
key.dims       <- c("Milk.product", "Item.of.milk", "Year")
data <- NULL # empty data object first
data           <- import.dm(DScode=DScode,
                            geo.dim=geo.dim,
                            geo.select=geo.select,
                            key.dims=key.dims,
                            origdata=origdata,
                            import=import
                 )
data[, min(Year)] 
#  Start of series: 1960

#source("plots.1be7.R")


setkey(data, "Item.of.milk")
superset.for.plots <- NULL # empty data object first
superset.for.plots <- data["Products obtained (1 000 t)"] 

superset.for.plots <- 
  superset.for.plots[Year<time.UB, list(Year, Value, Milk.product)]

setkey(superset.for.plots, "Milk.product")



# Generate a plot
subset.for.plot <- NULL # empty data object first 
subset.for.plot <- 
  superset.for.plots[c("Dairy cows' milk","Cows' milk - Total")]

full.filepath <- file.path(outgraph,
                           "dairy_non_dy_milk.wmf",
                           fsep="//"
                          )  

multi.line.ESTAT(
            subset.for.plot = subset.for.plot,
            x = "Year", y = "Value", group = "Milk.product", 
            this.plot.title = "Dairy milk and Total Milk production",
            ylab = "Thousand tonnes",
            full.filepath = full.filepath,
            for.file.type = for.file.type
                )



# Generate a plot
subset.for.plot <- NULL # empty data object first 
subset.for.plot <- 
  superset.for.plots["Milk yield per dairy cow (kg of milk per year)"]

full.filepath <- file.path(outgraph,
                           "milk_yield.wmf",
                           fsep="//"
                          )  

line.ESTAT(
            subset.for.plot=subset.for.plot,
            x="Year", y="Value", 
            this.plot.title="Milk Yield",
            ylab="Thousand tonnes",
            full.filepath=full.filepath,
            for.file.type=for.file.type
          )



#Fat contents and protein contents (cow's milk) - annual data"
# NON-GEO DATA DIMENSIONS:
#        * Item of milk
#        * Year
DScode         <- "1byp"
geo.dim        <- "Geopolitical.entity..reporting."
key.dims       <- c("Item.of.milk", "Year")
data <- NULL # empty data object first
data           <- import.dm(DScode=DScode,
                            geo.dim=geo.dim,
                            geo.select=geo.select,
                            key.dims=key.dims,
                            origdata=origdata,
                            import=import
                 )
data[, min(Year)] 
#  Start of series: 1970

source("plots.1byp.R")


# Do year selection first -- common superset for following plots
subset.for.plot <- NULL #  empty data object first
subset.for.plot <- data[Year<time.UB,list(Year,Value,Item.of.milk)]
# Shorten level label
levels(subset.for.plot$Item.of.milk) <- c("Fat", "Protein") 

full.filepath <- file.path(outgraph,
                           "fat_protein_annual.wmf",
                           fsep="//"
                          )  

multi.line.ESTAT(
            subset.for.plot=subset.for.plot,
            x="Year", y="Value", group="Item.of.milk", 
            this.plot.title="Fat and Protein content of cows' milk
   (annual)",
            ylab="% product of weight",
            legend.position=c(0.3,.8),
            full.filepath=full.filepath,
            for.file.type=for.file.type
              )



#Cows'milk collection and products obtained - monthly data
# NOTE:  for seasonality of production (cows milk collected, 
#         Fat and protein)
# NON-GEO DATA DIMENSIONS:
#        * Milk product
#        * Unit
#        * Month
DScode         <- "1aja"
geo.dim        <- "Geopolitical.entity..reporting."
key.dims       <- c("Milk.product","Unit.of.measure", "Month")
data <- NULL # empty data object first
data           <- import.dm(DScode=DScode,
                            geo.dim=geo.dim,
                            geo.select=geo.select,
                            key.dims=key.dims,
                            origdata=origdata,
                            import=import
                 )
data <- Month.dates(data)
setkeyv(data, key.dims)
data[, min(Month)] 
#  Start of series: 1968-01-01

source("plots..R")

# Generate a plot
subset.for.plot <- NULL # empty data object first
subset.for.plot <- data[J("Cows' milk collected",
                          "Fat content (% of product weight)"
                          )]

# Create repeating month and quarter variables
subset.for.plot[, mm:=substr(Month, 6, 7)]
setkey(subset.for.plot, "mm")
subset.for.plot[c("01","02","03"), Q:="Q1"]
subset.for.plot[c("04","05","06"), Q:="Q2"]
subset.for.plot[c("07","08","09"), Q:="Q3"]
subset.for.plot[c("10","11","12"), Q:="Q4"]
  
# # Protein not recorded on monthly basis for Ireland

full.filepath <- file.path(outgraph,
                           "fat_monthly.wmf",
                           fsep="//"
                          )  

monthly.points.ESTAT(
            subset.for.plot=subset.for.plot,
            x="Month", y="Value", 
            this.plot.title="Monthly Fat Content--Milk",
               ylab="% of product weight",
            full.filepath=full.filepath,
            for.file.type=for.file.type,
            col="Q",
            palette=c("#fdae61", "#d7191c","#abd9e9", "#2c7bb6")
                    )


#           palette=c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c")


# Generate a plot
subset.for.plot <- NULL #  empty data object first
subset.for.plot <- data[J("Cows' milk collected",
                          "Protein content (% of product weight)"
                          )]

# Create repeating month and quarter variables
subset.for.plot[, mm:=substr(Month, 6, 7)]
setkey(subset.for.plot, "mm")
subset.for.plot[c("01","02","03"), Q:="Q1"]
subset.for.plot[c("04","05","06"), Q:="Q2"]
subset.for.plot[c("07","08","09"), Q:="Q3"]
subset.for.plot[c("10","11","12"), Q:="Q4"]
 
# # Protein not recorded on monthly basis for Ireland

full.filepath <- file.path(outgraph,
                           "protein_monthly.wmf",
                           fsep="//"
                          )  


monthly.points.ESTAT(
            subset.for.plot=subset.for.plot,
            x="Month", y="Value", 
            this.plot.title=
              "Monthly Protein Content--Milk",
            ylab="% of product weight",
            full.filepath=full.filepath,
            for.file.type=for.file.type,
            col="Q",
            palette=c("#fdae61", "#d7191c","#abd9e9", "#2c7bb6")
                    )



##-------------------------------------------------------------------
## Agricultural prices and price indices
##-------------------------------------------------------------------
## Selling prices of agricultural products (absolute prices),
## land prices, and rents
##-------------------------------------------------------------------


#Selling prices of animal products (absolute prices)- monthly - 
#  old code - data from 1969 to2006
# NON-GEO DATA DIMENSIONS:
#        * Currency
#        * Animal products
#        * Month
DScode         <- "1c4x"
geo.dim        <- "Geopolitical.entity..declaring."
key.dims       <- c("Currency","Agricultural.product..old.codes.",
                    "Month"
                   )
data <- NULL # empty data object first
data           <- import.dm(DScode=DScode,
                            geo.dim=geo.dim,
                            geo.select=geo.select,
                            key.dims=key.dims,
                            origdata=origdata,
                            import=import
                 )
data <- Month.dates(data)
setkeyv(key.dims)
data[, min(Month)] 
#  Start of series: 1969-01-01

#source("plots..R")


#Land prices and rents - annual data
# NON-GEO DATA DIMENSIONS:
#        * Agricultural indicator
#        * Unit
#        * Year
DScode         <- "1bxx"
geo.dim        <- "Geopolitical.entity..declaring."
key.dims       <- c("Agricultural.indicator", "Unit", "Year")
data <- NULL # empty data object first
data           <- import.dm(DScode=DScode,
                            geo.dim=geo.dim,
                            geo.select=geo.select,
                            key.dims=key.dims,
                            origdata=origdata,
                            import=import
                 )
data[, min(Year)] 
#  Start of series: 1985

#source("plots..R")

#Purchase prices of the means of agricultural production (absolute 
#  prices) - annual - old codes - data from 1969 to 2005"
# NON-GEO DATA DIMENSIONS:
#        * Currency
#        * Agricultural product (old codes)
#        * Year
DScode         <- "18zd"
geo.dim        <- "Geopolitical.entity..declaring."
key.dims       <- c("Currency", "Agricultural.product..old.codes.",
                    "Year"
                   )
data <- NULL # empty data object first
data           <- import.dm(DScode=DScode,
                            geo.dim=geo.dim,
                            geo.select=geo.select,
                            key.dims=key.dims,
                            origdata=origdata,
                            import=import
                 )
data[, min(Year)] 
#  Start of series: 1969

#source("plots..R")


#Purchase prices of the means of agricultural production (absolute 
#  prices) - monthly - old codes - data from 1969 to 2006"
# NON-GEO DATA DIMENSIONS:
#        * Currency
#        * Agricultural product (old codes)
#        * Month
DScode         <- "18z7"
geo.dim        <- "Geopolitical.entity..declaring."
key.dims       <- c("Currency", "Agricultural.product..old.codes.",
                    "Month"
                   )
data <- NULL # empty data object first
data           <- import.dm(DScode=DScode,
                            geo.dim=geo.dim,
                            geo.select=geo.select,
                            key.dims=key.dims,
                            origdata=origdata,
                            import=import
                 )
data <- Month.dates(data)
setkeyv(data, key.dims)
data[, min(Month)] 
#  Start of series: 1969-01-01

#source("plots..R")


#Selling prices of animal products (absolute prices) - annual - old 
#  codes - data from 1969 to 2005
# NON-GEO DATA DIMENSIONS:
#        * Currency
#        * Agricultural product (old codes)
#        * Year
DScode         <- "1c5p"
geo.dim        <- "Geopolitical.entity..declaring."
key.dims       <- c("Currency", "Agricultural.product..old.codes.", 
                    "Year"
                   )
data <- NULL # empty data object first
data           <- import.dm(DScode=DScode,
                            geo.dim=geo.dim,
                            geo.select=geo.select,
                            key.dims=key.dims,
                            origdata=origdata,
                            import=import
                 )
data[, min(Year)] 
#  Start of series: 1969

#source("plots..R")




##-------------------------------------------------------------------
## Economic accounts for agriculture
##-------------------------------------------------------------------

# No historical data

##-------------------------------------------------------------------



##-------------------------------------------------------------------
## Regional Agricultural Statistics
##-------------------------------------------------------------------

#Land use by NUTS 2 regions
# NON-GEO DATA DIMENSIONS:
#        * Land use
DScode         <- "1bsj"
geo.dim        <- "Geopolitical.entity..reporting."
key.dims       <- c("Land.use", "Year")
data <- NULL # empty data object first
data           <- import.dm(DScode=DScode,
                            geo.dim=geo.dim,
                            geo.select=geo.select,
                            key.dims=key.dims,
                            origdata=origdata,
                            import=import
                 )
data[, min(Year)] 
#  Start of series: 1974

#source("plots..R")



#Animal populations (December) by NUTS 2 regions
# NON-GEO DATA DIMENSIONS:
#        * Live animals
DScode         <- "1bao"
geo.dim        <- "Geopolitical.entity..reporting."
key.dims       <- c("Live.animals", "Year")
data <- NULL # empty data object first
data           <- import.dm(DScode=DScode,
                            geo.dim=geo.dim,
                            geo.select=geo.select,
                            key.dims=key.dims,
                            origdata=origdata,
                            import=import
                 )
data[, min(Year)] 
#  Start of series: 1977

#source("plots..R")

##-------------------------------------------------------------------



##-------------------------------------------------------------------
## Unit value statistics for agricultural products
##-------------------------------------------------------------------

# No historical data

##-------------------------------------------------------------------


##-------------------------------------------------------------------
## Agricultural Labour Input Statistics
##-------------------------------------------------------------------

# No historical data

##-------------------------------------------------------------------



##-------------------------------------------------------------------
## Agri-Environmental Indicators
##-------------------------------------------------------------------
# Pressures and risks
#--------------------

# No historical data

##-------------------------------------------------------------------

###==================================================================
### End of Body
###==================================================================






###==================================================================
### Clean up
###==================================================================
setwd(startdir)
rm(list=ls())
