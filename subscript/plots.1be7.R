#### Plots using ESTAT data (via datamarket). 



# Common superset for following plots
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
