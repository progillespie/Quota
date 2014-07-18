#### Plots using ESTAT data (via datamarket). 



# Do year selection first -- common superset for following plots
superset.for.plots <- NULL # empty data object first
superset.for.plots <- data[Year<time.UB,list(Year,Value,Milk.product)]


# Any subsequent selections are made on the basis of Milk.product
setkey(superset.for.plots,"Milk.product") 



# Generate a plot
subset.for.plot <- NULL # empty data object first 
subset.for.plot <- 
  superset.for.plots[c("Cows' milk collected","Drinking milk")]

full.filepath <- file.path(outgraph,"milk_prod.wmf", fsep="//")  

multi.line.ESTAT(
            subset.for.plot=subset.for.plot,
            x="Year", y="Value",group="Milk.product",
            this.plot.title="Milk production",
            ylab="Thousand tonnes",
            legend.position=c(1,0.1),
            full.filepath=full.filepath,
            for.file.type=for.file.type
                )

 

# Generate a plot
subset.for.plot <- NULL # empty data object first 
subset.for.plot <- 
  superset.for.plots[c("Butter","Cheese")]

full.filepath <- file.path(outgraph,"but_chs_prod.wmf", fsep="//")  

multi.line.ESTAT(
            subset.for.plot=subset.for.plot,
            x="Year", y="Value", group="Milk.product", 
            this.plot.title="Butter and Cheese production",
            ylab="Thousand tonnes",
            full.filepath=full.filepath,
            for.file.type=for.file.type
              )
  


# Generate a plot
subset.for.plot <- NULL # empty data object first 
subset.for.plot <- 
  superset.for.plots["Skimmed-milk powder"]

full.filepath <- file.path(outgraph,"smp_prod.wmf", fsep="//")  

line.ESTAT(
            subset.for.plot=subset.for.plot,
            x="Year", y="Value", 
            this.plot.title="Skimmed-milk powder production",
            ylab="Thousand tonnes",
            full.filepath=full.filepath,
            for.file.type=for.file.type
          )
 


# Generate a plot
subset.for.plot <- NULL # empty data object first 
subset.for.plot <- 
  data[c("Stock of butter",
         "Stock of skimmed-milk powder")]

full.filepath <- file.path(outgraph,"but_smp_stocks.wmf", fsep="//")

multi.line.ESTAT(
            subset.for.plot=subset.for.plot,
            x="Year", y="Value", group="Milk.product", 
            this.plot.title="Stocks of Butter and SMP",
            ylab="Thousand tonnes",
            legend.position="top",
            full.filepath=full.filepath,
            for.file.type=for.file.type
                )

