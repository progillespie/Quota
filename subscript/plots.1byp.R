#### Plots using ESTAT data (via datamarket). 



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
