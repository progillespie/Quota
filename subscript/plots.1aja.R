#### Plots using ESTAT data (via datamarket). 



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
                           "fat_monthly.pdf",
                           fsep="/"
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
                           "protein_monthly.pdf",
                           fsep="/"
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

