#----------------------------------------------
# Define plotter function(s). Includes:
#   - line.ESTAT()
#   - multi.line.ESTAT()
#
# Uses ggplot to creat plots, then sets the 
#  theme I like with some tweaks, and also 
#  makes choices re: text, plot width, and plot
#  height based on whether the graph is for use
#  in a .doc or a .ppt
#  
#----------------------------------------------

# Other good themes.  
#theme_minimal(base_size=10, base_family="serif") +
#theme_bw(base_size=10, base_family="serif")      +
#theme_classic(base_size=10, base_family="serif") +

# If you find yourself using a theme with a box around the legend
#      legend.key = element_blank()    ,
#  ... will get rid of that for you.

# Same for plot area borders (top and right)
#       panel.border = element_blank()    ,
#       panel.grid.minor = element_blank(),
#       panel.grid.major = element_blank()
#theme(axis.line = element_line(color = 'black'))

line.ESTAT <- function(subset.for.plot,
                       x, y, 
                       this.plot.title,
                       xlabel=x,
                       ylabel=y,
                       full.filepath,
                       for.file.type="doc"
                      ) {


  # Create plot object
  plot <- ggplot(subset.for.plot,
                 aes_string(x     = x,
                            y     = y
                           )
                )
  
  # Add layers to plot object
  plot <- plot + geom_line() # make it a line plot
  plot <- plot + geom_vline(xintercept=1984, col="red")   
  
  # Apply theme to plot -- theme() tweaks theme_classic() a bit
  plot <- plot + theme_classic(base_size=10, base_family="serif")
  plot <- plot + xlab(xlabel)
  plot <- plot + ylab(ylabel)
  plot <- plot + theme(plot.title           = element_text(face="bold"),
                       legend.position      = "none"
                      )
  
  
  # PLOTTING
  
  # Conditional re: surrounding text and size of figure. Makes 
  #   appropriate choices for loading into .doc or .ppt  
  if (for.file.type=="ppt") {
    
      
    pptfont <- 18
    plot <- plot + theme(text=element_text(size=pptfont))
    
    # Cross referencing not an issue for ppt presentations, so
    #   attach surrounding text to the plot   
    plot <- plot + ggtitle(this.plot.title)
    
    # Place Source citation in plot using gridExtra package
    plot <- arrangeGrob(plot,
                        sub=textGrob("Source: Eurostat",
                                     x    =0.1,
                                     hjust=-0.1,
                                     vjust=0.1,
                                     gp   =gpar(fontface  ="italic",
                                                fontsize  =0.8*pptfont,
                                                fontfamily="serif"
                                               )
                                    )
                       )
    
    
    # Best w and h parameters for ppt
    #        width=5,
    #        height=3.5,
    #        width=4,
    #        height=2.86,
    
    ggsave(full.filepath,
           plot,
           width=8.25,
           height=5.9,
           units="in"
    )
    
  } else {
    
    # Surrounding text best handled by the word doc itself (mainly for
    #   cross referencing), so omit those from the plot.
    
    # Best w and h parameters for word doc
    #        width=3.5,
    #        height=2.5,
    
    ggsave(full.filepath,
           plot,
           width=3.5,
           height=2.5,
           units="in"
    )
    
  }
  
  return(plot)
}



multi.line.ESTAT <- function(subset.for.plot,
                             x, y, group,
                             this.plot.title,
                             xlabel=x,
                             ylabel=y,
                             legend.position=c(1,0),
                             full.filepath,
                             for.file.type="doc"
                            ) {


  # Create plot object
  plot <- ggplot(subset.for.plot,
                 aes_string(x     = x,
                            y     = y,
                            group = group
                 )
  )
  
  # Add layers to plot object
  plot <- plot + geom_line(aes_string(lty=group))
        # make it a line plot
  plot <- plot + geom_vline(xintercept=1984, col="red")      # add vertical line
  # Apply theme to plot -- theme() tweaks theme_classic() a bit
  plot <- plot + theme_classic(base_size=10, base_family="serif")
  plot <- plot + xlab(xlabel)
  plot <- plot + ylab(ylabel)
  plot <- plot + theme(legend.title         = element_blank(),
                       plot.title           = element_text(face="bold"),
                       legend.key.height    = unit(0.8,"line"),
                       legend.justification = c(1,0),
                       legend.background    = 
                         element_rect(fill="transparent"), 
                       legend.position      = legend.position
  )
  
  
  # PLOTTING
  
  # Conditional re: surrounding text and size of figure. Makes 
  #   appropriate choices for loading into .doc or .ppt  
  if (for.file.type=="ppt") {
    
    pptfont <- 18
    plot <- plot + theme(text=element_text(size=pptfont))
    # Cross referencing not an issue for ppt presentations, so
    #   attach surrounding text to the plot   
    plot <- plot + ggtitle(this.plot.title)
    
    # Place Source citation in plot using gridExtra package
    plot <- arrangeGrob(plot,
                        sub=textGrob("Source: Eurostat",
                                     x    =0.1,
                                     hjust=-0.1,
                                     vjust=0.1,
                                     gp   =gpar(fontface  ="italic",
                                                fontsize  =0.8*pptfont,
                                                fontfamily="serif"
                                               )
                                    )
                       )
    
    
    # Best w and h parameters for ppt
    #        width=5,
    #        height=3.5,
    #        width=4,
    #        height=2.86,
    
    ggsave(full.filepath,
           plot,
           width=8.25,
           height=5.9,
           units="in"
    )
    
  } else {
    
    # Surrounding text best handled by the word doc itself (mainly for
    #   cross referencing), so omit those from the plot.
    
    # Best w and h parameters for word doc
    #        width=3.5,
    #        height=2.5,
    
    ggsave(full.filepath,
           plot,
           width=3.5,
           height=2.5,
           units="in"
    )
    
  }
  
  return(plot)
}



monthly.points.ESTAT <- function(subset.for.plot,
                             x, y,
                             this.plot.title,
                             xlabel=x,
                             ylabel=y,
                             full.filepath,
                             for.file.type="doc",
                             col="black",
                             palette=c("red","green","blue")
                            ) {

## Now changing to Date class in data before function call  
#   # Month is currently a factor. Format as.Date() instead.
#   subset.for.plot$Month <- 
#   as.Date(paste(as.character(subset.for.plot$Month),
#                 "-01",
#                 sep=""
#                ),
#           format="%Y-%m-%d"
#           )


  # Create plot object
  plot <- ggplot(subset.for.plot,
                 aes_string(x     = x,
                            y     = y
                 )
  )
  
  date84 <- as.Date("1984-01-01", format="%Y-%m-%d")
  
  # Add layers to plot object
  plot <- plot + geom_point(aes_string(col=col)) # make it a scatterplot
  #plot <- plot + geom_point() # make it a scatterplot
  plot <- plot + geom_vline(xintercept=as.numeric(date84),
                            col="red")
  plot <- plot + scale_x_date()
  
  # Apply theme to plot -- theme() tweaks theme_classic() a bit
  plot <- plot + theme_classic(base_size=10, base_family="serif")
  plot <- plot + xlab(xlabel)
  plot <- plot + ylab(ylabel)
  plot <- plot + theme(legend.title         = element_blank(),
                       plot.title           = element_text(face="bold"),
                       legend.key.height    = unit(0.8,"line"),
                       legend.justification = c(1,0),
                       legend.background    = 
                         element_rect(fill="transparent"), 
                       legend.position      = c(1,0)
  )
  
  if (col!="black") {
    
    plot <- plot + theme(legend.position="right")   + 
      guides(colour = guide_legend(override.aes = list(size=4))) +
      scale_colour_manual(values=palette)
      #scale_colour_brewer(palette="Set1")  
      
  }
  
  # PLOTTING
  
  # Conditional re: surrounding text and size of figure. Makes 
  #   appropriate choices for loading into .doc or .ppt  
  if (for.file.type=="ppt") {
    
    pptfont <- 18
    plot <- plot + theme(text=element_text(size=pptfont))
    
    # Cross referencing not an issue for ppt presentations, so
    #   attach surrounding text to the plot   
    plot <- plot + ggtitle(this.plot.title)
    
    # Place Source citation in plot using gridExtra package
    plot <- arrangeGrob(plot,
                        sub=textGrob("Source: Eurostat",
                                     x    =0.1,
                                     hjust=-0.1,
                                     vjust=0.1,
                                     gp   =gpar(fontface  ="italic",
                                                fontsize  =0.8*pptfont,
                                                fontfamily="serif"
                                               )
                                    )
                       )
    
    
    # Best w and h parameters for ppt
    #        width=5,
    #        height=3.5,
    #        width=4,
    #        height=2.86,
    #        width=21.41,
    #        height=15,

    ggsave(full.filepath,
           plot,
           width=8.25,
           height=5.9,
           units="in"
    )
    
  } else {
    
    # Surrounding text best handled by the word doc itself (mainly for
    #   cross referencing), so omit those from the plot.
    
    # Best w and h parameters for word doc
    #        width=3.5,
    #        height=2.5,
    
    ggsave(full.filepath,
           plot,
           width=3.5,
           height=2.5,
           units="in"
    )
    
  }
  
  return(plot)
}
#----------------------------------------------
# End of function definitions
#----------------------------------------------