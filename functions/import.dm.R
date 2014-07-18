#----------------------------------------------
# Define importing function import.dm() 
#
#   This is just a "wrapper". Mainly sets options for dmlist().
#
#   It also saves a .csv copy locally.
#
#   dmlist() pulls data from DataMarket.com, which is a dataset
#    aggregating website. It has Eurostat data, and much more.    
#
#----------------------------------------------

# Specify what arguments import.dm() will take. These are local to 
#  the function, i.e. everything you define in the function will not
#  exist outside of it unless the function return()'s it. 
import.dm <- function(DScode,
                      geo.dim, 
                      geo.select,
                      key.dims,
                      origdata,
                      import=T
                     )  {

  # Create an empty object to keep data in
  data.dm <- NULL

  # Default behaviour is to download and save data. 
  
  # T is the logical argument TRUE (NO QUOTES!!!). 
  #   This is NOT THE SAME AS "T" or  "TRUE", which R sees 
  #   as literal strings.
  if (import==T) {
  data.dm <- data.table(dmlist(DScode),key=geo.dim)
  data.dm <- data.dm[geo.select]
  setkeyv(data.dm, key.dims)

  # Write the data out to an Rdata file, pasting the filepath origdata
  #  to the DScode and ending it in .Rdata. This is R's binary format
  #  which will load faster than re-reading a csv file.
  full.filepath <- paste(origdata, "/", DScode, ".Rdata", sep="")
  save(data.dm,file=full.filepath)

  
  # USING CSV PROBABLY NOT WORTH PUTTING AN EXTRA ARGUMENT IN THE 
  #   FUNCTION, BUT THE CODE IS BELOW FOR REFERENCE.
  
#   # Write the data out to a csv file, pasting the filepath origdata
#   #  to the DScode and ending it in .csv.
#   write.csv(data.dm,
#             paste(origdata,
#                   "/",
#                   DScode,
#                   ".csv",
#                   sep=""
#                  )
#            )
  
  # Print some output confirming that the file was written

  print(paste(full.filepath, "written."))
  
  }  

  # Load a local copy if option set (csv must exist!!)
  if (import==F) {
    
  full.filepath <- paste(origdata, "/", DScode, ".Rdata", sep="")
  load(full.filepath)  
  
# If re-reading from CSV, will need to do all this again (kept for
#   reference)    
#   data.dm <- data.table(read.csv(paste(origdata,
#                                        "/",
#                                        DScode,
#                                        ".csv",
#                                        sep=""
#                                       )
#                                 )
#                        )
#   
#   setkeyv(data.dm, geo.dim)
#   data.dm <- data.dm[geo.select]
#   setkeyv(data.dm, key.dims)
  
  print(paste("Local copy of ", full.filepath, "loaded."))
  
  } 


  # Give a descriptive error message if the wrong type of argument
  #  for import is given
  if (import!=T & import!=F) {
    print("Import is a logical argument. T or F (unquoted) only.")
  }


  # In either case, the data should be left in memory. Will just 
  #  print to the screen if you don't assign it to an object.
  return(data.dm)
  
  

}

#----------------------------------------------
# End of definition for function import.dm()
#----------------------------------------------
