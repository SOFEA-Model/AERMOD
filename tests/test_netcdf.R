library(ncdf4)
library(data.table)

# Utility function to return case_postdata object from .RData file.
load_case_postdata <- function(f) {
  env <- new.env()
  load(f, env)[1]
  env[["case_postdata"]]
}

# Read map between .RData (aermet_def_18081_aermod_18081) and netCDF files.
map <- fread("rdata_netcdf_map.csv")

# Get the list of netCDF files to check.
ncfiles <- unique(map[,netcdf])

# Iterate over netCDF files.
for(ncf in ncfiles)
{
  ncfstr <- basename(ncf)
  rdfstr <- paste(basename(map[netcdf == ncf,rdata]), collapse=", ")
  cat("\n\n")
  cat("NETCDF POSTFILE: ", ncfstr, "\n")
  cat("PLOT POSTFILES:  ", rdfstr, "\n")
  errval <- FALSE
  
#-------------------------------------------------------------------------------
# Process netCDF
#-------------------------------------------------------------------------------
  
  tryCatch(
  {
    # Open the neCDF file.
    nc <- nc_open(ncf)
    
    # Extract coordinate variables.
    ave <- as.vector(ncvar_get(nc, "ave"))
    grp <- trimws(as.vector(ncvar_get(nc, "grp")))
    rec <- as.vector(ncvar_get(nc, "rec"))
    time <- as.vector(ncvar_get(nc, "time"))
    
    # Extract concentration variable.
    conc <- as.vector(ncvar_get(nc, "conc"))
    
    # Close the neCDF file.
    nc_close(nc)
    
    # Merge cartesian product of coordinate variables with concentration.
    ndt <- cbind(CJ(ave, grp, rec, time, sorted=FALSE), conc)
    setnames(ndt, c("ave", "group", "rec", "time", "nc.avgconc"))
    rm(ave, grp, rec, time, conc)
    
    # Remove fill values.
    ndt <- na.omit(ndt, cols="nc.avgconc")
  
  }, error = function(e) {
    cat(paste("TEST FAILED: ", e), "\n", file=stderr())
    errval <<- TRUE
  })
  if (errval) next
  
#-----------------------------------------------------------------------------
# Process RData
#-----------------------------------------------------------------------------
  
  # Stack the corresponding .RData files into a data.table.
  rdt <- rbindlist(lapply(map[netcdf == ncf,rdata], load_case_postdata),
                   idcol="file")
  
  # Convert factors to character.
  rdt[, avgtime := as.character(avgtime)]
  rdt[, group := as.character(group)]
  
  # Calculate time and receptor index values over RData.
  rdt[, time := .GRP - 1, .(date)]
  rdt[, rec := seq_len(.N), rleid(date, avgtime, group)]
  
  # Make numeric averaging time from string averaging periods.
  rdt[avgtime == "1-HR",  ave := 1]
  rdt[avgtime == "3-HR",  ave := 3]
  rdt[avgtime == "24-HR", ave := 24]
  rdt[avgtime == "MONTH", ave := 720]
  
  # Ensure that the netcdf and rdata output has the same number of records.
  if (nrow(ndt) != nrow(rdt)) {
    cat("TEST FAILED:  Record count mismatch.\n", file=stderr())
    next
  }
  
#-------------------------------------------------------------------------------
# Analysis
#-------------------------------------------------------------------------------
  
  # Merge the RData and netCDF data tables.
  keycols <- c("time", "ave", "group", "rec")
  setkeyv(ndt, keycols)
  setkeyv(rdt, keycols)
  dt <- merge(ndt, rdt)
  
  # Ensure that all records are present.
  if (nrow(dt) != nrow(rdt)) {
    cat("TEST FAILED:  Records lost in merge.\n", file=stderr())
    next
  }
  
  # Calculate error statistics.
  summary <- dt[, .(max.error = max(abs(nc.avgconc - avgconc)),
                    rms.error = sqrt(mean((nc.avgconc - avgconc)^2)))]
  
  # Print summary to console.
  cat("Error Statistics:\n")
  print(summary)
  
  if (summary[,max.error] > 0.001) {
    cat("TEST FAILED:  Error exceeds threshold (0.001).\n", file=stderr())
    next
  }
  
  cat("TEST PASSED\n")
}
