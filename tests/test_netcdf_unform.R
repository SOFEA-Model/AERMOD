library(ncdf4)
library(data.table)
library(anytime)

# Error threshold.
epsilon <- 0.0005

read.unform.postfile <- function(x) {
  # Read an AERMOD unformatted postfile, assuming host machine byte order,
  # standard 4-byte prefix for record length, and 4-byte record terminator.
  #
  # Start  End     Description
  # 1      4       Record length (integer)
  # 5      8       Time (integer)
  # 9      12      Character record length (integer)
  # 13     20      Source group (character)
  # 21     N-4     Concentration array (double)
  # N-3    N       Record terminator (4 bytes)
  
  f <- x[[1]]
  ave <- x[[2]]
  
  nbytes <- file.size(f)
  h <- file(f, "rb")
  bytes <- readBin(h, "raw", n=nbytes, size=1)
  close(h)
  
  # Transform to raw matrix.
  reclen <- readBin(bytes, "integer", n=1, size=4) + 8
  stopifnot(nbytes %% reclen != integer(0))
  m <- matrix(bytes, nrow=nbytes/reclen, ncol=reclen, byrow=TRUE)
  
  # Calculate receptor count.
  nrec <- (reclen - 16 - 8) / 8
  
  # Read into data.table.
  rbindlist(apply(m, 1, function(row) {
    data.table(time = readBin(row[5:8], "integer", n=1, size=4),
               grp = trimws(rawToChar(readBin(row[13:20], "raw", n=8, size=1))),
               ave = as.integer(ave),
               rec = seq_len(nrec),
               avgconc = readBin(row[21:length(row)], "double", n=nrec, size=8))
  }))
}

# Read map between unformatted and netCDF postfiles.
map <- fread("test_netcdf_unform.csv")

# Get the list of netCDF files to check.
ncfiles <- unique(map[,netcdf])

# Iterate over netCDF files.
for(ncf in ncfiles)
{
  ncfstr <- basename(ncf)
  unfstr <- paste(basename(map[netcdf == ncf,unform]), collapse=", ")
  cat("\n\n")
  cat("NETCDF POSTFILE:  ", ncfstr, "\n")
  cat("UNFORM POSTFILES: ", unfstr, "\n")
  errval <- FALSE
  
#-------------------------------------------------------------------------------
# Process netCDF POSTFILEs
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
    
    # Get time units and convert from UDUNITS to AERMOD convention.
    # "hours since yyyy-mm-dd hh:mm:ss" -> yymmddhh
    time.units <- ncatt_get(nc, "time", attname="units")[[2]]
    time.units <- gsub("hours since ", "", time.units)
    time.start <- anytime(time.units, tz="UTC", asUTC=TRUE)
    
    # Format time variable using AERMOD convention.
    time <- time.start + time*60*60
    time <- as.integer(format(time, format="%y%m%d%H", tz="GMT")) + 1
    
    # Extract concentration variable.
    conc <- as.vector(ncvar_get(nc, "conc"))
    
    # Close the neCDF file.
    nc_close(nc)
    
    # Merge cartesian product of coordinate variables with concentration.
    ndt <- cbind(CJ(ave, grp, rec, time, sorted=FALSE), conc)
    setnames(ndt, c("ave", "grp", "rec", "time", "nc.avgconc"))
    rm(ave, grp, rec, time, conc)
    
    # Remove fill values.
    ndt <- na.omit(ndt, cols="nc.avgconc")
  
  }, error = function(e) {
    cat(paste("TEST FAILED: ", e), "\n", file=stderr())
    errval <<- TRUE
  })
  if (errval) next
  
#-------------------------------------------------------------------------------
# Process Unformatted POSTFILEs
#-------------------------------------------------------------------------------
  
  # Stack the corresponding unformatted files into a data.table.
  args <- apply(map[netcdf == ncf, c("unform", "ave")], 1, as.list)
  udt <- rbindlist(lapply(args, read.unform.postfile), idcol="file")
  
  # Ensure that the netcdf and rdata output has the same number of records.
  if (nrow(ndt) != nrow(udt)) {
    cat("--- TEST FAILED:  Record count mismatch.\n", file=stderr())
    next
  }
  
#-------------------------------------------------------------------------------
# Analysis
#-------------------------------------------------------------------------------
  
  # Merge the RData and netCDF data tables.
  keycols <- c("time", "ave", "grp", "rec")
  setkeyv(ndt, keycols)
  setkeyv(udt, keycols)
  dt <- merge(ndt, udt)
  
  # Ensure that all records are present.
  if (nrow(dt) != nrow(udt)) {
    cat("--- TEST FAILED:  Records lost in merge.\n", file=stderr())
    next
  }
  
  # Calculate error statistics.
  summary <- dt[, .(max.error = max(abs(nc.avgconc - avgconc)),
                    rms.error = sqrt(mean((nc.avgconc - avgconc)^2)))]
  
  # Print summary to console.
  cat("Error Statistics:\n")
  print(summary)
  
  if (summary[,max.error] > epsilon) {
    cat(paste0("--- TEST FAILED:  Error exceeds threshold (", epsilon, ").\n"),
        file=stderr())
    next
  }
  
  cat("+++ TEST PASSED\n")
}
