#' create stand parameters to run SureauR 
#'
#' @param file a path to a csv file with the input parameters (see details for how to format this file in help 
#' @param LAImax a numerical value indicating the maximum leaf area index of the stand (m2/m2) 
#' @param lat a numerical value indicating the latitude of the stand 
#' @param lon a numerical value indicating the longitude of the stand 
#' @return
#' a list with stand parameters that can be used as in input in \code{run.SureauR}
#' @export
#'
#' @examples
#' create.stand.parameters(lat = 44, lon = 5, LAImax = 5)
create.stand.parameters <- function(file, LAImax, lat, lon) {
  if (missing(file) & (missing(LAImax) | missing(lat) | missing(lon))) {
    stop(paste0("provide input file path or 'LAImax','lat' and 'lon'"))
  }

  stand_params <- list()
  if (!missing(file)) {
    if (file.exists(file)) {
      io <- read.csv(file = file, sep = ";",dec=".", head = T)
      colnames(io) <- c("Name", "Value")
      params <- c("LAImax", "lat", "lon")
      for (i in 1:length(params))
      {
        AAA <- which(io$Name == params[i]) ## line number of the variable

        if (length(AAA) == 0) # checking that it exists in input file  / stop running otherwise
          {
            stop(paste0("'", params[i], "' is not provided in input vegetation parameter file, check presence or spelling\n", file))
          } else if (length(AAA) > 1) {
          stop(paste0("'", params[i], "' is not provided several times in input vegetation parameter file, correct \n", file))
        } else if (length(AAA) == 1) {
          if (!is.na(as.numeric(io[AAA, "Value"]))) { # checking that parameter is numeric in input file /stop running otherwise
            eval(parse(text = paste0("stand_params$", params[i], "<-", as.numeric(as.character(io[AAA, "Value"])))))
          } else {
            stop(paste0(params[i], "must be numeric"))
          }
        }
      }
    } else {
      stop(paste0("Could not find input stand parameter file : ", file, " check path"))
    }
  } 
  
  else {
     if (!is.numeric(LAImax)) {
      stop("'LAImax' must be numeric.")
    }
    
    if (!is.numeric(lat)) {
      stop("'lat' must be numeric.")
    }
    if (!is.numeric(lon)) {
      stop("'lon' must be numeric.")
    }
    stand_params$lon <- lon
    stand_params$lat <- lat
    stand_params$LAImax <- LAImax
  }
  return(stand_params)
}
