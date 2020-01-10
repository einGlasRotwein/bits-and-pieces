
# Script to load work time data tracked so far and all the functions needed for tracking. Called by
# working_time.R

# Load data saved so far if a saved file already exists.
# Will give a warning when there currently is none.
tryCatch(load("./workingtime.RData"), 
         error = function(e) print("No working time data previously saved to be found."),
         warning = function(w) print("No working time data previously saved to be found. Please initialise a new dataframe with add_start()."))

#' Initialises the dataframe needed for tracking of work time
#'
#' @details Creates a dataframe with the columns \code{entry}, \code{start}, \code{stop}, 
#'          \code{tag}, \code{no_cakes} and \code{cake_tags}. All other columns are empty
#'          initially.
#'
#' @author Juli Tkotz \email{juliane.tkotz@@gmail.com}
#' 
initialise_df_wtime <- function () {
  df_wtime <- data.frame(entry = 1,
                         start = as.POSIXct(Sys.time()),
                         stop = as.POSIXct(NA),
                         tag = as.character(NA),
                         no_cakes = as.integer(NA),
                         cake_tags = as.character(NA),
                         stringsAsFactors = FALSE)
  
  return(df_wtime)
}

#' Starts tracking of working time.
#'
#' @param df_time The dataframe with the working times tracked so far.
#'
#' @details A dataframe with the columns \code{entry}, \code{start}, \code{stop}, \code{tag}, 
#'          \code{no_cakes} and \code{cake_tags} is needed to run the function. If no dataframe 
#'          with working times is provided, a fresh one is initialised.
#'          Will give a warning if the previous entry has no stop time.
#'          Adds a row to the working time dataframe, adding the current system time as start
#'          time. Also puts in the entry number (previous entry + 1). All other column entries
#'          are filled with \code{NA} initially.
#'
#' @examples
#'
#' wtime <- add_start(wtime)
#'
#' @author Juli Tkotz \email{juliane.tkotz@@gmail.com}
#' 
add_start <- function (df_time = NULL) {
  # If no dataframe with working times is provided, a fresh one is initialised.
  if (all(is.na(df_time))) {
    df_time <- initialise_df_wtime()
    return(df_time)
  }
  
  if (is.na(df_time$stop[nrow(df_time)])) {
    stop("Don't you want to stop the previous session?")
  }
  
  to_add <- data.frame(entry = df_time$entry[nrow(df_time)] + 1,
                       start = as.POSIXct(Sys.time()),
                       stop = as.POSIXct(NA),
                       tag = as.character(NA),
                       no_cakes = as.integer(NA),
                       cake_tags = as.character(NA))
  
  df_time <- rbind(df_time, to_add)
  
  return(df_time)
}

#' Stops tracking of working time.
#'
#' @param df_time The dataframe with the working times tracked so far.
#' @param tag Tags what happened on that day. Suggestion: A string with key words
#'            separated by dots.
#' @param no_cakes Number of cakes that appeared during the current entry. Put in 0
#'                 if there was no cake.
#' @param cake_tags Name of the person(s) who brought the cake as a string separated by dots.
#'                  If the cake is homemade, an "-h" is added behind the name, e.g. "John-h".
#'                  Put in \code{NA} if there was no cake.
#'
#' @details A dataframe with the columns \code{entry}, \code{start}, \code{stop}, \code{tag},
#'          \code{no_cakes} and \code{cake_tags} is needed to run the function. Via arguments, you 
#'          can provide tags about what happened during the day and specify the number of cakes
#'          and who made them.
#'          Will give a warning if there is no starting time yet.
#'
#' @examples
#'
#' wtime <- add_stop(wtime, "coding R.literature seach", 2, "Jenny-S.Peter"))
#'
#' @author Juli Tkotz \email{juliane.tkotz@@gmail.com}
#' 
add_stop <- function (df_time, tag, no_cakes, cake_tags) {
  if (!is.na(df_time$stop[nrow(df_time)])) {
    stop("Confucius says: You have to start before you can stop.")
  }
  
  df_time$stop[nrow(df_time)] <- Sys.time()
  df_time$tag[nrow(df_time)] <- tag
  df_time$no_cakes[nrow(df_time)] <- no_cakes
  df_time$cake_tags[nrow(df_time)] <- cake_tags
  
  return(df_time)
}
