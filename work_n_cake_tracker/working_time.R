
source("./working_time_under_the_hood.R")

# If you get the following warning:
# "No working time data previously saved to be found. Please initialise a new dataframe with add_start()."
# Uncomment and run the following line INSTEAD of the two lines below the heading START:
# wtime <- add_start()
# save(wtime, file = "./workingtime.RData")

# By default, the script will show you the last few lines of your working time dataframe, hence the
# tail() everywhere.

# START
tail(wtime <- add_start(wtime))
save(wtime, file = "./workingtime.RData")

# tags: What happened during the day? Suggestion: String with dots as separators.
# cake tags: Name of the person who brought the cake, -h attached to it if homemade
tail(wtime <- add_stop(wtime, tag = "coding.literature search", no_cakes = 2, cake_tags = "Jenny-h.Peter"))
save(wtime, file = "./workingtime.RData")

# Helpful library for adjusting dates and times.
library(lubridate)

# Examples:
day(wtime$start[1])
day(wtime$start[1]) <- 2
minute(wtime$start[1]) <- 10
