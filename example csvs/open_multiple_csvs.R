
# First, we want a list of all files in our folder so R knows what to read in.
# We store that list in a variable we call `filelist`. We use the function
# `list.file()` and don't even need to tell where to look - it will use the
# folder we're in. That is, I assume your R project lies in the folder where
# your .csv files are. We tell R to only look for .csv files via `pattern`.
# The weird $ tells R to look for .csv at the end of the filename - because my
# script as "csv" in its name, it would otherwise include that as well.
filelist <- list.files(pattern = ".csv$")

# If the data is in a folder within the folder your project lies in, the code
# will look like this - just replace YOURFOLDER with the folder's name.
filelist <- list.files(path = "./YOUFOLDER", pattern = ".csv$")

# You can add any file path you want. Let me know if you have issues with
# finding out what the right path is.
# Let's have a look at our file list to see if the names are correct.
filelist

# Looks good. Now we import all of these.
all_csvs <- lapply(filelist, read.table, sep = ";", stringsAsFactors = FALSE,
                   header = 1)

# What happened here? Lapply applied the function `read.table` to all the files
# in `filelist`. `sep` is in fact an argument for `read.table` and we would
# normally write something like `read.table(sep = ";", stringsAsFactors = FALSE,
# header = 1)`, but with lapply, we just add it afterwards.
# This is by the way another thing that could cause errors: In my files, the
# separator (`sep`) is a semikolon (";"), but it might be something else in your
# files, e.g. a tab ("\t").
# `stringsAsFactors = FALSE` usually is important because otherwise, it treats
# characters (the column "cond" in our case) as factors and we don't want that.
# I also tell R that it can find a header in row one.

# Take a look at our list:
all_csvs

# That's nice, but you want them all in one single dataframe. We can achieve
# that like this:
df_all_csvs <- Reduce(rbind.data.frame, all_csvs)

# That's it. `Reduce()` applies the function `rbind.data.frame()` (which binds
# together all dataframes by row) to all the dataframes in our list `all_csvs`.
# Take a look at the first 15 rows in our `df_all_csvs`.
head(df_all_csvs, 15)

# In our example, you can see the variable in the environment: It has 3 
# variables (columns) and 1000 obs. (observations = rows), which means it
# worked: I had 10 .csv files with 100 rows reach, so now I have one big
# dataframe with 1000 rows.

# Two things are REALLY important here:
# 1) Your .csv files need to have the same structure. It won't work when you
#    have e.g. different column names. Well, you CAN make it work, but it's
#    slightly more complicated then. Happy to help and write some code for you
#    in that case.
# 2) Note that I included an ID-column in each file. Imagine it being an ID for
#    e.g. a rat. Or maybe indicating the run of an experiment. IF YOU DON'T
#    HAVE SOMETHING INSIDE EACH FILE THAT CLEARLY IDENTIFIES IT, YOU ENTER THE
#    DANGER ZONE! Imagine each file was from one rat, but we didn't have an ID
#    column. It would be quite hard to tell which data came from which rat
#    after combining everything and you couldn't really work with the data. This
#    problem is easily fixed, though. Let me know if that is the case and we'll
#    work it out.
