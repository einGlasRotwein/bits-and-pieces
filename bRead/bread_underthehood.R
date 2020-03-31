
# Here is the dirty secret how the initial dataframe was created.

# Create the dataframe's names.
breads <- paste0("bread", 1:7, "_")
breads <- paste0(breads, rep(1:6, each = length(breads)))

types <- c("banana", "sourdough", "baguette", "toast", "brioche", "ciabatta")
breads <- paste0(types, "_", rep(breads, each = length(types)))

bread_names <- c("baker", breads)

# Empty dataframe to start with. Number of columns equals the number of names.
df_bread <- data.frame(matrix(NA, nrow = 3, ncol = length(bread_names)))
names(df_bread) <- bread_names # set names

df_bread[["baker"]] <- c("Gordon", "Juli", "rstats") # Let's have 3 bakers

# All the columns that end with 1 will contain "B", all the ones that end with 2 will contain "R",
# 3 = "E", 4 = "A", 5 = "D", 6 = " "

# We get the names that end with 1 like this:
grepl("_1$", names(df_bread)) # ... logical ...
names(df_bread)[grepl("_1$", names(df_bread))] # ... which can be used for indexing
# Which means we can access the respective columns in the dataframe like this:
df_bread[ , grepl("_1$", names(df_bread))]
# ... and set them to B!
df_bread[ , grepl("_1$", names(df_bread))] <- "B"

# Fill up the other letters.
df_bread[ , grepl("_2$", names(df_bread))] <- "R"
df_bread[ , grepl("_3$", names(df_bread))] <- "E"
df_bread[ , grepl("_4$", names(df_bread))] <- "A"
df_bread[ , grepl("_5$", names(df_bread))] <- "D"
df_bread[ , grepl("_6$", names(df_bread))] <- " " # empty column will make it nicer

# Save as text file so it can just be loaded.
write.csv(df_bread, file = "./bRead/df_bread.csv")
