library(ggplot2)
library(RMySQL)
library(tidyr)
library(tm)
library(data.table)
library(DBI)



# Make initial connect with the baseball database
baseball <- dbConnect(MySQL(),
                      user = 'davidheis',
                      password = 'BASSIst23!',
                      dbname = 'baseball',
                      host = 'localhost')

# Check out the structure of the baseball conenction
str(baseball)

# Examine tables in baseball and check out their structure
tables <- dbListTables(baseball)
tables

# Since this analysis is intended to help learn more about baseball over the years, let me first start 
# with my hometown team - the Cleveland Indians (Roll Tribe)! Now, using 'RMySQL', we can actually query
# our database from directly in R, but for my Indians analysis I will simply retrieve our database. 
# The reason you'd want to query from directly inside R is to reduce the amount of data being given to R,
# effectively reducing the amount of memory used. Later in the analysis I'll do this, since the numbe of rows
# I'll be working with will go from 2.5k to over 70k. Let's start the Indians analysis by accessing the
# "teams' table of our database. Then, let's take a look at its structure. 

teams <- dbReadTable(baseball, "teams")
str(teams)

# Right now our teams table is saved as a data frame object. Let's convert it to data.table and print
# it to the console to see a little more of our data
teams <- as.data.table(teams)
teams 

# If you look closely at the printout above, the 3 team listed is the 'Cleveland Forest Citys' - it appears
# that the Indians weren't the first team to play in Cleveland! Since the Indians are the current MLB team, let's
# subset our data.table to find the data for them

cle.teams <- grep("Cleveland Indians", value = TRUE, teams$name)
cle.teams <- unique(cle.teams)
indians <- teams[name %in% cle.teams]

# So now we have our data.table with just Indians data. Now let's answer some questions! 

# What year did the Indians begin playing?
indians[1]

# What were the best 10 years (by wins) in team history?
indians[, .(yearID), by = W][order(-W)][1:10]

# When was the last time they won the World Series?
indians[, .(yearID), by = WSWin][WSWin == "Y"][.N]

