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

# What have the best 10 years (by wins and win %) been like?
indians[, .(W), by = yearID][order(-yearID)][1:10]
indians[, .("Win Percentage" = (W/(W + L))), by = yearID][order(-yearID)][1:10]

# When was the last time they won the World Series?
indians[, .(yearID), by = WSWin][WSWin == "Y"][.N]

# Let's see if their attendance can be explained by the number of wins they have.
lm.attendance <- indians[, .(W, attendance)]
ggplot(lm.attendance, aes(x = W, y = attendance)) + geom_point()

# This initial plot looks like there is absolutely not correlation. Let's think about why. It is probably
# because over the years the size of their ballpark has increased, thus probably explaining how they won over 
# 110 games in one season, yet had a low level of attendance. Let's factor out the ballparks other than Progressive Field
# and Jacob's Field. Looking at the graph, it probably won't make a huge difference, but it's vital to be as 
# logically accurate as possible. I've chosen these two because these are actually the same ballpark, but Jacob's Field was 
# renamed Progressive Field upon being sponsored. 
lm.attendance <- indians[, .(W, attendance, park)][park == "Jacobs Field" | park == "Progressive Field"]
ggplot(lm.attendance, aes(x = W, y = attendance)) + 
      geom_point()

# Even with the change, there isn't that strong of a trend, albeit one certainly exists. Upon further review, it seems
# like the attendance metric is a total for the whole season. This is the only thing that makes logical sense, because even 
# in there worst year their attendance is over 1,000,000; doesn't seem all that possible. Let's just take an average over the 
# number of games played to find a per-game number. 
lm.attendance <- indians[, .(W, attendance, park, Ghome)][park == "Jacobs Field" | park == "Progressive Field"][, .("Per Game Attendance" = as.numeric(attendance)/as.numeric(Ghome),
                                                                                                                    "Wins" = W)]
ggplot(lm.attendance, aes(x = Wins, y = `Per Game Attendance`)) + 
  geom_point()


# Now let's see if we can find out what year the Indians had the most amount of players in the All-Star game. To do this, 
# we're going to need to use the 'allstarfull' table.
allstar <- dbReadTable(baseball, "allstarfull")
allstar <- as.data.table(allstar)
names(allstar)
str(allstar)

# This subsets the 'allstar' table by players 
allstar.indians <- allstar[teamID == "CLE"][, .("Number of Players" = .N), by = yearID]
ggplot(allstar.indians, aes(x = yearID, y = `Number of Players`)) + 
      geom_bar(stat = "identity", fill = "#ff0000", col = "black", width = 1, position = "dodge")

# Now let's look at how the amount the franchise paid each year to their players. To do this, let's read the 'salaries' table.
salaries <- dbReadTable(baseball, "salaries")
salaries <- as.data.table(salaries)
names(salaries)
str(salaries)
salaries

# Looking at the data.table printout, it seems that there is only salary data going back to 1985. That is too much of a problem, however.
# Now, this is broken down by team, and by player, so we need to first subset this to get all of the Indians player's salary infomration
salaries.indians <- salaries[teamID == "CLE"]

# Now let's sum the salaries by year
salaries.indians <- salaries.indians[, .("Total Paid" = sum(salary)), by = yearID]

# Finally, let's plot this in a barplot for some initial visualizations
ggplot(salaries.indians, aes(x = yearID, y = `Total Paid`)) +
    geom_bar(stat = "identity", fill = "#ff0000")

# Looks like the year 2000 was a rough on the checkbook for the franchise. Let's see if that paid off in the win column.
# Let's try to correlate and make a quick linear model. 
wins <- indians[yearID %in% 1985:2015][, .(W)]
salaries.indians[, Wins := wins]

ggplot(salaries.indians, aes(x = `Total Paid`, y = Wins)) + geom_point() + geom_smooth(method = "lm", se = FALSE)

# Well, this is certainly trending positively, but the correlation is definitely not strong whatsoever. Oh well, let's 
# keep exploring and maybe we'll start to uncover some of baseball's strong causal relationships as we progress. 





