# GET STATE INFO
# install.packages("noncensus")
require("noncensus")
data(states)

# FUNCTION: gets info for specified state and year

get.bridges = function(state, year) {
  link = paste("https://www.fhwa.dot.gov/bridge/nbi/", 
               year, "/delimited/",  
               as.character(states$state[which(states$name == state)]), 
               gsub(pattern = "\\d{2}(\\d{2})", replacement = "\\1", x=year), 
               ".txt", sep="")
  return(read.csv(link, quote=""))
}

# EXAMPLE for 2015 Wisconsin bridges:

WI15bridges = get.bridges("Wisconsin", "2015")
