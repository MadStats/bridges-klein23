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
  data = read.csv(link, quote="")
  return(data[,c(1,2,9,11,20,21,27,28,30,41, 102)])
}

# get 2016 wi bridges:

WI16bridges = get.bridges("Wisconsin", "2016")

M = WI16bridges
M = as.tbl(M)

# trim data to *just* wi (based on lat and long)
ggplot(data = M) + geom_point(mapping = aes(y = LAT_016, x = LONG_017)) # SHOULD look like WI - doesn't bc of data entry errors
M = filter(M,LONG_017 > 0) # remove errors
ggplot(data = M) +geom_point(mapping = aes(y = LAT_016, x = LONG_017)) # choppy bc lat and long units are time

min2dec = function(x){
  substr(x,3,8) %>% return
}
hist(M$LAT_016 %>% min2dec %>% as.numeric) # convert back to numeric
# roughly uniform
min2dec = function(x){
  as.numeric(substr(x,1,2)) + as.numeric(substr(x,3,8))/6e+05 %>% return
}
min2dec(M$LAT_016[1])
hist(M$LAT_016 %>% min2dec %>% as.numeric)

M = mutate(M,lat = min2dec(LAT_016), lon = min2dec(LONG_017))
ggplot(data = M) +geom_point(mapping = aes(y = lat, x = lon))

M = filter(M,lon<100)
ggplot(data = M) +geom_point(mapping = aes(y = lat, x = lon)) # home.

# what's up with the giant road? 
str(M[which(M$TRAFFIC_LANES_ON_028A>12),]) # 15 traffic lanes?!
# built 2014, apparently Dane Co. but 18 mi creek at north of state... delete for 
# bad data entry
M = M[-which(M$TRAFFIC_LANES_ON_028A>12),]

# make some plots
ggplot(data = M) + geom_point(mapping = aes(y = log(ADT_029), x = TRAFFIC_LANES_ON_028A)) # avg traffic vs. traffic lanes
ggplot(data = M) + geom_point(mapping = aes(y = lat, x = lon, col =TRAFFIC_LANES_ON_028A)) # no. traffic lanes over state
ggplot(data = M) + geom_point(mapping = aes(y = lat, x = lon, col =ADT_029)) # dark => before 1900
ggplot(data = M) + geom_point(mapping = aes(y = TRAFFIC_LANES_ON_028A, x = YEAR_BUILT_027)) # avg traffic vs. traffic lanes

# make fips codes
M = mutate(M, fips = STATE_CODE_001*1000+COUNTY_CODE_003)
M = M %>% mutate(fips = STATE_CODE_001*1000+COUNTY_CODE_003)

# what is the mean daily traffic?
wi1 = M %>% group_by(fips) %>% summarize(avTraffic = mean(ADT_029))
wi1 %>% transmute(region = fips, value = avTraffic) %>% county_choropleth(state_zoom = "wisconsin")

# how many traffic lanes are there, on average?
wi2 = M %>% group_by(fips) %>% summarize(avgLanes = mean(TRAFFIC_LANES_ON_028A))
wi2 %>% transmute(region = fips, value = avgLanes) %>% county_choropleth(state_zoom = "wisconsin")

# where are the historically significant bridges?
wi3 = M %>% group_by(fips) %>% summarize(numberHistSignif = length(which(HISTORY_037 < 4)))
wi3 %>% transmute(region = fips, value=numberHistSignif) %>% county_choropleth(state_zoom = "wisconsin")

# lanes and direction
length(which(M$TRAFFIC_DIRECTION_102 == 3))
