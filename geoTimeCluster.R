library('RMySQL')
library('ggplot2')
require("RColorBrewer")
require("class")
library("ggplot2")

uid <- "'5eedd0514bbc4c2c7b77903f13dbf95f4693638f'"
statements <- {}

statements <- append(statements, paste(
"SELECT longitude, latitude, FROM_UNIXTIME(timestamp) as time",
"FROM reports",
"WHERE uid LIKE", uid,
#Ignore Japan for now
#"WHERE longitude >= -33.6 AND longitude <= -32.9",
#"AND uid LIKE", uid,
#"AND dayofweek >= 1",
#"AND dayofweek <= 5",
#"AND (dayofweek < 1",
#"OR dayofweek > 5)",
"ORDER BY timestamp ASC"))

conn <- {}
for (i in 1:length(statements)) {
	# Setup graphs

	conn <- dbConnect(MySQL(), user="skillup", password="skillup", host="192.168.13.44", dbname="qori_analyzer")
	rso <- dbSendQuery(conn, statements[i])
	x <- fetch(rso,n=-1)
}

dbClearResult(rso)
dbDisconnect(conn)

#Weekday
#events <- strftime(x$time, format="%w")
#Hour
events <- strftime(x$time, format="%H")
#Hour.Minute
#events <- strftime(x$time, format="%H.%M")
#Day of the month
#events <- strftime(x$time, format="%e")

qplot(events, data=x, geom="histogram", xlab="Hour", ylab="Number of events") + geom_histogram(aes(fill = ..count..)) + scale_fill_gradient("Events", low = "blue", high = "red")

