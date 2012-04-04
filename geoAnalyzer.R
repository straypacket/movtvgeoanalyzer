library(RMySQL)

#conn <- dbConnect(MySQL(), user="skillup", password="skillup", host="192.168.12.37", dbname="qori_analyzer")
statementMorning <- paste(
	"SELECT longitude, latitude",#timestamp, FROM_UNIXTIME(timestamp) AS date,
	"FROM reports",
	"WHERE uid LIKE '5eedd0514bbc4c2c7b77903f13dbf95f4693638f'",
	#"AND latitude > '-71'",
	"AND EXTRACT(HOUR FROM timeofday) > 7",
	"AND EXTRACT(HOUR FROM timeofday) < 10",
	"group by longitude, latitude",
	"ORDER BY timestamp ASC"),
	#"LIMIT 100")
statementNight <- paste(
	"SELECT longitude, latitude",#timestamp, FROM_UNIXTIME(timestamp) AS date,
	"FROM reports",
	"WHERE uid LIKE '5eedd0514bbc4c2c7b77903f13dbf95f4693638f'",
	#"AND latitude > '-71'",
	"AND EXTRACT(HOUR FROM timeofday) > 18",
	"AND EXTRACT(HOUR FROM timeofday) < 24",
	"group by longitude, latitude",
	"ORDER BY timestamp ASC"),
	#"LIMIT 100")

# Morning
conn <- dbConnect(MySQL(), user="skillup", password="skillup", host="192.168.12.37", dbname="qori_analyzer")
rsoM <- dbSendQuery(conn, statementMorning)
xM <- fetch(rsoM)
count <- nrow(xM)

rowCount <- 0
dataxM <- {}
datayM <- {}
rsoM <- dbSendQuery(conn, statementMorning)
while(rowCount < count) {
		stopifnot(rowCount < count)
        y <- fetch(rsoM,n=1)
        rowCount <- rowCount + nrow(y)

        dataxM <- append(dataxM, y[1,1])
        datayM <- append(datayM, y[1,2])
}

# Night
conn <- dbConnect(MySQL(), user="skillup", password="skillup", host="192.168.12.37", dbname="qori_analyzer")
rsoN <- dbSendQuery(conn, statementNight)
xN <- fetch(rsoN)
count <- nrow(xN)

rowCount <- 0
dataxN <- {}
datayN <- {}
rsoN <- dbSendQuery(conn, statementNight)
while(rowCount < count) {
		stopifnot(rowCount < count)
        y <- fetch(rsoN,n=1)
        rowCount <- rowCount + nrow(y)

        dataxN <- append(dataxN, y[1,1])
        datayN <- append(datayN, y[1,2])
}

###
# Graphs
###

# Scatterplots with smoothed densities color representation
require("geneplotter")
require("RColorBrewer")
require("hdrcde")

layout(matrix(1:6, ncol=2, byrow=FALSE))
op <- par(mar=rep(2,4))

# Morning
x1 <- rbind(xM)
#smoothScatter(x1, nrpoints=0)
smoothScatter(x1)
#smoothScatter(x1, nrpoints=Inf, colramp=colorRampPalette(brewer.pal(9,"YlOrRd")), bandwidth=40)
colors <- densCols(x1)
plot(x1, col=colors, pch=20)

# Bivariate Highest density regions
#plot(datax,datay, pch="+", cex=.5)
hdr.boxplot.2d(dataxM,datayM)

# Night
x2 <- rbind(xN)
#smoothScatter(x2, nrpoints=0)
smoothScatter(x2)
#smoothScatter(x2, nrpoints=Inf, colramp=colorRampPalette(brewer.pal(9,"YlOrRd")), bandwidth=40)
colors <- densCols(x2)
plot(x2, col=colors, pch=20)

# Bivariate Highest density regions
#plot(datax,datay, pch="+", cex=.5)
hdr.boxplot.2d(dataxN,datayN)
