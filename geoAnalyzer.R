library(RMySQL)

conn <- dbConnect(MySQL(), user="skillup", password="skillup", host="192.168.12.37", dbname="qori_analyzer")
statement <- paste(
	"SELECT longitude, latitude",#timestamp, FROM_UNIXTIME(timestamp) AS date,
	"FROM reports",
	"WHERE uid LIKE '5eedd0514bbc4c2c7b77903f13dbf95f4693638f'",
	"AND latitude > '-71'",
	"group by longitude, latitude",
	"ORDER BY timestamp ASC")#,
	#"LIMIT 100")

rso <- dbSendQuery(conn, statement)
x1 <- fetch(rso)

rowCount <- 0
datax <- {}
datay <- {}
rso <- dbSendQuery(conn, statement)
while(rowCount < 100) {
		stopifnot(rowCount < 100)
        y <- fetch(rso,n=1)
        rowCount <- rowCount + nrow(y)

        datax <- append(datax, y[1,1])
        datay <- append(datay, y[1,2])
}
#print(data)

# Scatterplots with smoothed densities color representation
require("geneplotter")
require("RColorBrewer")
x <- rbind(x1)
layout(matrix(1:4, ncol=2, byrow=TRUE))
op <- par(mar=rep(2,4))
smoothScatter(x, nrpoints=0)
smoothScatter(x)
#smoothScatter(x, nrpoints=Inf, colramp=colorRampPalette(brewer.pal(9,"YlOrRd")), bandwidth=40)
colors <- densCols(x)
plot(x, col=colors, pch=20)

# Bivariate Highest density regions
require("hdrcde")
#par(mfrow=c(1,2))
#plot(datax,datay, pch="+", cex=.5)
hdr.boxplot.2d(datax,datay)