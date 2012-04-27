library('RMySQL')
library('fpc')
library('mclust')
library('edci')
require("geneplotter")
require("RColorBrewer")
require("class")
require("hdrcde")

#if (!"package:misc3d" %in% search()) {
#  Sys.sleep(15)
#}

# Setup graphs
lay <- layout(matrix(1:50, ncol=10, byrow=FALSE))
#op <- par(mar=rep(1,4), ask = FALSE)

uid <- "'6f01a0f212db893eca84e8ced20a81b44798031f'"
density <- c(0.025, 0.1, 0.25, 0.5, 1.0)
timing <- c('WE 6-8','WE 8-10','WE 10-12','WE 12-14','WE 14-16','WE 16-18','WE 18-20','WE 20-22','WE 22-24','WE 0-2','WE 2-6')
tod <- array(c(6,8,8,10,10,12,12,14,14,16,16,18,18,20,20,22,22,24,0,2),c(2,10))
statements <- {}

for (t in 1:(length(tod)/2)) {
	statements <- append(statements, paste(
	"SELECT longitude, latitude",
	"FROM reports",
	"WHERE uid LIKE", uid,
	"AND EXTRACT(HOUR FROM timeofday) >=", tod[1,t],
	"AND EXTRACT(HOUR FROM timeofday) <", tod[2,t],
	"AND (dayofweek < 1",
	"OR dayofweek > 5)",
	"ORDER BY timestamp ASC"))
}

conn <- {}
for (i in 1:length(statements)) {
		# Setup graphs

		conn <- dbConnect(MySQL(), user="skillup", password="skillup", host="192.168.13.44", dbname="qori_analyzer")
		rso <- dbSendQuery(conn, statements[i])
		x <- fetch(rso,n=-1)
		count <- nrow(x)

		rowCount <- 0
		datax <- {}
		datay <- {}
		rso <- dbSendQuery(conn, statements[i])
		while(rowCount < count) {
				stopifnot(rowCount < count)
		        y <- fetch(rso,n=1)
		        rowCount <- rowCount + nrow(y)

		        datax <- append(datax, y[1,2])
		        datay <- append(datay, y[1,1])
		}
		dbClearResult(rso)
		dbDisconnect(conn)

	x <- cbind(datax,datay)
	if(is.null(x)){
		for (den in 1:length(density)) { 
			plot(c(0,0), c(0,0), main=paste(timing[i],"density",density[den]), ylim = c(-33.50, -33.32), xlim = c(-70.85, -70.50))
		}
	}
	else{
		###
		# Density-based clustering (DBSCAN)
		###
		for (den in 1:length(density)) { 
			# Best for Caffati (2215)
			# safe
			#d <- dbscan(x,eps=0.1, MinPts=5, scale=1);
			d <- dbscan(x,eps=density[den], MinPts=5, scale=1, method="raw");
			#plot(d,x, main=paste(timing[i],"density",density[den]), ylim = c(-33.50, -33.32), xlim = c(-70.65, -70.50))
			#plot(x[d$cluster %in% 1:10,], main=timing[i], ylim = c(-33.50, -33.32), xlim = c(-70.65, -70.50))
			# Santiago + Vina del mar
			#plot(d, x, main=timing[i], ylim = c(-33.55, -32.9), xlim = c(-71.6, -70.5))
			# Best for Nico (2754)
			#d <- dbscan(x,eps=0.025, MinPts=5, scale=1);
			#plot(d, x, main=timing[i], ylim = c(-33.8, -33.3), xlim = c(-71, -70.45))
			# best for Luis (915)
			#d <- dbscan(x,eps=0.8, MinPts=5, scale=1);
			plot(d, x, main=paste(timing[i],"density",density[den]), xlim = c(139.7, 140.2), ylim = c(35.6, 36.1));
		}
	}
	#dev.copy2pdf(device="png", file=paste("./density",density[1],"-",density[length(density)],".png"), width=1920, height=1080, bg="white")

}
