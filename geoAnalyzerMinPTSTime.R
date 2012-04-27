library('RMySQL')
library('fpc')
library('mclust')
library('edci')
library('fpc')
require("geneplotter")
require("RColorBrewer")
require("class")
require("hdrcde")

#if (!"package:misc3d" %in% search()) {
#  Sys.sleep(15)
#}

# Setup graphs
lay <- layout(matrix(1:50, ncol=10, byrow=TRUE))
#op <- par(mar=rep(1,4), ask = FALSE)

uid <- "'5eedd0514bbc4c2c7b77903f13dbf95f4693638f'"
minPts <- c(3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
timing <- c('WD 6-8','WD 8-10','WD 10-12','WD 12-14','WD 14-16','WD 16-18','WD 18-20','WD 20-22','WD 22-24','WD 0-2','WD 2-6')
#timing <- c('WD 12-14','WD 14-16','WD 16-18','WD 18-20','WD 20-22')
tod <- array(c(6,8,8,10,10,12,12,14,14,16,16,18,18,20,20,22,22,24,0,2),c(2,10))
#tod <- array(c(12,14,14,16,16,18,20,22,22,24),c(2,5))
statements <- {}

for (t in 1:(length(tod)/2)) {
	statements <- append(statements, paste(
	"SELECT longitude, latitude",
	"FROM reports",
	"WHERE uid LIKE", uid,
	"AND EXTRACT(HOUR FROM timeofday) >=", tod[1,t],
	"AND EXTRACT(HOUR FROM timeofday) <", tod[2,t],
	"AND dayofweek >= 1",
	"AND dayofweek <= 5",
	"ORDER BY timestamp ASC"))
}

conn <- {}
cv <- array(list(),c(500,4))
acounter <- 1
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
		for (pts in 1:length(minPts)) { 
			plot(c(0,0), c(0,0), main=paste(timing[i],"minPts",minPts[pts]), ylim = c(-33.50, -33.32), xlim = c(-70.85, -70.50))
		}
	}
	else{
		###
		# Density-based clustering (DBSCAN)
		###
		for (pts in 1:length(minPts)) { 
			# Best for Caffati (2215)
			# safe
			#d <- dbscan(x,eps=0.1, MinPts=5, scale=1);
			d <- dbscan(x,eps=0.025, MinPts=minPts[pts], scale=1, method="raw");
			plot(d,x, main=paste(timing[i],"minPTS",minPts[pts]), ylim = c(-33.50, -33.32), xlim = c(-70.65, -70.50))
			#plot(x[d$cluster %in% 1:10,], main=timing[i], ylim = c(-33.50, -33.32), xlim = c(-70.65, -70.50))
			# Santiago + Vina del mar
			#plot(d, x, main=timing[i], ylim = c(-33.55, -32.9), xlim = c(-71.6, -70.5))
			# Best for Nico (2754)
			#d <- dbscan(x,eps=0.025, MinPts=5, scale=1);
			#plot(d, x, main=timing[i], ylim = c(-33.8, -33.3), xlim = c(-71, -70.45))
			# best for Luis (915)
			#d <- dbscan(x,eps=0.8, MinPts=5, scale=1)
			# Kantou
			#plot(d, x, main=paste(timing[i],"density",density[den]), xlim = c(139.7, 140.2), ylim = c(35.6, 36.1))
			# Tokyo
			#plot(d, x, main=paste(timing[i],"density",density[den]), xlim = c(139.7, 139.85), ylim = c(35.6, 35.8))

			if (max(d$cluster)) {

				if (max(d$cluster) > 1){
					frame <- subset(data.frame(x=datax,y=datay,clus=d$cluster), clus>0)
					currval <- cluster.stats(dist(subset(frame, select=x:y)), unlist(subset(frame, select=clus), use.names=FALSE), G2=TRUE,G3=TRUE)
					currval$density <- density[den]
					currval$timeslot <- timing[i]
					currval$nozero <- TRUE
					currval$manualSilMean <- mean(currval$clus.avg.silwidths)
					cv[[acounter,4]] <- currval
					cv[[acounter,1]] <- timing[i]
					cv[[acounter,2]] <- minPts[pts]
					cv[[acounter,3]] <- "NoZero"
					acounter <<- acounter + 1
				}
				else{
					currval <- cluster.stats(dist(x), d$cluster)
					currval$density <- density[den]
					currval$timeslot <- timing[i]
					currval$nozero <- FALSE
					currval$manualSilMean <- mean(currval$clus.avg.silwidths[-1])
					cv[[acounter,4]] <- currval
					cv[[acounter,1]] <- timing[i]
					cv[[acounter,2]] <- minPts[pts]
					cv[[acounter,3]] <- "Zero"
					acounter <<- acounter + 1
				}
			}
		}
	}
}

# Cluster Analyze/graph:
# Perhaps use cluster cohesion. It's impossible to compute
# without the outliers (cluster 0) when there's only one cluster
#for (time in 1:length(timing)) {
#	for (row in 1:nrow(cv)) {
#		if (!is.null(cv[row,][[1]]) && cv[row,][[3]] == "NoZero" && cv[row,][[1]] == timing[time]) {
#			print(paste(cv[row,][[1]], cv[row,][[2]], cv[row,][[4]]$wb.ratio))
#		}
#	}
#	#print("-------")
#}

# Cluster cohesion
for (time in 1:length(timing)) {
	for (row in 1:nrow(cv)) {
		if (!is.null(cv[row,][[1]]) && cv[row,][[1]] == timing[time]) {
			print(paste(cv[row,][[1]], cv[row,][[2]], cv[row,][[4]]$manualSilMean))
		}
	}
	#print("-------")
}
