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
density <- c(0.005, 0.010, 0.025, 0.05, 0.075, 0.1, 0.25, 0.5, 1.0)
minPts <- c(5, 10, 15, 20, 25, 30, 35, 45, 50, 55, 60, 65, 70)
timing <- c('WD_6-8','WD_8-10','WD_10-12','WD_12-14','WD_14-16','WD_16-18','WD_18-20','WD_20-22','WD_22-24','WD_0-2','WD_2-6')
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
cv <- array(list(),c(1500,5))
el <- list()
el$manualSilMean <- 0
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
			for (den in 1:length(density)) { 
				plot(c(0,0), c(0,0), main=paste(timing[i],"density",density[den],"minPts",minPts[pts]), ylim = c(-33.50, -33.32), xlim = c(-70.85, -70.50))
			}
		}
	}
	else{
		###
		# Density-based clustering (DBSCAN)
		###
		for (pts in 1:length(minPts)) { 
			for (den in 1:length(density)) { 
				# Best for Caffati (2215)
				# safe
				#d <- dbscan(x,eps=0.1, MinPts=5, scale=1);
				d <- dbscan(x,eps=density[den], MinPts=minPts[pts], scale=1, method="raw");
				#plot(d,x, main=paste(timing[i],"minPTS",minPts[pts],"density",density[den]), ylim = c(-33.50, -33.32), xlim = c(-70.65, -70.50))
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
						currval$minpts <- minPts[pts]
						currval$nozero <- TRUE
						currval$manualSilMean <- mean(currval$clus.avg.silwidths)
						cv[[acounter,5]] <- currval
						cv[[acounter,1]] <- timing[i]
						cv[[acounter,2]] <- minPts[pts]
						cv[[acounter,3]] <- density[den]
						cv[[acounter,4]] <- "NoZero"
						acounter <<- acounter + 1
					}
					else{
						currval <- cluster.stats(dist(x), d$cluster)
						currval$density <- density[den]
						currval$timeslot <- timing[i]
						currval$minpts <- minPts[pts]
						currval$nozero <- FALSE
						currval$manualSilMean <- mean(currval$clus.avg.silwidths[-1])
						cv[[acounter,5]] <- currval
						cv[[acounter,1]] <- timing[i]
						cv[[acounter,2]] <- minPts[pts]
						cv[[acounter,3]] <- density[den]
						cv[[acounter,4]] <- "Zero"
						acounter <<- acounter + 1
					}
				}
				else{

					cv[[acounter,5]] <- el
					cv[[acounter,1]] <- timing[i]
					cv[[acounter,2]] <- minPts[pts]
					cv[[acounter,3]] <- density[den]
					cv[[acounter,4]] <- "Zero"
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
#		if (!is.null(cv[row,][[1]]) && cv[row,][[4]] == "NoZero" && cv[row,][[1]] == timing[time]) {
#			print(paste(cv[row,][[1]], cv[row,][[2]], cv[row,][[5]]$wb.ratio))
#		}
#	}
#	#print("-------")
#}

cv_graph <- array(list(),c(1500,3))
cvg_counter <- 1
# Cluster cohesion
for (time in 1:length(timing)) {
	for (row in 1:nrow(cv)) {
		if (!is.null(cv[row,][[1]]) && cv[row,][[1]] == timing[time]) {
			print(paste(cv[row,][[1]],cv[row,][[2]],cv[row,][[3]],cv[row,][[5]]$manualSilMean))
			cv_graph[[cvg_counter,1]] <- paste(cv[row,][[1]],"_",cv[row,][[2]],sep="")
			cv_graph[[cvg_counter,2]] <- cv[row,][[3]]
			cv_graph[[cvg_counter,3]] <- cv[row,][[5]]$manualSilMean
			cvg_counter <- cvg_counter + 1
		}
	}
	#print("-------")
}
