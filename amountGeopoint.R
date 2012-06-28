library('RMySQL')
library('fpc')
library('mclust')
library('edci')
require("geneplotter")
require("RColorBrewer")
require("class")
require("hdrcde")
library("pdfCluster")

# Setup graphs
lay <- layout(matrix(1:8, ncol=2, byrow=TRUE))
#op <- par(mar=rep(1,4), ask = FALSE)

uid <- "'5eedd0514bbc4c2c7b77903f13dbf95f4693638f'"
density <- c(0.025)
minPts <- c(4, 8, 12, 16, 20, 24, 28, 34)
#points <- c(15, 250, 2500)
points <- c(12, 25, 50, 100, 250, 500, 1000, 2000)
#timing <- c('WD_6-8','WD_8-10','WD_10-12','WD_12-14','WD_14-16','WD_16-18','WD_18-20','WD_20-22','WD_22-24','WD_0-2','WD_2-6')
timing <- c('WD')
#tod <- array(c(6,8,8,10,10,12,12,14,14,16,16,18,18,20,20,22,22,24,0,2),c(2,10))
tod <- array(c(16,18),c(2,1))
#tod <- array(c(14,16),c(2,10))
statements <- {}

for (t in 1:(length(tod)/2)) {
	statements <- append(statements, paste(
	"SELECT longitude, latitude",
	"FROM reports",
	"WHERE uid LIKE", uid,
	#"AND EXTRACT(HOUR FROM timeofday) >=", tod[1,t],
	#"AND EXTRACT(HOUR FROM timeofday) <", tod[2,t],
	"AND dayofweek >= 1",
	"AND dayofweek <= 5",
	#"GROUP BY longitude, latitude",
	"ORDER BY timestamp DESC"))
	#"ORDER BY RAND() DESC"))
}

conn <- {}
cv <- array(list(),c(1500,8))
el <- list()
el$manualSilMean <- 0
acounter <- 1
for (p in 1:length(points)) {
	for (i in 1:length(statements)) {
			# Setup graphs

			conn <- dbConnect(MySQL(), user="skillup", password="skillup", host="192.168.13.44", dbname="qori_analyzer")
			if (points[p] == 'all'){
				rso <- dbSendQuery(conn, statements[i])
			}
			else {
				rso <- dbSendQuery(conn, paste(statements[i], "LIMIT", points[p]))
			}
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
			for (den in 1:length(density)) { 
				# Best for Caffati (2215)
				# safe
				#d <- dbscan(x,eps=0.1, MinPts=5, scale=1);
				st <- system.time( d <- dbscan(x,eps=density[den], MinPts=minPts[p], scale=1, method="raw") )["elapsed"]
				plot(d,x, main=paste(timing[i],"minPTS",minPts[p],"points",points[p],"#clusters",max(d$cluster)), ylim = c(-33.50, -33.32), xlim = c(-70.65, -70.50))
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

				print(paste(timing[i],points[p]))
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
						cv[[acounter,6]] <- points[p]
						cv[[acounter,7]] <- max(d$cluster)
						cv[[acounter,8]] <- st
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
						cv[[acounter,6]] <- points[p]
						cv[[acounter,7]] <- max(d$cluster)
						cv[[acounter,8]] <- st
						cv[[acounter,1]] <- timing[i]
						cv[[acounter,2]] <- minPts[pts]
						cv[[acounter,3]] <- density[den]
						cv[[acounter,4]] <- "Zero"
						acounter <<- acounter + 1
					}
				}
				else{
					cv[[acounter,5]] <- el
					cv[[acounter,6]] <- points[p]
					cv[[acounter,7]] <- max(d$cluster)
					cv[[acounter,8]] <- st
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

cv_graph <- array(list(),c(1500,3))
cvg_counter <- 1
# Cluster cohesion
for (time in 1:length(timing)) {
	for (row in 1:nrow(cv)) {
		if (!is.null(cv[row,][[1]]) && cv[row,][[1]] == timing[time]) {
			#if (cv[row,][[5]]$manualSilMean < 1 ) {
				#if (cv[row,][[7]] > 1) {
					print(paste(cv[row,][[1]],cv[row,][[2]],cv[row,][[3]],cv[row,][[6]],cv[row,][[7]],cv[row,][[8]],cv[row,][[5]]$manualSilMean,cv[row,][[5]]$pearsongamma,cv[row,][[5]]$dunn,cv[row,][[5]]$wb.ratio,mean(cv[row,][[5]]$separation)))
					cv_graph[[cvg_counter,1]] <- paste(cv[row,][[1]],"_",cv[row,][[2]],sep="")
					cv_graph[[cvg_counter,2]] <- cv[row,][[3]]
					cv_graph[[cvg_counter,3]] <- cv[row,][[5]]$manualSilMean
					cvg_counter <- cvg_counter + 1
				#}
			#}
		}
	}
}

