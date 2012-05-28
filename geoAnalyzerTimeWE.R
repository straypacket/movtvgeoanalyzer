library('RMySQL')
library('fpc')
library('mclust')
library('edci')
library('fpc')
require("geneplotter")
require("RColorBrewer")
require("class")
require("hdrcde")
require("ggplot2")

#if (!"package:misc3d" %in% search()) {
#  Sys.sleep(15)
#}

# Setup graphs
lay <- layout(matrix(1:4, ncol=2, byrow=TRUE))
#op <- par(mar=rep(1,4), ask = FALSE)

uid <- "'5eedd0514bbc4c2c7b77903f13dbf95f4693638f'"
density <- c(0.025)
timing <- c('Weekend 12-14','Weekend 14-16','Weekend 16-18','Weekend 18-20')
abc <-c('a)','b)','c)','d)','e)','f)','g)','h)','i)','j)','k)')
#timing <- c('WD 12-14','WD 14-16','WD 16-18','WD 18-20','WD 20-22')
tod <- array(c(12,14,14,16,16,18,18,20),c(2,4))
#tod <- array(c(12,14,14,16,16,18,18,20,20,22),c(2,5))
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
	"GROUP BY longitude, latitude",
	"ORDER BY timestamp ASC"))
}

conn <- {}
cv <- array(list(),c(500,4))
acounter <- 1
facet_frame <- {}
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
			plot(c(0,0), c(0,0), main=paste(timing[i],"density",density[den]), ylim = c(-33.55, -32.9), xlim = c(-71.6, -70.5))
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
			d <- dbscan(x,eps=density[den], MinPts=5, scale=1, method="raw")
			facet_frame <- rbind(facet_frame,data.frame(x=datax,y=datay,clus=d$cluster,time=timing[i]))	
			#plot(d,x, main=paste(abc[i],timing[i]), ylab = "Latitude", xlab = "Longitude", cex=1.5, ylim = c(-33.55, -32.9), xlim = c(-71.6, -70.5))
			#if (length(unique(d$cluster))-1) {
			#	legend("bottomleft", inset=.05, title="Clusters", legend=seq(1,length(unique(d$cluster))-1), fill=head(tail(palette(), n=-1), n=length(unique(d$cluster))-1), horiz=TRUE)
			#}
			#else{
			#	legend("bottomleft", inset=.05, title="Clusters", legend="No clusters found", n=1, horiz=TRUE)
			#}
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
					cv[[acounter,4]] <- currval
					cv[[acounter,1]] <- timing[i]
					cv[[acounter,2]] <- density[den]
					cv[[acounter,3]] <- "NoZero"
					acounter <<- acounter + 1
				}
				else{
					currval <- cluster.stats(dist(x), d$cluster)
					currval$density <- density[den]
					currval$timeslot <- timing[i]
					currval$nozero <- FALSE
					cv[[acounter,4]] <- currval
					cv[[acounter,1]] <- timing[i]
					cv[[acounter,2]] <- density[den]
					cv[[acounter,3]] <- "Zero"
					acounter <<- acounter + 1
				}
			}
		}
	}
}
ggplot(facet_frame, aes(x=x, y=y)) + facet_wrap(~ time, ncol=2) + geom_point(colour="black", size = 6.5) + geom_point(colour="pink", size = 6) + geom_point(aes(shape = factor(clus), alpha = clus), size = 4) + ylab("Latitude") + xlab("Longitude") + labs(shape="Cluster", alpha="Cluster", colour="Cluster") + opts(axis.text.x = theme_text(angle = 340, colour = "grey50")) + scale_alpha(breaks=c(0,3,3,3),labels=c("Outliar","1","2","3")) + scale_shape(labels=c("Outliar","1","2","3"))
#ggplot(facet_frame, aes(x=x, y=y)) + stat_binhex() + facet_wrap(~ time, ncol=2)

# Cluster Analyze/graph:
# Perhaps use cluster cohesion. It's impossible to compute
# without the outliers (cluster 0) when there's only one cluster
for (time in 1:length(timing)) {
	for (row in 1:nrow(cv)) {
		if (!is.null(cv[row,][[1]]) && cv[row,][[3]] == "NoZero" && cv[row,][[1]] == timing[time]) {
			print(paste(cv[row,][[1]], cv[row,][[2]], cv[row,][[4]]$wb.ratio))
		}
	}
	#print("-------")
}
