#!usr/bin/Rscript

############
# LIBRARIES
############
library(googleway)
library(ggplot2)

############
# FUNCTIONS
############
processDirections <- function(origin, destination, mode){

	current_time <- Sys.time() + 1
	api_key <- 'AIzaSyAULkC6XdfhC2MDnLmO9tpZDlJmHjBhZ3U'

	result <- google_directions(origin=origin, destination=destination, mode=mode, key=api_key, departure_time = current_time)  # May need to process to get into desired dataframe.  May need to make time of day

	if(mode=='driving'){
		time <- result$routes$legs[[1]]$duration_in_traffic$value/60  # Given in seconds
		miles <- result$routes$legs[[1]]$distance$value/1600  # Given in meters, this converts to miles
	}

	if(mode=='transit'){
		time <- result$routes$legs[[1]]$duration$value/60
		miles <- result$routes$legs[[1]]$distance$value/1600
	}

	time_of_day <- substr(current_time, 12, 16)

	return(data.frame(commute_distance=miles, commute_length=time, commute_start=current_time, commute_start_hhmm = time_of_day, mode=mode))

}


############
# GET DATA
############
origin <- "your_start_location_or_address"
destination <- "your_end_location_or_address"

car <- processDirections(origin=origin, destination=destination, mode="driving")
bus <- processDirections(origin=origin, destination=destination, mode="transit")


############
# SAVE DATA
############
fileout <- paste0('/path/out/', gsub(" ", "", origin), '_', gsub(" ", "", destination))

write.table(rbind(car,bus), file=paste0(fileout, '.csv'), quote=FALSE, sep=',', append=TRUE, row.names=FALSE, col.names=FALSE)


############
# PLOT DATA
############
columns <- c('Commute Distance', 'Commute Time', 'Commute Start', 'Time of Day', 'Mode')
data <- read.csv(paste0(fileout,'.csv'), col.names=columns, header=FALSE)
levels(data$Mode)[1] <- 'Driving'
levels(data$Mode)[2] <- 'Transit'

xlabels <- sort(unique(data$Time.of.Day))

xbreaks <- NULL
for(i in 1:length(xlabels)){
	if(i %% 12 == 0){
		xbreaks[i] <- i
	}
	else{
		xbreaks[i] <- NA
	}
}

xbreaks[1] <- 1
xlabels <- xlabels[xbreaks]

pdf(paste0(fileout,'_Plotted.pdf'))
ggplot(data, aes(x=as.integer(Time.of.Day), y=Commute.Time, colour=Mode)) + geom_point(size=.5) + theme_bw() + theme(panel.grid.minor = element_blank(), panel.grid.major = element_blank(), legend.key = element_blank(), axis.text.x  = element_text(angle=90, vjust=0.5, size=16), axis.title.x = element_text(size=16), axis.text.y = element_text(size=16), axis.title.y = element_text(size=16), legend.text = element_text(size=16), legend.title = element_text(size=16)) + xlab("") + ylab("Commute Time") + stat_smooth(aes(group=Mode), method=loess, alpha=.25, size=.5, n=length(unique(data$Time.of.Day))) + scale_x_continuous(breaks=xbreaks, labels=xlabels) + scale_y_continuous(limits=c(10,120), breaks=seq(10,120,10), labels=seq(10, 120, 10))
dev.off()






