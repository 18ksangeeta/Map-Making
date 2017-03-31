library(raster) # library to get map shape file
library(ggplot2) # for plotting and miscellaneuous things
library(ggmap) # for plotting
library(plyr) # for merging datasets
library(scales) # to get nice looking legends
library(maps)

usa.df <- map_data("state") # Get a shape file of states in the US
colnames(usa.df)[5] <- "State" # changing region to state value to match with the data.

# Get the data to be plotted
usa.dat <- read.table("us_2016_election_data.csv", header = T, sep = ",") #reading the data from the csv file
usa.dat$State <- tolower(usa.dat$State) #changing to lower case of the State column attributes of the data
usa.dat$Clinton=as.numeric(gsub("%", "", usa.dat$Clinton)) #removing % symbol character and converting into numeric format for ploting and calculations
usa.dat$Trump=as.numeric(gsub("%", "", usa.dat$Trump)) #removing % symbol character and converting into numeric format for ploting and calculations
usa.dat <- usa.dat[c("State", "Clinton", "Trump")] # retrieving and storing the required collection of data in the same variable
usa.dat$Clinton <- ifelse((usa.dat$Clinton - usa.dat$Trump)>0,(usa.dat$Clinton - usa.dat$Trump+140), (usa.dat$Clinton - usa.dat$Trump-10)) #To get the difference of data between Clinton and Trump to find out who had won with majority and also for the color differentiation added the constant values 140 and -10 for brighter colors.

# Merge the data collected with the shape file
usa.df <- join(usa.df, usa.dat, by = "State", type = "inner")

# Abbreviations of states and where thy should be plotted
states <- data.frame(state.center, state.abb) # centers of states and abbreviations
states <- states[subset, ] 

# Plotting function
p <- function(data, title) {
	ggp <- ggplot() + 
#		To draw borders of states
		geom_polygon(data = data, aes(x = long, y = lat, group = group, fill = Clinton), color = "black", size = 0.15) + 
# 		Use shades of red and blue for plotting; trans = "reverse" option 
# 		makes the shades go from dark to light as the income share increases, 
#		ensuring that darkest blue = worst case scenario that indiactes Trump has more percentage votes.
#		Here breaks and labels should have same length so for the break we provide a label to differentiate Trump and Clinton results instead of listing all the percentage values
		scale_fill_distiller(palette = "RdBu", breaks=c(-50,220),labels = c("Trump","Clinton"),
		trans = "reverse") + 
#		Add legend
		theme_nothing(legend = TRUE) + labs(title = title, fill = "") + 
#		Add state abbreviations		
		geom_text(data = states, aes(x = x, y = y, label = state.abb), size = 3)
	return(ggp)
}

figure.title <- "2016 USA presidential elections"

# Save the map to a file to viewing (you can plot on the screen also, but it takes
# much longer that way. The ratio of US height to width is 1:9.)

ggsave(p(usa.df, figure.title), height = 4, width = 4*1.9,
	 file = "usa_presidential_elections.jpg")


