##Getting the Data##


Disney <- read.csv("https://query.data.world/s/btiz7vu77feguves7tzgd4omhkxseh", header=TRUE, stringsAsFactors=FALSE)


##Overall Structure of Dataset##


summary(Disney)
str(Disney)
head(Disney)
tail(Disney)


##Cleaning the Data##


#Changing column names:

names(Disney)[1]<- "MovieTitle" 
names(Disney)[2]<- "ReleaseDate"
names(Disney)[3]<- "Genre"
names(Disney)[4]<- "MPAARating"
names(Disney)[5]<- "TotalGrossRevenue"
names(Disney)[6]<- "InflationAdjustedGrossRevenue"


#Coercing variable types:

Disney$Genre<- as.factor(Disney$Genre)
Disney$MPAARating<- as.factor(Disney$MPAARating)

Disney$TotalGrossRevenue = as.numeric(gsub("[\\$,]", "", Disney$TotalGrossRevenue)) #Make Total Gross Revenue column a numeric variable

Disney$InflationAdjustedGrossRevenue = as.numeric(gsub("[\\$,]", "", Disney$InflationAdjustedGrossRevenue)) #Make Inflation Adjusted Gross Revenue column a numeric variable

Disney$ReleaseDate = gsub("[,]", "", Disney$ReleaseDate) #Eliminate commas in Release Date observations
Disney$ReleaseDate = (strptime(Disney$ReleaseDate, format = "%b %e %Y" )) #Make Release Date column a date variable
Disney$ReleaseDate = as.POSIXct(Disney$ReleaseDate)


#Verify class variables:

class(Disney$Genre)
class(Disney$MPAARating)
class(Disney$TotalGrossRevenue)
class(Disney$InflationAdjustedGrossRevenue)
class(Disney$ReleaseDate) 


#Removing values:

Disney$TotalGrossRevenue[Disney$TotalGrossRevenue<1]<- NA #Clean any data point with Revenue=0
Disney$InflationAdjustedGrossRevenue[Disney$InflationAdjustedGrossRevenue<1]<- NA ##Clean any data point with Adjusted Revenue=0

Disney$MPAARating[Disney$MPAARating=="Not Rated"]<- NA #Make NA movies that are Not Rated
Disney$MPAARating[Disney$MPAARating==""]<- NA #Make NA Observations that are empty
Disney$Genre[Disney$Genre==""]<- NA #Make NA Observations that are empty


#Counting NA Values:
table(is.na(Disney)) #Check how many NA values in the dataset
table(is.na(Disney$MovieTitle)) #Check how many NA values in each column
table(is.na(Disney$ReleaseDate))
table(is.na(Disney$Genre))
table(is.na(Disney$MPAARating))
table(is.na(Disney$TotalGrossRevenue))
table(is.na(Disney$InflationAdjustedGrossRevenue))


summary(Disney) #Look at summary of clean dataset


##Exploratory Analysis##


library(ggplot2) #Call ggplot2 library


ggplot(Disney, aes(ReleaseDate, TotalGrossRevenue)) + #Relationship between release date and revenue
  geom_line(color="red") +
  theme_light()
  

ggplot(Disney, aes(ReleaseDate, InflationAdjustedGrossRevenue)) + #Relationship between release date and inflation adjusted revenue
  geom_line(color="blue") +
  theme_light()

ggplot(Disney, aes(ReleaseDate, TotalGrossRevenue)) + #Comparison between Total Gross Revenue and Inflation Adjusted Gross Revenue overtime
  geom_line(color="red") +
  geom_line(color="blue", aes(ReleaseDate, InflationAdjustedGrossRevenue)) +
  theme_light()

ggplot(Disney, aes(ReleaseDate, color=Genre)) + #Relationship between Release Date and Genre
  geom_freqpoly() + 
  theme_light()


##Relationship between Genres and Inflation Adjusted Revenues##

ggplot(Disney, aes(Genre)) + #Frequency of each Genre
  geom_bar(fill="pink") + 
  theme_light()

ggplot(Disney, aes(x="", y=frequency(MPAARating), fill=MPAARating)) + #Pie chart of frequency of each Genre
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) +
  theme_void()

ActionRev<-0 #Loop to compute the sum of total Inflation Adjusted Revenue for Genre "Action"
for(i in 1:nrow(Disney)) {
  if(is.na(Disney$Genre[i])||is.na(Disney$InflationAdjustedGrossRevenue[i])) {
    ActionRev<- ActionRev + 0
  } else {
    if(Disney$Genre[i]=="Action") {
      ActionRev<- ActionRev + Disney$InflationAdjustedGrossRevenue[i]
    }
  }
}  

AdventureRev<- 0 #Loop to compute the sum of total Inflation Adjusted Revenue for Genre "Adventure"
for(i in 1:nrow(Disney)) {
  if(is.na(Disney$Genre[i])||is.na(Disney$InflationAdjustedGrossRevenue[i])) {
    ActionRev<- ActionRev + 0
  } else {
    if(Disney$Genre[i]=="Adventure") {
      AdventureRev<- AdventureRev + Disney$InflationAdjustedGrossRevenue[i]
    }
  }
}  

BlackComedyRev<- 0 #Loop to compute the sum of total Inflation Adjusted Revenue for Genre "Black COmedy"
for(i in 1:nrow(Disney)) {
  if(is.na(Disney$Genre[i])||is.na(Disney$InflationAdjustedGrossRevenue[i])) {
    ActionRev<- ActionRev + 0
  } else {
    if(Disney$Genre[i]=="Black Comedy") {
      BlackComedyRev<- BlackComedyRev + Disney$InflationAdjustedGrossRevenue[i]
    }
  }
}  


ComedyRev<- 0 #Loop to compute the sum of total Inflation Adjusted Revenue for Genre "Comedy"
for(i in 1:nrow(Disney)) {
  if(is.na(Disney$Genre[i])||is.na(Disney$InflationAdjustedGrossRevenue[i])) {
    ActionRev<- ActionRev + 0
  } else {
    if(Disney$Genre[i]=="Comedy") {
      ComedyRev<- ComedyRev + Disney$InflationAdjustedGrossRevenue[i]
    }
  }
}  

ConcertPerfRev<- 0 #Loop to compute the sum of total Inflation Adjusted Revenue for Genre "Concert/Performance"
for(i in 1:nrow(Disney)) {
  if(is.na(Disney$Genre[i])||is.na(Disney$InflationAdjustedGrossRevenue[i])) {
    ActionRev<- ActionRev + 0
  } else {
    if(Disney$Genre[i]=="Concert/Performance") {
      ConcertPerfRev<- ConcertPerfRev + Disney$InflationAdjustedGrossRevenue[i]
    }
  }
}  

DocumentaryRev<- 0 #Loop to compute the sum of total Inflation Adjusted Revenue for Genre "Documentary"
for(i in 1:nrow(Disney)) {
  if(is.na(Disney$Genre[i])||is.na(Disney$InflationAdjustedGrossRevenue[i])) {
    ActionRev<- ActionRev + 0
  } else {
    if(Disney$Genre[i]=="Documentary") {
      DocumentaryRev<- DocumentaryRev + Disney$InflationAdjustedGrossRevenue[i]
    }
  }
}  

DramaRev<- 0 #Loop to compute the sum of total Inflation Adjusted Revenue for Genre "Drama"
for(i in 1:nrow(Disney)) {
  if(is.na(Disney$Genre[i])||is.na(Disney$InflationAdjustedGrossRevenue[i])) {
    ActionRev<- ActionRev + 0
  } else {
    if(Disney$Genre[i]=="Drama") {
      DramaRev<- DramaRev + Disney$InflationAdjustedGrossRevenue[i]
    }
  }
}  

HorrorRev<- 0#Loop to compute the sum of total Inflation Adjusted Revenue for Genre "Horror"
for(i in 1:nrow(Disney)) {
  if(is.na(Disney$Genre[i])||is.na(Disney$InflationAdjustedGrossRevenue[i])) {
    ActionRev<- ActionRev + 0
  } else {
    if(Disney$Genre[i]=="Horror") {
      HorrorRev<- HorrorRev + Disney$InflationAdjustedGrossRevenue[i]
    }
  }
}  

MusicalRev<- 0 #Loop to compute the sum of total Inflation Adjusted Revenue for Genre "Musical"
for(i in 1:nrow(Disney)) {
  if(is.na(Disney$Genre[i])||is.na(Disney$InflationAdjustedGrossRevenue[i])) {
    ActionRev<- ActionRev + 0
  } else {
    if(Disney$Genre[i]=="Musical") {
      MusicalRev<- MusicalRev + Disney$InflationAdjustedGrossRevenue[i]
    }
  }
}  

RomComRev<- 0 #Loop to compute the sum of total Inflation Adjusted Revenue for Genre "Romantic Comedy"
for(i in 1:nrow(Disney)) {
  if(is.na(Disney$Genre[i])||is.na(Disney$InflationAdjustedGrossRevenue[i])) {
    ActionRev<- ActionRev + 0
  } else {
    if(Disney$Genre[i]=="Romantic Comedy") {
      RomComRev<- RomComRev + Disney$InflationAdjustedGrossRevenue[i]
    }
  }
}  

ThrillerSuspRev<- 0 #Loop to compute the sum of total Inflation Adjusted Revenue for Genre "Thriller/Suspense"
for(i in 1:nrow(Disney)) {
  if(is.na(Disney$Genre[i])||is.na(Disney$InflationAdjustedGrossRevenue[i])) {
    ActionRev<- ActionRev + 0
  } else {
    if(Disney$Genre[i]=="Thriller/Suspense") {
      ThrillerSuspRev<- ThrillerSuspRev + Disney$InflationAdjustedGrossRevenue[i]
    }
  }
}  

WesternRev<- 0 #Loop to compute the sum of total Inflation Adjusted Revenue for Genre "Western"
for(i in 1:nrow(Disney)) {
  if(is.na(Disney$Genre[i])||is.na(Disney$InflationAdjustedGrossRevenue[i])) {
    ActionRev<- ActionRev + 0
  } else {
    if(Disney$Genre[i]=="Western") {
      WesternRev<- WesternRev + Disney$InflationAdjustedGrossRevenue[i]
    }
  }
}  

GenreRevs<- c(ActionRev, AdventureRev, BlackComedyRev, ComedyRev, ConcertPerfRev, DocumentaryRev, DramaRev, HorrorRev, MusicalRev, RomComRev, ThrillerSuspRev, WesternRev) #Create vector with Revenue values
NamesGenre<- c("ActionRev", "AdventureRev", "BlackComedyRev", "ComedyRev", "ConcertPerfRev", "DocumentaryRev", "DramaRev", "HorrorRev", "MusicalRev", "RomComRev", "ThrillerSuspRev", "WesternRev") #Create vector with Genre names
Genres<- data.frame(NamesGenre, GenreRevs) #Create dataset with Genre and its Revenues

ggplot(Genres, aes(x="", y=GenreRevs, fill=NamesGenre)) + #Pie chart of Genres according to Inflation Adjusted Revenue
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) +
  theme_void()


##Relationship between MPAARatings and Inflation Adjusted Revenue##

ggplot(Disney, aes(MPAARating)) + #Frequency of each Rating
  geom_bar(fill="pink") + 
  theme_light()

ggplot(Disney, aes(x="", y=frequency(MPAARating), fill=MPAARating)) + #Pie chart of frequency of MPAARatings
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) +
  theme_void()

#Simpler way to subset Revenues based on another variable (MPAARating):
GRev<- sum(subset(Disney$InflationAdjustedGrossRevenue, Disney$MPAARating=="G"), na.rm=TRUE) #Sum of Inflation Adjusted Revenue for every G Rated Movie
PGRev<- sum(subset(Disney$InflationAdjustedGrossRevenue, Disney$MPAARating=="PG"), na.rm=TRUE) #Sum of Inflation Adjusted Revenue for every PG Rated Movie
PG13Rev<- sum(subset(Disney$InflationAdjustedGrossRevenue, Disney$MPAARating=="PG-13"), na.rm=TRUE) #Sum of Inflation Adjusted Revenue for every PG-13 Rated Movie
RRev<- sum(subset(Disney$InflationAdjustedGrossRevenue, Disney$MPAARating=="R"), na.rm=TRUE) #Sum of Inflation Adjusted Revenue for every R Rated Movie

NamesRating<- c("G_Rev", "PG_Rev", "PG13_Rev", "R_Rev")
RatingRev<- c(GRev, PGRev, PG13Rev, RRev)

Rating<- data.frame(NamesRating, RatingRev) #Creating a dataset for MPAARatings and their respective Total Inflation Adjusted Revenues

ggplot(Rating, aes(x="", y=RatingRev, fill=NamesRating)) + #Pie chart of MPAARatings according to Inflation Adjusted Revenue
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) +
  theme_void()

