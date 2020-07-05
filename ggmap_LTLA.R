library(ggplot2)
library(ggmap)
library(dplyr)
register_google(key = "AIzaSyDCKkaKtYE6Xqa0PXBn38wEWTdWEHxmG4Y", write = TRUE)
#Load in latest C19 case data
temp <- tempfile()
source <- "https://coronavirus.data.gov.uk/downloads/csv/coronavirus-cases_latest.csv"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
data <- fread(temp)
data$`Specimen date` <- as.Date(data$`Specimen date`)
c19 <- data

#Filter to get a list of LTLA-level data from X date(s)
LTLA <- filter(c19, c19$`Area type` == "Lower tier local authority")
LTLA_id <- select(LTLA, 1, 2, 4, 8)
LTLA <- select(LTLA, 1, 4, 8)
LTLAdf <- as.data.frame(unique(LTLA$`Area name`))
colnames(LTLAdf) <- "Area.name"
names(LTLA) <- c("Area.name", "Specimen date", "Cumulative lab-confirmed cases")

LTLA_27_june <-  filter(LTLA, LTLA$`Specimen date` == "2020-06-27")
LTLA_26_june <-  filter(LTLA, LTLA$`Specimen date` == "2020-06-26")
LTLA_25_june <-  filter(LTLA, LTLA$`Specimen date` == "2020-06-25")
LTLA_24_june <-  filter(LTLA, LTLA$`Specimen date` == "2020-06-24")
LTLA_23_june <-  filter(LTLA, LTLA$`Specimen date` == "2020-06-23")
LTLA_22_june <-  filter(LTLA, LTLA$`Specimen date` == "2020-06-22")
LTLA_21_june <-  filter(LTLA, LTLA$`Specimen date` == "2020-06-21")

c <- full_join(LTLAdf,LTLA_27_june, by = "Area.name")
c <- full_join(c,LTLA_26_june, by = "Area.name")
c <- full_join(c,LTLA_25_june, by = "Area.name")
c <- full_join(c,LTLA_24_june, by = "Area.name")
c <- full_join(c,LTLA_23_june, by = "Area.name")
c <- full_join(c,LTLA_22_june, by = "Area.name")
c <- full_join(c,LTLA_21_june, by = "Area.name")

#Gives a list of columns which are not NAs (excluding column 1)
which(!is.na(c[1,]))[which(!is.na(c[1,])) != 1]


#Apply a loop to generate a new list (d) with these columns
d <- list()
for(row in 1:nrow(c)){
col1 <- min(which(!is.na(c[row,]))[which(!is.na(c[row,])) != 1])
col2 <- as.integer(min(which(!is.na(c[row,]))[which(!is.na(c[row,])) != 1]) + 1)
col3 <- as.integer(max(which(!is.na(c[row,]))[which(!is.na(c[row,])) != 1]) - 1)
col4 <- max(which(!is.na(c[row,]))[which(!is.na(c[row,])) != 1])
cols <- c(col1, col2, col3, col4)
d[[row]]<-(c[row, cols])
}

#Convert the list d into a matrix df, then a data frame df2
df <- data.frame(matrix(unlist(d), nrow=length(d), byrow=T))
df2<-as.data.frame(df)
colnames(df2) <- c("c1", "c2", "c3", "c4")

#Find the differences in time and cases, then calculated a rate of change
df2$difftime <- df2$c1 - df2$c3
df2$diffcases <- df2$c2 - df2$c4
df2$rate_of_change <- df2$diffcases / df2$difftime
df2$c1 <- as.Date(df2$c1, origin = "1970-01-01")
df2$c3 <- as.Date(df2$c3, origin = "1970-01-01")
colnames(df2) <- c("most_recent_date", "most_recent_cases", "least_recent_date", "least_recent_cases", "diff_time", "diff_cases", "rate_of_change")


#Combine into a complete table (e)
e <- cbind(c, df2)

#Remove NAs, replacing them with 0s
e$rate_of_change[is.na(e$rate_of_change)] <- 0

#Use names appended with , UK to search Google and fill in the blanks from the postcode search
f <- data.frame(e)
f$Area.name <- paste(e$Area.name,", UK", sep = "")
k <- as.data.frame(geocode(as.character(f$Area.name)))
map_data <- cbind(e, k)

map <- get_map('Leicester, UK', zoom = 7, color = "bw", maptype = "hybrid")
map <- get_map('london, uk', zoom = 9, color = "bw", maptype = "hybrid")
ggmap(map) + geom_point(data = map_data, aes(x = lon, y = lat), color='red', alpha=0.5, size = 3*map_data$rate_of_change)
ggmap(map) + geom_point(data = map_data, aes(x = lon, y = lat), color='blue', alpha=0.5, size = 0.01*map_data$Cumulative.lab.confirmed.cases.x)
ggmap(map) + geom_point(data = map_data, aes(x = lon, y = lat), color='blue', alpha=0.5, size = 0.01*map_data$Cumulative.lab.confirmed.cases.x) + geom_point(data = map_data, aes(x = lon, y = lat), color='red', alpha=0.5, size = 3*map_data$rate_of_change)

postcodes <- read.csv("Postcodes___Local_Authorities_only_v01.csv")
LTLA_id <- filter(LTLA_id, LTLA_id$Specimen.date == "2020-06-27")
g <- data.frame()
for (row in 1:nrow(LTLA_id)) {
  g[row,1] <- as.character(postcodes[match(as.character(LTLA_id[row,2]),postcodes$Local.Authority.Code),3])
}
g <- as.data.frame(g)
colnames(g)[1] <- "example_postcode"
LTLA_id$example_postcode <- g$example_postcode
LTLA_id <- LTLA_id[complete.cases(LTLA_id), ]
df <- as.data.frame(geocode(as.character(LTLA_id$example_postcode)))






#Find the first mention of an area code
codes <- unique(c19$Area.code)
match(codes, c19$Area.code)


f0 = function(c) {
  idx = ifelse(is.na(c), 0L, col(c))
  apply(idx, 1, max)
}


map = ggmap(base) + 
  stat_density2d(aes(x = lon, y = lat, fill = ..level.., alpha = ..level..),
                 bins = 5, geom = "polygon",
                 data = map_data) +
  scale_fill_gradient(low = "black", high = "red")
