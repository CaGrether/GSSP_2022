
# Loading libraries -------------------------------------------------------
library(stringr)

# Function to clean coordinates ----

clean_coords <- function(x){
  
  if(length(grep("^[0-9]+", x)) > 0){
    x <- strsplit(x, " |, ") [[1]]
    
    # negative or positive
    ewns <- ifelse(stringr::str_extract(x,"\\(?[EWNS]$") %in% c("E","N"),"","-")
    dms <- stringr::str_sub(x,1,stringr::str_length(x)-1)
    
    x <- paste0(ewns,dms) 
    
    z <- strsplit(x, "°|\\'|º")
    
    z <- sapply(z, strsplit, split='"')
    
    if(!is.matrix(z)) z <- matrix(unlist(z), ncol=length(z))
    
    if(nrow(z)<3) {
      n <- matrix(rep(rep(0, ncol(z)), 3-nrow(z)), ncol=ncol(z))
      z <- rbind(z, n)
    }
    
    z <- matrix(unlist(as.numeric(z)), nrow=nrow(z))
    
    
    x<-  z[1,] + z[2,]/60 + z[3,]/3600
    
  } else {
    x <- c(NA, NA)
    warningCondition("Not a coordinate, NA inserted")
  }
  return(x)
}

# Reading data ------------------------------------------------------------

affs <- readxl::read_excel("output/affiliations.xlsx")

# removing headers
affs <- affs[-which(affs$`GSSP Location`==affs$Stage),]

# removing everything with Enothem|Erathem|Period
affs <- affs[-grep("Eonothem|Erathem|Period", affs$Stage),]
affs$`Latitude, Longitude` <- NULL
affs$`GSSP Location` <- NULL

# Reading clean coordinates -----------------------------------------------

# getting list of files
stratf <- list.files(path="output", pattern="[A-Z][a-z]+\\.csv",
                     full.names = T)

coords <- do.call(rbind, lapply(stratf, function (x){
  read.csv(x, sep=";", fileEncoding = "UTF-8")[,c("Stage","GSSP.Location", "Latitude..Longitude", "Status")]
}))

#sometimes " written as '
coords$Latitude..Longitude <- gsub("\\'\\'", '\\"', coords$Latitude..Longitude)
coords$Latitude..Longitude <- gsub(";", ',', coords$Latitude..Longitude)

coords$Latitude..Longitude[coords$Stage=="Lochkovian Stage"] <- "49.8550°N 13.7920°E"
 
latlong <- lapply(coords$Latitude..Longitude, clean_coords)

latlong <- do.call(rbind, latlong)
colnames(latlong) <- c("lat", "lng")

#checking output
coords <- cbind(coords, latlong)

affs <- merge(affs, coords)

# Get countries -----------------------------------------------------------
library(countrycode)

# only ratified, check the anticipated ones

n <- which(!is.na(affs$lat)) # those with coords
length(n)
rat <- affs[n,]

countries <- unlist(lapply(strsplit(rat$GSSP.Location, ","), function(x) trimws(x[length(x)])))
countries[countries=="Chin"] <- "China"
countries[grep("England|Scotland", countries)] <- "UK"
countries[countries=="Nevada"] <- "USA"
countries[countries=="western Newfoundland"] <- "Canada"
rat$extracted_country <- countries

rat$country_code <- countrycode(countries, "country.name", "iso3c")
rat$country <- countrycode(rat$country_code, "iso3c", "country.name")

affs <- data.table::rbindlist(list(affs[-n,], rat), fill=T)

# Add age from GTS
library(fossilbrush)

gts <- fossilbrush::GTS2020
gts <- gts[gts$Type=="ICS Age",]

nrow(affs)
nrow(gts)

affs$Stage <- gsub(" Stage", "", affs$Stage)
affs$Stage[affs$Stage == "Upper"] <- "Upper Stage"

affs <- merge(affs, gts[,c("Interval", "FAD", "LAD")], by.x="Stage", by.y="Interval", all.x=T, all.y=F)
nrow(affs)
View(affs) # Stage 5 and Upper Stage missing

affs <- affs[order(affs$FAD, decreasing=T),]

openxlsx::write.xlsx(affs, file="data/data_cleaned.xlsx")
