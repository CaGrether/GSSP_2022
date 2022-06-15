# GSSP Project

## webscraping GSSP tables

library(rvest)
library(xml2)

url <- "https://stratigraphy.org/gssps/"

gssp <- read_html(url, encoding = "UTF-8") # read the page
writeLines(as.character(gssp), con="pages/gssp.html", useBytes=T) #save page

# Load data

# Reading html file
gssp <- readLines("pages/gssp.html", encoding = "UTF-8")
gssp <- paste(gssp, collapse="\n")

# Scrape data
gssp <- read_html(gssp)

tables <- gssp %>% 
  html_nodes("table") %>% 
  html_table()

View(tables[[1]])
class(tables)
length(tables)

#add empty column to each table for affiliations
affiliations <- c(NA)
aff.tables <- mapply(cbind, tables, "Affiliations"= affiliations, SIMPLIFY = FALSE)

# getting the headers of the tables
heads<- gssp %>% 
  html_nodes("h4") %>% 
  html_text()

# Clean tables ------------------------------------------------------------
lapply(1:length(aff.tables),  
       function(i) {
         write.csv2(aff.tables[[i]], 
                   file = file.path("output", paste0(heads[i], ".csv")), 
                   row.names = F, 
                   fileEncoding = "UTF-8")
        
       })

########## read them correctly
lapply(1: length(tables),
       function(n) {
         read.csv(file = file.path("output", paste0(heads[n], ".csv")),
                  fileEncoding = "UTF-8")
        })

# find out countries of extinction horizons
which(heads=="Paleogene")
nrow(tables[[3]])
tables[[3]][15,1]
tables[which(tables)]