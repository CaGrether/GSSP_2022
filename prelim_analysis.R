# Analysis
library(countrycode)
library(divDyn)

data(stages) # get stages table from divDyn

# Read data
dat <- readxl::read_excel("data/data_cleaned.xlsx")
dat$region <- countrycode(dat$country_code, "iso3c", "region23")  

sort(table(dat$country))
sort(table(dat$region))


# Reshape affiliation data
affs <- dat[, c("Stage", "Affiliations", "country")]

s <- strsplit(affs$Affiliations, split = ";|\\/") # also include double affiliation
s <- lapply(s, trimws)

affs <- data.frame(Stage = rep(affs$Stage, sapply(s, length)), 
                   Affliations = unlist(s),
           gssp=rep(affs$country, sapply(s, length)))


affs$aff_code <- countrycode(affs$Affliations, "country.name", "iso3c") #Czechoslovakia, NA
affs$aff_country <- countrycode(affs$aff_code, "iso3c", "country.name")
affs$aff_region <- countrycode(affs$aff_code, "iso3c", "region23")

head(affs)

sort(table(affs$aff_country))
sort(table(affs$aff_region))

# count GSSP countries
sum(table(dat$country)) #75
# count ratified
sum(table(dat$Status)) #78 - 4 (anticipated or not ratified) = 74
# count affiliations
length(grep("NA", affs$Affliations)) #26 +2(Dapingian, Bartonian) -> 473-28 = 45

# Map ---------------------------------------------------------------------
library(ggplot2)

wrld <- map_data("world")

dat <- merge(dat, stages, by.x="Stage", by.y="stage", all.x=T) # add strat data

nas <- dat[is.na(dat$system),]
#write.csv2(as.data.frame(nas$system, nas$Stage), "output/nas.csv") #add system data

nas <- read.csv2("output/nas.csv")
names(nas) <- c("Stage", "system")

for (i in 1:length(nas$Stage)) {  
  dat$system[which(dat$Stage == nas$Stage[i])] <- paste(nas$system[i])
}

# adding affliations to map data
wrld$code <- countrycode(wrld$region, "country.name", "iso3c")

cnt <- data.frame(table(affs$aff_code))

wrld <- merge(wrld, cnt, by.x="code", by.y="Var1", all.x = T)

wrld <- wrld[order(wrld$group, wrld$order),]

# plotting map ------------------------------------------------------------

library(RColorBrewer)

## periods to eras

unique(dat$system)
paleo <- c("Cambrian", "Ordovician", "Silurian", "Devonian", "Carboniferous", "Permian")
meso <- c("Triassic", "Jurassic", "Cretaceous")
ceno <- c("Paleogene", "Neogene", "Quaternary")

dat$Era <- NA
dat$Era[which(dat$system %in% paleo)] <- "Paleozoic"
dat$Era[which(dat$system %in% meso)] <- "Mesozoic"
dat$Era[which(dat$system %in% ceno)] <- "Cenozoic"

# world
world.gssp <- ggplot() +
  geom_polygon(data=wrld, aes(long, lat, group=group, fill = Freq), # Freq = how many affs from that country
              colour = NA) +
  scale_fill_distiller(palette = "Greens", na.value = "grey40", direction = 1) +
  geom_point(data=dat, aes(x=lng, y=lat, col=Era), size=2, shape=21,
             stroke=1.5, fill="white") +
  scale_colour_manual(values = brewer.pal(9, "Set1")[c(5,2,8)])

world.gssp <- world.gssp +
  coord_equal(ratio=1.2) +
  theme_void() 

world.gssp

# europe
library(sf)
europe <- world.gssp + 
  coord_equal(ratio = 1.2, xlim=c(-9.5, 28.2), ylim=c(35, 61)) 
europe

library(patchwork)

#svg("figs/world.svg", w=8, h=6)

world.gssp + europe +
  plot_annotation(tag_levels = "A") +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom", legend.justification = "left", legend.box = "vertical",
        legend.box.just = "left")

#dev.off()
#browseURL("figs/world.svg")


# network analysis --------------------------------------------------------

######## foreign

library(tidyverse)

sources <- affs %>%
  distinct(aff_region) %>%
  rename(label = aff_region)
destinations <- dat %>% 
  distinct(region) %>% 
  rename(label = region)

nodes <- full_join(sources, destinations, by = "label") %>% 
  na.omit()
nodes <- nodes %>% 
  rowid_to_column()

# sort by time
datsrt <- dat[order(dat$FAD, decreasing = T),]

sad <- data.frame(Sources = affs$aff_region, 
                  Destinations  = rep(datsrt$region, sapply(s, length)))

per_paper <- sad %>% 
  group_by(Sources, Destinations) %>% 
  summarise(weight = n()) %>% 
  ungroup()

edges <- per_paper %>% 
  left_join(nodes, by = c("Sources" = "label")) %>% 
  rename(from = rowid)
edges <- edges %>% 
  left_join(nodes, by = c("Destinations" = "label")) %>% 
  rename(to = rowid)

edges <- select(edges, from, to, weight)


library(tidygraph)
library(ggraph)

papers <- tbl_graph(nodes = nodes, edges = na.omit(edges), directed = TRUE) #take out rows with NA values #Anisian

papers


foreign <- ggraph(papers, layout = "linear") + 
  geom_edge_arc(aes(width = weight, 
                    col=weight), alpha = 0.6) + 
  scale_edge_width(range = c(0.2, 2), breaks=c(2,5,10)) +
  scale_edge_colour_gradient(low="#defcd6", high="darkgreen") +
  geom_node_text(aes(label=stringr::str_wrap(label, 17)), repel = TRUE, colour = "black", size = 3) +
  labs(edge_width = "Contributions") +
  theme_graph() +
  theme(legend.position = "bottom")



#svg("figs/foreign.svg", w=8, h=6)
foreign
#dev.off()
#browseURL("figs/foreign2.svg")


################ own


# sort by time
datsrt <- dat[order(dat$FAD, decreasing = T),]

# plot every GSSP country
gssp.cnt <- as.data.frame(datsrt$country_code)
names(gssp.cnt) <- "country_code"


sources <- affs %>%
  distinct(aff_code) %>%
  rename(label = aff_code)
destinations <- as.data.frame(unique(gssp.cnt)) 
names(destinations) <- "label"

nodes <- full_join(sources, destinations, by = "label") %>% 
  na.omit()
nodes <- nodes %>% 
  rowid_to_column()


sad <- data.frame(Sources = affs$aff_code, 
                  Destinations  = rep(gssp.cnt$country_code, sapply(s, length))) #whyyyyyyyyy??????????


per_paper <- sad %>% 
  group_by(Sources, Destinations) %>% 
  summarise(weight = n()) %>% 
  ungroup()

edges <- per_paper %>% 
  left_join(nodes, by = c("Sources" = "label")) %>% 
  rename(from = rowid)
edges <- edges %>% 
  left_join(nodes, by = c("Destinations" = "label")) %>% 
  rename(to = rowid)

edges <- select(edges, from, to, weight)

papers <- tbl_graph(nodes = nodes, edges = na.omit(edges), directed = TRUE) #take out rows with NA values 
                                                                            #Anisian: Paper, but no coordinates available
                                                                            #when NA in "from", then no paper available for the GSSP
papers

# one dataframe for own
own.sad <- sad %>% 
  subset(sad$Sources==sad$Destinations, ) 
own.cnt <- as.data.frame(sort(table(own.sad$Sources)))
names(own.cnt) <- c("Country", "Frequency")

# one dataframe for others
other.sad <- sad %>% 
  subset(sad$Sources!=sad$Destinations, )
other.cnt <- as.data.frame(unique(sort(other.sad$Sources)))
names(other.cnt) <- "Country"
other.cnt$Frequency <- 0

# exclude countries which are in own.cnt from other.cnt
other.cnt <- other.cnt[-which(other.cnt$Country%in% own.cnt$Country),]
all.cnt <- full_join(other.cnt, own.cnt)

all.cnt$Frequency <- all.cnt$Frequency+1 # plus 1 to each frequency for plotting purpose

all.cnt

# colouring regions
all.rgn <- countrycode(all.cnt$Country, "iso3c", "region23")
all.cnt <- cbind(all.cnt, all.rgn)


# circle diagram
library(packcircles)

packing <- circleProgressiveLayout(all.cnt$Frequency, sizetype='area')
packing$radius <- 0.95*packing$radius
all.cnt <- cbind(all.cnt, packing)

all.gg <- circleLayoutVertices(packing, npoints=50)
all.gg$region <- rep(all.cnt$all.rgn, each=51)

#plot
all <- ggplot() + 
  geom_polygon(data = all.gg, aes(x, y, group = id, fill=region), colour = "black", alpha = 0.6) +
  scale_fill_manual(values = c("#fcfc03", "#cfcf02", "#b3fc08", "#4efc03", "#03fc73", 
                               "#1ab305", "#027543", "#05b38a", "#95cc68", "#ad8802")) +
  geom_text(data = all.cnt, aes(x, y, size=3, label = Country)) +
  scale_size_continuous(range = c(1,4)) +
  theme_void() + 
  theme(legend.position="right") +
  guides(fill=guide_legend(title="Region")) +
  guides(size = "none") +
  coord_equal()

all

#svg("figs/all.svg", w=8, h=6)
all
#dev.off()
browseURL("figs/all.svg")