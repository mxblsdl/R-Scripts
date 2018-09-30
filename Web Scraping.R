## Web scraping
install.packages("tidyverse")
install.packages("rvest")
library(tidyverse)
library(rvest)
## Identify the url from where you want to extract data
base_url <-"https://www.basketball-reference.com/players/"
webpage <- read_html(base_url)
# Links to the player pages by letter
alphabet_links <- html_nodes(webpage, "#content li > a")
alphabet_links <- as.character(html_text(alphabet_links))
alphabet_links<-alphabet_links[1:25]
alphabet_links<-tolower(alphabet_links) # establishes end of url that applies to each webpage

# To loop
#Format the link to navigate to the basketball webpage
playerStats<-tibble()
for (i in 1:25) {
BBall.url <- paste0("https://www.basketball-reference.com/players/",alphabet_links[i],"/")

# Get the artist rank
BBnames<- html_nodes(BBall.Pages, "th a")
BBnames <- as.character(html_text(BBnames))

height<-html_nodes(BBall.Pages, ".center+ .right")
height<-as.character(html_text(height))

weight<-html_nodes(BBall.Pages, ".center~ .right+ .right")
weight<-as.numeric(html_text(weight))

bday<-html_nodes(BBall.Pages, ".right+ .left a")
bday<-as.character(html_text(bday))

careerStart<-html_nodes(BBall.Pages, ".left+ .right")
careerStart<-na.omit(as.numeric(html_text(careerStart)))

careerEnd<-html_nodes(BBall.Pages, ".right:nth-child(3)")
careerEnd<-na.omit(as.numeric(html_text(careerEnd)))

# Save it to a tibble
playerStats <-rbind(playerStats, tibble('Names'=BBnames,
                      height,
                      weight,
                      careerStart,
                      careerEnd))
}
h<-playerStats$height
playerStats$height<-sapply(strsplit(as.character(h),"-"),
     function(x){12*as.numeric(x[1]) + as.numeric(x[2])})
## figuring out carrer length in years
playerStats$CareerLength<-(playerStats$careerEnd-playerStats$careerStart+1)

## Splitting the first name and last name out
lastname<-vapply(strsplit(playerStats$Names," "), `[`, 2, FUN.VALUE=character(1))
firstname<-vapply(strsplit(playerStats$Names," "), `[`, 1, FUN.VALUE=character(1))
firstname
## Need to remove the periods from any first names as it messes up the urls later on
firstname <- sub("\\.", "", firstname)
## creating the different player urls
## need to make everything lowercase
player.urls <- paste0("https://www.basketball-reference.com/players/",tolower(strtrim(lastname,1)),"/",
                      tolower(strtrim(lastname, 5)),
                      tolower(strtrim(firstname, 2)),"01.html")

## creating the column names
col.names<-html_nodes(player.page,"#per_game .poptip")
col.names<-as.character(html_text(col.names))
col.names<-col.names[6:30]

stats<-list()
for (i in 1:length(player.urls)) {
  player.page<-read_html(player.urls[i])
  ## Call the stats per game for each player
  stats.perGame<-html_nodes(player.page, "#per_game tfoot tr:nth-child(1) .right")
  stats.perGame<-as.numeric(html_text(stats.perGame))
  ## add the stats to the tibble
  stats[[i]]<-stats.perGame
}
beepr::beep(3)

## Coerces into dataframe with correct column names
word.list<-stats
stat.matrix<-sapply(word.list, '[', seq(max(sapply(word.list, length))))
stat.matrix<-t(as.data.frame(stat.matrix))
colnames(stat.matrix)<-col.names
rownames(stat.matrix)<-paste0(lastname[1:193],",",firstname[1:193])


## Need to fix issue with incorrect url
browseURL(player.urls[157])
player.urls[157]<-"https://www.basketball-reference.com/players/p/pendeje02.html"
player.urls[194]<-"https://www.basketball-reference.com/players/b/bareajo01.html"
player.urls[233]<-"https://www.basketball-reference.com/players/b/batesbi01.html"
player.urls[373]<-"https://www.basketball-reference.com/players/b/bonsage01.html"
player.urls[616]<-"https://www.basketball-reference.com/players/c/capelca01.html"



