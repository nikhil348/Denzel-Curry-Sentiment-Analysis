library(genius)
library(tidyverse)
library(corpus)
library(tidytext)
library(scales)
library(ggthemes)
library(DT)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(RCurl)
library(XML)



setwd("Users/nikhilsharma//Documents/Denzel_SA")



##create functions to scrape and do sentiment analysis

nrc <- get_sentiments("nrc") ##save nrc dictionary as df
get_album_sentiments <- function(artist, album){
  df <- genius_album(artist = artist, album = album) ##scrape with genius package
  df <- df[, -c(2:3)] ##take out unnecessary columns
  df$lyric_stem <- text_tokens(df$lyric, stemmer = "en") ##lemmatize lyrics
  df <- data.frame(track_title = rep(df$track_title, sapply(df$lyric_stem, length)), word = unlist(df$lyric_stem)) ##make clean df
  df_sentiment <- merge(df, nrc, by = "word") ##inner join for sentiment counts
  df_sentiment <- as.data.frame(df_sentiment)
  return (df_sentiment)
}
get_album_sentiments_not_lem <- function(artist, album){
  df <- genius_album(artist = artist, album = album) ##scrape with genius package
  df <- df[, -c(2:3)] ##take out unnecessary columns
  return (df)
}


##scrape denzel's lyrics

albums <- c("Nostalgic 64","32 Zel/Planet Shrooms","Imperial", "13","TA13OO")
list_albums <- lapply(albums, get_album_sentiments, artist = "Denzel Curry")
for (i in 1:5) {list_albums[[i]]$album <- albums[i]}
sent_all_albums <- bind_rows(list_albums)
list_albums_not_lem <- lapply(albums, get_album_sentiments_not_lem, artist = "Denzel Curry")
for (i in 1:5) {list_albums_not_lem[[i]]$album <- albums[i]}
sent_all_albums_not_lem <- bind_rows(list_albums_not_lem)


##create wordcloud

set.seed(123)
wordcloud(sent_all_albums_not_lem$lyric, type ="text", lang = "english", colors=brewer.pal(8, "Dark2"), max.words = 300)



sent_all_albums <- sent_all_albums %>%  arrange(track_title)


##fix cash maniac sentiment counts

sent_all_albums[967:1179,]$track_title <- "CASH MANIAC | CAZH MAN1AC (ft. Nyyjerya)"
target <- c("anger", "fear")
cashmoney <- c("cash", "money")
sent_all_albums <- sent_all_albums %>%  filter(!(track_title=="CASH MANIAC | CAZH MAN1AC (ft. Nyyjerya)" & sentiment %in% target & word %in% cashmoney))


##create plot for sentiments across all albums

sent_all_albums$facet = factor(sent_all_albums$album, levels = c("Nostalgic 64", "32 Zel/Planet Shrooms", "Imperial", "13", "TA13OO"))
all_albums_plot <- ggplot(data = sent_all_albums, aes(x = sentiment, y = 1, color = album)) + 
  geom_bar(stat = "identity", width = 0.675) + 
  labs(x = "Sentiment", y = "Sentiment Counts") +
  ggtitle("Sentiment Counts Per Album") +
  theme(plot.title = element_text(size=20, face="bold"), legend.position = "none") +
  facet_wrap(~facet, ncol = 1) +
  scale_x_discrete(labels=c("anger" = "Anger","anticipation" = "Anticipation","disgust" = "Disgust","fear" = "Fear","joy" = "Joy","negative" = "Negative","positive" = "Positive","sadness" = "Sadness","surprise" = "Surprise","trust" = "Trust"))
all_albums_plot



ggsave(filename="albums.png", plot=all_albums_plot, width=240, height=410, units = "mm")


##get data for sentiment counts for each song

all_song_sentiments <- sent_all_albums %>% group_by(sentiment, track_title, album) %>% summarise(Freq=n()) %>% spread(sentiment, Freq)
all_song_sentiments[is.na(all_song_sentiments)] <- 0
write.csv(all_song_sentiments, file="allsongs.csv")



##taboo

taboo <- sent_all_albums %>% filter (album == "TA13OO")



taboo_sents <- taboo %>% group_by(sentiment, track_title) %>% summarise(Freq=n()) %>% spread(sentiment, Freq)
colnames(taboo_sents) <- c("Track Title","Anger","Anticipation","Disgust","Fear","Joy","Negative","Positive","Sadness","Surprise","Trust")


##filter and prep data for light act

light <- taboo_sents[c(11,1,3,8),]



sums <- as.data.frame(colSums(light[,2:11]))
colnames(sums)[1] <- "Count"
sums$Sentiment <- rownames(sums)
sums <- sums[,2:1]


##create plot for sentiments in light act

lg <- ggplot(data = sums, aes(x=Sentiment, y=Count)) + 
  geom_bar(stat="identity", show.legend=FALSE, fill = "gray70", width = 0.675) + 
  scale_colour_fivethirtyeight() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  ggtitle("Act 1: Light", subtitle = "TA13OO") +
  xlab("Sentiment") +
  ylab("Sentiment Counts")+
  coord_cartesian(ylim=c(0,200))
lg


##filter and prep data for gray act

grey <- taboo_sents[c(9,10,5,7,4),]



sums2 <- as.data.frame(colSums(grey[,2:11]))
colnames(sums2)[1] <- "Count"
sums2$Sentiment <- rownames(sums2)
sums2 <- sums2[,2:1]


##create plot for sentiments in gray act

gg <- ggplot(data = sums2, aes(x=Sentiment, y=Count)) + 
  geom_bar(stat="identity", show.legend=FALSE, fill = "gray50", width = 0.675) + 
  scale_colour_fivethirtyeight() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  ggtitle("Act 2: Gray", subtitle = "TA13OO") +
  xlab("Sentiment") +
  ylab("Sentiment Counts")+
  coord_cartesian(ylim=c(0,200))
gg


##filter and prep data for dark act

dark <- taboo_sents[c(12,6,13,2),]



sums3 <- as.data.frame(colSums(dark[,2:11]))
colnames(sums3)[1] <- "Count"
sums3$Sentiment <- rownames(sums3)
sums3 <- sums3[,2:1]


##create plot for sentiments in dark act

dg <- ggplot(data = sums3, aes(x=Sentiment, y=Count)) + 
  geom_bar(stat="identity", show.legend=FALSE, fill="black", width = 0.675) + 
  scale_colour_fivethirtyeight() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  ggtitle("Act 3: Dark", subtitle = "TA13OO") +
  xlab("Sentiment") +
  ylab("Sentiment Counts") +
  coord_cartesian(ylim=c(0,200))
dg



ggsave(filename="lg.png", plot=lg)
ggsave(filename="gg.png", plot=gg)
ggsave(filename="dg.png", plot=dg)


##get data for sentiment counts across album acts

colSums(light[,2:11])
colSums(grey[,2:11])
colSums(dark[,2:11])



summarysent <- rbind(colSums(light[,2:11]), colSums(grey[,2:11]), colSums(dark[,2:11]))
summarysent



write.csv(summarysent, file = "summary.csv")
