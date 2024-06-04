#### date data
library(tidytext)
library(dslabs)
library(tidyverse)
class(polls_us_election_2016$startdate)

as.numeric(polls_us_election_2016$startdate) %>% head(5)

# epoch
as.Date('1970-01-01') %>% as.numeric


polls_us_election_2016%>%filter(pollster=="Ipsos"&state=="U.S.")%>%
  ggplot(aes(startdate,rawpoll_trump))+
  geom_line()
### ggplot会自动注明月份

## lubridate package
library(lubridate)
set.seed(0530)
dates <- sample(polls_us_election_2016$startdate,10) %>% 
  sort
dates
tibble(date=dates,month=month(dates),day=day(dates),year=year(dates))

month(dates,label=T)

## YYYY-MM-DD
x<-c(20090101,"2009-01-02","2009 01 03","2009-1-4",
        "2009-1, 5","Created on 2009 1 6","200901 !!! 07")
ymd(x)
now()




#### text mining
data("trump_tweets")
head(trump_tweets)
?trump_tweets

campaign_tweets <- trump_tweets %>% extract(source,"source","Twitter for (.*)") %>%
  as_tibble() %>% filter(source%in%c('Android','iPhone')) %>% 
  filter(created_at>=ymd('2015-06-17')&created_at<ymd('2016-11-08')) %>% 
  filter(!is_retweet) %>% arrange(created_at)

### 看一哈安卓和苹果机发推的时间分布
campaign_tweets %>% mutate(hour=hour(with_tz(created_at,'EST'))) %>%
  count(source,hour) %>% group_by(source) %>% 
  mutate(percent=n/sum(n)) %>% ungroup %>%
  ggplot(aes(hour,percent,color=source)) + 
  geom_line() + geom_point() +
  labs(x='Hour of day (EST)', y= 'percentage of tweets')

### mining
# text as data
library(tidytext)
poem<-c("Roses are red,","Violets are blue,",
           "Sugar is sweet,","And so are you.")
example<-tibble(line=c(1,2,3,4),
                  text=poem)
example

example %>% unnest_tokens(word,text)

unnest_reg <- "([^A-Za-z_\\d#@']|'(?![A-Za-z_\\d#@]))"
campaign_tweets[3008,] %>% 
  unnest_tokens(word,text,token = 'regex',pattern=unnest_reg) %>%
  pull(word)

links <- "https://t.co/[A-Za-z\\d]+|&"

tweet_words <- campaign_tweets %>% mutate(text=str_replace_all(text,links,"")) %>% 
  unnest_tokens(word,text,token='regex',pattern=unnest_reg) %>% 
  filter(! word %in% stop_words$word & 
           !str_detect(word,"^\\d+$")) %>% 
  mutate(word=str_replace(word,"^'",""))

tweet_words %>% count(word) %>% arrange(desc(n))


android_iphone_or <- tweet_words %>% count(word,source) %>% 
  spread(source,n,fill=0) %>% 
  mutate(or=(Android+0.5)/(sum(Android)-Android-0.5) / 
           ((iPhone+0.5)/(sum(iPhone)-iPhone-0.5))) 
 
android_iphone_or %>%  filter(Android+iPhone>100) %>% 
  arrange(or)

## sentiment analysis
library(textdata)
# the bing lexicon divides words into positive and negative sentiments
get_sentiments("bing")

# the AFINN lexicon assigns a score -5~5 with -5 the most negative
get_sentiments('afinn')

get_sentiments('nrc') %>% count(sentiment)

nrc <- get_sentiments('nrc') %>% select(word,sentiment)
sentiment_counts <- tweet_words %>% left_join(nrc,by='word') %>%
  count(source,sentiment) %>% spread(source,n) %>% 
  mutate(sentiment=replace_na(sentiment,replace='none'))

sentiment_counts %>% mutate(Android=Android/(sum(Android)-Android),
                            iPhone=iPhone/(sum(iPhone)-iPhone),
                            or=Android/iPhone) %>% arrange(desc(or))


library(broom)
log_or<-sentiment_counts%>%
  mutate(log_or=log((Android/(sum(Android)-Android))/
                        (iPhone/(sum(iPhone)-iPhone))),
         se=sqrt(1/Android+1/(sum(Android)-Android)+
                     1/iPhone+1/(sum(iPhone)-iPhone)),
         conf.low=log_or-qnorm(0.975)*se,
         conf.high=log_or+qnorm(0.975)*se)%>%
  arrange(desc(log_or))

android_iphone_or%>%inner_join(nrc,by="word")%>%
  mutate(sentiment=factor(sentiment,levels=log_or$sentiment))%>%
  mutate(log_or=log(or))%>%
  filter(Android+iPhone>10&abs(log_or)>1)%>%
  mutate(word=reorder(word,log_or))%>%
  ggplot(aes(word,log_or,fill=log_or<0))+
  facet_wrap(~sentiment,scales="free_x",nrow=2)+
  geom_bar(stat="identity",show.legend=FALSE)+
  theme(axis.text.x=element_text(angle=90,hjust=1))