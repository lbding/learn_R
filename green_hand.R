### object

### workspace
#“As we define objects in the console, we are actually changing the workspace. ”
ls()

### function

### prebulit objects
data()

### variable names

### data type
library(dslabs)
data("murders")
head(murders);str(murders)
?Comparison

class(1L)
#"integer"
class(1)
#"numeric"

data(heights)
heights%>%filter(sex=="Male")%>%summarize(quantile(height,c(0,0.5,1)))

my_summary<-function(dat){
  x<-quantile(dat$height,c(0,0.5,1))
  tibble(min=x[1],median=x[2],max=x[3])
}
heights %>% group_by(sex) %>% do(my_summary)

### purrr
library(purrr)
map(1:10,function(x) {n=1:x;sum(n^3)})
# return a list

map_dbl(1:10,function(x) {n=1:x;sum(n^3)})
# return a vector

map_df(1:10,function(x) {n=1:x; tibble(row=n,sum2=sum(n^2),sum3=sum(n^3))})


### case_when
between(1:10,5,7)
## 双闭区间
download.file(url = 'https://www.rstudio.com/wp-content/uploads/2015/03/ggplot2-cheatsheet.pdf',
                destfile = 'ggplot2_cheatsheet.pdf')
list.files()
file.path()

### geom_density中的参数adjust

### ridge plot
library(ggridges)
p<-gapminder%>%
  mutate(dollars_per_day=gdp/population/365)%>%
  filter(year==2000&!is.na(dollars_per_day))%>%
  ggplot(aes(dollars_per_day,continent))+
  scale_x_continuous(trans="log2")
p+geom_density_ridges(jittered_points=T,
                        position=position_points_jitter(height=0),
                      point_shape='|',point_size=3)



### 扑克中的21点问题
### gtools::permutations() \ combinations()
library(gtools)
permutations(5,2)

suits<-c("Diamonds","Clubs","Hearts","Spades")
numbers<-c("Ace","Deuce","Three","Four","Five","Six","Seven",
             "Eight","Nine","Ten","Jack","Queen","King")
deck<-expand.grid(number=numbers,suit=suits)
deck<-paste(deck$number,deck$suit)

aces <- paste('Ace',suits)
facecard<- expand.grid(number=c('King','Queen','Jack','Ten'),
                       suit=suits)
facecard<- paste(facecard$number,facecard$suit)

hands<-permutations(52,2,v=deck)

hands2<-combinations(52,2,v=deck)

mean((hands[,1]%in%aces & hands[,2]%in%facecard)|
       (hands[,2]%in%aces & hands[,1]%in%facecard))
mean(hands2[,1]%in%aces & hands2[,2]%in%facecard)


blackjack<-function(){
  hand<-sample(deck,2)
  (hand[1]%in%aces&hand[2]%in%facecard)|
    (hand[2]%in%aces&hand[1]%in%facecard)
}
mean(replicate(10000,blackjack()))


