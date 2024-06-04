#### Regression
set.seed(1983)
galton_heights <- GaltonFamilies %>%
  filter(gender == "male") %>%
  group_by(family) %>%
  sample_n(1) %>%
  ungroup() %>%
  select(father, childHeight) %>%
  rename(son = childHeight)

rho <- mean(scale(galton_heights$father)*scale(galton_heights$son))

galton_heights%>%summarise(rho=cor(father,son))



https://www.youtube.com/watch?v=HL-XjMCPfio

https://www.youtube.com/watch?v=NeloljCx-1g

https://www.youtube.com/watch?v=xYxSZJ9GZ-w

https://www.youtube.com/watch?v=JSE5kfxkzfk


library(Lahman)

### 分层求线性回归的系数
dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR = round(HR/G, 1),
         BB = BB/G,
         R = R/G) %>%
  select(HR, BB, R) %>%
  filter(HR >= 0.4 & HR<=1.2)

get_slope <- function(data) {
  fit <- lm(R~BB,data=data)  
  data.frame(slope=fit$coefficients[2],
             se=summary(fit)$coefficients[2,2])
}

dat%>%
  group_by(HR)%>%
  do(get_slope(.))


##### 重点介绍broom包
library(broom)
##designed to facilitate the use of model fitting functions, 
## such as lm, with the tidyverse.

##functions in broom: tidy, glance, augment

tidy(fit,conf.int = T)
glance(fit)
augment(fit)

dat %>% group_by(HR) %>% do(tidy(lm(R~BB,data=.),conf.int=T)) %>% 
  filter(term=='BB') %>% select(HR,estimate,conf.low,conf.high) %>% 
  ggplot(aes(HR,estimate)) + geom_point() + 
  geom_errorbar(aes(ymin=conf.low,ymax=conf.high))

fit <- Teams %>%
  filter(yearID %in% 1961:2001) %>%
  mutate(BB = BB / G,
         singles = (H - X2B - X3B - HR) / G,
         doubles = X2B / G,
         triples = X3B / G,
         HR = HR / G,
         R = R / G) %>%
  lm(R ~ BB + singles + doubles + triples + HR, data = .) 

pa_per_game <- Batting %>% filter(yearID == 2002) %>%
  group_by(teamID) %>%
  summarize(pa_per_game = sum(AB+BB)/max(G)) %>%
  pull(pa_per_game) %>%
  mean

players <- Batting %>% filter(yearID %in% 1997:2001) %>%
  group_by(playerID) %>%
  mutate(PA = BB + AB) %>%
  summarize(G = sum(PA)/pa_per_game,
            BB = sum(BB)/G,
            singles = sum(H-X2B-X3B-HR)/G,
            doubles = sum(X2B)/G,
            triples = sum(X3B)/G,
            HR = sum(HR)/G,
            AVG = sum(H)/sum(AB),
            PA = sum(PA)) %>%
  filter(PA >= 1000) %>%
  select(-G) %>%
  mutate(R_hat = predict(fit, newdata = .))

players <- Salaries %>%
  filter(yearID == 2002) %>%
  select(playerID, salary) %>%
  right_join(players, by="playerID")


position_names <-
  paste0("G_", c("p","c","1b","2b","3b","ss","lf","cf","rf", "dh"))

tmp <- Appearances %>%
  filter(yearID == 2002) %>%
  group_by(playerID) %>%
  summarize_at(position_names, sum) %>%
  ungroup()

pos <- tmp %>%
  select(position_names) %>%
  apply(., 1, which.max)

players <- tibble(playerID = tmp$playerID, POS = position_names[pos]) %>%
  mutate(POS = str_to_upper(str_remove(POS, "G_"))) %>%
  filter(POS != "P") %>%
  right_join(players, by="playerID") %>%
  filter(!is.na(POS) & !is.na(salary))

players <- People %>%
  select(playerID, nameFirst, nameLast, debut) %>%
  mutate(debut = as.Date(debut)) %>%
  right_join(players, by="playerID")

players%>%select(nameFirst,nameLast,POS,salary,R_hat)%>%
  arrange(desc(R_hat))%>%top_n(10)


#### 一元二次方程
falling_object <- rfalling_object()

fit<-falling_object%>%
  mutate(time_sq=time^2)%>%
  lm(observed_distance~time+time_sq,data=.)
tidy(fit,conf.int = T)

augment(fit) %>%
  ggplot() +
  geom_point(aes(time, observed_distance)) +
  geom_line(aes(time, .fitted), col = "blue")


#### spurious association
N <- 25
g <- 1000000
sim_data <- tibble(group = rep(1:g, each=N),
                   x = rnorm(N * g),
                   y = rnorm(N * g))

res<-sim_data%>%
  group_by(group)%>%
  summarize(r=cor(x,y))%>%
  arrange(desc(r))
qplot(res$r,binwidth=0.1,color=I('black'))

sim_data %>% filter(group==res$group[which.max(res$r)]) %>% 
  do(tidy(lm(y~x,data=.)))

sim_data %>% filter(group==1:10) %>% group_by(group) %>%
  do(tidy(lm(y~x,data=.)))



data("admissions")

data(admissions)
admissions%>%group_by(gender)%>%
  summarize(total_admitted=round(sum(admitted/100*applicants)),
            not_admitted=sum(applicants)-sum(total_admitted))%>%
  select(-gender)%>%
  do(tidy(chisq.test(.)))%>%.$p.value