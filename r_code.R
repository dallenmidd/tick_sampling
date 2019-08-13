### Code for Borgmann-Winter and Allen manuscript
## How does the distance between drag cloth checks affect tick density estimate

# load required packages
require(tidyverse)
require(bbmle)

# load in data from persistence trials
dropoff_data <- read_csv('dropoff_rate_data.csv') 

nll_mod1 <- function(k, drag_dist, stay_on, list_stage)
{
  expProb <- exp(-k*drag_dist)
  nll <- dbinom(x = stay_on, size = 1, prob = expProb, log = T)
  return(-sum(nll))
}
nll_mod2 <- function(k_n, k_a, drag_dist, stay_on, list_stage)
{
  k <- ifelse(life_stage == 'N',k_n,k_a)
  expProb <- exp(-k*drag_dist)
  nll <- dbinom(x = stay_on, size = 1, prob = expProb, log = T)
  return(-sum(nll))
}
nll_mod3 <- function(k_n, k_m, k_f, drag_dist, stay_on, list_stage)
{
  k <- ifelse(life_stage == 'N',k_n,ifelse(life_stage == 'M', k_m,k_f))
  expProb <- exp(-k*drag_dist)
  nll <- dbinom(x = stay_on, size = 1, prob = expProb, log = T)
  return(-sum(nll))
}

dropoff_list <- dropoff_data %>% as.list()


fit_mod1 <- mle2(nll_mod1, start = list(k = 0.07), data = dropoff_list)
fit_mod2 <- mle2(nll_mod2, start = list(k_n = 0.05, k_a = 0.08), data = dropoff_list)
fit_mod3 <- mle2(nll_mod3, start = list(k_n = 0.05, k_m = 0.07, k_f = 0.1 ), data = dropoff_list)

pchisq(q = 2*(logLik(fit_mod2)- logLik(fit_mod1)), df = 1, lower.tail = F) %>% as.numeric()
pchisq(q = 2*(logLik(fit_mod3)- logLik(fit_mod2)), df = 1, lower.tail = F) %>% as.numeric()

a_rate <- coef(fit_mod2)['k_a']
n_rate <- coef(fit_mod2)['k_n']
confint(fit_mod2)

dropoff_data %>%
  filter(dist_start == 0) %>%
  group_by(life_stage) %>%
  count()

# reduction in nymphal density estimate when checking every 10, 20 or 30 m
# equation from diff eq in Milne et al. (1943)
(1-exp(-n_rate*10))/(n_rate*10)
(1-exp(-n_rate*20))/(n_rate*20)
(1-exp(-n_rate*30))/(n_rate*30)


# load in data from variable distance drag experiments
drag_data <- read_csv('variable_drag_dist_data.csv') %>%
  mutate(rep_treat = paste(date, rep, treatment,sep='-'),
         rep = paste(date, rep,sep='-') )

drag_data_mod <- drag_data %>%
  group_by(rep_treat) %>%
  summarise(
    female = sum(female),
    male = sum(male),
    adult = sum(female) + sum(male),
    nymph = sum(nymph),
    treatment = unique(treatment)
  ) %>%
  #gather('life_stage',"number",c('female','male','nymph'))
  gather('life_stage',"number",c('adult','nymph'))

# change life_stage == "nymph" for other life stages
drag_data_mod %>%
  mutate(treatment2 = as.numeric(substr(treatment,1,2))) %>%
  filter(life_stage == "adult") %>%
  glm(number ~ treatment2, data = ., family ='poisson') %>%
  summary()


drag_data_sum <-
  drag_data_mod %>%
  group_by(life_stage,treatment) %>%
  summarise(
   mean = mean(number),
   error = sd(number)/(n()^0.5),
   n = n()
  ) 
drop_fun <- function(d,r) 10*(1-exp(-d*r))/(d*(1-exp(-10*r)))

# if N v M v F
drag_data_sum <- drag_data_sum %>%
  ungroup() %>%
  mutate(pred = c(NA, mean[1]*drop_fun(20,f_rate), mean[1]*drop_fun(30,f_rate),
                  NA, mean[4]*drop_fun(20,m_rate), mean[4]*drop_fun(30,m_rate),
                  NA, mean[7]*drop_fun(20,n_rate), mean[7]*drop_fun(30,n_rate)) )

# if N v A
drag_data_sum <- drag_data_sum %>%
  ungroup() %>%
  mutate(pred = c(NA, mean[1]*drop_fun(20,a_rate), mean[1]*drop_fun(30,a_rate),
                  NA, mean[4]*drop_fun(20,n_rate), mean[4]*drop_fun(30,n_rate)) )

# if N v M v F
axis_lim <- tibble(
  y_min = c(0,0,0),
  y_max = c(0.4,0.4,4.75),
  y_lab = y_max*0.975,
  x_lab = rep(0.75,3),
  life_stage = c('female', 'male', 'nymph'),
  treatment = c('10 m','10 m','10 m'),
  label = c('A','B','C')
)

pdf('figure_1v2.pdf',width = 6,height = 4)
  y_axis_lab <- expression(paste('Individuals (per 60 ',m^2,')'))
  drag_data_sum %>%
    ggplot(aes(treatment,mean)) +
    facet_wrap(~life_stage, scales = 'free_y') +
    geom_col(fill=rgb(0.75,0.75,0.75)) + 
    geom_errorbar(aes(x=treatment, ymin=mean-error,ymax=mean+error),width=0.75) + 
    labs(x = "", y = y_axis_lab) +
    theme_classic() + 
    theme(strip.background = element_rect(color = 'transparent'), 
          strip.text = element_blank(),
          axis.text = element_text(color='black')) +
    geom_point(aes(x=treatment,y=pred),pch=1,size=3) +
    scale_x_discrete(expand=c(0,0)) +
    scale_y_continuous(expand=c(0,0)) +
    geom_blank(data=axis_lim,aes(y = y_min)) +
    geom_blank(data=axis_lim, aes(y=y_max)) +
    geom_text(data=axis_lim, aes(x=x_lab,y=y_lab,label=label),size=5)
dev.off()

# old version of figure 1
pdf('figure_1.pdf',width = 6,height = 4)
  y_axis_lab <- expression(paste('Individuals (per 60 ',m^2,')'))
  drag_data_sum %>%
    ggplot(aes(life_stage,mean,fill=treatment)) +
    geom_col(position = 'dodge2') + 
    geom_errorbar(aes(x=life_stage, ymin=mean-error,ymax=mean+error),position = position_dodge2()) + 
    labs(x = "Life stage", y = y_axis_lab) +
    geom_point(aes(x=life_stage,y=pred),position = position_dodge2(0.9),pch=1,size=3)
dev.off()



# if just N v A
axis_lim <- tibble(
  y_min = c(0,0),
  y_max = c(0.8,4.75),
  y_lab = y_max*0.975,
  x_lab = rep(0.75,2),
  life_stage = c('adult', 'nymph'),
  treatment = c('10 m','10 m'),
  label = c('A','B')
)

# version included in the manuscript
pdf('figure_1v3.pdf',width = 6,height = 4)
  y_axis_lab <- expression(paste('Individuals (per 60 ',m^2,')'))
  drag_data_sum %>%
    ggplot(aes(treatment,mean)) +
    facet_wrap(~life_stage, scales = 'free_y') +
    geom_col(fill=rgb(0.75,0.75,0.75)) + 
    geom_errorbar(aes(x=treatment, ymin=mean-error,ymax=mean+error),width=0.75) + 
    labs(x = "", y = y_axis_lab) +
    theme_classic() + 
    theme(strip.background = element_rect(color = 'transparent'), 
          strip.text = element_blank(),
          axis.text = element_text(color='black')) +
    geom_point(aes(x=treatment,y=pred),pch=1,size=3) +
    scale_x_discrete(expand=c(0,0)) +
    scale_y_continuous(expand=c(0,0)) +
    geom_blank(data=axis_lim,aes(y = y_min)) +
    geom_blank(data=axis_lim, aes(y=y_max)) +
    geom_text(data=axis_lim, aes(x=x_lab,y=y_lab,label=label),size=5)
dev.off()


# calculations for table 1
# find k in equation p(d) = exp(-k*d)
# where k is drop off rate, d is distance dragged, 
# and p(d) is probability tick stays on that long

# Schulze and Jordan (2001) http://doi.org/10.1603/0022-2585-38.4.606
# Amblyomma americanum -- sparse vegetation
- log(0.5)/60
# Amblyomma americanum -- dense vegetation
- log(0.5)/15.5
# Ixodes scapularis -- sparse vegetation
- log(0.5)/38
# Ixodes scapularis -- dense vegetation
- log(0.5)/11.5

# Uspensky (1993) http://doi.org/10.1007/BF00058507 
# Ixodes persulcatus -- 6 - 10 C
- log(30.5/50)/25
# Ixodes persulcatus -- 17 - 22 C
- log(34.6/50)/25
# Ixodes ricinus -- 6 - 10 C
- log(13.4/50)/25
# Ixodes ricinus -- 17 - 22 C
- log(28.2/50)/25

# Li and Dunley (1998) https://doi.org/10.1023/A:1006018432064
# report k directly