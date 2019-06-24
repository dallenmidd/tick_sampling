### Code for Borgmann-Winter and Allen manuscript
## How does the distance between drag cloth checks affect tick density estimate

# load required packages
require(tidyverse)
require(bbmle)

# load in data from persistence trials
dropoff_data <- read_csv('dropoff_rate_data.csv') 

nll_fun <- function(k, drag_dist, stay_on)
{
  expProb <- exp(-k*drag_dist)
  nll <- dbinom(x = stay_on, size = 1, prob = expProb, log = T)
  return(-sum(nll))
}

f_data <- dropoff_data %>% 
  filter(life_stage == 'F') %>%
  list(drag_dist = .$drag_dist, stay_on = .$stay_on)
m_data <- dropoff_data %>% 
  filter(life_stage == 'M') %>%
  list(drag_dist = .$drag_dist, stay_on = .$stay_on)
n_data <- dropoff_data %>% 
  filter(life_stage == 'N') %>%
  list(drag_dist = .$drag_dist, stay_on = .$stay_on)

f_fit <- mle2(nll_fun, start = list(k=0.1), data = f_data)
f_rate <- f_fit %>% coef() %>% unname()
confint(f_fit)

m_fit <- mle2(nll_fun, start = list(k=0.07), data = m_data)
m_rate <- m_fit %>% coef() %>% unname()
confint(m_fit)

n_fit <- mle2(nll_fun, start = list(k=0.04), data = n_data)
n_rate <- n_fit %>% coef() %>% unname()
confint(n_fit)

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
    #adult = sum(female) + sum(male),
    nymph = sum(nymph),
    treatment = unique(treatment)
  ) %>%
  gather('life_stage',"number",c('female','male','nymph'))
  #gather('life_stage',"number",c('adult','nymph'))

# change life_stage == "nymph" for other life stages
drag_data_mod %>%
  mutate(treatment2 = as.numeric(substr(treatment,1,2))) %>%
  filter(life_stage == "nymph") %>%
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
drag_data_sum <- drag_data_sum %>%
  ungroup() %>%
  mutate(pred = c(NA, mean[1]*drop_fun(20,f_rate), mean[1]*drop_fun(30,f_rate),
                  NA, mean[4]*drop_fun(20,m_rate), mean[4]*drop_fun(30,m_rate),
                  NA, mean[7]*drop_fun(20,n_rate), mean[7]*drop_fun(30,n_rate)) )

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
