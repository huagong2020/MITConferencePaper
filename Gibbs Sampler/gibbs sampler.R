
# This file contains the Gibbs sampler for estimating NBA positional offensive and defensive plus-minus
# Y = Xb + Zγ + e
# 
# Note.
# 1. no intercept
# 2. no hierarchical structure for low usage player and team effects
# 3. 7 offensive positions and 3 defensive positions
# 4. model uses data from 2015-16 to 2021-22 seasons; no 2019-20 data used

library(tidyverse)
library(ggridges)
#player id and official player position data
nba_id_data = read_csv('https://raw.githubusercontent.com/huagong2020/MITConferencePaper/main/data/playerId/nba_id_data.csv')

# clustered player position data
pivot_data = read_csv('https://raw.githubusercontent.com/huagong2020/MITConferencePaper/main/data/playerId/pivot_data.csv')


# set the number of iterations for the Gibbs sampler
# m = 4000 takes about 1 hours to run for each year's data
# for experiments, may consider reducing m to 50
m = 1000
burn_in = 200


for (k in c(2016:2019,2021:2022)) {
  
  # clean data ----
  year=k
  
  # download the stint data set from github
  stint = read_csv(paste0("https://raw.githubusercontent.com/huagong2020/MITConferencePaper/main/data/stint/stint_",year,".csv"))
  
  # remove stints with possessions <=0
  stint = stint %>% filter (possessions>0)
  
  #create a list of unique players
  player_list = unique(c(stint$offensePlayer1Id,stint$offensePlayer2Id,stint$offensePlayer3Id,stint$offensePlayer4Id,stint$offensePlayer5Id,
                         stint$defensePlayer1Id,stint$defensePlayer2Id,stint$defensePlayer3Id,stint$defensePlayer4Id,stint$defensePlayer5Id,
                         stint$offenseTeam,stint$defenseTeam))
  
  # create an empty matrix filled with 0
  # matrix include X and Z
  data_matrix = matrix(0,ncol=length(player_list)*2,nrow=nrow(stint))
  
  # assign 1 and -1 to the matrix
  # 1 represents offense and on the court and -1 represents defense and not on the court
  for (i in 1:nrow(stint)){
    
    data_matrix[i,which( player_list %in%stint$offensePlayer1Id[i] )] <- 1
    data_matrix[i,which( player_list %in%stint$offensePlayer2Id[i] )] <- 1
    data_matrix[i,which( player_list %in%stint$offensePlayer3Id[i] )] <- 1
    data_matrix[i,which( player_list %in%stint$offensePlayer4Id[i] )] <- 1
    data_matrix[i,which( player_list %in%stint$offensePlayer5Id[i] )] <- 1
    data_matrix[i,which( player_list %in%stint$offenseTeam[i] )] <- 1
    data_matrix[i,length(player_list)+which( player_list %in%stint$defensePlayer1Id[i] )] <- -1
    data_matrix[i,length(player_list)+which( player_list %in%stint$defensePlayer2Id[i] )] <- -1
    data_matrix[i,length(player_list)+which( player_list %in%stint$defensePlayer3Id[i] )] <- -1
    data_matrix[i,length(player_list)+which( player_list %in%stint$defensePlayer4Id[i] )] <- -1
    data_matrix[i,length(player_list)+which( player_list %in%stint$defensePlayer5Id[i] )] <- -1
    data_matrix[i,length(player_list)+which( player_list %in%stint$defenseTeam[i] )] <- -1
  }
  
  # calcuate points per 100 possession for each stint
  PL_100 = as.matrix(100*stint$points/stint$possessions,ncol=1)
  
  # create a data frame with player id 
  player_data = data.frame(ID=player_list)
  
  # merge player position information from nba_id_data
  player_data$ID = as.character(player_data$ID)
  nba_id_data$id = as.character(nba_id_data$id)
  player_data = player_data %>% left_join(nba_id_data %>%  select(id,Position) %>% rename(ID = id,POS = Position) %>% distinct(ID,.keep_all=T),by='ID')
  
  # derive 3 defensive positions 
  player_data = player_data %>% mutate(POS2 = case_when(
    POS =='C'|POS=='C-F'|POS=='F-C'  ~ 'C',
    POS =='F'|POS=='F-G' ~ 'F',
    POS =='G'|POS=='G-F' ~ 'G',
    is.na(POS) ~ 'Team'
  ))
  
  # merge offensive positions information (positions 1-7) from pivot_data
  pivot_data$ID = as.character(pivot_data$ID)
  player_data = player_data %>% left_join(pivot_data %>% filter (SEASON==year) %>% select(ID,group3) %>% rename('group'='group3'),by='ID')
  
  # identify low usage players and assign a different group name to these players (position 8)
  player_data$group[is.na(player_data$group) & player_data$POS2!='Team'] = max(player_data$group,na.rm = T)+1
  
  # teams position (position 9)
  player_data$group[player_data$POS2=='Team'] = max(player_data$group,na.rm = T)+1
  
  # position numbers
  # 1 - 'Roll&CutBig',
  # 2 - 'PostupBig',
  # 3 - 'MovementShooter'
  # 4 - 'SpotupShooter',
  # 5 - 'VersatileScorer',
  # 6 - 'SkilledBig',
  # 7 - 'Pick&RollAttacker',
  # 8 - 'LowUsageO',
  # 9 - 'Team',
  # 10 -'Big',
  # 11 -'Forward',
  # 12 - 'Guard',
  # 13 - 'LowUsageD'

  o_pos = player_data$group
  d_pos = player_data$POS2
  
  d_pos[d_pos=='C'] = max(o_pos)+1
  d_pos[d_pos=='F'] = max(o_pos)+2
  d_pos[d_pos=='G'] = max(o_pos)+3
  d_pos[d_pos=='Team'] = max(o_pos)
  # low usage players on defense
  d_pos[o_pos==(max(o_pos)-1)] = max(o_pos)+4
  
  # combine position numbers
  position = c(o_pos,d_pos)
  position = as.numeric(position)
  table(position)
  
  # Gibbs sampling data preparation ----
  X = data_matrix
  Y = PL_100 
  
  # prior for mu
  a=0
  b2=100
  
  # prior for tau2
  c=0.01
  d=0.01
  
  # prior for sigma2
  e=0.01
  f=0.01
  
  
  beta = matrix(0,ncol= dim(data_matrix)[2],m)
  mu = matrix (0,ncol=length(unique(position)),m)
  tau2 = matrix (100,ncol=length(unique(position)),m)
  sigma2 = matrix (1,ncol=1,m)
  
  
  # X'Y
  XY = t(X) %*% Y
  
  # X'X
  XX = t(X) %*% X
  
  # # Gibbs sampling loop starts ----
  for (i in 2:m) {
    
    # print per 100 iterations
    if(i %% 100 ==0) {print(i)}
    
    # sigma2/tau2
    sigma2_tau2 = sigma2[i-1,]/tau2[i-1,position]
    
    # calculate (XX+ sigma2/tau2)^-1
    xxi = solve(XX + diag(sigma2_tau2))
    
    # sample beta from a multivariate normal distribution
    # mean = (XX+ sigma2/tau2)^-1(XY + sigma2_tau2 * mu)
    # covariance = sigma2 * (XX+ sigma2/tau2)^-1
    # for low usage player and team:  mu = 0 and tau2 = 100
    # for other players:  mu = group mean and tau2 = group variance
    beta[i,] = t(xxi %*% (XY + (sigma2_tau2) * mu[i-1,position])) + t(rnorm(dim(X)[2],0,1)) %*% chol(sigma2[i-1,] * xxi)
    
    
    #-c(8,9,13) low usage players and team 
    for (j in (1:length(unique(position)))[-c(8,9,13)]) {
      # number of players belong to group j
      k = length(which (position == j))
      
      # draw mu from a normal distribution
      # mean = (sum(b) + a * tau2/b2)/ (k + tau2/b2)
      # variance = tau2 / (k + k + tau2/b2)
      mu[i,j] = rnorm (1, mean = (sum(beta[i,position==j]) + a * tau2[i-1,j]/b2)/(k+tau2[i-1,j]/b2) , sd = sqrt(tau2[i-1,j]/(k+tau2[i-1,j]/b2)))
      
      # draw tau2 from an inverse gamma distribution
      # parameter 1 = k/2+c
      # parameter 2 = summation (b-mu)^2 +d
      tau2[i,j] = 1/rgamma(1, shape = k/2+c, rate =  0.5 * sum((beta[i,position==j]- mu[i,j])^2) + d )
    }
    
    # draw sigma2 from an inverse gamma distribution
    # parameter 1 = n/2 + e
    # parameter 2 = (y - Xb)T(y - Xb) + f
    sigma2[i,] = 1/rgamma(1, shape = length(Y)/2 +e, rate = 0.5 * t(Y-X %*% beta[i,]) %*% (Y-X %*% beta[i,]) + f )
  }
  
  
  # store results
  assign(paste0('beta6_',year), beta)
  assign(paste0('mu6_',year), mu)
  assign(paste0('tau26_',year), tau2)
  assign(paste0('sigma26_',year), sigma2)
  
}


# 
# # visualize results ----
# draw density plots 

# # combine data
mu_data = rbind(data.frame(mu6_2016[(burn_in+1):m,]),
                data.frame(mu6_2017[(burn_in+1):m,]),
                data.frame(mu6_2018[(burn_in+1):m,]),
                data.frame(mu6_2019[(burn_in+1):m,]),
                data.frame(mu6_2021[(burn_in+1):m,]),
                data.frame(mu6_2022[(burn_in+1):m,]))
# 
# 
# mu_data = rbind(data.frame(tau26_2016[(burn_in+1):m,]),
#                 data.frame(tau26_2017[(burn_in+1):m,]),
#                 data.frame(tau26_2018[(burn_in+1):m,]),
#                 data.frame(tau26_2019[(burn_in+1):m,]),
#                 data.frame(tau26_2021[(burn_in+1):m,]),
#                 data.frame(tau26_2022[(burn_in+1):m,]))
# 
# 
# 
# add season
mu_data$season = rep(c(2016,2017,2018,2019,2021,2022),each=m-burn_in)

colnames(mu_data) [1:13] = c('Roll&CutBig','PostupBig','MovementShooter','SpotupShooter','VersatileScorer','SkilledBig','Pick&RollAttacker','LowUsageO','Team',
                             'Big','Forward','Guard','LowUsageD')



# convert data
mu_data= mu_data %>% pivot_longer(!c(season))

# defense
mu_data_plot = mu_data %>% filter (name %in% c('Big','Forward','Guard'))

# offense
mu_data_plot = mu_data %>% filter (name %in% c('Roll&CutBig','PostupBig','MovementShooter','SpotupShooter','VersatileScorer','SkilledBig','Pick&RollAttacker'))
mu_data_plot$name = factor(mu_data_plot$name,
                           levels=c('Roll&CutBig','PostupBig','SkilledBig',
                                    'MovementShooter','SpotupShooter',
                                    'VersatileScorer','Pick&RollAttacker'))


# visualize mu across seasons
ggplot(mu_data_plot,aes(y=reorder(name,value,FUN=mean),x=value,group=name)) +
  geom_density_ridges(aes(fill=reorder(name,value,FUN=mean)),alpha=0.7,quantile_lines=TRUE,vline_linetype=2,
                      quantile_fun=mean) +
  theme_ridges() + ylab('Position') + xlab('Plus-Minus Variance')+ ggtitle('Variance of NBA Positional Defensive Plus-Minus')+
  theme(legend.position='none') + facet_wrap(~season) + xlim(0,50)

# visualize mu across positions
ggplot(mu_data_plot,aes(y=as.factor(season),x=value,group=season)) +
  geom_density_ridges(aes(fill=as.factor(season)),alpha=0.7,quantile_lines=TRUE,vline_linetype=2,
                    quantile_fun=mean)+
  theme_ridges() + ylab('Season') + xlab('Plus-Minus Variance')+ ggtitle('Variance of NBA Positional Defensive Plus-Minus')+
  theme(legend.position='none')  +facet_wrap(~name) + xlim(0,50)

