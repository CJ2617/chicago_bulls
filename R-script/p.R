player <- left_join(player_1819,salaries_1819)
levels(as.factor(player$Pos))
max(player$G)
player_tot <- player[grepl('TOT', player$Tm),]
player1 <- player[(player$player_id %in% player_tot$player_id),]
player2 <- player[!(player$player_id %in% player_tot$player_id),]
player <- full_join(player2,player_tot)


# multilinear, y=salary
c_fit <- lm(salary~., data=c)
plot(c_fit)
tidy(c_fit,coef.int=TRUE)


comp <- team2[24:27,]
comp <- comp%>%
  select(c("Rk","FG","X3P.","X2P.","FT.","TRB", "AST","STL","BLK","TOV","PF"))
ggplot(comp,aes(Rk,FG.))+geom_col()
ggplot(comp,aes(Rk,X3P.))+geom_col()
ggplot(comp,aes(Rk,X2P.))+geom_col()
ggplot(comp,aes(Rk,FT.))+geom_col()

pair

team <- team2%>%
  select(c("Rk","FG","X3P.","X2P.","FT.","TRB", "AST","STL","BLK","TOV","PF"))


chi_team1 <- team1[27,] %>%  pivot_longer(
  cols = !Team, 
  names_to = "data", 
  values_to = "result")

mean_data <- team1%>%mutate(team="team",mean(W),mean(L),mean(PW),mean(PL), mean(MOV), 
                            mean(SOS), mean(SRS), mean(ORtg), mean(DRtg), 
                            mean(NRtg), mean(Pace), mean(FTr), mean(X3PAr), 
                            mean(TS.),  mean(eFG.), mean(TOV.), mean(ORB.), 
                            mean(FT.FGA), mean(DRB.))

mean_data <- tibble(mean_data[1,26:45])
mean_data <- mean_data %>%  pivot_longer(
  cols = !team, 
  names_to = "mean", 
  values_to = "result")

chi_team1 <-team1[27,] %>%  pivot_longer(
  cols = !Team, 
  names_to = "data", 
  values_to = "result")

chi_team1 <- chi_team1[3:21,]
chi_team1 <-chi_team1 %>%mutate(mean_result=mean_data$result)
chi_team1 <- chi_team1%>%mutate(diff=result-mean_result)
chi_team1
