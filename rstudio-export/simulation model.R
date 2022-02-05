#Simulation 
nshots <- c(248, 884, 419 , 339,272, 451,433,351,403)
p_hit <-c(0.50,0.52,0.46,0.56,0.47,0.46,0.54,0.52,0.62)
actualruns <- c(128,431,203,172,134,245,227,176,220)

n <- 10000
ExpectedRuns <- c()
SdRuns <- c()
Expected_below_actual <-c()
p_value <- c()

for (k in 1:length(nshots)){
  shots <- nshots[k]
  p <- p_hit[k]
  print(p)
  lizt <- c()
  for ( i in 1:n ){
    record <- rbinom(shots, 1, p)
    iter <- 1
    past_i <- record[1]
    pres_i <- 'cat'
    for (j in record){
      pres_i <- j
      if (pres_i != past_i) {
        iter <- iter + 1
      }
      past_i <- pres_i
    }
    lizt <- c(lizt,iter) #number of runs in each simulation
  }
  p_value <- c(p_value,t.test(x = lizt, mu = actualruns[k], alternative = "greater", conf.level = 0.99)$p.value)
  ExpectedRuns <- c(ExpectedRuns,mean(lizt)) #mean of runs based on simulation
  SdRuns <- c(SdRuns,sd(lizt)) #sd of runs based on simulation
  Expected_below_actual <- c(Expected_below_actual,mean(lizt<actualruns[k])) #mean of simulation > actual runs
  }

#if hothand exists then the actual will be smaller than the simulated mean
Players <- c("PlayerA", "PlayerB", "PlayerC","PlayerD","PlayerE","PlayerF","PlayerG","PlayerH","PlayerI")
df <- data.frame(Players, nshots, actualruns,ExpectedRuns, SdRuns,Expected_below_actual,p_value)
df$p_value <- round(df$p_value, digits=2)
saveRDS(df, file = "simulation_data.rds")

