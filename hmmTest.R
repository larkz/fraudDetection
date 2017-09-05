# Initial HMM
library(HMM)
library(MCMCpack)

old.par <- par(mar = c(0, 0, 0, 0))
par(old.par)


Amat_init <- rdirichlet(3,rep(1,3))
Bmat_init <- rdirichlet(3,rep(1,2))

hidden_states <- unique(as.character(sales_order_agg0$t1_category))

hmm_fraudstates = initHMM(c("A","B", "C", "D"), c("high","med", "low"))
hmm_catstates = initHMM(hidden_states, c("high","med", "low"))
              
obs <- salesOrderHF1$selling_price_class

print(hmm_catstates)
# Sequence of observation
# a = sample(c(rep("L",100),rep("R",300)))
# b = sample(c(rep("L",300),rep("R",100)))
# a = sample(c(rep("L",100),rep("R",300)))
# b = sample(c(rep("L",300),rep("R",100)))
# observation = c(a,b)

# Baum-Welch
bw_cat <- baumWelch(hmm_catstates, obs, 10)
bw_fraud <- baumWelch(hmm_fraudstates, obs, 100)

print(bw_fraud)




