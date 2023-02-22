library(R2jags)

###########
# MODEL SAB With JAGS
###########

model <- function(){
    # -----
    # PRIORS
    # -----
    # intercept
    intercept ~ dunif(-1e5, 1e5)
    # alpha
    alpha ~ dunif(-1e5, 1e5)
    # beta
    beta ~ dunif(-1e5, 1e5)
    # gamma
    #gamma ~ dunif(-1e5, 1e5)

    # -----
    # LIKELIHOOD
    # -----

    # presence absence, bernouilli
    for (i in 1:1000){
        logit(p[i]) <- intercept + alpha*lon[i] + beta*la[i] #+ gamma*year[i]
        pa[i] ~ dbern(p[i])
    }
}



# -----
# DATA
# -----

# presence absence
data_test %>% mutate(all_cl = ifelse(all_cl > 0, 1, 0)) -> data_test
# prends les 1000 premiers
data_test <- data_test[1:1000,]


jags_data <- list(
    pa = data_test$all_cl,
    lon = data_test$longitude,
    la = data_test$latitude
    #year = data_test$year_measured
    #n = nrow(data_test)
)

out <- jags(
    data = jags_data,
    n.chains = 3,
    inits = 
    list(c(intercept = 1,alpha =1, beta = 1),#, gamma = 1),
    c(intercept = 1,alpha =1, beta = 1),#, gamma = 1),
    c(intercept = 1,alpha =1, beta = 1)),#, gamma = 1)),
    parameters.to.save = c("intercept", "alpha", "beta"),#, "gamma"),
    model.file = model,
)

out_simple <- jags(
    data = jags_data,
    n.chains = 3,
    inits = 
    list(c(p = 0.5),
    c(p = 0.5),
    c(p = 0.5)),
    parameters.to.save = c("p"),
    model.file = model_simple,
)

# TEST !!

model_simple <- function(){
    # priors
    p ~ dunif(0, 1)
    # likelihood
    for (i in 1:1000){
        result[i] ~ dbern(p)
    }
}

test <- sample(0:1, 1000, replace = TRUE, prob = c(0.8, 0.2))

test_data <- list(
    result = as.integer(test)
)

out_test <- jags(
    data = test_data,
    n.chains = 3,
    inits = 
    function(){list(p = 0.5)},
    parameters.to.save = c("p"),
    model.file = model_simple,
)

model_sab <- function(){
    # priors
    a ~ dunif(-1e5, 1e5)
    b ~ dunif(-1e5, 1e5)
    c ~ dunif(-1e5, 1e5)
    # likelihood
    for (i in 1:n){
        # presence absence gaule
        logit(p[i]) <- a + b*deltaT[i] + c*adulte[i]
        pa[i] ~ dbern(p[i])
        
    }
}

model_simple <- function(){
    # priors
    p ~ dunif(0, 1)
    # likelihood
    for (i in 1:n){
        pa[i] ~ dbern(p)
    }
}

#10000 premiers
sap_pert1 <- sap_pert[1:10000,]

jags_data <- list(
    pa = sap_pert1$presence_gaule,
    deltaT = sap_pert1$deltaT,
    adulte = sap_pert1$arbres,
    n = nrow(sap_pert1)
)

out_sab <- jags(
    data = jags_data,
    n.chains = 3,
    inits = function(){
    list(a = runif(1,-500, -200),b =runif(1, -10,10), c = runif(1,1,10))},
    parameters.to.save = c("a", "b", "c"),
    model.file = model_sab
)

out_simple_sab <- jags(
    data = jags_data,
    n.chains = 3,
    inits = function(){
    list(p = runif(1,0,1))},
    parameters.to.save = c("p"),
    model.file = model_simple,
    n.iter = 10000
)

traceplot(out_simple_sab, pars = c("p"))
traceplot(out_sab, pars = c("a", "b", "c"))
