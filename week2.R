# simulate n sets of correlated normal random variables
# int, num[2], num -> num[n, 2]
sim_norm2 = function(n=10^5, sigma=c(1,1), rho=0){
    # L is Choleski decomposition of covariance matrix
    L = diag(sigma) %*% matrix(c(1, rho, 0, sqrt(1-rho^2)), 2, 2)
    L %*% matrix(rnorm(n*2), nrow=2)
}

# num, int -> list(num[n], num[n])
# given initial expected return and periods, simulate expected returns and returns
sim_returns = function(n=10^5, x0=0, phi=.94, sigma=c(.018, .18), rho=-.80756){
    X = sim_norm2(n, sigma=sigma, rho=rho)
    epsilon = X[1, ]
    # delta_{t+1} = delta1[i]
    delta1 = X[2, ]
    
    xt = rep(0, n)
    # r_{t+1} = rt1[i]
    rt1 = rep(0, n)
    
    xt[1] = x0
    for (i in 2:n){
        xt[i] = phi * xt[i-1] + epsilon[i]
    } 
    
    # r_{t+1} = rt[i]
    rt1 = xt + delta1
    
    #list(rep(0, n), rep(0, n))
    list(xt, rt1)
}


library(testthat)


test_week2 = function(){
    tol = 0.1
    expect_approx = function(x, y){
        expect_less_than(abs(x - y), tol)
    }
    
    test_that('sim_norm2 produces correct cor and sd', {
        X = sim_norm2(sigma=c(0.5, 5), rho=0.2)
        x = X[1,]
        y = X[2,]
        expect_approx(cor(x,y), 0.2)
        expect_approx(sd(x), 0.5)
        expect_approx(sd(y), 5)
    })
    
    test_that('sim_returns produce correct time series', {
        N = 10^7
        Y = sim_returns(n=N)
        t = 1:N
        xt = Y[[1]]
        rt1 = Y[[2]]
        #plot(t, xt, 'l', col='red')
        #plot(t, rt1, 'l')
        fit = lm(rt1~xt)
        print(summary(fit))
    })
    
}
test_week2()

