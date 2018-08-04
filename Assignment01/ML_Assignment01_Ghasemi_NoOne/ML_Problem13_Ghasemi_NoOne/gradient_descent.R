# ML_Assignment03_Ghasemi_NoOne
# Mohammad Sadegh Ghasemi


pinv <- function(A, eps=1e-8){
  L <- svd(A)
  d <- L$d
  i <- abs(d) > eps
  d[i] <- 1/d[i]
  L$v %*% diag(d, nrow=length(d)) %*% t(L$u)
}


normalize_features = function(X, mu=NULL, sigma_=NULL){
    m = nrow(X)
    Xnorm = matrix(0.0, nrow=nrow(X), ncol=ncol(X))

    if (is.null(mu)){
        mu = colMeans(X, dim=1)
    }
    if (is.null(sigma_)){
        sigma_ = apply(X, 2, sd)
    }
    # don't change the intercept term
    mu[1] = 0.0
    sigma_[1] = 1.0
    for (i in 1:m){
        Xnorm[i, ] = (X[i, ] - mu) / sigma_
    }
    # after wasting a couple of minutes on searching about returning multiple objects in R functions,
    # I decided to use bad way of changing global variables. ... Damn R
    Xnorm <<- Xnorm
    mu <<- mu
    sigma_ <<- sigma_
}


compute_cost = function(X, y, theta){
    m = nrow(y)
    costs = ((X %*% theta) - y) ^ 2
    cost <- sum(costs) / (2.0 * m)
}


gradient_descent = function(X, y, theta, alpha, num_iters, threshold){
    m = nrow(y)
    J_history = matrix(0.0, nrow=num_iters)
    final_idx = 0
    for (i in 1:num_iters) {
        h = X %*% theta
        errors = h - y
        delta = t(X) %*% errors
        theta = theta - (alpha / m) * delta
        J_history[i] = compute_cost(X, y, theta)
        final_idx = i
        if (!is.null(threshold) & i > 2) {
            if (
                (J_history[i - 1] - J_history[i]) < threshold &
                (J_history[i - 2] - J_history[i - 1]) < threshold
            ){
                break
            }
        }
    }
    if (final_idx == num_iters) {
        print('It did not converged! but J_history and theta computed approximately!')
    }
    theta <<-theta
    J_history <<- J_history[1:final_idx]
}


normal_equation = function(X, y){
    theta = pinv(t(X) %*% X) %*% t(X) %*% y
}


linearRegressionFit = function(X_train, y_train, alpha, threshold, max_iter, plotJ){
    if (nrow(X_train) != nrow(y_train)){
        print('X_train and y_train must be compatible!')
        return()
    }
    if (ncol(y_train) != 1){
        print('y_train should be in m * 1 dimension!')
        return()
    }

    initial_theta = rep(0.0, each=ncol(X_train))
    normalize_features(X_train)
    gradient_descent(Xnorm, y, initial_theta, alpha, num_iters, threshold)
    if (plotJ) {
        plot(J_history)
    }
    return(theta)
}


linearRegressionPredict = function(X_test , coefficients) {
    return(X %*% theta)
}


evaluate = function(y_train, y_test, y_predict) {
    SSE = sum((y_predict - y_test) ^ 2)
    y_train_mean = mean(y_train)
    SST = sum((y_train_mean - y_test) ^ 2)
    r.squared = 1 - (SSE / SST)
    RMSE = sqrt(SSE / nrow(y_test))

    return(c(RMSE, r.squared))
}



data = read.csv('data.csv', header=FALSE)
y = data.matrix(data[, 3])
X = data.matrix(cbind(1, data[, 1:2]))
alpha = 0.01
num_iters = 400
threshold = 1000

linearRegressionFit(X, y, alpha, threshold, num_iters, TRUE)
