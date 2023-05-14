# Euro-Put

# Inputs
S = 100     # Stock price at time zero  
E = 120     # Strike Price
T = 1       # Time Horizon in years
r = 0.05    # Risk free rate
sigma = 0.3 # Volatility
M = 50      # Number of time intervals
p = 1/2     # Probability of an increment in the stock price

# Function
euro_put_option = function(S, E, T, r, sigma, M, p) {
    
    # Step size
    delta_t = T/M
    
    # Defining u and d
    u = exp(sigma * sqrt(delta_t) + (r - (sigma ^ 2)/2) * delta_t)
    d = exp(- sigma * sqrt(delta_t) + (r - (sigma ^ 2)/2) * delta_t)
    
    # Computing option values at time T
    W = rep(0, M+1) # We create a vector of M+1 zeros
    
    for (i in 0:M) {
        W[i+1] = max(E - S * d^(M-i) * u^i, 0)
    }
    
    # Option value at time zero
    for (i in (M-1):0) {
        W = exp(-r * delta_t) * ((1 - p) * W[1:(i+1)] + p * W[2:(i+2)])
    }
    
    return(W)
}

# Example
euro_put_option(S = 100, E = 120, T = 1, r = 0.05, sigma = 0.3, M = 50, p = 1/2)

