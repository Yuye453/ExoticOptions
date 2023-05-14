# Down-and-out European Put

# Inputs
S = 2000     # Stock price at time zero  
E = 1800     # Strike Price
T = 1       # Time Horizon in years
r = 0.05    # Risk free rate
sigma = 0.3 # Volatility
M = 5      # Number of time intervals
B = 1000    # Barrier price B<S
p = 1/2     # Probability of an increment in the stock price

# Function
DownOut_Euro_Put_option = function(S, E, T, r, sigma, M, B, p) {
    
    # Step size
    delta_t = T/M
    
    # Defining u and d
    u = exp(sigma * sqrt(delta_t) + (r - (sigma ^ 2)/2) * delta_t)
    d = exp(- sigma * sqrt(delta_t) + (r - (sigma ^ 2)/2) * delta_t)
    
    # Compute asset prices at time T
    S_Mj = rep(0, M+1) # Generating a vector with M+1 zeros
    
    for (j in 0:M) {
        S_Mj[j+1] = S * u^j * d^(M-j)
    }    
    
    # Computing option values at time T
    W = rep(0, M+1) # We create a vector with M+1 zeros
    
    for (i in 0:M) {
        if (S_Mj[i+1] >= B) {
            W[i+1] = pmax(E - S_Mj[i+1], 0)
        } else {
            W[i+1] = 0
        }
    }
    
    # Option value at time zero
    S_ij = matrix(nrow = M, ncol = M) # Generating a matrix with M rows and M columns
    
    for (i in (M-1):0) {
        
        for (j in 0:i) {
            S_ij[i+1,j+1] = S * u^j * d^(M-j)
        }
        
        W = exp(-r*delta_t) * ((1-p)*W[1:(i+1)] + p*W[2:(i+2)])
        
        for (j in 0:i) {
            if (S_ij[i+1,j+1] < B) {
                W[j+1] = 0
            }
        }
        
    }
    
    return(W)
}

# Example
DownOut_Euro_Put_option(S = 2000, E = 1800, T = 1, r = 0.05, sigma = 0.3, M = 5, B = 1000, p = 1/2)

