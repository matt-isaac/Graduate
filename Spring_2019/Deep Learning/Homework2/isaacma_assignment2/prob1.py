# -*- coding: utf-8 -*-
"""
Deep Learning - Homework 2
prob1.py

@author: misaa
"""

import numpy as np
import matplotlib.pyplot as plt

def gen_n_points(n):
    """
    Returns n uniformly distributed, randomly generated values
    """
    return(np.random.uniform(-1, 3, size = n))
    
def gen_resp(inpt):
    """
    Evaluates and returns response values corresponding to inpt. 
    y = x^2 - 3x + 2
    """
    resp = (inpt * inpt) - 3 * inpt + 1
    return(resp)
    
def add_noise(resp, sigma):
    """
    adds gaussian 0-mean noise, with standard deviation sigma to resp.
    """
    noise = np.random.normal(loc = 0, scale = np.sqrt(sigma), size = len(resp))
    resp_wnoise = resp + noise
    return(resp_wnoise)
    
def generate_data(n, sigma):
    """
    Generates n data points to be used for this exercise. 
    """
    xs = gen_n_points(n) # generate x values
    ys = gen_resp(xs) # generate y values
    ys = add_noise(ys, sigma) # add noise to y values
    plt.scatter(xs, ys) # create scatter plot
    plt.title("N = " + str(n) + "; Sigma = " + str(sigma))
    plt.xlabel('x')
    plt.ylabel('y')
    plt.show()
    return({'x': xs, 'y': ys})

def exp_mat(x, degree):
    """
    Takes 1D array (x) and makes a matrix where the columns correspond
    to x, x^2, x^3, ..., x^degree.
    """
    xmat = np.array(x)
    #only need to change array if degree is >= 2
    if(degree >= 2): 
        for i in range(2, degree + 1): # iterate over degrees
            newcol = np.power(x, i) # create new column
            xmat = np.vstack((xmat, newcol)) # add it to matrix
    xmat = np.vstack((xmat, np.ones(shape = (1, len(x))))) # add column of 1s for intercept
    return(np.transpose(xmat)) # had to transpose to get dimensions correct...
    
def find_weights(x, y, degree):
    """
    finds and returns optimal weights for a polynomial of degree 'degree'. 
    """
    x = exp_mat(x, degree) # create data matrix with columns corresponding to x, x^2, ..., x^degree. 
    
    y = np.reshape(y, (len(y),1)) # format ys
    
    t1 = np.linalg.inv(np.matmul(np.transpose(x), x)) # [t]erm [1] to calculate weights vector
    t2 = np.dot(x, y) # [t]erm [2] to calculate weights vector
    w = np.dot(t1, t2) # dot product of t1 and t2 to obtain weights
    
    mse = calc_mse(x, y, w) # calculate MSE
    print('MSE:' + str(mse))
    
    return(w) 
    
def find_weights_ridge(x, y, degree, lmbda):
    """
    find and return weights for ridge regression
    See comments for find_weights() - works nearly exactly the same. 
    lmbda is penalization value
    """
    
    x = exp_mat(x, degree)
    y = np.reshape(y, (len(y),1))
    
    t1 = np.linalg.inv((np.diag([lmbda]*(degree+1))) + np.matmul(np.transpose(x), x))
    t2 = np.dot(np.transpose(x), y)
    w = np.dot(t1, t2)
    
    mse = calc_mse(x, y, w)
    print('MSE:' + str(mse))
    
    return(w)
    
def poly_fit(data, n, sigma, lmbda):
    """
    top-level function to fit ridge regression. 
    Calls find_weights_ridge(), and plots polynomial. 
    """
    x = data['x']
    y = data['y']
    
#    w1 = find_weights(x, y, degree = 1)
#    w2 = find_weights(x, y, degree = 2)
    w9 = find_weights_ridge(x, y, degree = 9, lmbda = lmbda)
    
    x_grid = np.linspace(min(x), max(x), num = 100)
    
#    xs_grid1 = exp_mat(x_grid, 1)
#    xs_grid2 = exp_mat(x_grid, 2)
    xs_grid9 = exp_mat(x_grid, 9)
    
#    yhat1 = np.dot(np.transpose(xs_grid1), w1)
#    yhat2 = np.dot(np.transpose(xs_grid2), w2)
    yhat9 = np.dot(xs_grid9, w9)
    
#    plt.plot(x_grid, yhat1, label = "degree = 1")
#    plt.plot(x_grid, yhat2, label = "degree = 2")
    plt.plot(x_grid, yhat9, label = "degree = 9")
    plt.scatter(x, y)
    plt.legend(loc = 'upper right')
    plt.title("N = " + str(n) + "; Sigma = " + str(sigma) + "; Lambda = " + str(lmbda))
    plt.xlabel('x')
    plt.ylabel('y')
    plt.show()
    
#    return(w1, w2, w9)
    return(w9)
    
def calc_mse(x, y, weights):  
    """
    calculates MSE
    """
    error = y - np.dot(x, weights)
    mse = (1/len(y)) * np.dot(np.transpose(error), error)
    return(mse)    
    
# Generate Data    
np.random.seed(seed = 1234)
np.random.seed(seed = 321)
 

#n15_s05 = generate_data(15, 0.05)
#n15_2_w = poly_fit(n15_s05, n = 15, sigma = 0.05, lmbda = 10) # underfitting
#n15_2_w = poly_fit(n15_s05, n = 15, sigma = 0.05, lmbda = -.005) # overfitting
#n15_2_w = poly_fit(n15_s05, n = 15, sigma = 0.05, lmbda = 0.05) # appropriate fit
 
n100_s05 = generate_data(100, 0.05)    
#n100_2_w = poly_fit(n100_s05, n = 100, sigma = 0.05, lmbda = 50) # underfitting
n100_2_w = poly_fit(n100_s05, n = 100, sigma = 0.05, lmbda = 0.005) # overfitting
#n100_2_w = poly_fit(n100_s05, n = 100, sigma = 0.05, lmbda = 1) # appropriate fit

