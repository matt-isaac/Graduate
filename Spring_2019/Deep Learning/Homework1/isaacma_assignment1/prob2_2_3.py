# -*- coding: utf-8 -*-
"""
Problem 5.2

Created on Wed Jan  9 09:30:40 2019

@author: misaa
"""

import numpy as np
import matplotlib.pyplot as plt

# randomly generate N points sampled uniformly 
# in the interval x ∈ [−1, 3]

def gen_n_points(n):
    return(np.random.uniform(-1, 3, size = n))
    
def gen_resp(inpt):
    resp = (inpt * inpt) - 3 * inpt + 1
#    inpt_resp = np.vstack((inpt, resp))
    return(resp)
    
def add_noise(resp, sigma):
    noise = np.random.normal(loc = 0, scale = sigma, size = len(resp))
    resp_wnoise = resp + noise
    return(resp_wnoise)
    
def master(n, sigma):
    xs = gen_n_points(n)
    ys = gen_resp(xs)
    ys = add_noise(ys, sigma)
    plt.scatter(xs, ys)
    plt.title("N = " + str(n) + "; Sigma = " + str(sigma))
    plt.xlabel('x')
    plt.ylabel('y')
    plt.show()
    return({'x': xs, 'y': ys})
    
n15_s0 = master(15, 0)
n15_s05 = master(15, 0.05)
n15_s2 = master(15, 0.2)   

n100_s0 = master(100, 0)
n100_s05 = master(100, 0.05)
n100_s2 = master(100, 0.2)

def exp_mat(x, degree):
    xmat = np.array(x)
    if(degree >= 2):
        for i in range(2, degree + 1):
            newcol = np.power(x, i)
            xmat = np.vstack((xmat, newcol))
    xmat = np.vstack((xmat, np.ones(shape = (1, len(x)))))
    return(xmat)
    
def find_weights(x, y, degree):
    x = exp_mat(x, degree)
    y = np.reshape(y, (len(y),1))
    
    t1 = np.linalg.inv(np.matmul(x, np.transpose(x)))
    t2 = np.dot(x, y)
    w = np.dot(t1, t2)
    
    mse = calc_mse(x, y, w)
    print('MSE:' + str(mse))
    
    return(w)
    
def poly_fit(data, n, sigma):
    x = data['x']
    y = data['y']
    
    w1 = find_weights(x, y, degree = 1)
    w2 = find_weights(x, y, degree = 2)
    w9 = find_weights(x, y, degree = 9)
    
    x_grid = np.linspace(min(x), max(x), num = 100)
    
    xs_grid1 = exp_mat(x_grid, 1)
    xs_grid2 = exp_mat(x_grid, 2)
    xs_grid9 = exp_mat(x_grid, 9)
    
    yhat1 = np.dot(np.transpose(xs_grid1), w1)
    yhat2 = np.dot(np.transpose(xs_grid2), w2)
    yhat9 = np.dot(np.transpose(xs_grid9), w9)
    
    plt.plot(x_grid, yhat1, label = "degree = 1")
    plt.plot(x_grid, yhat2, label = "degree = 2")
    plt.plot(x_grid, yhat9, label = "degree = 9")
    plt.scatter(x, y)
    plt.legend(loc = 'upper right')
    plt.title("N = " + str(n) + "; Sigma = " + str(sigma))
    plt.xlabel('x')
    plt.ylabel('y')
    plt.show()
    
    return(w1, w2, w9)
    
def calc_mse(x, y, weights):    
    error = y - np.dot(np.transpose(x), weights)
    mse = (1/len(y)) * np.dot(np.transpose(error), error)
    return(mse)    
    
n15_1_w = poly_fit(n15_s0, n = 15, sigma = 0)   
n15_2_w = poly_fit(n15_s05, n = 15, sigma = 0.05)     
n15_3_w = poly_fit(n15_s2, n = 15, sigma = 0.2)     
#
n100_1_w = poly_fit(n100_s0, n = 100, sigma = 0)   
n100_2_w = poly_fit(n100_s05, n = 100, sigma = 0.05)     
n100_3_w = poly_fit(n100_s2, n = 100, sigma = 0.2)


    
