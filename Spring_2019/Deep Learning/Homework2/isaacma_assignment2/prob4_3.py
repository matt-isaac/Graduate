# -*- coding: utf-8 -*-
"""
Deep Learning - Homework 2
prob4_3.py

@author: misaa
"""

import numpy as np

def percep(x_array, w_array, bias):
    """
    takes inputs (x_array), weights (w_array), and bias. 
    Returns output of perceptron. 
    """
    value = np.dot(w_array, x_array)
    if (value + bias) <= 0:
        return 0
    else:
        return 1
    
def sigmoid(x_array, w_array, bias):
        """
    takes inputs (x_array), weights (w_array), and bias. 
    Returns output of sigmoid neuron. 
    """
    value = np.dot(w_array, x_array) + bias
    return(1/(1+np.exp(-value)))
  
def network(x_array, node_type):
    """
    This function represents the network described in problem 4.3, 
    by utilizing either peprceptron() or sigmoid(). User can choose
    perceptrons or sigmoid neurons by the paramter 'node_type'. 
    Possible values are 'perceptron' or 'sigmoid'
    """
    # initialize weight arrays
    w1_array = np.array([0.6, 0.5, -0.6])
    w2_array = np.array([-0.7, 0.4, 0.8])
    
    #initialize biases
    b1 = -0.4
    b2 = -0.5
    
    # outputs from 1st layer
    p1_out = node_type(x_array, w1_array, b1)
    p2_out = node_type(x_array, w2_array, b2)
    
    # inputs to second layer come from outputs of first
    x2_array = np.array([p1_out, p2_out])
    w3_array = np.array([1, 1])
    b3 = -0.5
    
    # get final node output, and return
    p3_out = np.round(node_type(x2_array, w3_array, bias = b3), decimals = 4)
    print(str(p3_out))
    return(p3_out)
    
    
x_array1 = np.array([0, 0, 0])
x_array2 = np.array([1, 0, 0])
x_array3 = np.array([0, 1, 0])
x_array4 = np.array([0, 0, 1])
x_array5 = np.array([1, 1, 0])
x_array6 = np.array([0, 1, 1])
x_array7 = np.array([1, 0, 1])
x_array8 = np.array([1, 1, 1])

    
network(x_array1, sigmoid)
network(x_array2, sigmoid)
network(x_array3, sigmoid)
network(x_array4, sigmoid)
network(x_array5, sigmoid)
network(x_array6, sigmoid)
network(x_array7, sigmoid)
network(x_array8, sigmoid)

