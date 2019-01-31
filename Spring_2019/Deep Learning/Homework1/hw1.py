# -*- coding: utf-8 -*-
"""
DEEP LEARNING - HWK 1

Created on Tue Jan  8 13:21:40 2019

@author: misaa
"""

import numpy as np

"""
Problem 2.3

"""

a = np.matrix('1 -2; 3 4; -5 6')
b = np.matrix('1 0 0; 0 1 0; 0 0 1')
c = np.matrix('2 2 1; 2 3 2; 1 2 2')

def is_symmetric(a, tol = 1e-8):
    if np.shape(a)[0] != np.shape(a)[1]:
        return False
    else:
        return (np.abs(a - a.T) <= tol).all()


def check_psd(mat):
    #check symmetric
    if(not is_symmetric(mat)):
        return("not symmetric")
    else:
        is_pd = False
        is_psd = False
        print(np.linalg.eigvals(mat))
        # Check eigenvalues > 0
        if np.all(np.linalg.eigvals(mat) > 0):
            is_pd = True
            is_psd = True
    
        # check eigenvalues >= 0
        elif np.all(np.linalg.eigvals(mat) >= 0):
            is_pd = False
            is_psd = True
        return(is_pd, is_psd)
        
        
check_psd(a) # not PD, not PSD
check_psd(np.matmul(np.transpose(a),a)) # is PD, is PSD
check_psd(np.matmul(a, np.transpose(a))) # not PD, is PSD
check_psd(b) # is PD, is PSD
check_psd(-b) # not PD, not PSD
check_psd(c) # is PD, is PSD
check_psd(c - (0.1 * b)) # is PD, is PSD
check_psd(c - (0.01 * np.matmul(a, np.transpose(a)))) # not PD, not PSD

