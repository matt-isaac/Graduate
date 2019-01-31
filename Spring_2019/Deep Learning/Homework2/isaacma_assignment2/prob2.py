# -*- coding: utf-8 -*-
"""
Deep Learning - Homework 2
prob2.py

@author: misaa
"""

import scipy.spatial.distance as sp
import scipy.stats as ss
import numpy as np
import matplotlib.pyplot as plt

# read in data:
filename = 'data_seed.dat'
dat = np.fromfile(filename, sep = '\t')
dat = np.reshape(dat, (210,8))
      
def knn_single_point(data, obs, k):
    """
    Finds the k nearest neighbors, takes a majority vote, and returns the 
    predicted class for the observation in question
    
    Parameters
    ----------
    data : numpy array
        'training' data, with class as last column
        
    obs : numpy array
        observation to have class predicted. 
        
    Returns
    -------
    maj_vote : numeric
        The predicted class resulting from the majority vote among the k nearest
        neighbors
    """
    rows, cols = np.shape(data)
    distances = np.zeros(shape = (rows, 1)) # container for calculated distances
    data = np.hstack((data, distances))
    for row in data: #iterate over data
        dist = sp.euclidean(obs, row[0:7])# calculate distance betwen obs and row.
        row[8] = dist
    data_sorted = data[np.argsort(data[:, 8])] # sort distances
    k_nn = data_sorted[0:k,:] # take top k nearest neighbors
    maj_vote = ss.mode(k_nn[:,7])[0][0] # gets voted value and extracts from array
    return(maj_vote)

def knn_classification(train_data, test_data, k):
    """
    performs knn classification on train_data, predicts classification for 
    test_data. 
    """
    # format test_data
    test_rows, test_cols = np.shape(test_data)
    zeros = np.zeros(shape = (test_rows, 2))
    test_data = np.hstack((test_data, zeros))
    
    # iterate over test_data and make prediction.
    for row in test_data:
        obs = row[0:7]
        pred_class = knn_single_point(data = train_data, obs = obs, k = k)
        row[8] = pred_class
        row[9] = pred_class == row[7]
    accuracy = np.sum(test_data[:,9])/test_rows
    return(accuracy)
        
def cv_5fold(data, k):
    """
    Performs 5fold cross validation on k-nn classification
    """
    np.random.shuffle(data) # shuffle data
    l = data.shape[0]
    # split data into 5 equal parts.
    i1, i2, i3, i4= int(l * 0.2), int(l * 0.4), int(l * 0.6), int(l * 0.8)
    d1, d2, d3, d4, d5 = data[: i1], data[i1 + 1 : i2], data[i2 + 1 : i3], data[i3 + 1 : i4], data[i4 + 1 :]
    
    data_list = [d1, d2, d3, d4, d5]
    pa = []
    counter = 0
    for ds in data_list: # iterate over 5 data partitions
        train_list = [x for i,x in enumerate(data_list) if i!=counter] # use other 4 data sets for training
        train_data = np.vstack(train_list) # format training data
        test_data = ds 
        result = knn_classification(train_data, test_data, k) # peform knn classification
        pa.append(result) # save accuracy result
    avg_pa = np.round(np.mean(pa), decimals = 3) # take average of all 5 results. 
    print(avg_pa)
    return(avg_pa)
    
def cv_loo(data, k):
    """
    Performs leave one out cross validation on k-nn classification
    """
    np.random.shuffle(data)
    pa = []
    counter = 0
    for row in data: # iterate over 1 row at a time
        data2 = data
        row = np.reshape(row, newshape = (1, 8)) # reformat row
        train_data = np.delete(data2, (counter), axis = 0) # remove row from data temporarily
        result = knn_classification(train_data, row, k) # perform knn classification
        pa.append(result)
        counter = counter + 1
    avg_pa = np.round(np.mean(pa), decimals = 3) # average accuracy results. 
    print(avg_pa)
    return(avg_pa)

# run knn for all values of k
ks = [1, 5, 10, 15]
accuracy_l = []
accuracy_5 = []
for k in ks:
    result_l = cv_loo(data = dat, k = k)
    accuracy_l.append(1 - result_l)
    
    res_5 = cv_5fold(data = dat, k = k)
    accuracy_5.append(1 - res_5)
    
plt.plot(ks, accuracy_l, label = "LOOCV")
plt.plot(ks, accuracy_5, label = "5-fold CV")
plt.xlabel("k")
plt.ylabel("accuracy")
plt.title("CV Results")
plt.legend(loc = 'upper right')
plt.show()


