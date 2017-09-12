from __future__ import division
import numpy as np
from hmmlearn import hmm
from sklearn.preprocessing import LabelEncoder

###############
    
import pandas as pd
import numpy as np
import os
    
os.chdir('/users/admin/Google Drive/Enterprise/EnterpriseSimulation')
    
c1Hist = pd.read_csv('c1HistMode.csv')
c2Hist = pd.read_csv('c2HistMode.csv')
    
files = filter(os.path.isfile, os.listdir( os.curdir ) ) 
    
c1Hist['value'].as_matrix
c2Hist['value'].as_matrix
    
import numpy as np
from hmmlearn import hmm    

np.random.seed(42)
    
model = hmm.MultinomialHMM(n_components=3)    
model.startprob_ = np.array([0.6, 0.3, 0.1])
    
model.transmat_ = np.array([[0.7, 0.2, 0.1],
                            [0.3, 0.5, 0.2],
                            [0.3, 0.3, 0.4]])
    
model.emissionprob_ = np.array([[0.2, 0.1, 0.1, 0.1],
                                [0.1, 0.1, 0.1, 0.1],
                                [0.1, 0.1, 0.1, 0.1]])
    
    
X1 = map(lambda x: [x], c1Hist['value'].values)
X2 = map(lambda x: [x], c2Hist['value'].values)
    
    
X = np.concatenate([X1, X2])
lengths = [len(X1), len(X2)]
        
#######################
    
mkvHists = pd.read_csv('mkvHistsHealthyClusMulti.csv')
mkvLengths = pd.read_csv('mkvLengthsHealthyMulti.csv')