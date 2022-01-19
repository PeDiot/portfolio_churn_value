"""Decription. 

Script for data preprocessing.

Example: 

In [1]: from ml import (
   ...: BACKUP_PATH,
   ...: DATA_PATH,
   ...: load_rdata,
   ...: CleanedData,
   ...: Preprocessing,
   ...: )

In [2]: r_data = load_rdata(file_name="train_test_data.RData")

In [3]: data_train, data_test = r_data.values()

In [4]: train_prep = Preprocessing(data=data_train)

In [5]: cleaned_train_data = CleanedData(
   ...: X=train_prep.get_feature_vector(),                          
   ...: y=train_prep.get_target_vector(),
   ...: numeric_features=train_prep.get_numeric_features(),      
   ...: categorical_features=train_prep.get_categorical_features()
   ...: )

In [6]: file_path = DATA_PATH + "train.pkl"   

In [8]: cleaned_train_data.to_pickle(file_path=file_path)        

In [9]: from pickle import load

In [10]: with open(file_path, "rb") as file:
    ...:     train = load(file)

In [11]: train
Out[11]: 
CleanedData(X=       Latitude   Longitude  ... Monthly_Charges Total_Charges
2     36.641520 -121.622188  ...          115.80       8476.50
3     37.990574 -120.261821  ...           79.35        661.25
4     34.066964 -117.937007  ...          105.75       7322.50
...         ...         ...  ...             ...           ...
4917  37.801776 -122.402293  ...           25.00        332.50
4918  33.000269 -117.072093  ...           25.00        789.20
4919  39.117537 -122.284654  ...           73.75        545.15
4920  38.232389 -122.324944  ...           59.90        788.35
4921  36.798882 -120.019511  ...           99.95       1931.75

[4922 rows x 20 columns], y=array([2846., 5463., 5841., ..., 5638., 5254., 3670.]), numeric_features=['Latitude', 'Longitude', 'Monthly_Charges', 'Total_Charges', 'CLTV'], categorical_features=['Gender', 'Senior_Citizen', 'Partner', 'Dependents', 'Phone_Service', 'Multiple_Lines', 'Internet_Service', 'Online_Security', 'Online_Backup', 'Device_Protection', 'Tech_Support', 'Streaming_TV', 'Streaming_Movies', 'Contract', 'Paperless_Billing', 'Payment_Method'])In [12]: type(train)
Out[12]: ml.preprocessing.CleanedData

In [13]: train.X
Out[13]: 
       Latitude   Longitude  Gender  ...    Payment_Method Monthly_Charges Total_Charges
0     36.527243 -118.594938    Male  ...  Electronic check           24.25         24.25
1     33.936291 -118.332639    Male  ...  Electronic check           89.95       1178.40
2     36.641520 -121.622188  Female  ...     Bank transfer          115.80       8476.50
3     37.990574 -120.261821    Male  ...     Bank transfer           79.35        661.25
4     34.066964 -117.937007    Male  ...       Credit card          105.75       7322.50
...         ...         ...     ...  ...               ...             ...           ...
4917  37.801776 -122.402293  Female  ...     Bank transfer           25.00        332.50
4918  33.000269 -117.072093    Male  ...       Credit card           25.00        789.20
4919  39.117537 -122.284654    Male  ...      Mailed check           73.75        545.15
4920  38.232389 -122.324944    Male  ...  Electronic check           59.90        788.35
4921  36.798882 -120.019511    Male  ...  Electronic check           99.95       1931.75

[4922 rows x 20 columns]

"""

import pandas as pd
from pandas.core.frame import DataFrame
import numpy as np

from dataclasses import dataclass

from pyreadr import read_r
from pickle import dump 

from typing import (
    Dict, 
    Tuple, 
    List
)

import sys
sys.path.append('../')

BACKUP_PATH = "./ml/backup/"
DATA_PATH = "C:/Users/pemma/OneDrive - Université de Tours/Mécen/M2/S2/04 - 3R/portfolio_churn_value/data/"

def load_rdata(file_name: str) -> Dict[str, DataFrame]: 
    """Read R data file."""
    if file_name[-5:] not in ("RData", "Rda"): 
        raise ValueError("Only .RData files can be read.")
    file_path = DATA_PATH + file_name
    return read_r(path = file_path)

@dataclass
class CleanedData: 
    X: DataFrame
    y: np.ndarray
    numeric_features: List[str]
    categorical_features: List[str] 

    def to_pickle(self, file_path: str): 
        """Save to pickle."""
        with open(file_path, "wb") as file: 
            dump(self, file) 

class Preprocessing:
    """Automate preprocessing on Telco data."""

    def __init__(self, data: DataFrame):
        self.data = self.select_variables(data)  
    
    @staticmethod
    def select_variables(data: DataFrame) -> DataFrame: 
        """Return data with interesting variables."""
        return data.drop(
            labels=[
                "CustomerID",
                "City", 
                "Zip_Code",
                "Tenure_Months",  
                "Churn_Label", 
                "Churn_Value", 
                "Churn_Score", 
                "Churn_Reason", 
            ], 
            axis=1
        )

    def get_numeric_features(self) -> List[str]:
        """Return names of numeric columns."""
        return [ 
            col
            for col in self.data 
            if self.data[col].dtype == "float"
        ] 

    def get_categorical_features(self) -> List[str]:
        """Return names of categorical columns."""
        return [ 
            col
            for col in self.data 
            if self.data[col].dtype == "category"
        ] 

    def get_feature_vector(self) -> DataFrame: 
        """Return the matrix of features."""
        return self.data.drop(labels=["CLTV"], axis=1)

    def get_target_vector(self) -> DataFrame: 
        """Return target variable."""
        return self.data["CLTV"].values
