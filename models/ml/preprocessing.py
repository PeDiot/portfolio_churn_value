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
   ...: X=train_prep.X,
   ...: y=train_prep.y,
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
CleanedData(X=      Gender Senior_Citizen Partner Dependents  Tenure_Months  ...        Contract Paperless_Billing    Payment_Method Monthly_Charges Total_Charges
0       Male             No      No         No            1.0  ...  Month-to-month               Yes  Electronic check           24.25         24.25
1       Male             No      No         No           14.0  ...  Month-to-month               Yes  Electronic check           89.95       1178.40
2     Female             No     Yes        Yes           72.0  ...        Two year               Yes     Bank transfer          115.80       8476.50
3       Male             No     Yes         No            9.0  ...  Month-to-month               Yes     Bank transfer           79.35        661.25
4       Male             No     Yes        Yes           68.0  ...        One year                No       Credit card          105.75       7322.50
...      ...            ...     ...        ...            ...  ...             ...               ...               ...             ...           ...
4917  Female             No     Yes        Yes           13.0  ...        Two year               Yes     Bank transfer           25.00        332.50
4918    Male             No      No        Yes           31.0  ...  Month-to-month                No       Credit card           25.00        789.20
4919    Male            Yes      No         No            7.0  ...  Month-to-month                No      Mailed check           73.75        545.15
4920    Male            Yes      No         No           13.0  ...  Month-to-month               Yes  Electronic check           59.90        788.35
4921    Male             No      No         No           19.0  ...  Month-to-month               Yes  Electronic check           99.95       1931.75

[4922 rows x 19 columns], y=array([2846., 5463., 5841., ..., 5638., 5254., 3670.]), numeric_features=['Tenure_Months', 'Monthly_Charges', 'Total_Charges'], categorical_features=['Gender', 'Senior_Citizen', 'Partner', 'Dependents', 'Phone_Service', 'Multiple_Lines', 'Internet_Service', 'Online_Security', 'Online_Backup', 'Device_Protection', 'Tech_Support', 'Streaming_TV', 'Streaming_Movies', 'Contract', 'Paperless_Billing', 'Payment_Method'])
"""

from os import stat
import pandas as pd
from pandas.core.frame import DataFrame
import numpy as np

from dataclasses import dataclass

from pyreadr import read_r
from pickle import dump, load 

from typing import (
    Dict, 
    Tuple, 
    List
)

import sys
sys.path.append('../')

BACKUP_PATH = "C:/Users/pemma/OneDrive - Université de Tours/Mécen/M2/S2/04 - 3R/portfolio_churn_value/backup/ml/"
DATA_PATH = "C:/Users/pemma/OneDrive - Université de Tours/Mécen/M2/S2/04 - 3R/portfolio_churn_value/data/"

def load_rdata(file_name: str) -> Dict[str, DataFrame]: 
    """Read R data file."""
    if file_name[-5:] not in ("RData", "Rda"): 
        raise ValueError("Only .RData files can be read.")
    file_path = DATA_PATH + file_name
    return read_r(path = file_path)

def load_pickle(file_path: str): 
    """Load pickle file."""
    if file_path[-3:] != "pkl": 
        raise ValueError("Only .pkl files can be read.") 
    with open(file_path, "rb") as file: 
        data = load(file) 
    return data 

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
        self._data = self._select_variables(data)  
        self.X = self._get_feature_vector(self._data)
        self.y = self._get_target_vector(self._data)
    
    @staticmethod
    def _select_variables(data: DataFrame) -> DataFrame: 
        """Return data with interesting variables."""
        return data.drop(
            labels=[
                "CustomerID",
                "City", 
                "Latitude", 
                "Longitude", 
                "Zip_Code",
                "Churn_Label", 
                "Churn_Value", 
                "Churn_Score", 
                "Churn_Reason", 
            ], 
            axis=1
        )
    
    @staticmethod
    def _get_feature_vector(data: DataFrame) -> DataFrame: 
        """Return the matrix of features."""
        return data.drop(labels=["CLTV"], axis=1)

    @staticmethod
    def _get_target_vector(data: DataFrame) -> DataFrame: 
        """Return target variable."""
        return data["CLTV"].values

    def get_numeric_features(self) -> List[str]:
        """Return names of numeric columns."""
        return [ 
            col
            for col in self.X.columns
            if self.X[col].dtype == "float"
        ] 

    def get_categorical_features(self) -> List[str]:
        """Return names of categorical columns."""
        return [ 
            col
            for col in self.X.columns 
            if self.X[col].dtype == "category"
        ] 
