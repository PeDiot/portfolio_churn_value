"""Description.

Helpful methods to use the library.
"""
from pyreadr import read_r
from pickle import dump, load 

from pandas.core.frame import DataFrame
from typing import Dict

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