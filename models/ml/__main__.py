"""Description.

Usage example of the ml library. 

Help: 
    - Set the random seed with set PYTHONHASHSEED=0.
    - Run the script using python -m ml.
"""


from .utils import (
    load_pickle,
    DATA_PATH, 
)
from .config import load_config
from .training import train_models

from rich import print

print("Start training models...")
print("-"*100)
config = load_config()
print(config)
file_path = DATA_PATH + "train.pkl"
train = load_pickle(file_path)
print("-"*100)
train_models(
    train=train,
    config=config
) 


