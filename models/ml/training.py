"""Description.

Automate training process using pipelines, grid search and cross-validation.

Example: 

set PYTHONHASHSEED=0

In [1]: from ml import (
   ...: load_pickle,
   ...: load_config,
   ...: train_models
   ...: )

In [2]: config = load_config(file_name="config_init.yaml")

In [3]: from ml import DATA_PATH

In [4]: file_path = DATA_PATH + "train.pkl"

In [5]: train = load_pickle(file_path)

In [7]: train_models(
   ...: train=train,
   ...: config=config,
   ...: comp_grid=[5, 10, 15, 20, None]
   ...: ) 
Training(estimator=DummyRegressor(), params={'strategy': 'mean'}, n_comp=10)
[Parallel(n_jobs=5)]: Using backend ThreadingBackend with 5 concurrent workers.
[CV] END .................., score=(train=0.000, test=-0.000) total time=   0.7s
[CV] END .................., score=(train=0.000, test=-0.000) total time=   0.8s
[Parallel(n_jobs=5)]: Done   2 out of   5 | elapsed:    1.1s remaining:    1.7s
[CV] END .................., score=(train=0.000, test=-0.023) total time=   0.8s
[CV] END .................., score=(train=0.000, test=-0.002) total time=   0.8s
[CV] END .................., score=(train=0.000, test=-0.006) total time=   0.8s
[Parallel(n_jobs=5)]: Done   5 out of   5 | elapsed:    1.1s finished...
"""

import numpy as np 
import pandas as pd

from pickle import dump 
from os import cpu_count, mkdir
from os.path import isdir, isfile

from random import shuffle

from rich import print

from typing import (
    List, 
    Dict, 
    Optional, 
    Tuple, 
)

from joblib import Parallel, delayed

from sklearn.decomposition import PCA

from sklearn.preprocessing import StandardScaler, OneHotEncoder

from sklearn.pipeline import Pipeline

from sklearn.compose import ColumnTransformer

from sklearn.model_selection import (
    ParameterGrid, 
    cross_validate, 
)

from .config import Config
from .utils import BACKUP_PATH
from .preprocessing import CleanedData

CPU_COUNT = cpu_count()
         
class ModelTraining: 
    """Fit model using pipeline and cross-validation after PCA.
    
    Example: 
    
    In [1]: from ml import (
   ...: DATA_PATH,
   ...: load_pickle,
   ...: ModelTraining
   ...: )

    In [2]: file_path = DATA_PATH + "train.pkl"

    In [3]: train = load_pickle(file_path)

    In [4]: from sklearn.linear_model import LinearRegression

    In [5]: est = LinearRegression()

    In [6]: params = {"fit_intercept": True}

    In [7]: training = ModelTraining(
    ...: train=train,
    ...: estimator=est,
    ...: params=params
    ...: )

    In [8]: p = training.init_pipeline()

    In [9]: p
    Out[9]: 
    Pipeline(steps=[('preprocessor',
                    ColumnTransformer(transformers=[('num', StandardScaler(),
                                                    ['Latitude', 'Longitude',
                                                    'Monthly_Charges',
                                                    'Total_Charges']),
                                                    ('cat',
                                                    OneHotEncoder(drop='first',
                                                                    handle_unknown='ignore'),
                                                    ['Gender', 'Senior_Citizen',
                                                    'Partner', 'Dependents',
                                                    'Phone_Service',
                                                    'Multiple_Lines',
                                                    'Internet_Service',
                                                    'Online_Security',
                                                    'Online_Backup',
                                                    'Device_Protection',
                                                    'Tech_Support',
                                                    'Streaming_TV',
                                                    'Streaming_Movies',
                                                    'Contract',
                                                    'Paperless_Billing',
                                                    'Payment_Method'])])),
                    ('model', LinearRegression())])

    In [10]: cv_res = training.cross_val_fit(p=p, cv=5)
    [Parallel(n_jobs=5)]: Using backend LokyBackend with 5 concurrent workers.
    [CV] END ..................., score=(train=0.147, test=0.164) total time=   0.0s
    [CV] END ..................., score=(train=0.152, test=0.140) total time=   0.1s
    [CV] END ..................., score=(train=0.162, test=0.096) total time=   0.1s
    [Parallel(n_jobs=5)]: Done   2 out of   5 | elapsed:    3.4s remaining:    5.2s
    [CV] END ..................., score=(train=0.147, test=0.158) total time=   0.1s
    [CV] END ..................., score=(train=0.152, test=0.119) total time=   0.1s
    [Parallel(n_jobs=5)]: Done   5 out of   5 | elapsed:    3.4s finished

    In [11]: cv_res
    Out[11]: 
    {'fit_time': array([0.11214757, 0.12530136, 0.12597275, 0.12541962, 0.1262989 ]),
    'score_time': array([0.02525163, 0.02701044, 0.03919625, 0.03423405, 0.03022146]),
    'estimator': ..., 
    'test_score': array([0.16396312, 0.13968953, 0.11936518, 0.15795364, 0.0958488 ]),
    'train_score': array([0.14717183, 0.15244502, 0.15243035, 0.14687251, 0.16204635])}
    """

    def __init__(
        self, 
        train: CleanedData,
        estimator, 
        params: Dict,
        n_comp: Optional[int] = None
    ):
        self.X, self.y = train.X, train.y
        self._numeric_features = train.numeric_features
        self._categorical_features = train.categorical_features
        self.estimator = estimator
        self.params = params 
        self.n_comp = n_comp


        if type(self.params) != dict:
            raise ValueError("'params' must be a dictionnary.")

        if self.n_comp is not None: 
            if self.n_comp <= 0 or type(self.n_comp) != int:
                raise ValueError("The number of principal components must be a striclty positive integer.")

        self._estimator_name = self.estimator.__class__.__name__
        self._BACKUP_PATH_dir = self._create_dir()
        self._key = self._get_hash_key() 

    def __repr__(self) -> str:
        return f"Training(estimator={repr(self.estimator)}, params={self.params}, n_comp={self.n_comp})"
    
    def _get_hash_key(self) -> str:
        """Return a unique key to identify the trained model.""" 
        return str(
            hash(
                f"{self._estimator_name},{self.params},{self.n_comp}"
            )
        )

    def _create_dir(self): 
        """Create a new directory."""
        dir_path = BACKUP_PATH + str(self._estimator_name) + "/"
        if not isdir(dir_path): 
            mkdir(dir_path)
        return dir_path

    def _init_model(self): 
        """Initialize model from estimator and params grid."""
        return self.estimator.set_params(**self.params)

    def _init_preprocessor(self) -> ColumnTransformer:
        """Return transformer for """
        numeric_transformer = StandardScaler()
        categorical_transformer = OneHotEncoder(handle_unknown="ignore", drop="first")
        return ColumnTransformer(
            transformers=[
                ("num", numeric_transformer, self._numeric_features),
                ("cat", categorical_transformer, self._categorical_features),
            ]
        )

    def init_pipeline(self) -> Pipeline: 
        """Return a pipeline with PCA as first step."""
        preprocessor = self._init_preprocessor()
        if self.n_comp is not None:
            steps = [
                ( "preprocessor", preprocessor ), 
                ( "pca", PCA(n_components=self.n_comp) ),
                ( "model", self._init_model() ),
            ]
        else:
            steps = [
                ( "preprocessor", preprocessor ), 
                ( "model", self._init_model() ),
            ]
        return Pipeline(steps=steps)

    def cross_val_fit(self,
        p: Pipeline, 
        cv: int
    ) -> Dict: 
        """Fit cross-validation with pipeline as estimator."""
        if cv <= 0 or type(cv) != int:
            raise ValueError("The number of folds must be a strictly positive integer.")
        return cross_validate(
            estimator=p, 
            X=self.X, 
            y=self.y, 
            cv=cv, 
            return_estimator=True,
            return_train_score=True, 
            verbose=3, 
            n_jobs=max(5, cv)
        )

    def check_backup(self) -> bool:
        """Check whether object has already been tested and backuped."""
        if isfile(self._BACKUP_PATH_dir + self._key + ".pkl"): 
            return True 
        return False  

    def display_results(
        self, 
        cv_results: Dict, 
    ):
        """Show average train and test scores from cross-validation."""
        return {
            "avg_train_score": np.mean(cv_results["train_score"]), 
            "avg_test_score": np.mean(cv_results["test_score"])
        }

    def save_cv_results(
        self, 
        cv_results: Dict, 
    ):
        with open(self._BACKUP_PATH_dir + self._key + ".pkl", "wb") as file:
            dump(obj=cv_results, file=file)

def train_models(
    train: CleanedData, 
    config: Config, 
    cv: float = 5,  
    comp_grid: Optional[list[int]] = None 
): 
    """Train and save multiple models using pipeline, grid search and cross validation."""

    def _process(est, params: List): 
        """Cross-validation process to parallelize."""
        if comp_grid is not None: 
            for n_comp in comp_grid:
                training = ModelTraining(
                    train=train, 
                    estimator=est,
                    params=params, 
                    n_comp=n_comp
                )
                print(training)
                if not training.check_backup():
                    p = training.init_pipeline()
                    cv_results = training.cross_val_fit(p, cv)
                    training.save_cv_results(cv_results)
                    print( training.display_results(cv_results) )
                else: 
                    print("Model already trained.")
        else:
            training = ModelTraining(
                    train=train,  
                    estimator=est,
                    params=params
                )
            print(training)
            if not training.check_backup():
                p = training.init_pipeline()
                cv_results = training.cross_val_fit(p, cv)
                training.save_cv_results(cv_results)
                print( training.display_results(cv_results) )
            else: 
                print("Model already trained.")

    n_jobs = CPU_COUNT - max(cv, 5) - 1
    estimators, grids = config.init_models()
    for estimator, grid in zip(estimators, grids):
        estimator = [ estimator for _ in range(len(grid)) ]
        grid = list(ParameterGrid(grid))
        shuffle(grid)
        Parallel(
            n_jobs=n_jobs
        )(
            delayed(_process)(est, params)
            for (est, params) in zip(estimator, grid)
        )

        
