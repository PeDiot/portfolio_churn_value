"""Description. 

Machine Learning library to fit regression models.

"""

from .utils import (
    BACKUP_PATH, 
    DATA_PATH, 
    load_rdata, 
    load_pickle, 
)

from .preprocessing import (
    CleanedData, 
    Preprocessing,
)

from .estimators import ( 
    DummyEstimator, 
    LREstimator, 
    RidgeEstimator, 
    TreeEstimator, 
    RFEstimator, 
    GBEstimator, 
    MLPEstimator,
)

from .config import (
    Config, 
    create_config, 
    load_config, 
)

from .training import(
    CPU_COUNT,
    ModelTraining, 
    train_models, 
)

from .selection import (
    ModelDir, 
    get_files_paths, 
    get_cv_results,
    get_best_estimator, 
    save_best_estimator,
    load_best_estimator,
)

