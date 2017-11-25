import pandas as pd
import numpy as np
from sklearn import linear_model
from sklearn.metrics import mean_squared_error

# read in our data
happy = pd.read_csv("/Users/Philip/Schools/TAMU/STAT_636/homework/happiness.csv")
# keep only the columns we care about
happy = happy[happy.columns[range(4,10+1)]]

X_train = happy[happy.columns[range(0,6)]]
# generate intercept column - this will have the same length as the other column
one_col = pd.DataFrame(np.ones(X_train.shape[0]))
one_col.rename(columns={0:'int'}, inplace=True  )

# merge these
X_train = pd.concat([one_col, X_train], axis=1)

# response
Y_train = pd.DataFrame(happy[happy.columns[6]])

# create a function to conduct LOOCV

def loocv_lm(X_train, Y_train):

    a = range(X_train.shape[0])
    mse_list = []

    for i in range(X_train.shape[0]):
        # design matrix
        loo_dat = X_train.iloc[a[:i] + a[(i+1):], :]
        valid_dat = X_train.iloc[a[i], :]
        valid_dat = valid_dat.values.reshape(1, -1) # manipulation required for sklearn predictions

        # response vector
        loo_resp = Y_train.iloc[a[:i] + a[(i+1):], :]
        valid_resp = Y_train.iloc[a[i], :]

        loo_regr = linear_model.LinearRegression(fit_intercept=False) # we have already handled this
        loo_regr.fit(loo_dat, loo_resp)

        # make a prediction
        loo_pred = loo_regr.predict(valid_dat)

        # calculate the mean-squared error
        mse = mean_squared_error(valid_resp, loo_pred)
        mse_list.append(mse)

    # get the final mse for all of the models    
    mse_tot = sum(mse_list) / len(mse_list)
    
    return mse_tot, mse_list

mse_tot, mse_list = loocv_lm(X_train=X_train, Y_train=Y_train)

# bootstrapping function for our list of squared errors

def loo_bootstrap(x, B=10000):
    """
    function to simulate a sampling distribution for our iterative MSE values.
    x needs to be in list format
    """
    # holding shell
    mean_list = []
    
    for i in range(0, B):
        hold_samp = np.random.choice(x, size=len(x), replace=True)
        hold_mean = hold_samp.mean()
        mean_list.append(hold_mean)
    
    return mean_list

boot = loo_bootstrap(x=mse_list)
# compare
print("Bootstrapped MSE is: ",sum(boot) / len(boot))
print("LOOCV MSE is: ", mse_tot)
