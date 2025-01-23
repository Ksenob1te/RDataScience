from copy import deepcopy

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
from sqlalchemy import create_engine
from os.path import join, dirname
from dotenv import load_dotenv
import os

MODE_SELECT = 0

# -------------- CREATE TEST DATA --------------
df_def = pd.read_csv("survey.csv").tail(250)
df_def.columns = df_def.columns.str.lower()
df_def['mydepv'] = df_def['mydepv'] == 1
df_def['price10'] = (df_def['price'] == 10).astype(float)
df_def['price20'] = (df_def['price'] == 20).astype(float)
df_def['price30'] = (df_def['price'] == 30).astype(float)

# -------------- FOR CHANGED PRICE --------------
df_price = deepcopy(df_def)
df_price['price30'] += (df_price['price'] == 10).astype(float)
# -----------------------------------------------


data_def = pd.DataFrame()
data_def["mydepv"] = pd.DataFrame(df_def, columns=['mydepv'])
data_def["features"] = pd.DataFrame(
    df_def, columns=['income', 'age', 'price20', 'price30']
).apply(
    lambda row: (row[['income', 'age', 'price20', 'price30']].to_numpy()), axis=1
)

data_price = pd.DataFrame()
data_price["mydepv"] = pd.DataFrame(df_def, columns=['mydepv'])
data_price["features"] = pd.DataFrame(
    df_price, columns=['income', 'age', 'price20', 'price30']
).apply(
    lambda row: (row[['income', 'age', 'price20', 'price30']].to_numpy()), axis=1
)

data_theory = pd.DataFrame()
data_theory["mydepv"] = pd.DataFrame(df_def, columns=['mydepv'])
data_theory["features"] = pd.DataFrame(
    df_def, columns=['income', 'age', 'price10', 'price20', 'price30']
).apply(
    lambda row: (row[['income', 'age', 'price10', 'price20', 'price30']].to_numpy()), axis=1
)

data_theory_2 = pd.DataFrame(
    df_def, columns=['income', 'age', 'price10', 'price20', 'price30']
)
data_theory_2["mydepv"] = pd.DataFrame(df_def, columns=['mydepv'])
data_theory_2["price"] = df_def[['price20', 'price30', 'price30']].sum(axis=1)
data_theory_2["features"] = data_theory_2.apply(
    lambda row: (row[['income', 'age', 'price']].to_numpy()), axis=1
)
data_theory_2 = data_theory_2.drop(columns=['price10', 'price20', 'price30', 'income', 'age', 'price'])



# ----------------------------------------------

# ---------------- DATABASE CONNECTION ----------------
dotenv_path = join(dirname(dirname(__file__)), '.env')
load_dotenv(dotenv_path)
password = os.environ["DB_PASSWORD"]

engine = create_engine(f'postgresql://postgres:{password}@localhost:5433/lab5ds')
print("CONNECTION STARTED")
connection = engine.connect()
coefficients_def = pd.read_sql("SELECT * FROM survey_logregr;", connection)
coefficients_price = pd.read_sql("SELECT * FROM survey_logregr1;", connection)
coefficients_theory = pd.read_sql("SELECT * FROM survey_logregr_theory;", connection)
coefficients_theory_2 = pd.read_sql("SELECT * FROM survey_logregr_theory_2;", connection)
# data = pd.read_sql("SELECT * FROM survey2;", connection)
connection.close()

print("CONNECTION DONE")
# -----------------------------------------------------


def sigmoid(x):
    return 1 / (1 + np.exp(-x))

def analyze_predictions(_coefficients, _data):
    _coefficients = _coefficients['coef'].values

    _data['prediction'] = _data["features"].apply(
        lambda x: sigmoid(np.dot(np.array(_coefficients[0]), np.array(x).astype(float)))
    )

    _data['predicted_class'] = (_data['prediction'] >= 0.5).astype(int)
    true_data = _data[_data['mydepv'] == True]
    false_data = _data[_data['mydepv'] == False]

    plt.figure(figsize=(10, 6))

    plt.hist(true_data['prediction'], bins=30, alpha=0.6, label='True', color='green', edgecolor='black')
    plt.hist(false_data['prediction'], bins=30, alpha=0.6, label='False', color='red', edgecolor='black')

    plt.axvline(0.5, color='black', linestyle='--', label='Decision Boundary')
    plt.title('Predicted Probabilities Distribution by Outcome', fontsize=16)
    plt.xlabel('Predicted Probability (Score)', fontsize=14)
    plt.ylabel('Frequency', fontsize=14)
    plt.legend(loc='upper center', fontsize=12)
    plt.tight_layout()

    plt.show()

# ---------------- ROC CURVE ----------------

def draw_roc_curve(_data):
    y_true = _data['mydepv'].to_numpy()
    y_scores = _data['prediction'].to_numpy()

    sorted_indices = np.argsort(y_scores)[::-1]
    y_scores_sorted = y_scores[sorted_indices]
    y_true_sorted = y_true[sorted_indices]

    thresholds = np.unique(y_scores_sorted)
    tpr = []
    fpr = []

    P = np.sum(y_true_sorted)
    N = len(y_true_sorted) - P

    for thresh in thresholds:
        tp = np.sum((y_scores_sorted >= thresh) & (y_true_sorted == 1))
        fp = np.sum((y_scores_sorted >= thresh) & (y_true_sorted == 0))

        tpr.append(tp / P)
        fpr.append(fp / N)

    auc = 0
    for i in range(1, len(fpr)):
        auc += (fpr[i] - fpr[i - 1]) * (tpr[i] + tpr[i - 1]) / 2

    plt.figure(figsize=(10, 6))
    plt.plot(fpr, tpr, color='blue', lw=2, label=f'ROC curve (AUC = {auc:.2f})')
    plt.plot([0, 1], [0, 1], color='gray', linestyle='--', lw=2, label='Random')

    plt.title('Receiver Operating Characteristic (ROC) Curve', fontsize=16)
    plt.xlabel('False Positive Rate (FPR)', fontsize=14)
    plt.ylabel('True Positive Rate (TPR)', fontsize=14)
    plt.legend(loc='lower right', fontsize=12)
    plt.grid(alpha=0.4)
    plt.tight_layout()

    plt.show()

analyze_predictions(coefficients_def, data_def)
draw_roc_curve(data_def)
analyze_predictions(coefficients_price, data_price)
draw_roc_curve(data_price)

analyze_predictions(coefficients_theory, data_theory)
draw_roc_curve(data_theory)

analyze_predictions(coefficients_theory_2, data_theory_2)
draw_roc_curve(data_theory_2)
# -------------------------------------------

# ------- TEST PREDICTION MASS -------

# ---------------- DATABASE CONNECTION ----------------
connection = engine.connect()
data_def = pd.read_sql("SELECT * FROM survey2;", connection).head(500)
data_price = pd.read_sql("SELECT * FROM survey3;", connection).head(500)
data_theory = pd.read_sql("SELECT * FROM survey_theory;", connection).head(500)
connection.close()
# -----------------------------------------------------
coefficients_def = coefficients_def['coef'].values
data_def['prediction'] = data_def["features"].apply(
    lambda x: sigmoid(np.dot(np.array(coefficients_def[0]), np.array(x).astype(float)))
)
print("Probability mass (default) and sum of mydepv: ", sum(data_def['prediction']), sum(data_def['mydepv']))

coefficients_price = coefficients_price['coef'].values
data_price['prediction'] = data_price["features"].apply(
    lambda x: sigmoid(np.dot(np.array(coefficients_price[0]), np.array(x).astype(float)))
)
print("Probability mass (price change) and sum of mydepv: ", sum(data_price['prediction']), sum(data_price['mydepv']))

coefficients_theory = coefficients_theory['coef'].values
data_theory['prediction'] = data_theory["features"].apply(
    lambda x: sigmoid(np.dot(np.array(coefficients_theory[0]), np.array(x).astype(float)))
)
print("Probability mass (theory change) and sum of mydepv: ", sum(data_theory['prediction']), sum(data_theory['mydepv']))
# -----------------------------------

custom_feature = [58, 25, 1, 0]
prediction = sigmoid(np.dot(np.array(coefficients_def[0]), np.array(custom_feature).astype(float)))
print(f"Prediction for default model: {prediction * 100:.2f}%")

prediction = sigmoid(np.dot(np.array(coefficients_price[0]), np.array(custom_feature).astype(float)))
print(f"Prediction for price change model: {prediction * 100:.2f}%")

custom_feature = [58, 25, 0, 1, 0]
prediction = sigmoid(np.dot(np.array(coefficients_theory[0]), np.array(custom_feature).astype(float)))
print(f"Prediction for theory change model: {prediction * 100:.2f}%")
