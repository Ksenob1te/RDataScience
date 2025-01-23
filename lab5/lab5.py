from sqlalchemy import create_engine, text
from os.path import dirname, join
from dotenv import load_dotenv
import os
import pandas as pd


dotenv_path = join(dirname(dirname(__file__)), '.env')
load_dotenv(dotenv_path)

password = os.environ["DB_PASSWORD"]
engine = create_engine(f'postgresql://postgres:{password}@localhost:5433/lab5ds')
connection = engine.connect()

connection.execute(text("""
    DROP TABLE IF EXISTS survey;

    CREATE TABLE survey (
        MYDEPV INTEGER,
        Price INTEGER,
        Income INTEGER,
        Age INTEGER
    );
"""))
connection.commit()

df = pd.read_csv("survey.csv").head(500)
df.columns = df.columns.str.lower()
df.to_sql('survey', engine, if_exists='append', index=False)


connection.execute(text("""
ALTER TABLE survey
ADD COLUMN price20 INTEGER,
ADD COLUMN price30 INTEGER,
ADD COLUMN price10 INTEGER,
ADD COLUMN price30N INTEGER,
ALTER COLUMN MYDEPV TYPE BOOLEAN
USING MYDEPV = 1;

UPDATE survey
SET price20 = CASE WHEN Price = 20 THEN 1 ELSE 0 END,
    price10 = CASE WHEN Price = 10 THEN 1 ELSE 0 END,
    price30 = CASE WHEN Price = 30 THEN 1 ELSE 0 END;
"""))
connection.commit()

connection.execute(text("""
DROP TABLE IF EXISTS survey2;

CREATE TABLE survey2 AS
SELECT
    MYDEPV,
    ARRAY[Income, Age, price20, price30] AS features
FROM survey;
"""))
connection.commit()

connection.execute(text("""
DROP TABLE IF EXISTS survey3;

CREATE TABLE survey3 AS
SELECT
    MYDEPV,
    ARRAY[Income, Age, price20, price30 + price10] AS features
FROM survey;
"""))
connection.commit()

connection.execute(text("""
DROP TABLE IF EXISTS survey_theory;

CREATE TABLE survey_theory AS
SELECT
    MYDEPV,
    ARRAY[Income, Age, price10, price20, price30] AS features
FROM survey;
"""))
connection.commit()

connection.execute(text("""
DROP TABLE IF EXISTS survey_theory_2;

CREATE TABLE survey_theory_2 AS
SELECT
    MYDEPV,
    ARRAY[Income, Age, price10 * 0 + price20 * 1 + price30 * 2] AS features
FROM survey;
"""))
connection.commit()

# Why we cant use Euler's distance for the model?
# model is not linear, this model should give only (0, 1) values, also values 0 and 1 should be unreachable (as it goes for the limit in the infinity)
# u cant be symmetrical from 0 and 1, like y = 0.1 and y = 0.9 is highly different
# euler distance is following gaussian distribution, but we need to use bernoulli distribution

result = connection.execute(text("""
DROP TABLE IF EXISTS survey_logregr;
DROP TABLE IF EXISTS survey_logregr_summary;

SELECT madlib.logregr_train(
    'survey2', -- input table
    'survey_logregr', -- output table
    'mydepv', -- dependent variable
    'features', -- independent variables
    NULL, -- weights
    20, -- max iterations
    'irls' -- optimization method (Iterative Reweighted Least Squares)
);
"""))
connection.commit()

# condition_no - indicates multicollinearity (predictors are highly correlated)
# log_likelihood - sum of log likelihoods for each observation (simply sum of errors)

result = connection.execute(text("""
DROP TABLE IF EXISTS survey_logregr1;
DROP TABLE IF EXISTS survey_logregr1_summary;

SELECT madlib.logregr_train(
    'survey3',
    'survey_logregr1',
    'mydepv',
    'features',
    NULL,
    20,
    'irls'
);
"""))
connection.commit()

result = connection.execute(text("""
DROP TABLE IF EXISTS survey_logregr_theory;
DROP TABLE IF EXISTS survey_logregr_theory_summary;

SELECT madlib.logregr_train(
    'survey_theory',
    'survey_logregr_theory',
    'mydepv',
    'features',
    NULL,
    20,
    'irls'
);
"""))
connection.commit()

result = connection.execute(text("""
DROP TABLE IF EXISTS survey_logregr_theory_2;
DROP TABLE IF EXISTS survey_logregr_theory_2_summary;

SELECT madlib.logregr_train(
    'survey_theory_2',
    'survey_logregr_theory_2',
    'mydepv',
    'features',
    NULL,
    20,
    'irls'
);
"""))
connection.commit()

connection.close()

