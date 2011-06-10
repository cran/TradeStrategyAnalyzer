CREATE TABLE annotations(
  id INT,
  txdate NUM,
  annotation NUM
, title text);

CREATE TABLE holdings 
( row_names TEXT,
	PORTFOLIOID REAL,
	ACTIVITY_TS TEXT,
	TICKER TEXT,
	NUMSHARES REAL 
);

CREATE TABLE market 
( row_names TEXT,
	ACTIVITY_TS TEXT,
	TICKER TEXT,
	PRICE REAL,
	VOLUME INTEGER 
);

CREATE TABLE pairCorrelationInfo 
( row_names TEXT,
	TICKER1 TEXT,
	TICKER2 TEXT,
	CORR REAL,
	ABSCORR REAL,
	MEANDIFF REAL,
	SDEVDIFF REAL 
);

CREATE TABLE portfolios 
( row_names TEXT,
	PORTFOLIOID REAL,
	ALGORITHM TEXT,
	STARTAMOUNT REAL,
	PERFORMANCE REAL 
);

CREATE TABLE stocks 
( row_names TEXT,
	TICKER TEXT,
	COMPANY TEXT,
	INDUSTRY TEXT 
);

CREATE TABLE sumHoldings 
( row_names TEXT,
	id REAL,
	txdate TEXT,
	x REAL 
);
CREATE TABLE transactions 
( row_names TEXT,
	PORTFOLIOID REAL,
	ACTIVITY_TS TEXT,
	DECISION TEXT,
	TICKER TEXT,
	NUMSHARES REAL 
);
