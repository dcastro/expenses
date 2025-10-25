CREATE TABLE transactions (
  id TEXT PRIMARY KEY,
  account TEXT,
  date TEXT,
  desc TEXT,
  amount REAL,
  tag TEXT,
  details TEXT,
  created_on DATETIME DEFAULT CURRENT_TIMESTAMP NOT NULL
);

ALTER TABLE transactions ADD baby BOOLEAN;

-----------------------------------------------
-- Queries
-----------------------------------------------

insert into transactions2 select * from transactions limit 3;

select * from transactions
where
id not like '"0%'
AND date >= '2022-01-01'
;
