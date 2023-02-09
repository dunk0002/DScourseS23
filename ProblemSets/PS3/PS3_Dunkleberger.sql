#!/bin/sql
BEGIN;
CREATE TABLE flis (policyID REAL, statecode TEXT, county TEXT, eq_site_limit REAL, hu_site_limit REAL, fl_site_limit REAL, fr_site_limit REAL, tiv_2011 REAL, tiv_2012 REAL, eq_site_deductible REAL, hu_site_deductible REAL, fl_site_deductible REAL, fr_site_deductible REAL, point_latitude REAL, point_longitude REAL, line TEXT, construction TEXT, point_granularity REAL);
COMMIT;
.mode csv
.separator ","
.import FL_insurance_sample.csv flis 
SELECT * FROM flis LIMIT 10;
SELECT county FROM flis GROUP BY county ;
SELECT AVG(tiv_2011 - tiv_2012) AS avgPropertyAppreciation FROM flis ;
SELECT construction, COUNT (*) FROM flis GROUP BY construction ;


