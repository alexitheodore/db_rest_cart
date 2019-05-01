
-- ROLLBACK; BEGIN;

--- // --- 			--- // --- 
--- \\ --- PRODUCTS	--- \\ ---
--- // --- 			--- // --- 

-- !!! THIS SCRIPT IS AN INITIALIZATION SCRIPT AND WILL BUILD FROM SCRATCH !!! --

-- versioning
/*

Version 0.1
	- 

*/

DROP SCHEMA IF EXISTS products CASCADE;
CREATE SCHEMA products;
SET search_path TO "global";


/*

 ----------------
	||		||		
	||		||		
	||		||		

TABLE: products

*/

CREATE TABLE products.dir
(
	product_code		TEXT			PRIMARY KEY
,	item_type			INT 			REFERENCES carts.item_types	(item_type)
,	name_retail			TEXT			NOT NULL
,	name_common			TEXT			NOT NULL
,	value				NUMERIC
,	price				NUMERIC
,	description			TEXT
,	properties			JSONB
,	is_active			BOOLEAN			DEFAULT TRUE
)
;


DO $$
BEGIN PERFORM create_trigger_snapshot('products','dir'); END; 
$$ LANGUAGE PLPGSQL;


/*
----------
	||	 
	||	 
	||	 
*/
CREATE TABLE products.shipping_properties
(
	product_code	TEXT			PRIMARY KEY			REFERENCES products.dir (product_code) ON UPDATE CASCADE
,	kit_units		NUMERIC(10,2)
,	weight			NUMERIC(10,1)
)
;


/*
_|_|_|_|
_|
_|_|_|
_|
_|
*/
CREATE OR REPLACE FUNCTION products.apiv_products_list(
	OUT json_out 	JSONB
) AS
$$
BEGIN

select 
	json_agg(row_to_json(dir))
into json_out 
from products.dir
-- where 
-- 	is_active
;

END;
$$
LANGUAGE PLPGSQL
STABLE
;


/*
_|_|_|_|
_|
_|_|_|
_|
_|
*/
CREATE OR REPLACE FUNCTION accepted_products(IN product_codes JSONB) RETURNS BOOLEAN AS
$$
BEGIN

RETURN jsonb_accepted_keys(product_codes, (SELECT array_agg(product_code) FROM products.dir));

END;
$$
LANGUAGE PLPGSQL
STABLE
;

CREATE OR REPLACE FUNCTION accepted_products(IN product_codes TEXT[]) RETURNS BOOLEAN AS
$$
BEGIN

RETURN product_codes <@ (SELECT array_agg(product_code) FROM products.dir);

END;
$$
LANGUAGE PLPGSQL
STABLE
;