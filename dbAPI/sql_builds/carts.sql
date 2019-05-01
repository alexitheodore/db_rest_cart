--- // --- 		--- // --- 
--- \\ --- CART	--- \\ ---
--- // --- 		--- // --- 

-- !!! THIS SCRIPT IS AN INITIALIZATION SCRIPT AND WILL BUILD FROM SCRATCH !!! --

-- versioning
/*

Version 0.1
	- 

*/

DROP SCHEMA IF EXISTS carts CASCADE;
CREATE SCHEMA carts;
SET search_path TO "global";

/*

 ----------------
	||		||		
	||		||		
	||		||		

TABLE: item_types

*/

CREATE TABLE carts.item_types 
(
	item_type	INT		PRIMARY KEY
,	name		TEXT	NOT NULL
,	description	TEXT
,	active		BOOLEAN DEFAULT TRUE
)
;


DO $$
BEGIN PERFORM create_trigger_snapshot('carts','item_types'); END; 
$$ LANGUAGE PLPGSQL;


/*

------------------
	||		||		
	||		||		
	||		||		

TABLE: dir

*/

CREATE TABLE carts.dir
(
	cart_id				SERIAL		PRIMARY KEY
,	user_id				INT			NOT NULL		REFERENCES users.dir (user_id) ON UPDATE CASCADE
,	cart_name			TEXT		DEFAULT to_char(now(), 'Mon FMDDth, YYYY')
,	started_date		TIMESTAMP 	DEFAULT now()
,	ordered_date		TIMESTAMP
,	modified_date 		TIMESTAMP
,	deleted_date		TIMESTAMP
,	properties			JSONB
)
;

CREATE INDEX carts_dir_index ON carts.dir (cart_id);


DO $$
BEGIN PERFORM create_trigger_snapshot('carts','dir'); END; 
$$ LANGUAGE PLPGSQL;



/*

------------------
	||		||		
	||		||		
	||		||		

TABLE: items

*/

CREATE TABLE carts.items 
(
	cart_item_id 	SERIAL			PRIMARY KEY
,	cart_id			INT				NOT NULL	REFERENCES carts.dir (cart_id) ON UPDATE CASCADE
,	added_date		TIMESTAMP		NOT NULL	default now()

,	item_type		INT				NOT NULL	REFERENCES carts.item_types (item_type) ON UPDATE CASCADE
,	product_code	TEXT						REFERENCES products.dir (product_code) ON UPDATE CASCADE
,	coupon_code		TEXT 						REFERENCES coupons.dir (coupon_code) ON UPDATE CASCADE

,	properties		JSONB
,	quantity		INT				NOT NULL	DEFAULT 1
,	price			DOLLAR			NOT NULL	-- PRICE IS FOR JUST ONE, total price is multiplied out times the quantity
	
,	ordered_date	TIMESTAMP
)
;

CREATE INDEX carts_items_id_index ON carts.items (cart_id);
CREATE INDEX carts_items_item_id_index ON carts.items (cart_item_id);
CREATE INDEX carts_items_ordered_date_index ON carts.items (ordered_date);

DO $$
BEGIN PERFORM create_trigger_snapshot('carts','items'); END; 
$$ LANGUAGE PLPGSQL;


-- product_code/coupon_code are dedicated columns because of references and for speed/integrity
-- properties includes things like shipping_address, notes, etc.
-- deleted_date signifies items or whole carts that were deleted
-- ordered_date signifiies whole carts that were ordered



/*
_|_|_|_|
_|
_|_|_|
_|
_|
*/
CREATE OR REPLACE FUNCTION carts.trigger_cart_update_timestamp() RETURNS TRIGGER AS
$$
BEGIN

IF (TG_OP = 'DELETE') THEN
    UPDATE carts.dir SET
		modified_date = now()
	WHERE
		cart_id = OLD.cart_id
	;
    RETURN OLD;
ELSIF (TG_OP = 'UPDATE') THEN
    UPDATE carts.dir SET
		modified_date = now()
	WHERE
		cart_id = NEW.cart_id
	;
    RETURN NEW;
ELSIF (TG_OP = 'INSERT') THEN
    UPDATE carts.dir SET
		modified_date = now()
	WHERE
		cart_id = NEW.cart_id
	;
    RETURN NEW;
END IF;
RETURN NULL; -- result is ignored since this is an AFTER trigger

END;
$$
LANGUAGE PLPGSQL
VOLATILE
;


CREATE TRIGGER cart_update_timestamp
	AFTER UPDATE OR INSERT OR DELETE 
	ON carts.items
	FOR EACH ROW
	EXECUTE PROCEDURE carts.trigger_cart_update_timestamp()
;


-- This function automates the carts-wide UPSERT functionality. For qualifying item_types, if an insert is attempted for a cart that already has that same item_type, first the existing one is deleted and then the second one is added
CREATE OR REPLACE FUNCTION carts.only_one_item_each() RETURNS TRIGGER AS
$$
BEGIN
IF (SELECT count(*) FROM carts.items WHERE cart_id = NEW.cart_id and item_type = NEW.item_type) > 0 THEN
	DELETE FROM carts.items WHERE cart_id = NEW.cart_id and item_type = NEW.item_type;
END IF;

RETURN NEW;

END;
$$
LANGUAGE PLPGSQL
VOLATILE
;



-- This trigger enforces which item_types are limited to one per order.
CREATE TRIGGER cart_item_type_limiter
BEFORE INSERT ON carts.items
FOR EACH ROW
WHEN (NEW.item_type IN (51/*it*/)) 
EXECUTE PROCEDURE carts.only_one_item_each()
;

/*

** THIS NEEDS TO BE FIXED FOR 9.6


CREATE TABLE carts.items_deleted LIKE carts.items;
-- The point of this table is to move items over to it when they are deleted. This is better than demarkating them and leaving them in the cart.items table because the later option requires constant attention and makes things like triggers, etc. difficult. This method also keeps the cart items table clean and clear (to maximize index times).
-- creating the carts.items_deleted table via the LIKE method makes the carts.items table the central source of truth and eliminates needing to keep track of changes.

ALTER TABLE carts.items_deleted
	RENAME COLUMN ordered_date to deleted_date
;

*/

/*
------------------
	||		||		
	||		||		
	||		||		
*/

CREATE TABLE carts.bins
(
	id				SERIAL		PRIMARY KEY
,	cart_id			INT			NOT NULL	REFERENCES carts.dir (cart_id) ON UPDATE CASCADE ON DELETE CASCADE
,	coupon_code		TEXT		NOT NULL	REFERENCES coupons.dir (coupon_code) ON UPDATE CASCADE ON DELETE CASCADE
,	deficits		JSONB		NOT NULL

,	UNIQUE (cart_id, coupon_code)

)
;

-- when a coupon cannot be applied for reasons of insufficient cart contents, then it is dropped in the coupon bin to be held until it is applicable (upsell)
-- the deficits column lists the product_code's and their respective deficit quantities (the # of items required for the code to become applicable)


/*
------------------
	||		||		
	||		||		
	||		||		
*/

CREATE TABLE carts.bins_default
(
	id				SERIAL		PRIMARY KEY
,	coupon_code		TEXT		NOT NULL	REFERENCES coupons.dir (coupon_code) ON UPDATE CASCADE ON DELETE CASCADE

)
;

-- lists coupon_codes that are automatically applied to all carts (this is done by the trigger_coupon_update_*)
-- most will not be applicable initially, but will fall into the main coupon bin



/*
_|_|_|_|
_|
_|_|_|
_|
_|
*/
CREATE OR REPLACE FUNCTION carts.apiv_user_carts(
	IN	json_in		JSONB
,	OUT	json_out	JSONB
) AS
$$
DECLARE
	user_id_in INT := json_in->'user_id';
	carts JSONB := '{}';
BEGIN
json_out := '{}';

-- validation checks
CASE
WHEN user_id_in IS NULL THEN  -- must specify user_id
	json_out := json_out & return_code(-1001); 
	return; 
WHEN NOT users.exists(user_id_in) THEN -- check if user_id is valid
	json_out := json_out & return_code(-1310); 
	return;
ELSE
	NULL;
END CASE;

-- get carts
WITH
	user_carts as
(
SELECT
	row_number() over () as row_num
,	cart_id
,	cart_name	
from carts.dir
where
	user_id = user_id_in
)
SELECT 	
	json_agg(
		row_num::text +> (('cart_id' +> cart_id) & ('cart_name' +> cart_name))
		)
into carts
FROM user_carts
;


json_out := json_out & ('carts'::text +> carts);


IF carts IS NULL 
THEN
	json_out := json_out & return_code(1323); -- user has no carts. this is technically a successful return, even if its saying nothing exists
ELSE
	json_out := json_out & return_code(1);
END IF;


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
CREATE OR REPLACE FUNCTION carts.apix_create(
	IN	json_in		JSONB
,	OUT json_out	JSONB
) 
AS $$
DECLARE
	temp_rec record;
	user_id_in INT := json_in->'user_id';
BEGIN
-- json_out := '{}';

CASE
WHEN user_id_in IS NULL THEN 
	json_out := json_out & return_code(-1001);
	return;
WHEN NOT users.exists(user_id_in) THEN -- check if user_id is valid
	json_out := json_out & return_code(-1310); 
	return;
ELSE -- user_id exists
	INSERT INTO carts.dir (user_id) VALUES (user_id_in) returning cart_id into temp_rec;
	json_out := json_out & ('cart_id' +> temp_rec.cart_id);
	json_out := json_out & return_code(1);
	return;
END CASE;
	
END;
$$

LANGUAGE PLPGSQL
VOLATILE
;


COMMENT ON FUNCTION carts.apix_create(JSONB) is 
$$

VERSIONS:

	~1~
	- 

$$
;


/*
_|_|_|_|
_|
_|_|_|
_|
_|
*/
-- this isn't an api function because in order to perform coupon checking, a wrapper function is required (such as apix_add or apix_delete)
CREATE OR REPLACE FUNCTION carts.add(
	IN	json_in		JSONB
,	OUT	json_out	JSONB
) 

AS

$$
DECLARE
	temp_rec record;
	cart_id_in INT := json_in->>'cart_id';
	item_type_in INT := json_in->>'item_type';
	product_code_in TEXT := json_in->>'product_code';
	coupon_code_in TEXT := upper(json_in->>'coupon_code');
	quantity_in	INT := json_in->>'quantity';
	properties_in	JSONB  := (json_in->'properties')::JSONB;
	cart_item_id_in int;
BEGIN
json_out := '{}';

<<add_items>>
BEGIN

-- TO DO/CONSIDER: rather than querying the carts table multiple times, put all of the cart contents in a staging area for quick and easy access

-- VALIDATION --
CASE 
WHEN empty(cart_id_in) THEN -- must specify cart_id
	json_out := json_out & return_code(-1301);
	return;
WHEN empty((SELECT TRUE FROM carts.dir WHERE cart_id = cart_id_in)) THEN -- not a valid  valid cart_id
	json_out := json_out & return_code(-1302);
	return;	
ELSE
END CASE;

-- user_id enforcement 
IF json_in?'user_id' THEN
	IF NOT (SELECT TRUE FROM carts.dir WHERE cart_id = cart_id_in and user_id = json_in->#'user_id') THEN
		json_out := json_out & return_code(-1310);
		return;
	END IF;
END IF;

CASE
	-- PRODUCTS --
	WHEN item_type_in = 10/*it*/ THEN

		-- VALIDATION --
		IF empty(product_code_in) THEN
			json_out := json_out & return_code(-1303); 
			return; 
		END IF;	

		-- check to see if the same product is already in the cart so that quantity can be updated instead of duplicating
		select cart_item_id into temp_rec from carts.items 
		where 
			cart_id = cart_id_in
		and	product_code = product_code_in
		-- add any other identity constraints here
		;

		IF temp_rec IS NULL -- from the above
		THEN -- INSERT INTO CART --
			INSERT INTO carts.items (cart_id, item_type, product_code, properties, quantity, price)
				SELECT
					cart_id_in
				,	item_type_in
				,	product_code_in
				,	properties_in
				,	COALESCE(quantity_in::int, 1)
				,	price

				from products.dir
				where
					product_code = product_code_in
			returning * into temp_rec
			;
		ELSE -- UPDATE QUANTITY --
			UPDATE carts.items set
			quantity = (quantity + quantity_in)
			where
				cart_item_id = temp_rec.cart_item_id
			;
			json_out := json_out & return_code(1); -- updated already existing item in cart
			return;
		END IF;

	json_out := json_out & return_code(1);	-- successfully added new item to cart
	return;

	-- >> PURCHASE GIFT CARD (physical) --
	WHEN item_type_in = 31/*it*/ THEN

		-- INSERT INTO CART --
		INSERT INTO carts.items (cart_id, item_type, product_code, price)
		SELECT
			cart_id_in
		,	item_type_in
		,	product_code
		,	json_in->#'value'
		FROM products.dir
		WHERE
			product_code = 'GIFT-CARD-PRINT'
		;

		json_out := json_out & return_code(1);
		RETURN;


	-- >> PURCHASE GIFT CARD (digital) --
	WHEN item_type_in = 40/*it*/ THEN

		-- INSERT INTO CART --
		INSERT INTO carts.items (cart_id, item_type, product_code, price)
		SELECT
			cart_id_in
		,	item_type_in
		,	product_code
		,	json_in->#'value'
		FROM products.dir
		WHERE
			product_code = 'GIFT-CERT-EMAIL'
		;

		json_out := json_out & return_code(1);
		RETURN;


	-- >> RUSH FEE --
	WHEN item_type_in = 41/*it*/ THEN
		-- MAKE SURE THERE ISN'T ALREADY A RUSH ITEM --
		IF (SELECT count(*) from carts.items where cart_id = cart_id_in and item_type = item_type_in) > 0 THEN
			json_out := json_out & return_code(-1306);
			return;
		END IF;


		-- INSERT INTO CART --
		INSERT INTO carts.items (cart_id, item_type, product_code, price)
		SELECT
			cart_id_in
		,	item_type_in
		,	product_code
		,	price -- this will need to be updated if we want to have a sliding scale depending on the quantity of items in the cart
		FROM products.dir
		WHERE
			product_code = json_in->>'product_code'
		;	
				
		json_out := json_out & return_code(1); -- successfully added coupon to cart
		RETURN;

	-- >> COUPON --
	WHEN item_type_in IN (50/*it*/,-52/*it*/) THEN

		-- RULES FOR ENGAGEMENT --
		
		/*
			TODO check to see if coupon is already applied
		*/

		-- VALIDATE COUPON --		
		json_in := json_in & coupons.request(json_in); -- this checks a whole bunch of stuff and if clear, then gives a positive return code
		IF return_code(json_in) != 1 THEN -- catch error_code from coupons.request
			json_out := json_out & (json_in#&'{return_code,exit_point}');
			RETURN;
		END IF;

		IF json_in?'certificate' THEN item_type_in := -52/*it*/; END IF; -- if its a coupon, then change the item_type
		
		IF NOT empty(json_in->'coupon_binned') THEN -- coupon was binned, pass the word
			json_out := json_out & (json_in#&'{return_code,exit_point}');
			RETURN;
		END IF;

		-- if this point is reached, then the coupon is fully permitted

		-- INSERT INTO CART --
		INSERT INTO carts.items (cart_id, item_type, coupon_code, price, properties)
		SELECT
			cart_id_in
		,	item_type_in
		,	coupon_code_in
		,	COALESCE(-1.0*(json_in->##'coupon_value'),0)
		,	json_in#'referral_code'
		;
				
		json_out := json_out & return_code(1); -- successfully added coupon to cart
		return;


	-- >> SHIPPING --
	WHEN item_type_in = 51/*it*/ THEN

		-- RULES FOR ENGAGEMENT --
		IF NOT shipping.valid_address(properties_in->'shipping'->'address')
			THEN json_out := return_code(-1339); RETURN;
		END IF;

		CASE properties_in->'shipping'->>'rate_type'
			WHEN 'srq_id' THEN
				properties_in := properties_in & ('amount'+>
					(select price from shipping.rate_quotes where srq_id = properties_in->'shipping'->#'srq_id')
					);
			WHEN 'flat_rate' THEN
				properties_in := properties_in & ('amount'+>(setting_lookup('global/shipping/domestic_flat_rate')::int));
			WHEN 'blank' THEN
				properties_in := properties_in & ('amount'+>0);
		ELSE 
			json_out := json_out & return_code(-1312); -- missing required fields
			RETURN;
		END CASE;


		-- INSERT INTO CART --
		-- any existing items will automatically be replaced thanks to triggers
		INSERT INTO carts.items (cart_id, item_type, properties, quantity, price)
			SELECT
				cart_id_in
			,	item_type_in
			,	properties_in
			,	1
			,	properties_in->##'amount'
			;

		json_out := json_out & return_code(1); -- successfully added shipping item to cart
		return;
		
	-- >> TAX --
	WHEN item_type_in = 52/*it*/ THEN

		IF (SELECT
				TRUE
			FROM carts.dir
			WHERE 
				cart_id = cart_id_in 
			AND properties?'taxable'
			)
		THEN
			-- check to see if it is tax exempt and exit if so
			IF (SELECT TRUE FROM carts.items WHERE cart_id = cart_id_in AND item_type = 50/*it*/ AND properties?'tax_free') THEN
				json_out := json_out & return_code(1);
				return;
			END IF;

			-- REMOVE ANY EXISTING TAX ITEMS --
			DELETE FROM carts.items WHERE cart_id = cart_id_in AND item_type = 52/*it*/;
			-- INSERT INTO CART --
			SELECT -- start first by inserting into a temp_rec, so that we can then verify things worked before applying
				cart_id_in
			,	item_type_in
			,	properties#'tax_rate' as tax_rate
			,	1 as quantity
			,	tax_total
			into temp_rec
			FROM carts.dir
			JOIN carts.totals(cart_id_in) using (cart_id)
			WHERE
				cart_id = cart_id_in
			;

			IF temp_rec.tax_total > 0 THEN
				INSERT INTO carts.items (cart_id, item_type, properties, quantity, price)
				VALUES (
					temp_rec.cart_id_in
				,	temp_rec.item_type_in
				,	temp_rec.tax_rate
				,	temp_rec.quantity
				,	temp_rec.tax_total
				)
				;
				json_out := json_out & return_code(1); -- successfully added tax item to cart
			ELSE
				json_out := json_out & return_code(-1307); -- successfully added tax item to cart
			END IF;

		END IF;
		
		RETURN;	
	
	-- >> Payment --
	WHEN item_type_in = -51/*it*/ THEN

		-- RULES FOR ENGAGEMENT --
		
		-- VALIDATE --		
		CASE
			WHEN NOT json_in ? 'amount'
--			OR	empty(json_in->'')
		THEN -- 
			json_out := json_out & return_code(-1325); -- must specify total_due
			RETURN;
		ELSE
		END CASE;

		-- INSERT INTO CART --
		INSERT INTO carts.items (cart_id, item_type, price, properties)
		SELECT
			cart_id_in
		,	item_type_in
		,	(json_in->##'amount')
		,	properties_in
		;
		
		json_out := json_out & return_code(1); -- successfully added payment item to cart
		return;		
				
	ELSE
		json_out := json_out & return_code(-1309); -- unknown item_type
		return;
			
			
	END CASE;
	
	EXCEPTION WHEN OTHERS THEN
		json_out := json_out & return_code(-1) & ('violation'+>SQLERRM);
		return;

END;	

END;
$$

LANGUAGE PLPGSQL
VOLATILE
-- NO SECURITY DEFINER -- this is not an API function and shouldn't have public access
;


/*
_|_|_|_|
_|
_|_|_|
_|
_|
*/
CREATE OR REPLACE FUNCTION carts.coupons_refresh(
	IN	cart_id_in INT
) RETURNS VOID AS
$$
DECLARE
	coupons	RECORD;
BEGIN

FOR coupons IN (

WITH
	cart_coupons as
(
DELETE FROM carts.items WHERE cart_id = cart_id_in AND item_type IN (50/*it*/, -52/*it*/) RETURNING *
)
,	bin_coupons as
(
DELETE FROM carts.bins WHERE cart_id = cart_id_in RETURNING *
)
,	pre_coupons_list as
(
SELECT item_type, coupon_code, 1 as priority FROM cart_coupons
UNION
SELECT 50/*it*/, coupon_code, 2 FROM bin_coupons
UNION
SELECT 50/*it*/, coupon_code, 3 from carts.bins_default
)
SELECT
	*
FROM pre_coupons_list
ORDER BY priority
) LOOP

	PERFORM carts.add(('cart_id'+>cart_id_in) & row_to_json(coupons)::jsonb);

END LOOP;

RETURN;

END;
$$

LANGUAGE PLPGSQL
VOLATILE
;


CREATE OR REPLACE FUNCTION carts_tax_update() RETURNS TRIGGER AS
$$
DECLARE
	json_out JSONB;
	tax_rate NUMERIC;
BEGIN

-- raise exception 'triggggeer here';
CASE 
WHEN TG_NAME IN ('carts_tax_upsert_item_trigger', 'carts_tax_del_item_trigger') THEN
	CASE
	WHEN TG_OP IN ('INSERT','UPDATE') THEN
		SELECT
			carts.add(('item_type'+>52/*it*/) & ('cart_id'+>new.cart_id))
		INTO json_out
		;
	WHEN TG_OP IN ('DELETE') THEN
		SELECT
			carts.add(('item_type'+>52/*it*/) & ('cart_id'+>old.cart_id))
		INTO json_out
		;
	END CASE;
WHEN TG_NAME IN ('carts_tax_upsert_shipping_trigger', 'carts_tax_del_shipping_trigger') THEN
	CASE
	WHEN TG_OP IN ('INSERT','UPDATE') THEN
		IF upper(new.properties->'shipping'->'address'->>'state') = 'FL' THEN
			tax_rate := tax_rate(left(new.properties->'shipping'->'address'->>'zip',5));
			UPDATE carts.dir SET properties = properties & ('taxable'+>TRUE) & ('tax_rate'+>tax_rate) WHERE cart_id = new.cart_id;
		ELSE
			UPDATE carts.dir SET properties = properties-'taxable'-'tax_rate' WHERE cart_id = new.cart_id;
			DELETE FROM carts.items WHERE cart_id = new.cart_id AND item_type = 52/*it*/;
		END IF;

		SELECT
			carts.add(('item_type'+>52/*it*/) & ('cart_id'+>new.cart_id))
		INTO json_out
		;
	WHEN TG_OP IN ('DELETE') THEN
		UPDATE carts.dir SET properties = properties-'taxable'-'tax_rate' WHERE cart_id = old.cart_id;
		DELETE FROM carts.items WHERE cart_id = old.cart_id AND item_type = 52/*it*/;
	END CASE;
ELSE
END CASE;

IF return_code(json_out) < 0 THEN RETURN NEW; END IF;

RETURN NEW; 	-- for AFTER UPDATES

END;
$$
LANGUAGE PLPGSQL
VOLATILE
;


-- drop trigger carts_tax_upsert_item_trigger on carts.items;
CREATE TRIGGER carts_tax_upsert_item_trigger
AFTER INSERT OR UPDATE
ON carts.items
FOR EACH ROW
WHEN (new.item_type = ANY(setting_lookup('products/taxable')::INT[]))
EXECUTE PROCEDURE carts_tax_update()
;

-- drop trigger carts_tax_del_item_trigger on carts.items;
CREATE TRIGGER carts_tax_del_item_trigger
AFTER DELETE
ON carts.items
FOR EACH ROW
WHEN (old.item_type = ANY(setting_lookup('products/taxable')::INT[]))
EXECUTE PROCEDURE carts_tax_update()
;

-- drop trigger carts_tax_upsert_shipping_trigger on carts.items;
CREATE TRIGGER carts_tax_upsert_shipping_trigger
AFTER INSERT OR UPDATE
ON carts.items
FOR EACH ROW
WHEN (new.item_type = 51/*it*/)
EXECUTE PROCEDURE carts_tax_update()
;

-- drop trigger carts_tax_del_shipping_trigger on carts.items;
CREATE TRIGGER carts_tax_del_shipping_trigger -- a trigger is only needed for updates and deletes because 
AFTER DELETE
ON carts.items
FOR EACH ROW
WHEN (old.item_type = 51/*it*/)
EXECUTE PROCEDURE carts_tax_update()
;

-- the trigger simply notices when tax should be applied, but leaves the specifics up to the carts.add() to figure out


/*
_|_|_|_|
_|
_|_|_|
_|
_|
*/
CREATE OR REPLACE FUNCTION carts.apix_add(
	IN	json_in		JSONB
,	OUT	json_out	JSONB
) 
AS
$$
BEGIN

SELECT
	carts.add(json_in)
INTO json_out
;

IF return_code(json_out) < 0 THEN 
	return; 
END IF;


PERFORM carts.coupons_refresh((json_in->#'cart_id'));


END;
$$

LANGUAGE PLPGSQL
VOLATILE
;



/*
_|_|_|_|
_|
_|_|_|
_|
_|
*/
-- This is the detailed view for cart contents
CREATE OR REPLACE FUNCTION carts.apiv_contents(
	IN	json_in		JSONB
,	OUT	json_out	JSONB
) AS
$$
DECLARE
	cart_id_in	INT := json_in->'cart_id';	
BEGIN
json_out := '{}';

-- validation
IF cart_id_in IS NULL then
	json_out := json_out & return_code(-1301);
	return;
ELSEIF (SELECT ordered_date IS NOT NULL FROM carts.dir WHERE cart_id = cart_id_in) THEN
	json_out := json_out & return_code(-1308);
	return;
END IF;

-- user_id enforcement 
IF json_in?'user_id' THEN
	IF NOT (SELECT TRUE FROM carts.dir WHERE cart_id = cart_id_in and user_id = json_in->#'user_id') THEN
		json_out := json_out & return_code(-1310);
		return;
	END IF;
END IF;

WITH
	item_contents as
(
SELECT 
	cart_id
,	cart_item_id
,	parent_cart_item_id
,	item_type
,	quantity

,	product_code
,	coupon_code
,	image_id
,	items.properties
,	price
,	CASE
		WHEN item_type = 50/*it*/ THEN coupons.info(coupon_code)
		-- other items needing descriptions can go here as well
	END as description

FROM carts.dir
LEFT JOIN carts.items USING (cart_id)
WHERE 
	cart_id = cart_id_in 
ORDER BY item_type, added_date
)
,	cart_items as
(
SELECT
	cart_id
,	COALESCE(jsonb_agg(row_to_json(item_contents)::jsonb-'cart_id') FILTER (WHERE cart_item_id IS NOT NULL),'[]'::jsonb) as items
FROM item_contents
GROUP BY cart_id
)
,	carts_dir as
(
SELECT
	cart_id
,	user_id
,	cart_name
,	started_date
,	modified_date
,	ordered_date
,	properties
FROM carts.dir
WHERE
	cart_id = cart_id_in 
)

,	cart_contents as
(
SELECT
	*
-- TODO: need to remove any columns that are not needed so as to only supply the minimum necessary amount of data.
FROM cart_items
JOIN carts_dir USING (cart_id)
LEFT JOIN carts.totals(cart_id) using (cart_id) -- left join for when there are no contents in the cart
)

SELECT
	row_to_json(cart_contents)
INTO json_out
FROM cart_contents
;

IF json_out is NULL THEN 
	json_out := json_out & return_code(-1302);
	return; 
END IF;

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
CREATE OR REPLACE FUNCTION carts.delete(
	IN	json_in		JSONB
,	OUT	json_out	JSONB
) AS $$
DECLARE
	temp_rec record;
	cart_id_in INT := json_in->#'cart_id';
	cart_item_id_in INT := json_in->#'cart_item_id';
	
BEGIN
json_out := '{}';


-- validation
IF cart_id_in IS NULL OR cart_item_id_in IS NULL THEN 
	json_out := json_out & return_code(-1315);
	return; 
END IF;

-- user_id enforcement 
IF json_in?'user_id' THEN
	IF NOT (SELECT TRUE FROM carts.dir WHERE cart_id = cart_id_in and user_id = json_in->#'user_id') THEN
		json_out := json_out & return_code(-1310);
		return;
	END IF;
END IF;


-- remove the whole cart --
IF json_in?&'{cart_id,delete_cart}' THEN


	-- update the time of deletion (ordered_date will become deleted_date)
	update carts.items set
		ordered_date = now()
	where
		cart_id = cart_id_in
	;

	update carts.dir set
		deleted_date = now()
	where
		cart_id = cart_id_in
	;

	-- add cart items to the deleted table
	insert into carts.items_deleted
		select * from carts.items where cart_id = cart_id_in
	;

	-- delete the cart
	delete from carts.dir where cart_id = cart_id_in;

	-- remove cart items from the main table
	delete from carts.items where cart_id = cart_id_in
	RETURNING cart_item_id as success INTO temp_rec
	;

-- remove only the cart item --
ELSEIF cart_item_id_in IS NOT NULL THEN

	-- update the time of deletion (ordered_date will become deleted_date)
	update carts.items set
		ordered_date = now()
	where
		cart_item_id = cart_item_id_in
	OR 	parent_cart_item_id = cart_item_id_in
	;

	-- add to the deleted table
	insert into carts.items_deleted
		select * from carts.items where cart_item_id = cart_item_id_in OR parent_cart_item_id = cart_item_id_in
	;

	-- remove from the main table
	WITH
		deletes as
	(
	delete from carts.items where cart_item_id = cart_item_id_in OR parent_cart_item_id = cart_item_id_in
	RETURNING *
	)
	select
		array_agg(cart_item_id) as success
	INTO temp_rec
	FROM deletes
	;

END IF;

IF empty(temp_rec.success) THEN 
	json_out := json_out & return_code(-1316);
	return;
ELSE
	json_out := json_out & return_code(1) & ('cart_item_id'+>temp_rec.success);
	RETURN;	
END IF;

END;
$$

LANGUAGE PLPGSQL
VOLATILE
-- SECURITY DEFINER -- this is not an API function and shouldn't have public access
;


COMMENT ON FUNCTION carts.delete(JSONB) is 
$$
-- both cart_id and cart_item_id are required as a form of two factor validation
-- items aren't physically removed, they are moved to a deleted table

VERSIONS:

	~1~
	- 

$$
;


/*
_|_|_|_|
_|
_|_|_|
_|
_|
*/
CREATE OR REPLACE FUNCTION carts.apix_delete(
	IN	json_in		JSONB
,	OUT	json_out	JSONB
) 
AS
$$
DECLARE

BEGIN

SELECT
	carts.delete(json_in)
INTO json_out
;

IF return_code(json_out) < 0 THEN 
	return; 
END IF;

PERFORM carts.coupons_refresh((json_in->>'cart_id')::int);

END;
$$

LANGUAGE PLPGSQL
VOLATILE
;



/*
_|_|_|_|
_|
_|_|_|
_|
_|
*/
-- ROLLBACK; BEGIN;

CREATE OR REPLACE FUNCTION carts.update(
	IN	json_in		JSONB
,	OUT	json_out	JSONB
) AS
$$
DECLARE
	temp_rec 			record;
	cart_id_in			INT 	:= json_in->'cart_id';
    cart_item_id_in		INT 	:= json_in->'cart_item_id';
    properties_in		JSONB 	:= json_in->'properties';
    quantity_in			INT 	:= json_in->'quantity';
    item_type_in		INT 	:= (SELECT item_type from carts.items where cart_item_id = cart_item_id_in);
	
BEGIN
json_out := '{}';

-- validation
IF COALESCE(cart_id_in, cart_item_id_in) IS NULL THEN 
	json_out := json_out & return_code(-1315);
	return; 
END IF;

-- validation
IF quantity_in <= 0 THEN
	json_out := json_out & return_code(-1313);
	return; 
END IF;

-- user_id enforcement 
IF json_in?'user_id' THEN
	IF NOT (SELECT TRUE FROM carts.dir WHERE cart_id = cart_id_in and user_id = json_in->#'user_id') THEN
		json_out := json_out & return_code(-1310);
		return;
	END IF;
END IF;

UPDATE carts.items SET 
	properties 	= 	COALESCE(properties_in, properties)
,	quantity 	=	COALESCE(quantity_in, quantity)
,	price = (
		CASE -- only these item_types are allowed to define their own price:
			WHEN item_type IN (31/*it*/,51/*it*/) AND properties_in?'price'
				THEN properties_in->##'price' 
		ELSE 
			price 
		END) 
,	product_code = 	COALESCE(json_in->>'product_code',product_code)

WHERE 
	cart_item_id = cart_item_id_in
AND ordered_date IS NULL -- so that you don't work on a previously ordered cart
RETURNING * INTO temp_rec;

-- cascade any changes to child items
CASE

WHEN item_type_in = 10/*it*/ AND json_in?'quantity' THEN -- IF a quantity is specified for a print item, update any children items of qualifying item types
	UPDATE carts.items SET
		quantity = quantity_in
	WHERE
		parent_cart_item_id = cart_item_id_in
	AND item_type in (21/*it*/)
	;

WHEN item_type_in = 10/*it*/ AND json_in?'product_code' THEN
	IF carts.item_stand_product_code(cart_item_id_in) IS NULL THEN
		DELETE FROM carts.items 
		WHERE
			parent_cart_item_id = cart_item_id_in
		AND item_type in (21/*it*/)
		;
	ELSE
		UPDATE carts.items SET
			product_code = carts.item_stand_product_code(cart_item_id_in)
		WHERE
			parent_cart_item_id = cart_item_id_in
		AND item_type in (21/*it*/)
		;
	END IF;
ELSE
END CASE;

IF temp_rec.cart_item_id IS NULL THEN 
	json_out := json_out & return_code(-1316);
	return; 
END IF;

json_out := json_out & return_code(1);

END;
$$

LANGUAGE PLPGSQL
VOLATILE
-- SECURITY DEFINER -- this is not an API function and shouldn't have public access
;


COMMENT ON FUNCTION carts.update(JSONB) is 
$$
-- both cart_id and cart_item_id are required as a form of two factor validation
-- the only two parameters available for updating are properties and quantity because other parameters should really just be a remove & add sequence rather than an update
-- 

VERSIONS:

	~1~
	- 

$$
;


/*
_|_|_|_|
_|
_|_|_|
_|
_|
*/
CREATE OR REPLACE FUNCTION carts.apix_update(
	IN	json_in		JSONB
,	OUT	json_out	JSONB
) 
AS
$$
DECLARE

BEGIN
select
	carts.update(json_in)
into json_out
;

IF return_code(json_out) < 0 THEN 
	return; 
END IF;

PERFORM carts.coupons_refresh(json_in->#'cart_id');

END;
$$

LANGUAGE PLPGSQL
VOLATILE
;


/*
_|_|_|_|
_|
_|_|_|
_|
_|
*/
CREATE OR REPLACE FUNCTION carts.apix_billing_info(
	IN	json_in		JSONB
,	OUT	json_out	JSONB
)

AS

$$
DECLARE
	cart_id_in		INT 	:= json_in->'cart_id';
    properties_in	JSONB 	:= json_in->'properties';
	
BEGIN

-- validation
CASE 
WHEN cart_id_in IS NULL THEN 
	json_out := json_out & return_code(-1315);
	return;
WHEN 
	NOT properties_in ?| '{billing_address,pp_payer_id,pp_payment_id}'::TEXT[]
	THEN -- must specify billing info
		json_out := return_code(-1317);
		RETURN;
ELSE
END CASE;

-- user_id enforcement 
IF json_in?'user_id' THEN
	IF NOT (SELECT TRUE FROM carts.dir WHERE cart_id = cart_id_in and user_id = json_in->#'user_id') THEN
		json_out := json_out & return_code(-1310);
		return;
	END IF;
END IF;


UPDATE carts.dir SET 
	properties = 
			properties
		&	(properties_in # 'billing_address')
		&	(properties_in # 'pp_payer_id')
		&	(properties_in # 'pp_payment_id')

WHERE 
	cart_id = cart_id_in
;

json_out := json_out & return_code(1);

END;
$$

LANGUAGE PLPGSQL
VOLATILE
;


COMMENT ON FUNCTION carts.apix_billing_info(JSONB) is 
$$

VERSIONS:

	~1~
	- 

$$
;


/*
_|_|_|_|
_|
_|_|_|
_|
_|
*/
CREATE OR REPLACE FUNCTION carts.apiv_shipping(
	IN	json_in		JSONB
,	OUT	json_out	JSONB
)

AS

$$
DECLARE
	cart_id_in		INT 	:= json_in->'cart_id';
    properties_in	JSONB 	:= json_in->'properties';
    cart_products	JSONB;
	
BEGIN

-- validation
CASE 
WHEN empty(cart_id_in) THEN 
	json_out := return_code(-1315); return;
ELSE
END CASE;

-- user_id enforcement 
IF json_in?'user_id' THEN
	IF NOT (SELECT TRUE FROM carts.dir WHERE cart_id = cart_id_in and user_id = json_in->#'user_id') THEN
		json_out := json_out & return_code(-1310);
		return;
	END IF;
END IF;

json_out := 'packages'+>shipping.apiv_packaging_prescription(json_in#'cart_id');

-- get the shipping address and various info's and pass it through
SELECT
	properties & json_out
INTO json_out
FROM carts.items
WHERE
	cart_id = cart_id_in
AND item_type = 51/*it*/ -- shipping item
LIMIT 1 -- for now limited to just one shipping item
;

IF empty(json_out) THEN 
	json_out := return_code(-1807);
END IF;

END;
$$

LANGUAGE PLPGSQL
VOLATILE
;


/*
_|_|_|_|
_|
_|_|_|
_|
_|
*/
CREATE OR REPLACE FUNCTION carts.apix_rename(
	IN	json_in		JSONB
,	OUT	json_out	JSONB
) 
AS
$$
DECLARE
	new_name 		record;
	cart_id_in		INT		:= json_in->#'cart_id';
	cart_name_in	TEXT	:= json_in->>'cart_name';
BEGIN
json_out := '{}';

-- validation
CASE 
WHEN cart_id_in IS NULL THEN 
	json_out := json_out & return_code(-1315);
	return;
ELSE
END CASE;

-- user_id enforcement 
IF json_in?'user_id' THEN
	IF NOT (SELECT TRUE FROM carts.dir WHERE cart_id = cart_id_in and user_id = json_in->#'user_id') THEN
		json_out := json_out & return_code(-1310);
		return;
	END IF;
END IF;

	IF COALESCE(cart_id_in::text, cart_name_in) IS NULL THEN -- both cart_id and cart_name must be specified
		json_out := json_out & return_code(-1311);
		return; 
	END IF; 

	UPDATE carts.dir SET cart_name = cart_name_in where cart_id = cart_id_in returning cart_name into new_name;
	
	IF new_name.cart_name IS NULL 
	THEN 
		json_out := json_out & return_code(-1302); -- invalid cart_id
		return;
	ELSE
		json_out := json_out & return_code(1); -- successfully renamed cart
		return;
	END IF;
	
END;
$$

LANGUAGE PLPGSQL
VOLATILE
;


COMMENT ON FUNCTION carts.apix_rename(JSONB) is 
$$

VERSIONS:

	~1~
	- 

$$
;


/*
_|_|_|_|
_|
_|_|_|
_|
_|
*/
CREATE OR REPLACE FUNCTION carts.apiv_bin_contents(
	IN	json_in		JSONB
,	OUT json_out 	JSONB
) AS
$$
DECLARE
	
BEGIN
json_out := '{}';


-- VALIDATION --
IF empty(json_in#'cart_id') THEN -- must specify a cart_id
	json_out := json_out & return_code(-1301);
	RETURN;
END IF;

-- user_id enforcement 
IF json_in?'user_id' THEN
	IF NOT (SELECT TRUE FROM carts.dir WHERE cart_id = cart_id_in and user_id = json_in->#'user_id') THEN
		json_out := json_out & return_code(-1310);
		return;
	END IF;
END IF;


json_out := 
	(SELECT
		json_agg(coupon_code +> ('deficits'::text +> deficits))
	FROM carts.bins
	WHERE
		cart_id = (json_in->>'cart_id')::int
	)
;

END;
$$

LANGUAGE PLPGSQL
STABLE
;


COMMENT ON FUNCTION carts.apiv_bin_contents(JSONB) is 
$$


VERSIONS:

	~1~
	- 

$$
;


/*
_|_|_|_|
_|
_|_|_|
_|
_|
*/
CREATE OR REPLACE FUNCTION carts.apiv_payment_info(
	IN	json_in		JSONB
,	OUT json_out 	JSONB
) AS
$$
DECLARE
	
BEGIN
json_out := '{}';

-- VALIDATION --
IF empty(json_in#'cart_id') THEN -- must specify a cart_id
	json_out := json_out & return_code(-1301);
	RETURN;
END IF;

-- user_id enforcement 
IF json_in?'user_id' THEN
	IF NOT (SELECT TRUE FROM carts.dir WHERE cart_id = cart_id_in and user_id = json_in->#'user_id') THEN
		json_out := json_out & return_code(-1310);
		return;
	END IF;
END IF;


select
		json_out 
	& 	(properties #& '{billing_address,pp_payer_id,pp_payment_id}')
	&	(carts.apiv_contents(json_in) #& '{checkout_total,shipping_total,tax_total}')
INTO json_out
FROM carts.dir
WHERE
	cart_id = json_in ->'cart_id'
;

IF json_out IS NULL THEN json_out := json_out & return_code(-1318); END IF;

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
CREATE OR REPLACE FUNCTION carts.apix_checkout(
	IN	json_in		JSONB
,	OUT	json_out	JSONB
) 
AS
$$
DECLARE

	cart_id_in		INT		:= json_in->#'cart_id';
	payment_verif	RECORD;
	
BEGIN
json_out := '{}';


-- validation --
IF	
		empty(cart_id_in)
	OR 	(json_in->>'payment_model_type' != 'test_order')
	THEN json_out := json_out & return_code(-1312); RETURN;

ELSEIF (SELECT TRUE FROM carts.dir where cart_id = cart_id_in AND ordered_date IS NOT NULL)
--	 	⤷ Lets make sure it hasn't already been ordered, shall we? Yes, I think that'd be a wonderful idea.
	THEN json_out := json_out & return_code(-1308); RETURN;

-- check to make sure that items that require shipping have supplied it
ELSEIF (
SELECT 
	(count(*) FILTER (WHERE item_type < 30/*it*/) = 0) -- either there aren't any shippable items
OR 	(count(*) FILTER (WHERE item_type = 51/*it*/) = 1) -- or there is a shipping item
FROM carts.items 
WHERE 
	cart_id = cart_id_in
AND ordered_date IS NULL
	) IS NOT TRUE
	THEN
		json_out := 'cart_items'+>(select json_agg(row_to_json(items)) from carts.items where cart_id = cart_id_in);
		json_out := json_out & return_code(-1335); RETURN;

END IF;

-- TODO: ^ need to make accomodations for items which do not require shipping


-- at this point, the balance is expected to be $0.00 - either because its already been paid for, or because it was a free order
-- but iff not, then the following alternatives are available
CASE 
	WHEN json_in->>'payment_model_type' = 'test_order' THEN
		json_out := carts.apix_add(('item_type' +> -51/*it*/) & json_in); -- add the payment item
		json_in := json_in & '{"payment_model_prefix":"TEST", "test_order":true}'::JSONB; 
		-- 	⤷ set the properties of the order for the orders.create() below

	WHEN json_in->>'payment_model_type' = '1' THEN
		RETURN; -- temp
	WHEN json_in->>'payment_model_type' = '3' THEN
		RETURN; -- temp
	WHEN json_in->>'payment_model_type' = '4' THEN
		RETURN; -- temp

	ELSE
		IF (SELECT checkout_total from carts.totals(cart_id_in)) <> 0 
		-- ⤷ 	make sure everything has been paid for; the balance due should be 0
		-- 		if payment was already given (as it should be) then the payment item should already be in the cart
		--		technically if the balance is negative, the order will need administrative help to fix it
			THEN json_out := json_out & return_code(-101); RETURN;
		END IF;

END CASE
;

-- if all is good, hand the rest off to orders.create() (and pass back the return_code)
json_out := orders.create(json_in);


END;
$$

LANGUAGE PLPGSQL
VOLATILE
;


/*
_|_|_|_|
_|
_|_|_|
_|
_|
*/
CREATE OR REPLACE FUNCTION carts.apiv_notes(
	IN	json_in		JSONB
,	OUT json_out 	JSONB
) AS
$$
DECLARE
	
BEGIN

-- validation --
IF NOT json_in?'cart_id' THEN json_out := json_out & return_code(-1302); RETURN; END IF;

SELECT
	('notes' +> cart_notes) & ('cart_id'+>cart_id)
INTO json_out
FROM carts.dir
WHERE
	cart_id = json_in->#'cart_id'
;

IF empty(json_out->'cart_id') THEN json_out := json_out & return_code(-1310); RETURN; END IF;

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
CREATE OR REPLACE FUNCTION carts.apix_update_notes(
	IN	json_in		JSONB
,	OUT	json_out	JSONB
) AS
$$
DECLARE
    next_note	TEXT[];
    del_index	INT;
BEGIN

IF json_in?'add_note' THEN
	next_note := array[fractime()::text, prod_user_num()::TEXT, json_in->>'add_note'];

	UPDATE carts.dir SET
		cart_notes = prod_note_append(cart_notes, next_note)
	WHERE
		cart_id = json_in->#'cart_id'
	;

	json_out := json_out & return_code(1);
	RETURN;

ELSEIF json_in?'delete_note' THEN
	del_index := json_in->#'delete_note';

	UPDATE carts.dir SET
		cart_notes = array_cat(cart_notes[0:(del_index-1)],cart_notes[(del_index+1):(array_upper(cart_notes,1))])
	WHERE
		cart_id = json_in->#'cart_id'
	;

	json_out := json_out & return_code(1);
	RETURN;

ELSE
	json_out := json_out & return_code(-100);
	RETURN;

END IF;

END;
$$
LANGUAGE PLPGSQL
VOLATILE
;



/*
*/

CREATE TYPE carts.cart_totals AS
(
	cart_id 				INT
,	checkout_total			DOLLAR
,	cart_total 				DOLLAR
,	product_total 			DOLLAR
,	subtotal 				DOLLAR
,	amount_discountable		DOLLAR
,	amount_discounted		DOLLAR
,	amount_cert_redeemable 	DOLLAR
,	shipping_total 			DOLLAR
,	tax_total				DOLLAR
,	taxed_total 			DOLLAR
,	cash_total 				DOLLAR
,	cert_redemption_total 	DOLLAR
,	refund_total 			DOLLAR
,	revenue_total 			DOLLAR
)
;

CREATE OR REPLACE FUNCTION carts.totals(IN cart_id_in INT) RETURNS SETOF carts.cart_totals AS
$$
BEGIN
RETURN QUERY
SELECT
	cart_id_in
,	COALESCE(sum(price*quantity),0.0)::DOLLAR as checkout_total 
		-- the amount required to be paid at checkout, net discounts, certificates, cash payments, etc.
		-- this is what determines both how much the customer has to pay in the end AND whether its all been paid at checkout
,	COALESCE(sum(price*quantity) FILTER (WHERE item_type > 0),0.0)::DOLLAR as cart_total
		-- the total value of the cart/order before any payments
,	COALESCE(sum(price*quantity) FILTER (WHERE item_type < 30),0.0)::DOLLAR as product_total 
		-- the amount of product in cart before discounts, certificates, shipping, tax, etc.
		-- this is used specifically for determining if/when product-value based coupons can be used
,	COALESCE(sum(price*quantity) FILTER (WHERE item_type > 0 and item_type <= 50),0.0)::DOLLAR as subtotal 
		-- the amount due before shipping and tax
		-- this is used for the conventional display purposes
,	COALESCE(sum(price*quantity) FILTER (WHERE item_type < 20),0.0)::DOLLAR as amount_discountable 
		-- the amount that is discountABLE 
		-- this is used for calculating discounts, etc.
,	COALESCE(sum(price*quantity) FILTER (WHERE item_type = 50/*it*/),0.0)::DOLLAR as amount_discounted 
		-- the amount that can be discountED
,	COALESCE(sum(price*quantity) FILTER (WHERE item_type < 20),0.0)::DOLLAR as amount_cert_redeemable 
		-- the amount that can be paid by certificate
,	COALESCE(sum(price*quantity) FILTER (WHERE item_type = 51/*it*/),0.0)::DOLLAR as shipping_total 
		-- the total shipping amount
,	COALESCE(
		sum(price*quantity*(dir.properties->##'tax_rate')) 
			FILTER (WHERE (dir.properties?'tax_rate') AND item_type < 20)
		,0.0)::DOLLAR as tax_total 
		-- the total amount taxABLE
,	COALESCE(sum(price*quantity) FILTER (WHERE item_type = 52/*it*/),0.0)::DOLLAR as taxed_total
		-- the total amount taxED
,	COALESCE(sum(price*quantity) FILTER (WHERE item_type = -51/*it*/),0.0)::DOLLAR as cash_total
,	COALESCE(sum(price*quantity) FILTER (WHERE item_type = -52/*it*/),0.0)::DOLLAR as cert_redemption_total
,	COALESCE(sum(price*quantity) FILTER (WHERE item_type = -53/*it*/),0.0)::DOLLAR as refund_total
,	COALESCE(sum(price*quantity) FILTER (WHERE item_type > 0 AND item_type NOT IN (31/*it*/,40/*it*/)),0.0)::DOLLAR as revenue_total
from carts.dir
join carts.items using (cart_id)
WHERE
	cart_id = cart_id_in
;

END;
$$
LANGUAGE PLPGSQL
STABLE
;

/*
 \      /
  \    /
   \  /
____\/____
*/
CREATE OR REPLACE VIEW carts.contents_summary AS
with
	cart_items as
(
select
	cart_id
,	product_code + ' (x' + count(*)::text +')' as items
,	product_code
from carts.items
where
	product_code is NOT NULL
group by cart_id, product_code
)
select
	cart_id
,	string_agg(items, ', ') as products
,	array_agg(product_code) as product_codes
from cart_items
group by cart_id
;


/*
_|_|_|_|
_|
_|_|_|
_|
_|
*/
create type pf_carts_list as
(
	cart_id			INT
,	email 			TEXT
,	full_name		TEXT
,	products		TEXT
,	product_codes	TEXT[]
,	total 			DOLLAR
,	started_date	TIMESTAMP
,	ordered_date	TIMESTAMP
,	modified_date	TIMESTAMP
,	deleted_date	TIMESTAMP
)
;


CREATE OR REPLACE FUNCTION carts.apiv_list(
	IN 	json_args 	JSONB
,	OUT json_out	JSONB
) AS
$$
DECLARE
	sql_exec TEXT;
BEGIN

sql_exec := pf_base_apiv_list(
		'carts'
	,	json_args
	,	'{"limit":100, "order":"modified_date", "order_dir":"desc"}'::JSONB
	,	'{product_codes, checkout_total, started_date, modified_date, ordered_date, deleted_date}'::text[]
)
;

IF NOT empty(sql_exec)
THEN
	sql_exec := 'SELECT jsonb_agg(row_to_json(base::pf_carts_list)) from ('|| sql_exec ||') base;';
	EXECUTE sql_exec into json_out;
	json_out := 'rows'+>json_out;
	RETURN;
ELSE
	json_out := return_code(-1002);
RETURN;

END IF;

END;
$$

LANGUAGE PLPGSQL
STABLE
;


-- This function only exists to CSOT the logic as it is needed for both adding and updating
CREATE OR REPLACE FUNCTION carts.item_stand_product_code(
	IN 	cart_item_id_in 	INT
,	OUT stand_product_code	TEXT
) AS
$$
DECLARE
BEGIN

SELECT 
	(pds.properties->'stand'->>'product_code')
INTO stand_product_code
FROM products.dir pds
JOIN carts.items USING (product_code)
WHERE 
	cart_item_id = cart_item_id_in
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
CREATE OR REPLACE FUNCTION carts.apiv_item_types(
	OUT json_out 	JSONB
) AS
$$
BEGIN
select 
	json_agg(row_to_json(item_types)) into json_out 
from carts.item_types
;
END;
$$
LANGUAGE PLPGSQL
STABLE
;