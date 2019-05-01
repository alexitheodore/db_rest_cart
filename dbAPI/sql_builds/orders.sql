
-- ROLLBACK; BEGIN;

--- // --- 			--- // --- 
--- \\ ---  ORDERS	--- \\ ---
--- // --- 			--- // --- 

-- !!! THIS SCRIPT IS AN INITIALIZATION SCRIPT AND WILL BUILD FROM SCRATCH !!! --

-- versioning
/*

Version 0.1
	- 

*/

DROP SCHEMA IF EXISTS orders CASCADE;
CREATE SCHEMA orders;
SET search_path TO "global";


/*
----------
	||	 
	||	 
	||	 
*/
CREATE TABLE orders.statuses 
(
	status_code		INT		PRIMARY KEY
,	status_name		TEXT	NOT NULL
,	status_color	TEXT
)
;


/*
_|_|_|_|
_|
_|_|_|
_|
_|
*/
CREATE OR REPLACE FUNCTION orders.apiv_status_list(
	OUT json_out 	JSONB
) AS
$$
BEGIN
select 
	json_agg(row_to_json(statuses)) into json_out 
from orders.statuses
;
END;
$$
LANGUAGE PLPGSQL
STABLE
;




/*
----------
	||	 
	||	 
	||	 
*/
CREATE TABLE orders.dir
(
	order_id		SERIAL			PRIMARY KEY
,	user_id			INT													REFERENCES users.dir (user_id)				ON UPDATE CASCADE
,	order_date 		TIMESTAMP		NOT NULL 		DEFAULT NOW()
,	active			BOOLEAN							DEFAULT TRUE
)
;

CREATE INDEX orders_dir_index ON orders.dir (order_id, user_id);

DO $$
BEGIN PERFORM create_trigger_snapshot('orders','dir'); END; 
$$ LANGUAGE PLPGSQL;


COMMENT ON TABLE orders.dir is 
$$

-- notes is an array that should contain the following keys: note, eid for each entry (a new array)
-- Tags is a simple 1D array of possible tag values

$$
;


CREATE TABLE orders.shipping_methods
(
	shipping_type	INT			PRIMARY KEY -- this is the internal key name
,	carrier_class	TEXT		NOT NULL -- this is the carrier's key name
,	carrier			TEXT		CHECK (carrier <@ setting_lookup('shipping/carriers')::text[]) 
,	method			TEXT		NOT NULL -- questionable need
,	retail_name		TEXT		NOT NULL
,	transit_time	INTERVAL
,	international	BOOLEAN		DEFAULT FALSE

,	unique (carrier_class)	-- there cannot be more than one of each carrier_class
,	unique (carrier, method)	-- there cannot be more than one combination of carrier and method

)
;

DO $$
BEGIN PERFORM create_trigger_snapshot('orders','shipping_methods'); END; 
$$ LANGUAGE PLPGSQL;



/*
----------
	||	 
	||	 
	||	 
*/
CREATE TABLE orders.groups 
(
	group_id				SERIAL		PRIMARY KEY
,	group_name				TEXT		NOT NULL	DEFAULT '1'	
,	shipping_type			INT			REFERENCES orders.shipping_methods (shipping_type) ON UPDATE CASCADE
,	shipping_address		JSONB 		--CHECK (valid_address(shipping_address)) --> not doing this because rejecting at this point is a bad idea - faults should be caught earlier
,	postage_cost			DOLLAR
,	packages				JSONB

,	tracking_number			TEXT							-- REFERENCES shipping logs ON UPDATE CASCADE
,	shipped_date			TIMESTAMP
,	open					BOOLEAN		DEFAULT	TRUE

,	CONSTRAINT tracking_number_and_shipped_date CHECK (empty(shipped_date) = empty(tracking_number)) -- either both are null or both are not null 

)
;

CREATE INDEX groups_dir_group_id_index ON orders.groups (group_id);

DO $$
BEGIN PERFORM create_trigger_snapshot('orders','groups'); END; 
$$ LANGUAGE PLPGSQL;



/*
----------
	||	 
	||	 
	||	 
*/
CREATE TABLE orders.items
(
	item_id				SERIAL			PRIMARY KEY
,	order_id			INT				REFERENCES orders.dir (order_id)						ON UPDATE CASCADE
,	group_id			INT				REFERENCES orders.groups (group_id)						ON UPDATE CASCADE
,	group_position		INT				
,	parent_item_id		INT 			REFERENCES orders.items (item_id)						ON UPDATE CASCADE

,	item_type			INT				REFERENCES carts.item_types (item_type)					ON UPDATE CASCADE
,	item_date 			TIMESTAMP		NOT NULL 	DEFAULT now()
,	description			TEXT
,	price				DOLLAR			NOT NULL
,	status_code			INT				DEFAULT 0 	REFERENCES orders.statuses (status_code) 	ON UPDATE CASCADE
,	payment_id			TEXT			-- This doesn't reference the payments table because payment_id isn't the PK
,	tags				TEXT[]
,	visible				BOOLEAN			NOT NULL 		DEFAULT TRUE
,	properties			JSONB

,	product_code		TEXT			REFERENCES products.dir (product_code) 					ON UPDATE CASCADE
,	coupon_code			TEXT			REFERENCES coupons.dir (coupon_code) 					ON UPDATE CASCADE
,	attachment			JSONB
,	group_check			TEXT
--		⤷ in leu of tracking number, this signifies when an item has been added to a group after a shipping event
-- ,	UNIQUE (group_id, group_position) --> this proved problematic (9-7-16)

,	CHECK ((item_type < 0) OR (group_id IS NOT NULL))
)
;


CREATE INDEX orders_items_index ON orders.items (order_id, item_id);
CREATE INDEX orders_items_group_id_index ON orders.items (group_id);
CREATE INDEX orders_items_item_type_index ON orders.items (item_type);

DO $$
BEGIN PERFORM create_trigger_snapshot('orders','items'); END; 
$$ LANGUAGE PLPGSQL;




/*
_|_|_|_|
_|
_|_|_|
_|
_|
*/
CREATE OR REPLACE FUNCTION orders.active(
	IN	order_id_in	INT
) RETURNS BOOLEAN AS
$$
select active from orders.dir where order_id = $1;
$$

LANGUAGE SQL
STABLE
;


/*
_|_|_|_|
_|
_|_|_|
_|
_|
*/
CREATE OR REPLACE FUNCTION orders.create(
	IN	json_in		JSONB
,	OUT json_out 	JSONB
) AS
$$
	DECLARE
	cart_items 		RECORD;
	cart_id_in 		INT := (json_in->#'cart_id');
	order_id_in		INT;
	group_id_in		INT;
	payment_item	BOOLEAN := FALSE;
	new_tid			TEXT;
	order_items 	RECORD;
	temp_json		JSONB;
	item_qty		INT;
	
BEGIN
json_out := '{}';


-- validation --
IF		empty(cart_id_in)
	AND NOT json_in?'test_order'
THEN
	json_out := json_out & return_code(-1501);
	RETURN;
END IF;

IF json_in?'test_order' THEN
	json_in := 
			json_in 
		&	('merch_payment_id'+>'funct_test')
		&	('merchant'+>'funct_test')
		&	('last_four'+>'funct_test')
		;
END IF;

-- TODO: check to make sure cart adds up to zero
-- TODO: check to make sure cart was not already added (might want to create an override for this though)
-- TODO: annotate the cart_id to the notes 

-- determine the payment_id
new_tid := payments.gen_payment_id(json_in);


-- fill orders.dir --
INSERT INTO orders.dir (user_id, active)
SELECT
	user_id
,	COALESCE(json_in->?'test_order',TRUE)
FROM carts.dir 
WHERE 
	cart_id = cart_id_in
LIMIT 1
RETURNING order_id INTO order_id_in
;

json_out := json_out & ('order_id' +> order_id_in); -- 

-- fill orders.groups --
INSERT INTO orders.groups 
(
	shipping_type
,	shipping_address
,	packages
)
SELECT
	properties->'shipping'->#'type'
,	properties->'shipping_address'
,	properties->'packages'
FROM carts.items
WHERE
	cart_id = cart_id_in
AND item_type = 41
LIMIT 1

RETURNING group_id INTO group_id_in
;

IF empty(group_id_in) THEN
	INSERT INTO orders.groups 
	(
		shipping_type
	)
	VALUES
	(	
		000
	)
	RETURNING group_id INTO group_id_in
	;
END IF
;


-- fill orders.items --

FOR cart_items IN (
	SELECT 
		* 
	,	row_number() OVER () AS item_position
--							⤷ ordering logic goes here	
	FROM carts.items 
	WHERE 
		cart_id = cart_id_in 
	)
LOOP

	item_qty := 1;

	WHILE item_qty <= cart_items.quantity LOOP
		CASE 
			WHEN cart_items.item_type = 31/*it*/ THEN 	-- process any certificates
				cart_items.coupon_code := coupons.apix_create('{"status":0,"coupon_quantity":1,"coupon_type":"3"}')->'coupon_codes'->>0;
			WHEN cart_items.item_type = 10/*it*/ THEN
				IF cart_items.product_code IS NULL THEN
					json_out := return_code(-1329) & ('cart_item_id'+>cart_items.cart_item_id);
					RETURN;
				END IF;
			ELSE
		END CASE;

		INSERT INTO orders.items
		(
			order_id			
		,	item_type			
		-- ,	description			
		,	price
		,	payment_id
		,	group_id
		,	group_position
		,	product_code
		,	coupon_code
		,	properties
		,	status_code
		)
			VALUES
		(
			order_id_in	
		,	cart_items.item_type
		-- ,	description
		,	cart_items.price
		,	new_tid
		,	CASE WHEN cart_items.item_type > 0 THEN group_id_in ELSE NULL END					-- only non-transaction (shipable) items get grouped
		,	CASE WHEN cart_items.item_type > 0 THEN cart_items.item_position ELSE NULL END			-- ""
		,	cart_items.product_code
		,	cart_items.coupon_code
		,	cart_items.properties
		,	10
		)
		returning * into order_items
		;

		-- THIS IS WHERE ANY ITEM-SPECIFIC SCRIPTS SHOULD GO --

		-- PROCESS ANY CERTIFICATES
		IF order_items.item_type = 31/*it*/ THEN
			-- create certificate and add value to it
			SELECT 
				coupons.cert_value_update(
					jsonb_build_object(
							'item_id',order_items.item_id
						,	'coupon_code',order_items.coupon_code
						,	'action','add_value'
					)
				)
				INTO temp_json
			;

			IF return_code(temp_json) < 1 THEN json_out := temp_json; RETURN; END IF;

			-- TODO: queue email or something?
		ELSEIF order_items.item_type = -52/*it*/ THEN
			-- create certificate and add value to it
			PERFORM coupons.cert_value_update(
				jsonb_build_object(
						'item_id',order_items.item_id
					,	'coupon_code',order_items.coupon_code
					,	'action','use_value'
				)
			);
		END IF;

		-- process any referral codes as necessary --
		IF order_items.item_type = 50/*it*/ AND cart_items.properties?'referral_code' THEN 
			perform async_sql_request(
				$asr$ select orders.process_referral_redemption('item_id'+>$1) $asr$
			,	array[order_items.item_id]::text[]
			)
			;
		END IF;

		item_qty := item_qty +1;
	END LOOP;		

END LOOP;

-- TODO: check domain
-- TODO: create order data folder

-- IF at this point everything worked alright, then go ahead and close out the cart (before now, the cart would be entirely intact)

-- clean up carts.items --
UPDATE carts.items SET
	ordered_date = now()
WHERE
	cart_id = cart_id_in
;
UPDATE carts.dir SET
	ordered_date = now()
WHERE
	cart_id = cart_id_in
;



-- At this point everything has been done and you are free to send the email!
PERFORM orders.send_confirmation_email(order_id_in);

json_out := json_out & return_code(1) & ('exit_point' +> 'orders/1');



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
CREATE OR REPLACE FUNCTION orders.apix_update_order(
	IN	json_in		JSONB
,	OUT json_out 	JSONB
) AS
$$
BEGIN

-- validation is done for each case below

CASE 
	WHEN json_in?'update_group_name' THEN
		-- validation --
		IF	NOT json_in->'update_group_name'?&'{group_id,group_name}' THEN
			json_out := json_out & return_code(-102);
			RETURN;
		END IF;
	UPDATE orders.groups SET
		group_name = json_in->'update_group_name'->>'group_name'
	WHERE
		group_id = json_in->'update_group_name'->#'group_id'
	;
	json_out := return_code(0);
	return;
ELSE
END CASE;

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
CREATE OR REPLACE FUNCTION orders.apiv_notes(
	IN	json_in		JSONB
,	OUT json_out 	JSONB
) AS
$$
DECLARE
	
BEGIN

-- validation --
IF NOT json_in?'order_id' THEN json_out := json_out & return_code(-1502); RETURN; END IF;

SELECT
	('notes' +> order_notes) & ('order_id'+>order_id)
INTO json_out
FROM orders.dir
WHERE
	order_id = json_in->#'order_id'
;

IF empty(json_out->'order_id') THEN json_out := json_out & return_code(-1515); RETURN; END IF;

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
CREATE OR REPLACE FUNCTION orders.apix_update_notes(
	IN	json_in		JSONB
,	OUT	json_out	JSONB
) AS
$$
DECLARE
    next_note	TEXT[];
    del_index	INT;
BEGIN

-- validation --
IF NOT json_in?'order_id' THEN 
	json_out := json_out & return_code(-1507); 
	RETURN;
END IF;

IF json_in?'add_note' THEN
	next_note := array[fractime()::text, prod_user_num()::TEXT, json_in->>'add_note'];

	UPDATE orders.dir SET
		order_notes = prod_note_append(order_notes, next_note)
	WHERE
		order_id = json_in->#'order_id'
	;

	json_out := json_out & return_code(1) & ('exit_point' +> 'orders/2');
	RETURN;

ELSEIF json_in?'delete_note' THEN
	del_index := json_in->#'delete_note';

	UPDATE orders.dir SET
		order_notes = array_cat(order_notes[0:(del_index-1)],order_notes[(del_index+1):(array_upper(order_notes,1))])
	WHERE
		order_id = json_in->#'order_id'
	;

	json_out := json_out & return_code(1) & ('exit_point' +> 'orders/3');
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
_|_|_|_|
_|
_|_|_|
_|
_|
*/
CREATE OR REPLACE FUNCTION orders.apix_add_order_tag(
	IN	json_in		JSONB
,	OUT json_out 	JSONB
) AS
$$
DECLARE
	
BEGIN

-- validation --
IF	NOT json_in?'order_id' THEN
	json_out := json_out & return_code(-1502);
	RETURN;
ELSEIF NOT json_in?'tags' THEN
	json_out := json_out & return_code(-1504);	--a tag is required
	RETURN;
END IF;

UPDATE orders.dir
SET
	tags = tags || (json_in->'tags')::text[]
WHERE
	order_id = json_in->#'order_id'
returning 'order_id'+>order_id INTO json_out
;

IF json_out?'order_id' THEN
	json_out := return_code(0);
ELSE
	json_out := return_code(-101);
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
CREATE OR REPLACE FUNCTION orders.apix_update_shipping_properties(
	IN	json_in		JSONB
,	OUT json_out 	JSONB
) AS
$$
BEGIN

-- validation --
IF NOT json_in?'group_id' THEN
	json_out := json_out & return_code(-1505);
	RETURN;
END IF;

BEGIN

	UPDATE orders.groups
	SET
		shipping_type 		= COALESCE(json_in->#'shipping_type',shipping_type)
	,	shipping_address	= COALESCE(json_in->'shipping_address',shipping_address)
	,	packages			= COALESCE(json_in->'packages',packages)
	WHERE
		group_id = json_in->#'group_id'
	RETURNING 'group_id'+>group_id INTO json_out
	;

EXCEPTION when others THEN
	json_out := return_code(-1) & ('violation'+>SQLERRM); -- this is a genuine -1 return code
	RETURN;

END;

IF	json_out IS NULL
THEN
	json_out := json_out & return_code(-101);
	RETURN;
END IF;

json_out := return_code(0);

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
CREATE OR REPLACE FUNCTION orders.apix_group_colapse(
	IN	json_in		JSONB
,	OUT json_out 	JSONB
) AS
$$
BEGIN

-- validation --
IF NOT json_in?'group_id' THEN
	json_out := json_out & return_code(-1505);
	RETURN;
END IF;

UPDATE orders.groups SET
	open = COALESCE(json_in->?'open',open)
WHERE
	group_id = json_in->#'group_id'
RETURNING 'group_id'+>group_id INTO json_out
;

IF	json_out IS NULL
THEN
	json_out := json_out & return_code(-101);
	RETURN;
END IF;

json_out := return_code(0);

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
CREATE OR REPLACE FUNCTION orders.apix_item_update(
	IN	json_in		JSONB
,	OUT json_out 	JSONB
) AS
$$
BEGIN

-- validation --
IF NOT json_in?'item_id' THEN
	json_out := json_out & return_code(-1507);
	RETURN;
ELSEIF json_in?&'{item_id,item_type,description,price,tags,properties,product_code,coupon_code,group_id,group_position}' THEN
END IF;

UPDATE orders.items
SET
	item_type = COALESCE(json_in->#'item_type',item_type)
,	description = COALESCE(json_in->>'description',description)
,	price = COALESCE(json_in->##'price',price)
,	tags = COALESCE((json_in->'tags')::text[],tags)
,	visible = COALESCE(json_in->?'visible',visible)
,	properties = COALESCE(json_in->'properties',properties)

,	product_code = COALESCE(json_in->>'product_code',product_code)
,	coupon_code = COALESCE(json_in->>'coupon_code',coupon_code)

,	group_id = COALESCE(json_in->#'group_id',group_id)
,	group_position = COALESCE(json_in->#'group_position',group_position)

WHERE
	item_id = json_in->#'item_id'
returning 'item_id'+>item_id INTO json_out
;

IF	json_out IS NULL
THEN
	json_out := json_out & return_code(-1508);
	RETURN;
END IF;

json_out := return_code(0);

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
CREATE OR REPLACE FUNCTION orders.apiv_item_notes(
	IN	json_in		JSONB
,	OUT json_out 	JSONB
) AS
$$
DECLARE
	
BEGIN

-- validation --
IF NOT json_in?'item_id' THEN 
	json_out := json_out & return_code(-1507); 
	RETURN; 
END IF;

SELECT
	('notes' +> item_notes) & ('item_id'+>item_id)
INTO json_out
FROM orders.items
WHERE
	item_id = json_in->#'item_id'
;

IF empty(json_out->'item_id') THEN json_out := json_out & return_code(-1508); RETURN; END IF;

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
CREATE OR REPLACE FUNCTION orders.apix_update_item_notes(
	IN	json_in		JSONB
,	OUT	json_out	JSONB
) AS
$$
DECLARE
    next_note	TEXT[];
    del_index	INT;
BEGIN

-- validation --
IF NOT json_in?'item_id' THEN 
	json_out := json_out & return_code(-1507); 
	RETURN; 
END IF;

IF json_in?'add_note' THEN
	next_note := array[fractime()::text, prod_user_num()::TEXT, json_in->>'add_note'];

	UPDATE orders.items SET
		item_notes = prod_note_append(item_notes, next_note)
	WHERE
		item_id = json_in->#'item_id'
	;

	json_out := json_out & return_code(1) & ('exit_point' +> 'orders/4');
	RETURN;

ELSEIF json_in?'delete_note' THEN
	del_index := json_in->#'delete_note';

	UPDATE orders.items SET
		item_notes = array_cat(item_notes[0:(del_index-1)],item_notes[(del_index+1):(array_upper(item_notes,1))])
	WHERE
		item_id = json_in->#'item_id'
	;

	json_out := json_out & return_code(1) & ('exit_point' +> 'orders/5');
	RETURN;

ELSE
	json_out := json_out & return_code(-101);
	RETURN;

END IF;

END;
$$
LANGUAGE PLPGSQL
VOLATILE
;


/*
 \      /
  \    /
   \  /
____\/____
*/
CREATE OR REPLACE VIEW orders.products
AS
SELECT
	product_code
,	name_retail
,	name_common
FROM products.dir
;

-- This view exists to provide a simpler column list for joins that don't need the whole products.dir list and would otherwise cause ambiguous column names.


/*
 \      /
  \    /
   \  /
____\/____
*/
CREATE OR REPLACE VIEW orders.item_types
AS
SELECT * FROM carts.item_types
;


COMMENT ON VIEW orders.item_types is 
$$

-- THIS IS JUST A MIRROR VIEW OF carts.item_types

$$
;


/*
 \      /
  \    /
   \  /
____\/____
*/
CREATE OR REPLACE VIEW orders.rushes AS

SELECT
	order_id
,	group_id
FROM orders.items
WHERE
	item_type = 41/*it*/
;



/*
_|_|_|_|
_|
_|_|_|
_|
_|
*/
CREATE TYPE items_list as (
	order_id		INT
,	item_id			INT
,	size			TEXT
,	nofn			TEXT
,	item_status		TEXT
,	misc			TEXT
,	item_type		INT
,	order_status	TEXT
,	order_type		TEXT
,	item_status_code INT
)
;

CREATE OR REPLACE FUNCTION orders.items_list_base(
	IN query_args		JSONB
,	IN query_args2		JSONB	DEFAULT '{}'::jsonb
) RETURNS SETOF items_list AS
$$
DECLARE
	sql_exec	TEXT;
BEGIN

query_args := '{"limit":100}'::JSONB & query_args2 & query_args;

sql_exec :=
$sql$
	with
		items as
	(
	select
		*
	from orders.items
	--JOIN BACKLOG left join (select item_id, TRUE as backloged from backlog) bklg using (item_id)
	--JOIN PRINT_QUEUE join (select item_id from print_queue) pq using (item_id)
	WHERE
		item_type IN (10/*it*/,21/*it*/)
	--WHERE item_id
	--WHERE item_type
	--WHERE backloged
	--WHERE product_code
	--WHERE order_id
	)
	,	orders as
	(
	select
		order_id
	,	status_name as order_status
	,	''::text as order_type
	from orders.dir, orders.order_status(order_id)
	LEFT JOIN orders.statuses using (status_code)
	where
		order_id in (select order_id from items)
	)
	,	base as
	(
	select
		order_id
	,	item_id
	,	name_common as size
	,	status_name as item_status
	,	status_code as item_status_code
	,	''::text as misc
	,	order_status
	,	order_type
	,	item_type
	from items
	LEFT JOIN orders 			using (order_id)
	LEFT JOIN orders.groups 	using (group_id)
	LEFT JOIN orders.products 	using (product_code)
	LEFT JOIN orders.statuses 	using (status_code)
	)
	select
		order_id
	,	item_id
	,	size
	,	row_number() over (partition by order_id) || ' of ' || count(*) over (partition by order_id) as nofn
	,	item_status
	,	misc
	,	item_type
	,	order_status
	,	order_type
	,	item_status_code
	from base
	WHERE
		TRUE
	--WHERE size
	--WHERE order_status
	--WHERE item_status
	--ORDER
	--LIMIT


	;
$sql$
;

sql_exec := place_sql_constraints(sql_exec, query_args);
sql_exec := replace(sql_exec,';','');
IF query_args?'debug' THEN 
	RAISE EXCEPTION '%', sql_exec || chr(10) || query_args; 
END IF;

RETURN QUERY EXECUTE sql_exec;

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
create type pf_items_list as
(
	order_id		INT
,	item_id			INT
,	size			TEXT
,	nofn			TEXT
,	item_status		TEXT
,	misc			TEXT
,	order_status	TEXT
,	order_type		TEXT
)
;

CREATE OR REPLACE FUNCTION orders.apiv_items_list(
	IN 	json_args 	JSONB
,	OUT json_out	JSONB
) AS
$$
DECLARE
	sql_exec TEXT;
BEGIN

sql_exec := pf_base_apiv_list(
		'items'
	,	json_args
	,	'{"limit":100, "order":"order_dir":"desc"}'::JSONB
	,	'{product_code, item_status, order_status, item_id, order_id, size, order_type}'::text[]
)
;

IF NOT empty(sql_exec)
THEN
	sql_exec := 'SELECT jsonb_agg(row_to_json(base::pf_items_list)) from ('|| sql_exec ||') base;';
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

/*
_|_|_|_|
_|
_|_|_|
_|
_|
*/
create type orders_list as (
	order_id		INT
,	status_code		INT
,	order_date		TIMESTAMP
,	misc			TEXT
,	order_total		DOLLAR
,	user_id			INT
,	order_type		TEXT
)
;


CREATE OR REPLACE FUNCTION orders.orders_list_base(
	IN query_args		JSONB
,	IN query_args2		JSONB	DEFAULT '{}'::jsonb
) RETURNS SETOF orders_list AS
$$
DECLARE
	sql_exec	TEXT;
BEGIN

query_args := '{"limit":100}'::JSONB & query_args2 & query_args;

sql_exec :=
$sql$
with
	base as
(
select
	order_id
,	status_code
,	order_date
,	NULL::text as tags_n_notes
,	user_id
,	'Normal'::text as order_type
from orders.dir
join orders.order_status_code_cache using (order_id)
-- join orders.totals(order_id) using (order_id)
WHERE
	active
--WHERE order_id
--WHERE order_date
--WHERE user_id
--WHERE status_code
--WHERE order_type
--ORDER
--LIMIT
)
select
	order_id
,	status_code
,	base.order_date
,	tags_n_notes
,	revenue_total
,	user_id
,	order_type
from base
join orders.totals(order_id) using (order_id)
;
$sql$
;

sql_exec := place_sql_constraints(sql_exec, query_args);
sql_exec := replace(sql_exec,';','');
IF query_args?'debug3' THEN 
	RAISE EXCEPTION '%', sql_exec || chr(10) || query_args; 
END IF;

RETURN QUERY EXECUTE sql_exec;

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
CREATE OR REPLACE FUNCTION orders.apiv_orders_list(
	IN 	json_args 	JSONB
,	OUT json_out	JSONB
) AS
$$
DECLARE
	sql_exec TEXT;
BEGIN

sql_exec := pf_base_apiv_list(
		'orders'
	,	json_args
	,	'{"limit":100, "order":"order_date", "order_dir":"desc"}'::JSONB
	,	'{order_id, status_code, order_date, order_type, order_total}'::text[]
)
;

IF json_args?'debug2' THEN 
	RAISE EXCEPTION '%', sql_exec || chr(10) || json_args; 
END IF;

IF NOT empty(sql_exec)
THEN
	sql_exec := 'SELECT jsonb_agg(row_to_json(base::orders_list)) from ('|| sql_exec ||') base;';
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


/*
_|_|_|_|
_|
_|_|_|
_|
_|
*/
CREATE OR REPLACE FUNCTION global.status_name(IN INT) RETURNS TEXT AS
$$
select status_name from orders.statuses where status_code = $1;
$$

LANGUAGE SQL
STABLE
;


/*
_|_|_|_|
_|
_|_|_|
_|
_|
*/
CREATE OR REPLACE FUNCTION orders.apiv_shipping(
	IN	json_in		JSONB
,	OUT json_out 	JSONB
) AS
$$
DECLARE
	
BEGIN

-- validation --
IF	NOT json_in?'order_id'
THEN
	json_out := json_out & return_code(-1502);
	RETURN;
END IF;

with
	items as
(
select
	group_id
,	name_common + ' (x' + count(*)::text + ')' as group_products
,	array_agg(distinct status_name(status_code)) = array['qc2p'] as ship_ready
from orders.items ois
left join products.dir using (product_code)
where
	ois.item_type = 10/*it*/
and	order_id = json_in->'order_id'
group by group_id, name_common
)
,	groups as
(
select
	group_name
,	group_products
,	sms.retail_name
,	shipping_address->>'country' != 'us' as is_international
,	ship_ready -- allows the "print label" button
,	packages
,	tracking_number
,	open
,	CASE 
		WHEN ship_ready THEN 'green'
		WHEN shipped_date IS NOT NULL THEN 'blue'
		ELSE 'yellow'
	END as status
from items
join orders.groups using (group_id)
join orders.shipping_methods sms using (shipping_type)
)
select
	jsonb_agg(row_to_json(groups)::jsonb)
into json_out
from groups
;

-- validation --
IF	json_out IS NULL
THEN
	json_out := json_out & return_code(-1513);
	RETURN;
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
CREATE OR REPLACE FUNCTION orders.apiv_order_details(
	IN	json_in		JSONB
,	OUT json_out	JSONB
) AS
$$
DECLARE
	order_id_in INT := json_in->#'order_id';
	
BEGIN

IF empty(order_id_in) THEN -- 
	 json_out := return_code(-1502);
	RETURN;
END IF;

with
	order_info as
(
select
		(row_to_json(od)::jsonb)
	&	(row_to_json(ud)::jsonb)
	&	(row_to_json(totals)::jsonb)
	&	('order_status'+>orders.order_status(order_id))
	&	(support.pins(json_in#'order_id')#'pins')
	as order_info
from orders.dir od
join users.info ud using (user_id)
JOIN orders.totals(order_id) using (order_id)
where
	order_id = order_id_in
)
,	order_items as
(
SELECT
	*
FROM orders.items
WHERE
	order_id = order_id_in
)
,	pre_items as
(
select
	group_id
,	item_id
,	group_position
,	parent_item_id
,	item_type
,	item_date
,	description
,	price
,	status_code
,	payment_id
,	item_notes
,	tags
,	visible
,	properties
,	product_code
,	coupon_code
,	attachment
,	group_check
,	production.print_count(item_id)
,	support.pins(('item_id'+>item_id))->'pins' as pins
from order_items
where
	item_type > 0
)
,	items as
(
select
	group_id
,	'items'+>(jsonb_agg(row_to_json(pre_items)::jsonb)) as item_details
from pre_items
group by group_id
)
,	pre_payment_items as
(
select
	item_id
,	price
,	visible
,	item_date
,	item_type
,	payment_id
,	payment_address
,	item_notes
,	support.pins(('item_id'+>item_id))->'pins' as pins
from order_items
join (select payment_id, payment_address from payments.dir) payments using (payment_id)
where
	item_type < 0
)
,	payment_items as
(
select
	'payment_items'+>(jsonb_agg(row_to_json(pre_payment_items)::jsonb)) as payment_items
from pre_payment_items
)
,	groups as
(
select
	'order_groups'+>jsonb_agg(
		row_to_json(og)::jsonb & item_details & ('group_status'+>orders.group_status(og.group_id))
		) as group_info
from orders.groups og
right join items using (group_id)
)
select
	(	order_info
	& 	group_info
	&	payment_items
	)
into json_out
from 
	groups,	order_info, payment_items
;

IF empty(json_out) THEN -- 
	json_out := return_code(-1514);
	RETURN;
END IF;

END;
$$

LANGUAGE PLPGSQL
STABLE
;


/*
Status Caches
*/


CREATE TABLE orders.order_status_code_cache
(
	order_id 		INT 	UNIQUE 	REFERENCES orders.dir (order_id)			ON UPDATE CASCADE	ON DELETE CASCADE
,	status_code 	INT 			REFERENCES orders.statuses (status_code) 	ON UPDATE CASCADE
)
;

CREATE INDEX orders_order_status_index ON orders.order_status_code_cache (order_id);

CREATE TABLE orders.group_status_code_cache
(
	group_id 		INT 	UNIQUE 	REFERENCES orders.groups (group_id)			ON UPDATE CASCADE	ON DELETE CASCADE
,	status_code 	INT 			REFERENCES orders.statuses (status_code) 	ON UPDATE CASCADE
)
;

CREATE INDEX orders_group_status_index ON orders.group_status_code_cache (group_id);

/*
_|_|_|_|
_|
_|_|_|
_|
_|
*/
-- This function centralizes the logic for determining the group status_code.
-- When refresh is true, it goes through the logic tree of determining the status, otherwise it relies on the cached value.
-- Refreshing is only intended to be used by the trigger that's hit when order.items change or administratively as needed.
-- When refresh = false, the function is only intended to be used on a one-by-one basis
-- If joining statuses is needed, then the orders.group_status_code_cache should just be joined
CREATE OR REPLACE FUNCTION orders.group_status(
	IN	group_id_in	INT
,	IN 	refresh 	BOOLEAN DEFAULT FALSE
,	OUT status_code INT
) AS
$$
DECLARE
	status_codes int[];
BEGIN

IF NOT refresh THEN
	SELECT gscc.status_code into status_code from orders.group_status_code_cache gscc where group_id = group_id_in;
	RETURN;
END IF;

SELECT array_agg(distinct items.status_code) INTO status_codes from orders.items where group_id = group_id_in and item_type > 0;

CASE 
	WHEN 0010 = all(status_codes) -- if all the statuses are = 0010 then 0010
		THEN status_code = 0010; RETURN; 

	WHEN 0000 = all(status_codes) -- if any of the statuses are = 0000 then 0000
		THEN status_code = 0000; RETURN; 

	WHEN 0-10 = any(status_codes) -- if any of the statuses are = 0-10 then 0-10
		THEN status_code = 0-10; RETURN; 

	WHEN 0010 <= all(status_codes) AND 0100 > all(status_codes)  -- if all the statuses are >= 0010 but the min(*) < 0100
		THEN status_code = 0020; RETURN;

	WHEN 0100 = all(status_codes) -- if all the statuses are = 0100 then 0100
		THEN status_code = 0100; RETURN; 

	WHEN 0-20 = all(status_codes) -- if all the statuses are 0-20 then 0-20
		THEN status_code = 0-20; RETURN; 

	ELSE
		status_code = 00-1; RETURN;
	-- TODO: need to issue an email/log for un-found status condition

END CASE;
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
-- This function centralizes the logic for determining the order status_code.
-- When refresh is true, it goes through the logic tree of determining the status, otherwise it relies on the cached value.
-- Refreshing is only intended to be used by the trigger that's hit when order.items change or administratively as needed.
-- When refresh = false, the function is only intended to be used on a one-by-one basis
-- If joining statuses is needed, then the orders.order_status_code_cache should just be joined
CREATE OR REPLACE FUNCTION orders.order_status(
	IN	order_id_in	INT
,	IN 	refresh 	BOOLEAN DEFAULT FALSE
,	OUT status_code INT
) AS
$$
DECLARE
	status_codes int[];
BEGIN

IF NOT refresh THEN
	SELECT oscc.status_code into status_code from orders.order_status_code_cache oscc where order_id = order_id_in;
	RETURN;
END IF;

SELECT array_agg(distinct items.status_code) INTO status_codes from orders.items where order_id = order_id_in and item_type > 0;

CASE 
	WHEN 0010 = all(status_codes) -- if all the statuses are = 0010 then 0010
		THEN status_code = 0010; RETURN; 

	WHEN 0000 = all(status_codes) -- if any of the statuses are = 0000 then 0000
		THEN status_code = 0000; RETURN; 

	WHEN 0-10 = any(status_codes) -- if any of the statuses are = 0-10 then 0-10
		THEN status_code = 0-10; RETURN; 

	WHEN 0010 <= all(status_codes) AND 0100 > all(status_codes)  -- if all the statuses are >= 0010 but the min(*) < 0100
		THEN status_code = 0020; RETURN;

	WHEN 0100 = all(status_codes) -- if all the statuses are = 0100 then 0100
		THEN status_code = 0100; RETURN; 

	WHEN 0-20 = all(status_codes) -- if all the statuses are 0-20 then 0-20
		THEN status_code = 0-20; RETURN; 

	ELSE
		status_code = 00-1; RETURN;
	-- TODO: need to issue an email/log for un-found status condition
END CASE;
END;
$$

LANGUAGE PLPGSQL
STABLE
;


CREATE OR REPLACE FUNCTION status_code_cache_refresh() RETURNS TRIGGER AS
$$
DECLARE
	group_id_in 	INT;
	order_id_in 	INT;
	group_sc		INT;
	order_sc		INT;
BEGIN

IF 	TG_OP = 'DELETE' THEN
	group_id_in := OLD.group_id;
	order_id_in := OLD.order_id;
ELSE
	group_id_in := NEW.group_id;
	order_id_in := NEW.order_id;
END IF
;

group_sc := orders.group_status(group_id_in, TRUE); -- get the refreshed status
order_sc := orders.order_status(order_id_in, TRUE); -- get the refreshed status

INSERT INTO orders.group_status_code_cache
VALUES
	(group_id_in, group_sc)
ON CONFLICT (group_id) DO UPDATE SET 
	status_code = group_sc
;

INSERT INTO orders.order_status_code_cache
VALUES
	(order_id_in, order_sc)
ON CONFLICT (order_id) DO UPDATE SET 
	status_code = order_sc
;

RETURN NULL; -- result is ignored since this is an AFTER trigger

END;
$$
LANGUAGE PLPGSQL
VOLATILE
;

CREATE TRIGGER status_code_cache_refresh_trigger
AFTER 
		INSERT
	OR 	DELETE
	OR 	UPDATE OF status_code
ON orders.items
FOR EACH ROW
EXECUTE PROCEDURE status_code_cache_refresh()
;


/*
_|_|_|_|
_|
_|_|_|
_|
_|
*/
CREATE OR REPLACE FUNCTION orders.order_id_from_item_id(INT) RETURNS INT AS
$$
SELECT order_id FROM orders.items WHERE item_id = $1;
$$
LANGUAGE SQL
STABLE
;


/*
_|_|_|_|
_|
_|_|_|
_|
_|
*/
CREATE OR REPLACE FUNCTION orders.apiv_order_id_from_item_id(INT) RETURNS INT AS
$$
BEGIN
	RETURN (SELECT order_id FROM orders.items WHERE item_id = $1);
END
$$
LANGUAGE PLPGSQL
STABLE
;


-- This function processes any use of a referral coupon by crediting the referral code owner and sending an email out
-- The real reason why its a function is so that it can be done asyncronously from order placement as its not important 
-- enough of an opereration to add overhead to placing an order, or on error, to hold back an order
CREATE OR REPLACE FUNCTION orders.process_referral_redemption(
	IN	json_in		JSONB
,	OUT	json_out	JSONB
) AS
$$
DECLARE
BEGIN

-- validation --
IF NOT json_in?|'{item_id}' THEN -- 
	json_out := return_code(-1);
	RETURN;
END IF;

with 
	base as
(
select
	order_id
,	unnest(array[31/*it*/,50/*it*/]) as item_type
,	unnest(array[-price,price])
,	FALSE as visible
,	item_id as parent_item_id
,	coupon_code
from orders.items
where
	item_id = json_in->#'item_id'
AND item_type = 50/*it*/
AND properties?'referral_code'
)
,	insert_ois as
(
-- insert the items for referral redemption account credit and the coupon code to "pay" for it
insert into orders.items (order_id, item_type, price, visible, parent_item_id, coupon_code) 
-- (a group_id is not included because they aren't being shipped and these items will be invisible to the customer anyways)
select * from base
returning *
)
,	insert_certs as
(
-- add account credit to referrer's code (in new group)
insert into coupons.certificates (coupon_code, item_id, value)
select
	coupon_code
,	item_id
,	price
from insert_ois
where
	item_type = 31/*it*/
returning *
)
select
	return_code(1)
into json_out
from insert_certs
;


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
CREATE OR REPLACE FUNCTION orders.apix_add_item(
	IN	json_in		JSONB
,	OUT json_out 	JSONB
) AS
$$
DECLARE
	item_type_in INT;
BEGIN

-- validation --
IF	NOT json_in?&'{order_id,item_type}' THEN
	json_out := json_out & return_code(-101);
	RETURN;
END IF;

item_type_in := json_in->#'item_type';

CASE
	WHEN item_type_in = -51/*it*/ THEN

		INSERT INTO orders.items
		(	order_id
		,	item_type
		,	price
		,	payment_id
		)
		VALUES
		(	json_in->#'order_id'
		,	item_type_in
		,	-(json_in->##'amount')
		,	json_in->>'payment_id'
		)
		;
	json_out := return_code(0);
	RETURN;

	WHEN item_type_in = -53/*it*/ THEN

		INSERT INTO orders.items
		(	order_id
		,	item_type
		,	price
		,	payment_id
		)
		VALUES
		(	json_in->#'order_id'
		,	item_type_in
		,	-(json_in->##'amount')
		,	json_in->>'payment_id'
		)
		;
	json_out := return_code(0);
	RETURN;

ELSE
END CASE;


END;
$$
LANGUAGE PLPGSQL
VOLATILE
;

