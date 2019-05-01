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

DROP SCHEMA IF EXISTS coupons CASCADE;
CREATE SCHEMA coupons;
SET search_path TO "global";

/*
----------
	||	 
	||	 
	||	 
*/
CREATE TABLE coupons.statuses 
(
	status		INT			PRIMARY KEY
,	name		TEXT		NOT NULL
-- consider reducing this down to just a textual key and changing the name to status_cupn 
)
;

COMMENT ON TABLE coupons.statuses is 
$$
-- the statuses table exists for the benefit of reference enforcement
$$
;

/*
----------
	||	 
	||	 
	||	 
*/
CREATE TABLE coupons.categories 
(
	category		TEXT		PRIMARY KEY
,	description		TEXT		NOT NULL

)
;

COMMENT ON TABLE coupons.categories is 
$$
-- the categories table exists for the benefit of reference enforcement
$$
;

/*
------------------
	||		||		
	||		||		
	||		||		
*/

CREATE TABLE coupons.dir 
(
	coupon_code			TEXT		PRIMARY KEY 	CHECK(coupon_code = translate(coupon_code, setting_lookup('coupons/illegal_characters'), ''))
,	coupon_type			INT			NOT NULL		DEFAULT 1	CHECK(coupon_type IN (1,2,3,4,5,6))					-- (1: normal, 2: alias, 3: certificate, 4: voucher, 5: referral, 6: empty/trigger)
,	coupon_parent		TEXT		REFERENCES coupons.dir												-- when the code_type is "alias", the parent coupon_code is used for all non-set properties
,	desc_retail			TEXT		NOT NULL DEFAUlT ''													-- the coupon description as seen by customers
,	desc_internal		TEXT		NOT NULL DEFAUlT ''													-- the coupon description as seen internally
,	category			TEXT		NOT NULL DEFAULT ''	CHECK (array[category] <@ setting_lookup('coupons/categories')::TEXT[])			-- just an organizational thing; diabled for aliases
,	tags				TEXT[]		NOT NULL DEFAULT '{}'	CHECK (tags <@ setting_lookup('coupons/tags')::TEXT[])	-- just an organizational thing
,	created_date		TIMESTAMP 	NOT NULL DEFAULT now()

-- CONSTRAINTS --
,	status				INT			DEFAULT 0 REFERENCES coupons.statuses (status) ON UPDATE CASCADE	-- status; parent coupon dominates
,	expires				TIMESTAMP	NOT NULL DEFAULT NOW() + '1 year'::interval							-- expiration date - coupon becomes inactive; parent coupon dominates
,	req_register		BOOLEAN		NOT NULL DEFAULT FALSE									-- coupon requires registered user;
,	req_admin			BOOLEAN		NOT NULL DEFAULT FALSE									-- coupon can only be used by admins; 
,	max_total_uses		INT			NOT NULL DEFAULT 1										-- max total uses for coupon; 
,	max_user_uses		INT			NOT NULL DEFAULT 1										-- maximum uses of coupon per user;
,	email_restrict		TEXT[]		NOT NULL DEFAULT '{}'									-- coupon is restricted to specificed email addresses;
,	req_product_codes	JSONB 		NOT NULL DEFAULT '{}' CHECK (accepted_products(req_product_codes))			-- required products for coupon to be applied;
,	min_product_total	DOLLAR		CHECK (min_product_total > 0.0)				-- minimum sum of products;
,	domestic_only		BOOLEAN		NOT NULL DEFAULT FALSE									-- coupon can only be used when shipping destination is domestic;
,	product_cat			TEXT		NOT NULL DEFAULT 'all_prints' CHECK (array[product_cat] <@ setting_lookup('coupons/product_cat')::TEXT[])		-- options: All prints / User Uploaded prints / Art Store prints;

-- PRIORITIZATION --
,	mixable				BOOLEAN		NOT NULL DEFAULT TRUE									-- allows for class and priority to even take affect; 
,	class				TEXT		DEFAULT 'C'		CHECK (class IN ('A', 'B', 'C', 'D'))	-- class;
,	priority			INT			DEFAULT 5		CHECK (priority BETWEEN 0 AND 9)		-- priority
,	binable				BOOLEAN		NOT NULL DEFAULT TRUE									-- allows for coupons to be binned for later if not immediately applicable

-- VALUE & APPLICATION --
,	percentage			FLOAT		CHECK (percentage between 0 and 1)						-- percentage applied
,	value				DOLLAR		CHECK (value >= 0)										-- total value applied
,	user_restrict		BOOLEAN		NOT NULL 		DEFAULT FALSE							-- upon placing of order, coupon is restricted to user
,	free_shipping		BOOLEAN		NOT NULL 		DEFAULT FALSE							-- free shipping is applied with coupon
,	tax_free			BOOLEAN		NOT NULL 		DEFAULT FALSE							-- tax is omitted with coupon

-- ADMIN --
-- ,	edit_locked			INT			REFERENCES prod_users.dir (prod_user_num) ON UPDATE CASCADE	-- when not NULL, then is locked by the eid who locked it
,	email_notify		TEXT[]		NOT NULL DEFAULT '{}'									-- references employee_id								-- when the order is placed, then the specified employee is notified
,	favorite			BOOLEAN		NOT NULL DEFAULT FALSE									-- denotes a favorite coupon for admins
,	cart_icon			TEXT		NOT NULL DEFAULT ''										-- specifies which coupon icon to use

)
;

ALTER TABLE coupons.dir ADD CONSTRAINT coupon_parent_fkey FOREIGN KEY (coupon_parent) REFERENCES coupons.dir (coupon_code) ON UPDATE CASCADE ON DELETE CASCADE; -- this has to be done separate because you cannot reference a table (self) until it exists

CREATE INDEX coupons_dir_index ON coupons.dir (coupon_code);

CREATE TRIGGER trigger_snapshot
AFTER UPDATE ON coupons.dir
FOR EACH ROW
WHEN (current_setting('dbAPI.snapshots')::BOOLEAN AND old.* IS DISTINCT FROM new.*)
EXECUTE PROCEDURE snapshot_take()
;

CREATE OR REPLACE FUNCTION coupons.trigger_coupon_update_new() returns TRIGGER AS 
$$
BEGIN

NEW.coupon_code := upper(NEW.coupon_code);

RETURN NEW;
END;
$$

LANGUAGE PLPGSQL
VOLATILE
;

CREATE TRIGGER coupon_force_uppercase_insert
	BEFORE INSERT
	ON coupons.dir
	FOR EACH ROW
	WHEN (new.coupon_code != upper(new.coupon_code))
	EXECUTE PROCEDURE coupons.trigger_coupon_update_new()
;
CREATE TRIGGER coupon_force_uppercase_updates
	BEFORE UPDATE
	ON coupons.dir
	FOR EACH ROW
	WHEN (new.coupon_code != old.coupon_code AND new.coupon_code != upper(new.coupon_code))
	EXECUTE PROCEDURE coupons.trigger_coupon_update_new()
;


/*
*/
-- drop TRIGGER trigger_cart_coupons_refresh on coupons.dir

CREATE OR REPLACE FUNCTION coupons_refresh() RETURNS trigger AS
$$
DECLARE
BEGIN

PERFORM
	carts.coupons_refresh(cart_id)
from carts.items
where
	ordered_date IS NULL
and item_type = 50/*it*/
and coupon_code = new.coupon_code
;

RETURN NULL;

END;
$$
LANGUAGE PLPGSQL
VOLATILE
;

CREATE TRIGGER trigger_cart_coupons_refresh
AFTER UPDATE ON coupons.dir
FOR EACH ROW
WHEN (old.* IS DISTINCT FROM new.*)
EXECUTE PROCEDURE coupons_refresh()
;


/*
_|_|_|_|
_|
_|_|_|
_|
_|
*/
create type pf_coupons_list as
(
	coupon_code				text
,	category				text
,	tags					TEXT[]
,	created_date			timestamp
,	status					INT
,	percentage				FLOAT
,	value					DOLLAR
,	free_shipping			BOOLEAN
,	favorite				BOOLEAN	
)
;

CREATE OR REPLACE FUNCTION coupons.apiv_list(
	IN 	json_args 	JSONB
,	OUT json_out	JSONB
) AS
$$
DECLARE
	sql_exec TEXT;
BEGIN

sql_exec := pf_base_apiv_list(
		'coupons'
	,	json_args
	,	'{"limit":100, "order":"created_date", "order_dir":"desc"}'::JSONB
	,	'{coupon_code, category, tags, created_date, status, percentage, value, free_shipping, favorite}'::text[]
)
;

IF NOT empty(sql_exec)
THEN
	sql_exec := 'SELECT jsonb_agg(row_to_json(base::pf_coupons_list)) from ('|| sql_exec ||') base;';
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
------------------
	||		||		
	||		||		
	||		||		
*/

CREATE TABLE coupons.certificates
(
	certificate_id		SERIAL 		PRIMARY KEY
,	coupon_code			TEXT		REFERENCES coupons.dir (coupon_code)
,	added_date			TIMESTAMP	NOT NULL DEFAULT now()
,	value				DOLLAR		NOT NULL DEFAULT 0.0
,	notes 				TEXT
,	item_id 			INT 		REFERENCES orders.items (item_id) ON UPDATE CASCADE ON DELETE CASCADE
)
;

COMMENT ON TABLE coupons.certificates is 
$$

-- payment_id's are not stored here as they are referenced more correctly through the order_items table

$$
;


/*
_|_|_|_|
_|
_|_|_|
_|
_|
*/
CREATE OR REPLACE FUNCTION coupons.apix_gen_code(
	IN	json_in		JSONB 	DEFAULT '{}'
,	OUT json_out 	JSONB
) AS 
$$
DECLARE

	loop_safety INT := 1;
	coupon_code_search text;
	
BEGIN

json_out := '{}';

IF empty(json_in->>'coupon_code') THEN -- if the coupon code is empty, default to a random pattern
	json_in := json_in || ('coupon_code'::text +> '%%%%%%'::text);
END IF;

LOOP -- loop, generating codes until one is not already taken
	coupon_code_search := upper(random_string_from_pattern(json_in->>'coupon_code')); 
	IF EXISTS (SELECT TRUE FROM coupons.dir WHERE upper(coupon_code) = coupon_code_search) THEN -- check to see if it exists, and if so, loop again
		loop_safety := loop_safety + 1;
	ELSE
		json_out := json_out & ('coupon_code' +> coupon_code_search);
		json_out := json_out & return_code(1); 
		RETURN;
		EXIT;
	END IF;

	IF loop_safety > 100 THEN -- sorry, having a hard time finding a pattern that isn't already taken (try again)
		json_out := json_out & return_code(-1402);
		RETURN;
		EXIT;
	END IF;

END LOOP;

END;
$$

LANGUAGE PLPGSQL
STABLE
;


COMMENT ON FUNCTION coupons.apix_gen_code(JSONB) is 
$$
-- All this function does is generate a reliable, untaken code based on the pattern, if given

--	Pattern wildcards:
-- @: random letter
-- #: random number
-- %: random letter or number


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
CREATE OR REPLACE FUNCTION coupons.apix_create(
	IN	json_in		JSONB
,	OUT json_out 	JSONB
) AS
$$
DECLARE

	coupon_qty INT := 1;
	coupon_created_qty INT := 0;
	coupon_code_in TEXT := upper(json_in->>'coupon_code');
	coupon_code_new JSONB;
	coupon_code_new_list TEXT[];
	temp_rec record;
	
BEGIN
json_out := '{}';

-- requirements --
IF empty(coupon_code_in) THEN -- generate a default code if one wasn't already provided
	json_in := json_in || coupons.apix_gen_code('{}');
END IF;

IF NOT empty(json_in->'coupon_qty') THEN -- initialize the coupon_qty if not already specified
	coupon_qty := (json_in->>'coupon_qty')::int
	RETURN;
END IF;


-- add in tags

IF NOT array[lower(json_in->>'tags')] <@ setting_lookup('coupons/tags')::TEXT[]
	THEN perform setting_update('coupons/tags', (setting_lookup('coupons/tags')::TEXT[] || lower(json_in->>'tags'))::text);
END IF;

WHILE coupon_created_qty < coupon_qty LOOP -- create the coupon_code for as many as are being requested

	IF coupon_code_in ~* '[@#%]' OR empty(coupon_code_in) THEN -- if the code is unspecified or has wildcards, then fill them - otherwise skip to save time
		coupon_code_new := coupons.apix_gen_code('coupon_code' +> coupon_code_in); -- generate a code
		IF return_code(coupon_code_new) < 0 THEN -- catch any errors
			json_out := json_out & (coupon_code_new#'return_code');
			RETURN;
		END IF;
		json_in := json_in || coupon_code_new;
	ELSE
		coupon_qty := 1; -- if no wildcards, you can't make more than one
	END IF;

	BEGIN

	-- go ahead and insert the coupon with all its properties
	PERFORM jsonb_table_insert('coupons', 'dir', json_in); -- this assumes that all the necessary columns are in the JSONB array, and if not, catches the errors below

		EXCEPTION
			WHEN not_null_violation THEN -- missing values
				json_out := json_out & return_code(-1403);
				RETURN;
			WHEN check_violation THEN -- unacceptable values
				json_out := json_out & return_code(-1404);
				RETURN;
			WHEN unique_violation THEN -- coupon_code already exists
				json_out := json_out & return_code(-1405);
				RETURN;
			WHEN others THEN -- SQL error
				json_out := json_out & return_code(-1000);
				RETURN;
			--TODO: send admin email alert (this kind of error should not happen)

	END;

	coupon_created_qty = coupon_created_qty + 1;
	coupon_code_new_list = coupon_code_new_list || (coupon_code_new->>'coupon_code');
	-- json_out := json_out & (('coupon_code_' || coupon_created_qty)::text +> (json_in->>'coupon_code'));
	
END LOOP;

json_out := json_out & ('coupon_codes' +> coupon_code_new_list);
json_out := json_out & return_code(1);

END;
$$

LANGUAGE PLPGSQL
VOLATILE
;



/*_|_|_|_|
_|
_|_|_|
_|
_|
*/
CREATE OR REPLACE FUNCTION coupons.request(
	IN	json_in		JSONB
,	OUT json_out 	JSONB
) 
AS
$$
DECLARE

	coupon_code_in TEXT := upper(json_in->>'coupon_code');
	cart_id_in INT := json_in->>'cart_id';
	user_id_in INT := json_in->#'user_id';
	cd RECORD;
	pcd RECORD;
	ci RECORD;
	temp_req RECORD;
	contents JSONB := carts.apiv_contents(json_in);
	
BEGIN
json_out := '{}';

IF empty(user_id_in) THEN user_id_in := (select user_id from carts.dir where cart_id = cart_id_in); END IF;

IF coupon_code_in IN (select coupon_code from carts.items where cart_id = cart_id_in) THEN
	json_out := json_out & return_code(1419); -- coupon code is already in the cart foo'...
	RETURN;
END IF;


SELECT * INTO cd FROM coupons.dir WHERE coupon_code = coupon_code_in;

-- VALIDATION -- 
IF empty(cd.coupon_code) THEN
	json_out := json_out & return_code(-1407); -- invalid coupon_code
	RETURN;
END IF;

LOOP -- this is only being used to allow the changing of coupon types mid-function (its a work around for the lack of a "goto" clause in postgres)
CASE cd.coupon_type -- there are various different coupon types

WHEN 2 THEN -- for alias codes, fill in all the empty/unspecified parameters with those of the parent coupon

	IF cd.coupon_parent IS NULL THEN
		json_out := json_out & return_code(-1408); -- must specify parent code when code type is alias
		RETURN;
	END IF;

	SELECT * INTO pcd FROM coupons.dir WHERE coupon_code = cd.coupon_parent; -- pull all the coupon properties from the parent code
	
	-- the following columns are inherited from the parent coupon when the alias is NULL
	-- columns which cannot accept null or have a mandatory default need not be included since they will never be expressed when the alias is not null
	-- this has to be done manually as there is *no* automated way in PLPGSQL and the columns have to be cherry-picked manually anyways
	
	cd.desc_retail := COALESCE(cd.desc_retail, pcd.desc_retail);
	cd.desc_internal := COALESCE(cd.desc_internal, pcd.desc_internal);
	cd.tags := COALESCE(cd.tags, pcd.tags);
	cd.status := COALESCE(cd.status, pcd.status);
	cd.req_register := COALESCE(cd.req_register, pcd.req_register);
	cd.req_admin := COALESCE(cd.req_admin, pcd.req_admin);
	cd.max_total_uses := COALESCE(cd.max_total_uses, pcd.max_total_uses);
	cd.max_user_uses := COALESCE(cd.max_user_uses, pcd.max_user_uses);
	cd.email_restrict := COALESCE(cd.email_restrict, pcd.email_restrict);
	cd.req_product_codes := COALESCE(cd.req_product_codes, pcd.req_product_codes);
	cd.min_product_total := COALESCE(cd.min_product_total, pcd.min_product_total);
	cd.domestic_only := COALESCE(cd.domestic_only, pcd.domestic_only);
	cd.mixable := COALESCE(cd.mixable, pcd.mixable);
	cd.class := COALESCE(cd.class, pcd.class);
	cd.priority := COALESCE(cd.priority, pcd.priority);
	cd.binable := COALESCE(cd.binable, pcd.binable);
	cd.percentage := COALESCE(cd.percentage, pcd.percentage);
	cd.value := COALESCE(cd.value, limited_float(pcd.value));
	cd.user_restrict := COALESCE(cd.user_restrict, pcd.user_restrict);
	cd.free_shipping := COALESCE(cd.free_shipping, pcd.free_shipping);
	cd.tax_free := COALESCE(cd.tax_free, pcd.tax_free);
	-- cd.edit_locked := COALESCE(cd.edit_locked, pcd.edit_locked);
	cd.email_notify := COALESCE(cd.email_notify, pcd.email_notify);
	cd.favorite := COALESCE(cd.favorite, pcd.favorite);
	cd.cart_icon := COALESCE(cd.cart_icon, pcd.cart_icon);

WHEN 3 THEN -- for certificates, check for value and update the coupon value to be the cart total value or the remaining certificate balance, whichever is least
	
	cd.value := LEAST(
		contents->##'amount_cert_redeemable',
		(SELECT COALESCE(sum(value),0) as cert_balance FROM coupons.certificates WHERE coupon_code = coupon_code_in)
		);
	cd.percentage := 0; -- just making sure it gets processed correctly at the end
	json_out := json_out & ('certificate'+>TRUE); -- this sets a flag for the cart to be able to recognize that the coupon code is for a certificate, which is handled as a different item_type (i.e. the coupon code is just a trigger for the certificate)

WHEN 5 THEN -- for referral codes

	--first we need to determine whether the user of the code is the owner or not
	IF coupon_code_in = 'RFR'||user_id_in THEN -- the requestor is the owner
		cd.coupon_type := 3;
		CONTINUE; -- this sends us back to the beginning of the loop with all other things the same except that the the coupon is now interpreted (routed, technically) as a certificate
	ELSE
		cd.coupon_type := 2; -- the requestor is NOT the owner and is therefore a referee
		json_out := json_out & ('referral_code'+>TRUE);
		CONTINUE; -- this sends us back to the beginning as a regular coupon, whence it'll pull the parent_code properties while keeping the original coupon_code name
	END IF;

ELSE
END CASE;

	EXIT; -- exit the loop in all cases which don't explicitly call to continue it
END LOOP;

CASE
	-- EXIT CASES / HARD CONSTRAINTS (WHEN VIOLATED, CANNOT BE BINNED) -- 
	WHEN cd.status != 0 THEN -- sorry, not active
		json_out := json_out & return_code(1409);
		return;
	
	WHEN cd.expires IS NOT NULL AND cd.expires < now() THEN -- sorry, expired
		json_out := json_out & return_code(1410);
		return;

	WHEN (cd.req_register AND NOT users.exists(user_id_in)) THEN -- sorry, requires registered user
		json_out := json_out & return_code(1411);
		return;
	
	WHEN (cd.req_admin AND NOT empty(json_in->'user_admin')) THEN -- sorry, requires admin privs
		json_out := json_out & return_code(1412);
		return;

	WHEN NOT empty(cd.email_restrict) AND NOT -- when the email_restrict is populated AND therefor if the cart-owner's email is in the list
		(SELECT 
			email <@ cd.email_restrict 
		FROM carts.dir 
		JOIN users.creds using (user_id)
		WHERE
			cart_id = cart_id_in
		) THEN -- sorry, not for your email
		json_out := json_out & return_code(1413);
		return;

	-- will have to pull this from orders table
	/*WHEN cd.max_total_uses < (select total uses) THEN -- sorry, used too many times
		json_out := json_out & return_code(-1.7);
		return;
	*/

	-- will have to pull this from orders table
	/*WHEN cd.max_user_uses < (select total uses) THEN -- sorry, used too many times
		json_out := json_out & return_code(-1.8);
		return;
	*/

	WHEN cd.domestic_only AND contents->>'country' != 'us' THEN -- sorry, domestic only
		json_out := json_out & return_code(1414);
		return;


	-- BIN CASES --
	WHEN not empty(cd.req_product_codes) THEN -- when the coupon requires certain products and quantities to be in the cart

		-- here the required products and quantities are compared with the products and quantities in the cart
		-- what is returned is a JSONB list of {"product_code":deficit_qty, ...} and will be used to communicate the up-sell to the customer
		with
			required as
		(
		select
			key as product_code
		,	value::int as qty_req
		from jsonb_each_text(cd.req_product_codes)
		)
		,	cart_contents as
		(
		select 
			product_code
		,	quantity as qty_avail
		FROM carts.items
		WHERE 
			cart_id = cart_id_in
		AND	item_type = 10/*it*/
		)
		,	report as
		(
		select
			product_code
		,	qty_req - COALESCE(qty_avail,0) as qty_needed
		from required
		left join cart_contents using (product_code)
		)
		select
			'req_product_qty' +> json_agg(product_code+>qty_needed) as req_product_qty
		INTO temp_req
		from report
		where
			qty_needed > 0
		;


		IF NOT temp_req.req_product_qty IS NOT NULL THEN -- coupon will try to be binned
	
			IF cd.binable THEN
				-- add to / update the bin
				DELETE FROM carts.bins WHERE cart_id = cart_id_in AND coupon_code = coupon_code_in;
				INSERT INTO carts.bins (cart_id, coupon_code, deficits)
					VALUES (cart_id_in, coupon_code_in, temp_req.req_product_qty);
	
				json_out := json_out & return_code(1415) & ('coupon_binned' +> TRUE) & ('exit_point' +> 1); -- success (partially): coupon has been binned
				return;
			ELSE
				json_out := json_out & return_code(1416); -- success (technically): though coupon cannot be binned
				RETURN;
			END IF;
		END IF;

		-- If the returned temp_req.req_product_qty is NULL, that means that there were no products required that went unsatisfied and the coupon is GOOD to use (nothing else needed here)


	WHEN cd.min_product_total > (contents->##'product_total') THEN -- sorry, still need to spend more

		IF cd.binable THEN
			-- add to / update the bin
			DELETE FROM carts.bins WHERE cart_id = cart_id_in AND coupon_code = coupon_code_in;
			INSERT INTO carts.bins (cart_id, coupon_code, deficits)
				VALUES (cart_id_in, coupon_code_in, 'sub_total' +> min_product_total);

				json_out := json_out & return_code(1415) & ('coupon_binned' +> TRUE) & ('exit_point' +> 2); -- success (partially): coupon has been binned
			return;
		ELSE
			json_out := json_out & return_code(1416); -- success (technically): coupon cannot be binned
			RETURN;
		END IF;

	/*
		TODO need some way of identifying regular print vs art store, vs etc.
	*/
	
	ELSE 
	-- Prioritization --

	FOR temp_req IN (
		SELECT
			coupon_code, class, priority, mixable
		FROM carts.items
		JOIN coupons.dir USING (coupon_code)
		WHERE
			cart_id = cart_id_in
		AND item_type = 50/*it*/
		) 
	LOOP

		IF NOT (temp_req.mixable OR cd.mixable) THEN -- if either the current or candidate coupons are not mixable, then stop right there
			json_out := json_out & return_code(1417);
			RETURN;
			EXIT;
		END IF;
		

		IF temp_req.class = cd.class AND temp_req.priority > cd.priority THEN -- if the candidate coupon and the currently held coupon are in the same class AND if the currently held coupon is of a greater priority, then reject the candidate.
			json_out := json_out & ('conflicting_coupon' +> temp_req.coupon_code); -- sorry, you cannot use that coupon with this coupon
			json_out := json_out & return_code(1417); 
			RETURN;
			EXIT;
		END IF;
	
	END LOOP;


			
	-- If at this point there has not been a return, that means the coupon is safe to add (woohoo!) and just needs the appropriate application(s) --
	-- APPLICATION --

	CASE 
	WHEN cd.percentage > 0 THEN
		json_out := json_out & ('coupon_value' +> (cd.percentage*(contents->##'amount_discountable')))
		;
	WHEN cd.value > 0 THEN
		json_out := json_out & ('coupon_value' +> LEAST(cd.value, contents->##'amount_discountable'))
		;
	WHEN cd.free_shipping THEN -- for free shipping coupons, the "binable" feature is mandatory
		IF empty(contents->'shipping_price') THEN -- if shipping hasn't been added yet, then bin the coupon
			DELETE FROM carts.bins WHERE cart_id = cart_id_in AND coupon_code = coupon_code_in;
			INSERT INTO carts.bins (cart_id, coupon_code, deficits)
				VALUES (cart_id_in, coupon_code_in, 'requires_shipping' +> TRUE);
			json_out := json_out & return_code(1415) & ('coupon_binned' +> TRUE) & ('exit_point' +> 3); -- success (partially): coupon has been binned
			return;
		ELSE
			cd.value := contents->'shipping_price'; -- otherwise set the coupon value to the shipping price
		END IF;
	ELSE
	END CASE;

/*	IF NOT empty(cd.user_restrict) THEN -- the second someone touches (don't even have to actually use) a coupon, it becomes attached to them
		UPDATE coupons.dir set email_restrict = email_restrict || users.email
		FROM users.dir
		WHERE
			coupons.coupon_code = coupon_code_in
		AND users.user_id = user_id_in
		AND users.class IS NULL
		;
	END IF;*/


	/*
		TODO need to figure out tax free option
	*/


END CASE;

json_out := json_out & return_code(1);

END;
$$

LANGUAGE PLPGSQL
VOLATILE
;


COMMENT ON FUNCTION coupons.request(JSONB) is 
$$
-- Given a coupon_code, will verify if it is acceptable for the cart


VERSIONS:

INPUT PARAMETERS:
- coupon_code
- cart_id

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
CREATE OR REPLACE FUNCTION coupons.apix_update(
	IN	json_in		JSONB
,	OUT json_out 	JSONB
) AS
$$
DECLARE
BEGIN
json_out := '{}';

IF NOT exists((SELECT coupon_code from coupons.dir WHERE coupon_code = upper(json_in->>'coupon_code'))) THEN
	json_out := json_out & return_code(-1407);
	RETURN;
END IF;

UPDATE coupons.dir
SET
	coupon_type = COALESCE((json_in->#'coupon_type'), coupon_type)
,	coupon_parent = COALESCE((json_in->>'coupon_parent'), coupon_parent)
,	desc_retail = COALESCE((json_in->>'desc_retail'), desc_retail)
,	desc_internal = COALESCE((json_in->>'desc_internal'), desc_internal)
,	category = COALESCE((json_in->>'category'), category)
,	tags = COALESCE((json_in->'tags')::text[], tags)
,	status = COALESCE((json_in->#'status'), status)
,	expires = COALESCE((json_in->>'expires')::timestamp, expires)
,	req_register = COALESCE((json_in->?'req_register'), req_register)
,	req_admin = COALESCE((json_in->?'req_admin'), req_admin)
,	max_total_uses = COALESCE((json_in->#'max_total_uses'), max_total_uses)
,	max_user_uses = COALESCE((json_in->#'max_user_uses'), max_user_uses)
,	email_restrict = COALESCE((json_in->'email_restrict')::text[], email_restrict)
,	req_product_codes = COALESCE((json_in->'req_product_codes'), req_product_codes)
,	min_product_total = COALESCE((json_in->##'min_product_total'), min_product_total)
,	domestic_only = COALESCE((json_in->?'domestic_only'), domestic_only)
,	product_cat = COALESCE((json_in->>'product_cat'), product_cat)
,	class = COALESCE((json_in->>'class'), class)
,	priority = COALESCE((json_in->#'priority'), priority)
,	mixable = COALESCE((json_in->?'mixable'), mixable)
,	binable = COALESCE((json_in->?'binable'), binable)
,	percentage = COALESCE((json_in->##'percentage'), percentage)
,	value = COALESCE((json_in->##'value'), value)
,	user_restrict = COALESCE((json_in->?'user_restrict'), user_restrict)
,	free_shipping = COALESCE((json_in->?'free_shipping'), free_shipping)
,	tax_free = COALESCE((json_in->?'tax_free'), tax_free)
-- ,	edit_locked = (CASE WHEN json_in?'edit_locked' THEN json_in->#'edit_locked' ELSE edit_locked END)
,	email_notify = COALESCE((json_in->'email_notify')::TEXT[], email_notify)
,	favorite = COALESCE((json_in->?'favorite'), favorite)
,	cart_icon = COALESCE((json_in->>'cart_icon'), cart_icon)

WHERE
	coupon_code = upper(json_in->>'coupon_code')
;

json_out := json_out & return_code(1418);

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
CREATE OR REPLACE FUNCTION coupons.apix_delete(
	IN	json_in		JSONB
,	OUT json_out 	JSONB
) 

AS

$$
DECLARE
	
BEGIN
json_out := '{}';


IF NOT exists((SELECT coupon_code from coupons.dir WHERE coupon_code = upper(json_in->>'coupon_code'))) THEN
	json_out := json_out & return_code(-1407);
	RETURN;
END IF;


DELETE FROM coupons.dir
WHERE
	coupon_code = upper(json_in->>'coupon_code')
;

json_out := json_out & return_code(1);

END;
$$

LANGUAGE PLPGSQL
VOLATILE
;


COMMENT ON FUNCTION coupons.apix_delete(JSONB) is 
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
CREATE OR REPLACE FUNCTION coupons.apiv_details(
	IN	json_in		JSONB
,	OUT json_out 	JSONB
) AS
$$
DECLARE
	
BEGIN
json_out := '{}';


IF empty(json_in->>'coupon_code') THEN -- must specify a coupon_code
	json_out := json_out & return_code(-1407);
	RETURN;
END IF;

SELECT
	row_to_json(dir)
INTO json_out
FROM coupons.dir
WHERE
	coupon_code = upper(json_in->>'coupon_code')
;

IF empty(json_out) THEN -- coupon_code doesn't exist
	json_out := json_out & return_code(-1407);
	RETURN;
END IF;

END;
$$

LANGUAGE PLPGSQL
STABLE
;


COMMENT ON FUNCTION coupons.apiv_details(JSONB) is 
$$
-- this function is intended for the admin panel
-- it outputs all detailed properties of the given coupon_code

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
CREATE OR REPLACE FUNCTION coupons.apiv_notes(
	IN	json_in		JSONB
,	OUT json_out 	JSONB
) AS
$$
DECLARE
BEGIN

-- validation --
IF NOT json_in?'coupon_code' THEN json_out := json_out & return_code(-1420); RETURN; END IF;

SELECT
	('notes' +> coupon_notes) & ('coupon_code'+>coupon_code)
INTO json_out
FROM coupons.dir
WHERE
	coupon_code = upper(json_in->>'coupon_code')
;

IF empty(json_out->'coupon_code') THEN json_out := json_out & return_code(-1310); RETURN; END IF;

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
CREATE OR REPLACE FUNCTION coupons.apix_update_notes(
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

	UPDATE coupons.dir SET
		coupon_notes = prod_note_append(coupon_notes, next_note)
	WHERE
		coupon_code = json_in->>'coupon_code'
	;

	json_out := json_out & return_code(1);
	RETURN;

ELSEIF json_in?'delete_note' THEN
	del_index := json_in->#'delete_note';

	UPDATE coupons.dir SET
		coupon_notes = array_cat(coupon_notes[0:(del_index-1)],coupon_notes[(del_index+1):(array_upper(coupon_notes,1))])
	WHERE
		coupon_code = json_in->>'coupon_code'
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


CREATE OR REPLACE FUNCTION coupons.apiv_cert_uses(
	IN	json_in		JSONB
,	OUT json_out 	JSONB
) AS
$$
DECLARE
BEGIN

-- validation --
IF NOT json_in?'coupon_code' THEN json_out := json_out & return_code(-1420); RETURN; END IF;

select
		('uses'+> jsonb_agg(row_to_json(certificates)))
	&	('balance'+>sum(value))

into json_out
from coupons.certificates
where
	coupon_code = json_in->>'coupon_code'
;

IF empty(json_out->'uses') THEN json_out := return_code(-1421); RETURN; END IF;

return;

END;
$$
LANGUAGE PLPGSQL
STABLE
;


CREATE OR REPLACE FUNCTION coupons.cert_value_update(
	IN	json_in		JSONB
,	OUT json_out 	JSONB
) AS
$$
DECLARE
BEGIN

IF NOT json_in?&'{coupon_code, item_id, action}' THEN
	json_out := return_code(-1423); RETURN;
END IF;

CASE json_in->>'action'
	WHEN 'add_value' THEN
		
		INSERT INTO coupons.certificates (coupon_code, value, item_id, notes)
		SELECT -- pulling from order_items so that there is no discrepancy
			coupon_code
		,	price
		,	item_id
		,	'purchased in order ' || order_id
		from orders.items
		where
			item_id = json_in->#'item_id'
		and coupon_code = json_in->>'coupon_code'
		and price > 0
		and item_type = 31/*it*/
		returning 'certificate_id'+>certificate_id into json_out
		;
	
	WHEN 'use_value' THEN

		INSERT INTO coupons.certificates (coupon_code, value, item_id, notes)
		SELECT -- pulling from order_items so that there is no discrepancy
			coupon_code
		,	price
		,	item_id
		,	'redeemed in order ' || order_id
		from orders.items
		where
			item_id = json_in->#'item_id'
		and coupon_code = json_in->>'coupon_code'
		and price < 0
		and item_type = -52/*it*/
		returning 'certificate_id'+>certificate_id into json_out
		;

		-- TODO: add in daily integrity check to make sure that order items and cert redemptions match
ELSE
	json_out := return_code(-1422); RETURN;
END CASE;

IF empty(json_out) THEN json_out := return_code(-1421); RETURN; END IF;

json_out := return_code(1); RETURN;


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
CREATE OR REPLACE FUNCTION coupons.apiv_statuses(
	OUT json_out 	JSONB
) AS
$$
BEGIN
select 
	json_agg(row_to_json(statuses)) into json_out 
from coupons.statuses
;
END;
$$
LANGUAGE PLPGSQL
STABLE
;