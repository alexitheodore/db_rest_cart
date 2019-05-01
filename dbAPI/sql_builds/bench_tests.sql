-- BEGIN;

DO $$

DECLARE
	json_data JSONB;

BEGIN

json_data := ('email'+>(random_string_from_pattern('@@@@@@@@@@@@@@')||'@gmail.com'))&('name_first'+>'Bench')&('name_last'+>'Tester');

json_data := users.apix_create(json_data);

json_data := carts.apix_create(json_data#'user_id');

json_data := carts.apix_add(
		json_data#'cart_id'
	&	('product_code'+>'product_1')
	&	('item_type'+>10/*it*/)
	);

END;
$$
LANGUAGE PLPGSQL
;

-- ROLLBACK;