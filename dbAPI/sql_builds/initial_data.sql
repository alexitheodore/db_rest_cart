--> Bookmark return_codes
DELETE FROM return_codes;
INSERT INTO return_codes 
(return_code, desc_detail)
VALUES
-- GLOBAL
(-1000,	'General Uncaught API Function SQL error'),
(00001,	'Generic, run-of-the-mill success'),
(-1002, 'you have supplied some bad or unaccepted parameters'),
(-1003, 'your API request contains invalid JSON'),
(-1004, 'you requested an undefined endpoint'),
(-1007, 'Sorry, you do not have permission for that.'),

-- Users
(01100,	'Users'),
(-1100,	'Must supply email, first name and last name.'),
(-1101,	'Invalid email address.'),
(-1102,	'That user already exists.'),
(-1103,	'Must supply a user_id.'),
(-1104,	'Invalid user class.'),

(00000,	'anonymous')
;


--> Bookmark settings
DELETE FROM SETTINGS;
INSERT INTO SETTINGS VALUES
--(setting_name, value, value_type, description)
	('users.classes', '{client,admin}', 'text[]', '')

,	('dbAPI.snapshots', 'TRUE', 'boolean', 'Determines whether update snapshots are enabled.')
,	('dbAPI.environments', '{live,dev}', 'text[]', 'List of named development environments.')
,	('dbAPI.environment', current_setting('dbAPI.environment'), 'text', 'The current environment type.')

,	('testing.dummy', '', 'text', '')

;


--> Bookmark item_types
DELETE FROM carts.item_types;
INSERT INTO carts.item_types VALUES
-- ** SEE DOCUMENTATION FOR RULES ON CONVENTIONS FOR ADDING/CHANGING ITEM_TYPES **

-- VALUE ITEMS
		-- PRODUCT ITEMS -- 10's through 30's 
			-- These are not different products, but different classes of products requiring different cart treatment. 
			-- For example physical products which have shipping, digital products which have downloads, services which are neither shipped nor downloaded, gift certificates, customizable products, etc.
	(10/*it*/,	'physical_product',	NULL,	TRUE),
	(20/*it*/,	'digital_product',	NULL,	TRUE),
			-- SERVICE ITEMS -- 40's and up
	(40/*it*/,	'service_type_1',	NULL, 	TRUE),
		-- DISCOUNTS --
	(50/*it*/,	'coupon',		NULL, 	TRUE),
		-- NON-VALUE ITEMS --
	(51/*it*/,	'shipping',		NULL, 	TRUE),
	(52/*it*/,	'tax',			NULL, 	TRUE),
	(53/*it*/,	'return',		NULL, 	TRUE),
		-- PAYMENT ITEMS --
	(-51/*it*/,	'cash',			NULL, 	TRUE),
	(-52/*it*/,	'cert_redem',	NULL, 	TRUE),
	(-53/*it*/,	'refund',		NULL, 	TRUE),
	(-54/*it*/,	'invoice',		NULL, 	TRUE)
;


--> Bookmark product_codes
DELETE FROM products.dir;
INSERT INTO products.dir	
	(product_code,		item_type,	name_retail,			name_common,			price,		properties,									is_active)
VALUES	
	('product_1',		10/*it*/,	'Product 1',  			'Product 1',  			10.00,		'{}', 										TRUE)
,	('product_2',		10/*it*/,	'Product 2',  			'Product 2',  			20.00,		'{}', 										TRUE)
	
,	('RUSH-FEE',		41/*it*/,	'Rush Fee', 			'Rush Fee', 			5.00,		'{}', 										TRUE)
	
,	('GIFT-CARD-PRINT',	31/*it*/,	'Mailed Gift Card', 	'Mailed Gift Card', 	NULL,		'{}', 										TRUE)
,	('GIFT-CERT-EMAIL',	40/*it*/,	'Emailed Gift Card',	'Emailed Gift Card', 	NULL,		'{}', 										TRUE)
;

