-- GRANT ALL PRIVILEGES ON DATABASE {database} TO the_architect;
REASSIGN OWNED BY the_architect TO postgres;
DROP SCHEMA IF EXISTS public CASCADE;
DROP SCHEMA IF EXISTS global CASCADE;
CREATE SCHEMA global AUTHORIZATION the_architect;

SET search_path TO "global";
ALTER DATABASE {database} SET search_path TO "global";

SET ROLE postgres;

CREATE EXTENSION IF NOT EXISTS pg_stat_statements;
CREATE EXTENSION IF NOT EXISTS ltree;
-- CREATE EXTENSION IF NOT EXISTS plpythonu;
CREATE EXTENSION IF NOT EXISTS dblink;
-- CREATE EXTENSION IF NOT EXISTS pgcrypto;
CREATE EXTENSION IF NOT EXISTS citext;

/*
This is a home-built json extension library. Its something that was started from scratch and needs plenty of work. Some functions are experiments; some are placeholders.
*/

/*
*/
CREATE OR REPLACE FUNCTION jsonb_build(
	IN 	key_in 		text
,	IN 	value_in 	anyelement
,	OUT	json_out	JSONB
) 
AS
$$
BEGIN
    json_out := jsonb_build_object(key_in,value_in);
END;
$$
LANGUAGE PLPGSQL
IMMUTABLE
;
COMMENT ON FUNCTION jsonb_build(TEXT, ANYELEMENT) IS 'Builds JSONB from TEXT and ANYELEMENT. This function is intended to be used by custom operator(s).';


CREATE OR REPLACE FUNCTION jsonb_build(
	IN 	key_in 		text
,	IN 	value_in 	text
,	OUT	json_out	JSONB
)
AS
$$
BEGIN
    json_out := jsonb_build_object(key_in,value_in);
END;
$$
LANGUAGE PLPGSQL
IMMUTABLE
;
COMMENT ON FUNCTION jsonb_build(TEXT, TEXT) IS 'Builds JSONB from TEXT and TEXT. This function is intended to be used by custom operator(s).';


CREATE OR REPLACE FUNCTION jsonb_build(
	IN 	key_in 		TEXT
,	IN 	value_in 	JSONB
,	OUT	json_out	JSONB
)
AS
$$
BEGIN
    json_out := jsonb_build_object(key_in,value_in);
END;
$$
LANGUAGE PLPGSQL
IMMUTABLE
;
COMMENT ON FUNCTION jsonb_build(TEXT, JSONB) IS 'Builds JSONB from TEXT and JSONB. This function is intended to be used by custom operator(s).';


DROP OPERATOR IF EXISTS +> (text, text);
CREATE OPERATOR +> (
    PROCEDURE = jsonb_build,
    LEFTARG = text,
    RIGHTARG = text,
    COMMUTATOR = +>
)
;
COMMENT ON OPERATOR +> (TEXT, TEXT) IS 'Builds a JSONB object from the left key and right text.';


DROP OPERATOR IF EXISTS +> (text, anyelement);
CREATE OPERATOR +> (
    PROCEDURE = jsonb_build,
    LEFTARG = text,
    RIGHTARG = anyelement,
    COMMUTATOR = +>
)
;
COMMENT ON OPERATOR +> (TEXT, ANYELEMENT) IS 'Builds a JSONB object from the left key and right value.';

DROP OPERATOR IF EXISTS +> (text, jsonb);
CREATE OPERATOR +> (
    PROCEDURE = jsonb_build,
    LEFTARG = text,
    RIGHTARG = jsonb,
    COMMUTATOR = +>
)
;
COMMENT ON OPERATOR +> (TEXT, JSONB) IS 'Builds a JSONB object from the left key and right JSONB.';


/*
*/

CREATE OR REPLACE FUNCTION jsonb_as_numeric(
	IN 	json_in 	JSONB
,	IN 	key_in 		TEXT
,	OUT	json_out	NUMERIC
)
AS
$$
BEGIN
    json_out := (json_in->>key_in)::NUMERIC;
END;
$$
LANGUAGE PLPGSQL
IMMUTABLE
;
COMMENT ON FUNCTION jsonb_as_numeric(JSONB, TEXT) IS 'Gets value from object cast as NUMERIC. This function is intended to be used by custom operator(s).';

DROP OPERATOR IF EXISTS ->## (jsonb, text);
CREATE OPERATOR ->## (
    PROCEDURE = jsonb_as_numeric,
    LEFTARG = jsonb,
    RIGHTARG = text,
    COMMUTATOR = ->##
)
;
COMMENT ON OPERATOR ->## (JSONB, TEXT) IS 'Returns the numeric value from the left object per the key given by right operator.';


CREATE OR REPLACE FUNCTION jsonb_as_int(
	IN 	json_in 	JSONB
,	IN 	key_in 		TEXT
,	OUT	json_out	INT
)
AS
$$
BEGIN
    json_out := (json_in->>key_in)::INT;
END;
$$
LANGUAGE PLPGSQL
IMMUTABLE
;
COMMENT ON FUNCTION jsonb_as_int(JSONB, TEXT) IS 'Gets value from object cast as INT. This function is intended to be used by custom operator(s).';


DROP OPERATOR IF EXISTS -># (jsonb, text);
CREATE OPERATOR -># (
    PROCEDURE = jsonb_as_int,
    LEFTARG = jsonb,
    RIGHTARG = text,
    COMMUTATOR = ->#
)
;
COMMENT ON OPERATOR -># (JSONB, TEXT) IS 'Returns the integer value from the left object per the key given by right operator.';

CREATE OR REPLACE FUNCTION jsonb_as_text_array(
	IN 	json_in 	JSONB
,	IN 	key_in 		TEXT
,	OUT	text_out	TEXT[]
)
AS
$$
BEGIN
    text_out := (json_in->key_in)::TEXT[];
END;
$$
LANGUAGE PLPGSQL
IMMUTABLE
;
COMMENT ON FUNCTION jsonb_as_text_array(JSONB, TEXT) IS 'Gets value from object cast as TEXT[]. This function is intended to be used by custom operator(s).';


DROP OPERATOR IF EXISTS ->>& (jsonb, text);
CREATE OPERATOR ->>& (
    PROCEDURE = jsonb_as_text_array,
    LEFTARG = jsonb,
    RIGHTARG = text
)
;
COMMENT ON OPERATOR ->>& (JSONB, TEXT) IS 'Returns the text array from the left object per the key given by right operator.';


CREATE OR REPLACE FUNCTION jsonb_as_boolean(
	IN 	json_in 	JSONB
,	IN 	key_in 		TEXT
,	OUT	json_out	BOOLEAN
)
AS
$$
BEGIN
    json_out := (json_in->>key_in)::BOOLEAN;
END;
$$
LANGUAGE PLPGSQL
IMMUTABLE
;
COMMENT ON FUNCTION jsonb_as_boolean(JSONB, TEXT) IS 'Gets value from object cast as BOOLEAN. This function is intended to be used by custom operator(s).';


DROP OPERATOR IF EXISTS ->? (jsonb, text);
CREATE OPERATOR ->? (
    PROCEDURE = jsonb_as_boolean,
    LEFTARG = jsonb,
    RIGHTARG = text,
    COMMUTATOR = ->?
)
;
COMMENT ON OPERATOR ->? (JSONB, TEXT) IS 'Returns the boolean value from the left object per the key given by right operator.';

CREATE OR REPLACE FUNCTION jsonb_as_date(
	IN 	json_in 	JSONB
,	IN 	key_in 		TEXT
,	OUT	json_out	DATE
)
AS
$$
BEGIN
    json_out := (json_in->>key_in)::date;
END;
$$
LANGUAGE PLPGSQL
IMMUTABLE
;
COMMENT ON FUNCTION jsonb_as_date(JSONB, TEXT) IS 'Gets value from object cast as DATE. This function is intended to be used by custom operator(s).';


DROP OPERATOR IF EXISTS ->@ (jsonb, text);
CREATE OPERATOR ->@ (
    PROCEDURE = jsonb_as_date,
    LEFTARG = jsonb,
    RIGHTARG = text,
    COMMUTATOR = ->@
)
;
COMMENT ON OPERATOR ->@ (JSONB, TEXT) IS 'Returns the date from the left object per the key given by right operator.';


/*The native pgs function for json_agg() does not work in a practical way, so this I made this one to.*/
CREATE OR REPLACE FUNCTION json_agg_statef (IN json_cur_in JSONB, IN json_next_in JSONB, OUT json_cur_out JSONB) AS
$$
BEGIN
    json_cur_out := json_cur_in & json_next_in;
END;
$$
LANGUAGE PLPGSQL
IMMUTABLE
;
COMMENT ON FUNCTION json_agg_statef(JSONB, JSONB) IS 'Replaces the factory-shipped version of this function with one that ignores NULLs and empty objects.';


DROP AGGREGATE IF EXISTS json_agg (JSONB);
CREATE AGGREGATE json_agg (JSONB)
(
    SFUNC = json_agg_statef,
    STYPE = JSONB
)
;
COMMENT ON AGGREGATE json_agg(JSONB) IS 'Replaces the factory-shipped version of this function with one that ignores NULLs and empty objects.';


/*
*/

CREATE OR REPLACE FUNCTION json_agg_array_statef (IN json_cur_in JSONB, IN json_next_in JSONB, OUT json_cur_out JSONB) AS

$$

BEGIN

CASE 
WHEN json_cur_in IS NULL THEN
    json_cur_out := json_next_in;
ELSE
    json_cur_out := json_build_array(json_next_in, json_cur_in);
END CASE;

END;

$$
LANGUAGE PLPGSQL
IMMUTABLE
;
COMMENT ON FUNCTION json_agg_array_statef(JSONB, JSONB) IS 'Safely aggregates json using the standard json_build_array() function but ignoring NULL objects.';

DROP AGGREGATE IF EXISTS json_agg_array (JSONB);
CREATE AGGREGATE json_agg_array (JSONB)
(
    SFUNC = json_agg_array_statef,
    STYPE = JSONB
)
;
COMMENT ON FUNCTION json_agg_array(JSONB) IS 'Safely aggregates json using the standard json_build_array() function but ignoring NULL objects.';


/*
*/

CREATE OR REPLACE FUNCTION json_select_keys_if_exist( -- selects multiple keys
    IN  json_in     JSONB
,   IN  key_list    TEXT[]
,   OUT json_out    JSONB
) 

AS

$$
DECLARE
    key_name TEXT;
    
BEGIN

FOREACH key_name IN ARRAY key_list LOOP

    IF NOT empty(json_in->key_name) THEN 
        json_out := json_out & (json_in # key_name);
    END IF;

END LOOP;

END;
$$
LANGUAGE PLPGSQL
IMMUTABLE
;
COMMENT ON FUNCTION json_select_keys_if_exist(JSONB, TEXT[]) IS 'Returns the object for the given key name(s) text array from the given JSONB array. This function is intended to be used by custom operator(s).';
-- separate function than "json_select_key_if_exists()" because combining them requires a polymorphic input and that requires usage to explicitly cast variables which is a pain


DROP OPERATOR IF EXISTS #& (JSONB, TEXT[]);
CREATE OPERATOR #& (
    PROCEDURE = json_select_keys_if_exist
,   LEFTARG = JSONB
,   RIGHTARG = TEXT[]
)
;
COMMENT ON OPERATOR #& (JSONB, TEXT[]) IS 'Returns the top-level key-value pairs in left object which are mentioned in the right key-name text array.';

/*
*/

CREATE OR REPLACE FUNCTION json_select_key_if_exists( -- selects one key
    IN  json_in     JSONB
,   IN  json_key    TEXT
,   OUT json_out    JSONB
) 

AS

$$
DECLARE
    
BEGIN

CASE
    WHEN json_in ? json_key
    THEN
        json_out := (json_key +> (json_in #> array[json_key]));
        RETURN;
    ELSE
        json_out := NULL;
        RETURN;
END CASE;
    

END;
$$

LANGUAGE PLPGSQL
IMMUTABLE
;
COMMENT ON FUNCTION json_select_key_if_exists(JSONB, TEXT) IS 'Returns the object for the given key name from the given JSONB array. This function is intended to be used by custom operator(s).';


DROP OPERATOR IF EXISTS # (JSONB, text);
CREATE OPERATOR # (
    PROCEDURE = json_select_key_if_exists
,   LEFTARG = JSONB
,   RIGHTARG = text
)
;
COMMENT ON OPERATOR # (JSONB, TEXT) IS 'Returns the top-level key-value pair in left object of the right key-name text.';



/*
*/

-- This function differs from that of the pgs native || functionality in that if either constitiutent is NULL, then the result is the other constituent. This makes it easy to append without needing to know whether the appended object is NULL or not.

CREATE OR REPLACE FUNCTION json_append(
    IN  json_a      JSONB
,   IN  json_b      JSONB
,   OUT json_out    JSONB
) 

AS

$$

BEGIN

CASE 
    WHEN json_a IS NULL
        THEN json_out := json_b;

    WHEN json_b IS NULL
        THEN json_out := json_a;

    ELSE
        json_out := json_a || json_b;
END CASE;

END;
$$

LANGUAGE PLPGSQL
IMMUTABLE
;
COMMENT ON FUNCTION json_append(JSONB, JSONB) IS 'Replaces the factory-shipped version of this function with one that ignores NULLs and empty objects.';


DROP OPERATOR IF EXISTS & (JSONB, JSONB);
CREATE OPERATOR & (
    PROCEDURE = json_append
,   LEFTARG = JSONB
,   RIGHTARG = JSONB
)
;
COMMENT ON OPERATOR & (JSONB, JSONB) IS 'Appends the left JSONB object to the right JSONB object.';



/*
*/
CREATE OR REPLACE FUNCTION json_setrsert(IN JSONB, IN TEXT[], IN ANYELEMENT, IN BOOLEAN DEFAULT TRUE) RETURNS JSONB AS
$$

BEGIN

IF ($1 #> $2) IS NULL
	THEN return jsonb_set(COALESCE($1,'{}'), $2[:(cardinality($2)-1)], to_jsonb(array[$3]), TRUE);
	ELSE return jsonb_insert($1, $2, to_jsonb($3), $4);
END IF;

END;
$$

LANGUAGE PLPGSQL
IMMUTABLE
;
COMMENT ON FUNCTION json_setrsert(JSONB, TEXT[], ANYELEMENT, BOOLEAN) IS 'Inserts a value into the array of the first argument at the destination designated by the second argument. It creates the array if it does not exist already. This is a combination of the native functions jsonb_set and jsonb_insert';


/*
*/

CREATE OR REPLACE FUNCTION jsonb_table_insert(
    IN  target_schema   TEXT
,   IN  target_table    TEXT
,   IN  update_values	JSONB
,	IN 	flags 			JSONB
,	OUT	json_out		JSONB
)
AS
$$
DECLARE
    exec_string TEXT;
BEGIN

with
	table_props as
(
select 
	column_name::text
,   udt_name::text 
,   data_type::text
from information_schema.columns 
where
	table_name = target_table
and table_schema = target_schema
)

,   insert_data as
(
select 
	key as column_name
,   value as column_value
from jsonb_each_text(update_values)
)

,   build as
(
select
	string_agg(column_name, ', ') as column_name_list
,   string_agg(quote_literal(column_value) || case_switch(data_type = 'ARRAY','::JSONB'::text,'') || '::' || udt_name, ', ') as column_value_list
from insert_data
join table_props using (column_name)    -- only the columns supplied in the JSON object will be pulled from the destination table
)

SELECT
		'INSERT INTO' & target_schema || '.' || target_table 
	&	'(' || column_name_list || ')'
	&	'VALUES (' || column_value_list ||')'
	&	'RETURNING row_to_json(' || target_table || ')'
	into exec_string
FROM build
;

IF flags->?'debug' THEN 
	json_out := 'sql'+> exec_string; RETURN;
END IF;

EXECUTE exec_string into json_out;

END;
$$
LANGUAGE PLPGSQL
VOLATILE
;

COMMENT ON FUNCTION jsonb_table_insert(TEXT, TEXT, JSONB, JSONB) IS 'Emulates a single-row table insert query using json where the supplied object keys correspond to the destination table column names. The first argument is the destination schema, the second argument is the destination table, the third argument is the json with values for columns (invalid column names are ignored) and the fourth argument is for diagnostic flags. The response is the newly added row.';



/*
*/

CREATE OR REPLACE FUNCTION jsonb_table_update(
    IN  target_schema   TEXT
,   IN  target_table    TEXT
,	IN	target_row		JSONB
,   IN  update_values	JSONB
,	IN 	flags 			JSONB
,	OUT	json_out		JSONB
)
AS
$$
DECLARE
    exec_string TEXT;
BEGIN

with
	table_props as
(
select 
	column_name::text
,   udt_name::text 
,   data_type::text
from information_schema.columns 
where
	table_name = target_table
and table_schema = target_schema
)

,   insert_data as
(
select 
	key as column_name
,   value as column_value
from jsonb_each_text(update_values)
)

,   build as
(
select
	string_agg(
        column_name & '=' & COALESCE(
            quote_literal(column_value) || case_switch(data_type = 'ARRAY','::JSONB'::text,'') || '::' || udt_name
        ,   'NULL' -- for when when the value is actually NULL
        )
    ,   ', '
    ) as data_list
from insert_data
join table_props using (column_name)    -- only the columns supplied in the JSON object will be pulled from the destination table
)
,	wheres as
(
select
	string_agg(key & '=' & quote_literal(value), ' and ') as wheres
from jsonb_each_text(target_row)
)

SELECT
	'UPDATE' & target_schema || '.' || target_table & 'SET' & data_list & 'WHERE' & wheres
	into exec_string
FROM build, wheres
;

IF flags->?'debug' THEN 
	json_out := 'sql'+> exec_string; return;
END IF;

EXECUTE exec_string;

END;
$$

LANGUAGE PLPGSQL
VOLATILE
;

COMMENT ON FUNCTION jsonb_table_update(TEXT, TEXT, JSONB, JSONB, JSONB) IS 'Emulates a single-row table update query using json where the supplied object keys correspond to the destination table column names. The first argument is the destination schema, the second argument is the destination table, the third argument is the json object to specify the where constraints, the fourth argument is the json object with values for updated columns (invalid column names are ignored) and the fith argument is for diagnostic flags. The response is the newly added row.';

/*
*/

CREATE OR REPLACE FUNCTION "text"(IN json_in JSONB) returns TEXT as
$$
SELECT (json_in#>>'{}')::TEXT;
$$
LANGUAGE sql
IMMUTABLE;
COMMENT ON FUNCTION "text"(JSONB) IS 'System function used for casting';
CREATE CAST (JSONB AS TEXT) WITH FUNCTION "text"(JSONB) AS ASSIGNMENT;


CREATE OR REPLACE FUNCTION text_array(IN json_array JSONB) returns TEXT[] as
$$
SELECT ARRAY(SELECT jsonb_array_elements_text(json_array));
$$
LANGUAGE sql
IMMUTABLE;
COMMENT ON FUNCTION text_array(JSONB) IS 'System function used for casting';
CREATE CAST (JSONB AS text[]) WITH FUNCTION text_array(JSONB) AS ASSIGNMENT;


CREATE OR REPLACE FUNCTION "int"(IN json_in JSONB) returns INT as
$$
SELECT (json_in#>>'{}')::INT;
$$
LANGUAGE sql
IMMUTABLE;
COMMENT ON FUNCTION "int"(JSONB) IS 'System function used for casting';
CREATE CAST (JSONB AS int) WITH FUNCTION "int"(JSONB) AS ASSIGNMENT;


CREATE OR REPLACE FUNCTION int_array(IN json_array JSONB) returns INT[] as
$$
SELECT ARRAY(SELECT jsonb_array_elements_text(json_array)::int);
$$
LANGUAGE sql
IMMUTABLE;
COMMENT ON FUNCTION int_array(JSONB) IS 'System function used for casting';
CREATE CAST (JSONB AS int[]) WITH FUNCTION int_array(JSONB) AS ASSIGNMENT;


CREATE OR REPLACE FUNCTION int_array(IN json_in JSONB, IN text_in TEXT) returns INT[] as
$$
SELECT (json_in->text_in)::int[];
$$
LANGUAGE sql
IMMUTABLE;
COMMENT ON FUNCTION int_array(JSONB, TEXT) IS 'Gets value from object cast as INT[]. This function is intended to be used by custom operator(s).';

CREATE OPERATOR ->#& (
    PROCEDURE = "int_array",
    LEFTARG = JSONB,
    RIGHTARG = TEXT,
    COMMUTATOR = ->#&
)
;
COMMENT ON OPERATOR ->#& (JSONB, TEXT) IS 'Returns the integer array value from the left object per the key given by right operator.';


CREATE OR REPLACE FUNCTION "numeric"(IN json_in JSONB) returns NUMERIC as
$$
SELECT (json_in#>>'{}')::NUMERIC;
$$
LANGUAGE sql
IMMUTABLE;
COMMENT ON FUNCTION "numeric"(JSONB) IS 'System function used for casting';
CREATE CAST (JSONB AS NUMERIC) WITH FUNCTION "numeric"(JSONB) AS ASSIGNMENT;

/*
*/

DROP OPERATOR IF EXISTS ** (jsonb, none);
CREATE OPERATOR ** (
    PROCEDURE = jsonb_pretty
,   LEFTARG = jsonb
)
;
COMMENT ON OPERATOR ** (JSONB, NONE) IS 'Returns the left object made pretty.';


/*
*/

CREATE OR REPLACE FUNCTION jsonb_array_append(
    IN  json_base   JSONB
,   IN  json_wedge  JSONB
,   OUT json_out    JSONB
) AS
$$
DECLARE
    wedge TEXT;
    comma TEXT := ',';
BEGIN

CASE
    WHEN jsonb_typeof(json_wedge) = 'string'
        THEN wedge := quote_ident(json_wedge::text);
    WHEN json_wedge IS NULL OR json_wedge::text = '{}' OR json_wedge::text = '[]' 
    THEN
        json_out := json_base;
        return;
    WHEN json_base IS NULL OR json_base::text = '{}' OR json_base::text = '[]' 
    THEN
        comma := '';
        wedge := json_wedge::text;
    ELSE
        wedge := json_wedge::text;
END CASE;

CASE jsonb_typeof(json_base)::text
    WHEN 'array' THEN
        json_out := (left(json_base::text,-1)+comma+wedge+']')::jsonb
        return;
    WHEN 'object' THEN
        IF jsonb_typeof(json_wedge) = 'array' 
            THEN RAISE EXCEPTION 'Cannot append object and array types';
            RETURN;
        END IF;
        json_out := (left(json_base::text,-1)+comma+right(json_wedge::text,-1))::jsonb
        return;
    ELSE
        RAISE EXCEPTION 'Unusual case "%" not found.', jsonb_typeof(json_base)::text;
        RETURN;
END CASE;

END;
$$

LANGUAGE PLPGSQL
IMMUTABLE
;
COMMENT ON FUNCTION jsonb_array_append(JSONB, JSONB) IS 'Inteligently appends the second argument to the first depending on what datatype the second is.';


/*
*/
CREATE OR REPLACE FUNCTION jsonb_accepted_keys(
    IN  value_in    JSONB
,   IN  keys        TEXT[]
)
RETURNS BOOLEAN AS
$$
BEGIN

RETURN
(
SELECT 
    count(*)= 0  
FROM jsonb_object_keys(value_in) 
WHERE 
    NOT jsonb_object_keys <@ keys
)
;

END;
$$

LANGUAGE PLPGSQL
IMMUTABLE
;
COMMENT ON FUNCTION jsonb_accepted_keys(JSONB, TEXT[]) IS 'Returns false if any of the keys provided in the first JSON argument are not in the second TEXT[] argument (in otherwords: unaccepted).';

DROP OPERATOR IF EXISTS ?&! (JSONB, TEXT[]);
CREATE OPERATOR ?&! (
    PROCEDURE = jsonb_accepted_keys,
    LEFTARG = JSONB,
    RIGHTARG = TEXT[]
)
;
COMMENT ON OPERATOR ?&! (JSONB, TEXT[]) IS 'Returns false if any of the keys provided in the left object are not specified in the left text array.';



/*
*/
CREATE OR REPLACE FUNCTION jsonb_keys_coexist(
    IN  value_in    JSONB
,   IN  keys        TEXT[]
)
RETURNS BOOLEAN AS
$$
BEGIN

RETURN
(
SELECT 
    count(*) > 1 
FROM jsonb_object_keys(value_in) 
WHERE 
    jsonb_object_keys <@ keys
)
;

END;
$$

LANGUAGE PLPGSQL
IMMUTABLE
;
COMMENT ON FUNCTION jsonb_keys_coexist(JSONB, TEXT[]) IS 'Returns true if *more than one* of the specified keys in the second argument exists at the top level of the first argument.';

/*
*/

CREATE OR REPLACE FUNCTION delete_keys(
    IN  value_in    JSONB
,   IN  keys        TEXT[]
,   OUT value_out   JSONB
) AS
$$
DECLARE
    key  TEXT;
BEGIN

FOREACH key IN ARRAY keys LOOP
    value_in := value_in-key;
END LOOP;

    value_out := value_in;

END;
$$

LANGUAGE PLPGSQL
IMMUTABLE
;
COMMENT ON FUNCTION delete_keys(JSONB, TEXT[]) IS 'Returns the first JSON argument with the keys specified in the second TEXT[] argument removed.';


DROP OPERATOR IF EXISTS -& (JSONB, TEXT[]);
CREATE OPERATOR -& (
    PROCEDURE = delete_keys,
    LEFTARG = JSONB,
    RIGHTARG = TEXT[]
)
;
COMMENT ON OPERATOR -& (JSONB, TEXT[]) IS 'Returns the left object with the keys in the right text array removed from it.';



/*
*/

CREATE OR REPLACE FUNCTION cast_to_jsonb(IN TEXT) RETURNS JSONB AS
$$
BEGIN
    return $1::jsonb;
END;
$$
LANGUAGE PLPGSQL
IMMUTABLE
;
COMMENT ON FUNCTION cast_to_jsonb(TEXT) IS 'System function used for casting';

CREATE OPERATOR @ (
    PROCEDURE = cast_to_jsonb,
    RIGHTARG = TEXT
)
;
COMMENT ON OPERATOR @ (NONE, TEXT) IS 'Casts the right text into JSONB.';



/*
*/


CREATE TYPE key_value AS
(
    key     TEXT
,   value   TEXT
)
;


CREATE OR REPLACE FUNCTION jsonb_each_casted(
    IN json_in JSONB
) RETURNS SETOF key_value AS
$$
DECLARE
BEGIN

RETURN QUERY
select
    key
,   CASE
        WHEN jsonb_typeof(jbe.value) = 'array' THEN replace(replace(jbet.value::text,'[','{'),']','}')
        WHEN jsonb_typeof(jbe.value) = 'string' THEN jbet.value
        WHEN jsonb_typeof(jbe.value) = 'null' THEN NULL
        ELSE jbet.value
    END
from jsonb_each(json_in) jbe
join jsonb_each_text(json_in) jbet using (key)
;

END;
$$
LANGUAGE PLPGSQL
IMMUTABLE
;
COMMENT ON FUNCTION jsonb_each_casted(JSONB) IS 'System function used for casting';


/*
*/


CREATE OR REPLACE FUNCTION jsonb_delta(
    IN json_left JSONB
,   IN json_right JSONB
,   OUT json_out JSONB
) AS
$$
BEGIN

with
    base as
(
select 
    key
,   case when a.value != b.value then ('left'+>b.value)&('right'+>a.value) ELSE NULL END as changes
from jsonb_each(json_left) a 
join jsonb_each(json_right) b using (key)
)
select
    json_agg(key+>changes)
into json_out
from base
where
    changes IS NOT NULL
;

END;
$$
LANGUAGE PLPGSQL
IMMUTABLE
;
COMMENT ON FUNCTION jsonb_delta(JSONB, JSONB) IS 'Computes a diff between the first and second JSONB arguments.';


/*
*/

CREATE OR REPLACE FUNCTION is_numeric(IN JSONB) RETURNS BOOLEAN AS
$$
BEGIN
    return jsonb_typeof($1) = 'number';
END;
$$
LANGUAGE PLPGSQL
IMMUTABLE
;
COMMENT ON FUNCTION is_numeric(JSONB) IS 'Determines if the provided object is numeric. This function is intended to be used by custom operator(s).';

CREATE OPERATOR ?# (
    PROCEDURE = is_numeric,
    LEFTARG = JSONB
)
;
COMMENT ON OPERATOR ?# (JSONB, NONE) IS 'Returns TRUE if the left object is numeric.';

--- // --- 						--- // --- 
--- \\ ---   COMMON FUNCTIONS 	--- \\ ---
--- // --- 						--- // --- 

-- !!! THIS SCRIPT IS AN INITIALIZATION SCRIPT AND WILL BUILD FROM SCRATCH !!! --


CREATE DOMAIN DOLLAR AS numeric(10,2);
CREATE DOMAIN DOLLARS AS numeric(10,2)[];

/*
_|_|_|_|
_|
_|_|_|
_|
_|
*/
CREATE OR REPLACE FUNCTION empty(
	IN 	datum	ANYELEMENT
) 
RETURNS BOOLEAN AS
$$
BEGIN

CASE
	WHEN datum::text IS NULL THEN return TRUE;
	WHEN datum::text in ('', '{}', 'null', '[]') THEN return TRUE;
	ELSE return FALSE;
END CASE;

END;
$$

LANGUAGE PLPGSQL
IMMUTABLE
;


/*
_|_|_|_|
_|
_|_|_|
_|
_|
*/
CREATE OR REPLACE FUNCTION case_false(
	IN anyelement
,	IN anyelement
,	IN anyelement
) 
RETURNS anyelement AS
$$
select case when $1 = $2 then $3 else $1 end;
$$
LANGUAGE SQL
IMMUTABLE
;


/*
_|_|_|_|
_|
_|_|_|
_|
_|
*/
CREATE OR REPLACE FUNCTION case_true(
	IN anyelement
,	IN anyelement
,	IN anyelement
) 
RETURNS anyelement AS
$$
select case when $1 = $2 then $1 else $3 end;
$$
LANGUAGE SQL
IMMUTABLE
;


/*
_|_|_|_|
_|
_|_|_|
_|
_|
*/
CREATE OR REPLACE FUNCTION case_switch(
	IN boolean
,	IN anyelement
,	IN anyelement
) 
RETURNS anyelement AS
$$
select case when $1 then $2 else $3 end;
$$
LANGUAGE SQL
IMMUTABLE
;


/*
_|_|_|_|
_|
_|_|_|
_|
_|
*/
CREATE OR REPLACE FUNCTION case_empty(
	IN anyelement
,	IN anyelement
) 
RETURNS anyelement AS
$$
select case when empty($1) then $2 else $1 end;
$$
LANGUAGE SQL
IMMUTABLE
;



/*
_|_|_|_|
_|
_|_|_|
_|
_|
*/
CREATE OR REPLACE FUNCTION random_alpha_numeric() 

RETURNS TEXT AS

$$
select UPPER((string_to_array('abcdefghjklmnpqrstuvwxyz23456789',NULL))[(random()*32)::int])
$$

LANGUAGE SQL
STABLE -- this must be "stable" or "volatile" in order for the function to be run uniquely for each loop w/out caching (resulting in the same answer every time)
;

/*
_|_|_|_|
_|
_|_|_|
_|
_|
*/
CREATE OR REPLACE FUNCTION random_letter() 

RETURNS TEXT AS

$$
select chr(floor(26 * random())::int + 65);
$$

LANGUAGE SQL
STABLE -- this must be "stable" or "volatile" in order for the function to be run uniquely for each loop w/out caching (resulting in the same answer every time)
;

/*
_|_|_|_|
_|
_|_|_|
_|
_|
*/
CREATE OR REPLACE FUNCTION random_digit() 

RETURNS INT AS

$$
select floor(9 * random())::int
$$

LANGUAGE SQL
STABLE -- this must be "stable" or "volatile" in order for the function to be run uniquely for each loop w/out caching (resulting in the same answer every time)
;

/*
_|_|_|_|
_|
_|_|_|
_|
_|
*/
CREATE OR REPLACE FUNCTION random_string_from_pattern(in pattern_string TEXT) 
RETURNS TEXT AS
$$

DECLARE

	pattern TEXT[];
	pattern_n text;
	n int := 1 ;
	string_out text;
	new_pattern TEXT[];
	probe text := '';


BEGIN

pattern := string_to_array(pattern_string, NULL);
new_pattern := pattern;

FOREACH pattern_n in ARRAY pattern LOOP
	case
		when pattern_n = '@' then 
			new_pattern[n] := random_letter();
		when pattern_n = '#' then 
			new_pattern[n] := random_digit()::text;
		when pattern_n = '%' then 
			new_pattern[n] := random_alpha_numeric()::text;
		else
	end case;

	probe := probe || n::text || pattern_n;


	n := (n + 1);

end LOOP;
	


return 
	array_to_string(new_pattern,'')
-- 	n
;

END;

$$

LANGUAGE PLPGSQL
STABLE;

COMMENT ON FUNCTION random_string_from_pattern(TEXT) is 
$$
Pattern wildcards:
-- @: random letter
-- #: random number
-- %: random letter or number
$$
;

---<<<

/*
_|_|_|_|
_|
_|_|_|
_|
_|
*/
CREATE OR REPLACE FUNCTION now_std() returns TIMESTAMP AS
$$
select to_char(CURRENT_TIMESTAMP,'YYYY-MM-DD HH24:MI:SS')::TIMESTAMP;
$$
LANGUAGE SQL
STABLE
;

-- COMMENT: the purpose of this function is to standardize the return of now() for only 2 digits for seconds


/*
_|_|_|_|
_|
_|_|_|
_|
_|
*/
CREATE OR REPLACE FUNCTION test_element_type(
	IN	value_in	TEXT
,	IN	cast_in		TEXT
) RETURNS BOOLEAN AS
$$
BEGIN
	BEGIN
	EXECUTE ('select ' || quote_literal(value_in) || '::' || cast_in);
	EXCEPTION when others THEN
		return FALSE;
	END;
RETURN TRUE;
END;
$$

LANGUAGE PLPGSQL
IMMUTABLE
;


/*
_|_|_|_|
_|
_|_|_|
_|
_|
*/
-- This function replaces all string-defined variables "%variable%" in the first argument with the value of the respective key in the second json object.

CREATE OR REPLACE FUNCTION replace_variables(
	IN	base_text		TEXT
,	IN	json_variables	JSONB
) RETURNS TEXT AS
$$
DECLARE
	each_variable RECORD;
BEGIN

FOR each_variable in (select * from jsonb_each_text(json_variables) where value IS NOT NULL)
LOOP
	base_text := replace(base_text,'%' || each_variable.key || '%', each_variable.value);
END LOOP;

-- if there are any left un-replaced, then blot them out
base_text := regexp_replace(base_text, '"%(.*?)%"', '', 'g');
base_text := regexp_replace(base_text, '%(.*?)%', '', 'g');

RETURN base_text;

END;
$$

LANGUAGE PLPGSQL
IMMUTABLE
;


-- This function replaces all "%" in the first argument with the value of the text in each respective position of the second TEXT[] argument.
CREATE OR REPLACE FUNCTION replace_variables(
    IN  base_text       TEXT
,   IN  text_variables  TEXT[]
) RETURNS TEXT AS
$$
DECLARE
    each_variable TEXT;
BEGIN

FOREACH each_variable IN ARRAY text_variables LOOP
    base_text := regexp_replace(base_text, '%', each_variable);
END LOOP;

RETURN base_text;

END;
$$

LANGUAGE PLPGSQL
IMMUTABLE
;


-- Create a function that always returns the first non-NULL item
CREATE OR REPLACE FUNCTION first_agg ( anyelement, anyelement )
RETURNS anyelement LANGUAGE SQL IMMUTABLE STRICT AS $$
        SELECT $1;
$$;
 
-- And then wrap an aggregate around it
CREATE AGGREGATE FIRST (
        sfunc    = first_agg,
        basetype = anyelement,
        stype    = anyelement
);
 
-- Create a function that always returns the last non-NULL item
CREATE OR REPLACE FUNCTION last_agg ( anyelement, anyelement )
RETURNS anyelement LANGUAGE SQL IMMUTABLE STRICT AS $$
        SELECT $2;
$$;
 
-- And then wrap an aggregate around it
CREATE AGGREGATE LAST (
        sfunc    = last_agg,
        basetype = anyelement,
        stype    = anyelement
);




CREATE OR REPLACE FUNCTION YEAR(IN DATE) RETURNS TEXT AS $$ select to_char($1,'YYYY'); $$ LANGUAGE SQL;
CREATE OPERATOR @ (
    PROCEDURE = year
,	LEFTARG = date
)
;
COMMENT ON OPERATOR @ (DATE, NONE) IS 'Returns the YYYY part of YYYY-MM-DD date';


CREATE OR REPLACE FUNCTION MONTH(IN DATE) RETURNS TEXT AS $$ select to_char($1,'YYYY-MM'); $$ LANGUAGE SQL;
CREATE OPERATOR @@ (
    PROCEDURE = month
,	LEFTARG = date
)
;
COMMENT ON OPERATOR @@ (DATE, NONE) IS 'Returns the YYYY-MM part of YYYY-MM-DD date.';


CREATE OR REPLACE FUNCTION year_to_int(IN DATE, IN INT) RETURNS BOOLEAN AS $$ select year($1)::INT = $2; $$ LANGUAGE SQL;
DROP OPERATOR IF EXISTS @= (DATE, INT);
CREATE OPERATOR @= (
	PROCEDURE = year_to_int
,	LEFTARG = DATE
,	RIGHTARG = INT
)
;
COMMENT ON OPERATOR @= (DATE, INT) IS 'Returns TRUE if year in date of left arg equals the right arg integer.';

--->>
CREATE OR REPLACE FUNCTION money_add(IN MONEY, IN ANYELEMENT) RETURNS MONEY AS 
$$
select $1 + $2::money;
$$
LANGUAGE SQL
IMMUTABLE
;

create or replace function money_add(IN ANYELEMENT, IN MONEY) RETURNS MONEY AS 
$$
select money_add($2,$1);
$$
LANGUAGE SQL
IMMUTABLE
;

CREATE OPERATOR + (
    PROCEDURE = money_add
,   LEFTARG = MONEY
,   RIGHTARG = ANYELEMENT
,   COMMUTATOR = +

)
;
COMMENT ON OPERATOR + (MONEY, ANYELEMENT) IS 'Returns the numerical sum for $ money.';

CREATE OPERATOR + (
    PROCEDURE = money_add,
    LEFTARG = ANYELEMENT,
    RIGHTARG = MONEY
)
;
COMMENT ON OPERATOR + (ANYELEMENT, MONEY) IS 'Returns the numerical sum for $ money.';


/*
*/


CREATE or REPLACE FUNCTION 
safe_divide(
	IN numerator numeric
,	IN denomenator numeric
,	IN alternate numeric
) RETURNS numeric
AS $BODY$
DECLARE
	safe_quotient numeric;
BEGIN

IF denomenator <> 0 AND denomenator IS NOT NULL and numerator IS NOT NULL then 
	safe_quotient := numerator/denomenator;
else 
	safe_quotient := alternate;
end if;

RETURN safe_quotient;

END;

$BODY$
LANGUAGE plpgsql
IMMUTABLE;


CREATE OR REPLACE FUNCTION safe_divide(numeric, numeric) RETURNS numeric AS
$$
SELECT CASE WHEN $2 <> 0 THEN $1/$2 ELSE NULL END;
$$
LANGUAGE SQL
IMMUTABLE
;


DROP OPERATOR IF EXISTS // (numeric, numeric);
CREATE OPERATOR // (
	PROCEDURE = safe_divide
,	LEFTARG = numeric
,	RIGHTARG = numeric
)
;
COMMENT ON OPERATOR // (NUMERIC, NUMERIC) IS 'Returns NULL if fraction is NaN, otherwise returns the decimal fraction.';

DROP OPERATOR IF EXISTS # (numeric, int);
CREATE OPERATOR # (
	PROCEDURE = round
,	LEFTARG = numeric
,	RIGHTARG = int
)
;
COMMENT ON OPERATOR # (NUMERIC, INT) IS 'Returns the left float rounded to the number of decimals specified by the right.';


CREATE OR REPLACE FUNCTION safe_textcat(text, text) RETURNS text AS
$$
SELECT COALESCE($1,'') || COALESCE($2,'');
$$
LANGUAGE SQL
IMMUTABLE
;

DROP OPERATOR IF EXISTS + (text,text);
CREATE OPERATOR + (
	PROCEDURE = safe_textcat
,	LEFTARG = text
,	RIGHTARG = text
)
;
COMMENT ON OPERATOR + (TEXT, TEXT) IS 'Returns the right text concatenated to the left, where NULL is intepreted as blank for both sides.';


CREATE OR REPLACE FUNCTION nullsafe(TEXT) RETURNS TEXT AS
$$
select (case when $1 is NULL then ''::TEXT else $1 end);
$$
LANGUAGE SQL
IMMUTABLE
;


CREATE OR REPLACE FUNCTION date_diff (units TEXT, start_t TIMESTAMP, end_t TIMESTAMP) 
     RETURNS INT AS $$
   DECLARE
     diff_interval INTERVAL;
     diff INT = 0;
     years_diff INT = 0;
   BEGIN
     IF units IN ('yy', 'yyyy', 'year', 'mm', 'm', 'month') THEN
       years_diff = DATE_PART('year', end_t) - DATE_PART('year', start_t);
 
       IF units IN ('yy', 'yyyy', 'year') THEN
         -- SQL Server does not count full years passed (only difference between year parts)
         RETURN years_diff;
       ELSE
         -- If end month is less than start month it will subtracted
         RETURN years_diff * 12 + (DATE_PART('month', end_t) - DATE_PART('month', start_t)); 
       END IF;
     END IF;
 
     -- Minus operator returns interval 'DDD days HH:MI:SS'  
     diff_interval = end_t - start_t;
 
     diff = diff + DATE_PART('day', diff_interval);
 
     IF units IN ('wk', 'ww', 'week') THEN
       diff = diff/7;
       RETURN diff;
     END IF;
 
     IF units IN ('dd', 'd', 'day') THEN
       RETURN diff;
     END IF;
 
     diff = diff * 24 + DATE_PART('hour', diff_interval); 
 
     IF units IN ('hh', 'hour') THEN
        RETURN diff;
     END IF;
 
     diff = diff * 60 + DATE_PART('minute', diff_interval);
 
     IF units IN ('mi', 'n', 'minute') THEN
        RETURN diff;
     END IF;
 
     diff = diff * 60 + DATE_PART('second', diff_interval);
 
     RETURN diff;
   END;
   $$ LANGUAGE plpgsql;


CREATE OR REPLACE FUNCTION space_join(IN TEXT, IN TEXT, OUT TEXT) AS
$$
SELECT COALESCE($1 || ' ' || $2, $1, $2);
$$

LANGUAGE SQL
IMMUTABLE
;


DROP OPERATOR IF EXISTS & (text, text);
CREATE OPERATOR & (
    PROCEDURE = space_join,
    LEFTARG = text,
    RIGHTARG = text
)
;
COMMENT ON OPERATOR & (TEXT, TEXT) IS 'Returns the right text concatenated to the left with a space between. If either is NULL, then no spaces are added.';


/*
_|_|_|_|
_|
_|_|_|
_|
_|
*/
CREATE OR REPLACE FUNCTION case_switch_operator(
	IN 	BOOLEAN
,	IN 	TEXT[]
,	OUT TEXT
) AS
$$
select case when $1 then $2[1] ELSE $2[2] END;
$$
LANGUAGE SQL
IMMUTABLE
;

DROP OPERATOR IF EXISTS ? (BOOLEAN, TEXT[]);
CREATE OPERATOR ? (
    PROCEDURE = case_switch_operator,
    LEFTARG = BOOLEAN,
    RIGHTARG = TEXT[]
)
;
COMMENT ON OPERATOR ? (BOOLEAN, TEXT[]) IS 'If left arg is TRUE/FALSE then return first/second position of right arg.';


/*
_|_|_|_|
_|
_|_|_|
_|
_|
*/
CREATE OR REPLACE FUNCTION fit(IN string TEXT, IN max_length INT) RETURNS TEXT AS
$$
DECLARE
BEGIN

IF length(string) > max_length THEN
	return left(string,max_length/2-1) || '...' || right(string,max_length/2-2);
ELSE
	return string;
END IF;

END;
$$
LANGUAGE PLPGSQL
IMMUTABLE
;

/*
_|_|_|_|
_|
_|_|_|
_|
_|
*/
CREATE OR REPLACE FUNCTION strict(
	IN 	BOOLEAN
,	OUT BOOLEAN
) AS
$$
select 
    CASE 
        WHEN $1 THEN TRUE
        ELSE FALSE 
    END;
$$
LANGUAGE SQL
IMMUTABLE
;

DROP OPERATOR IF EXISTS ?? (boolean,NONE);
CREATE OPERATOR ?? (
    PROCEDURE = strict,
    LEFTARG = boolean
)
;
COMMENT ON OPERATOR ?? (BOOLEAN, NONE) IS 'Return TRUE only if left arg is TRUE, otherwise FALSE (no NULLs).';

/*
_|_|_|_|
_|
_|_|_|
_|
_|
*/
CREATE OR REPLACE FUNCTION limited_float(IN numeric) RETURNS numeric AS
$$
select case when $1::int = $1 then $1::int else $1#2 end
$$
LANGUAGE SQL
IMMUTABLE
;


CREATE OR REPLACE FUNCTION full_agg_array_statef
(
	IN array_in_agg anyarray
,	IN array_in anyarray
,	OUT array_out anyarray
) AS $$
BEGIN

array_out := array_in_agg || array_in;

END;
$$
LANGUAGE PLPGSQL
IMMUTABLE;


DROP AGGREGATE IF EXISTS full_agg_array (anyarray);
CREATE AGGREGATE full_agg_array(anyarray) 
(
    SFUNC = full_agg_array_statef,
    STYPE = anyarray
)
;


CREATE OR REPLACE FUNCTION strip_duplicates(IN array_in ANYARRAY, OUT array_out anyarray) AS
$$
DECLARE
BEGIN

select
    array_agg(DISTINCT arrys)
into array_out
from (select arrys from unnest(array_in) arrys) arys
;

END;
$$
LANGUAGE PLPGSQL
IMMUTABLE
;



CREATE OR REPLACE FUNCTION distinct_count(IN array_in anyarray, OUT array_out TEXT) AS
$$
DECLARE
BEGIN

with
	poo as
(
select 
	unnest
,	count(*)
from unnest(array_in) 
group by unnest
)
select
	string_agg(unnest::text || ' (x' || count ||')', chr(10))
into array_out
from poo
;


END;
$$
LANGUAGE PLPGSQL
IMMUTABLE
;

/*
*/
CREATE OR REPLACE FUNCTION text_contained_in_array(IN left_in TEXT, IN right_in TEXT[])
RETURNS BOOLEAN AS
$$
BEGIN
    RETURN (SELECT array[left_in] <@ right_in);
END;
$$
LANGUAGE PLPGSQL
IMMUTABLE
;


DROP OPERATOR IF EXISTS <@ (TEXT, TEXT[]);
CREATE OPERATOR <@ (
    PROCEDURE = text_contained_in_array
,   LEFTARG = TEXT
,   RIGHTARG = TEXT[]
)
;
COMMENT ON OPERATOR <@ (TEXT, TEXT[]) IS 'Returns TRUE if the left string is contained by the right array.';


CREATE OR REPLACE FUNCTION text_contained_in_array(IN left_in TEXT[], IN right_in TEXT)
RETURNS BOOLEAN AS
$$
BEGIN
    RETURN (SELECT array[right_in] <@ left_in);
END;
$$
LANGUAGE PLPGSQL
IMMUTABLE
;


DROP OPERATOR IF EXISTS @> (TEXT, TEXT[]);
CREATE OPERATOR @> (
    PROCEDURE = text_contained_in_array
,   LEFTARG = TEXT[]
,   RIGHTARG = TEXT
)
;
COMMENT ON OPERATOR @> (TEXT[], TEXT) IS 'Returns TRUE if the left array contains the right string.';


--->>
CREATE OR REPLACE FUNCTION INT_contained_in_array(IN left_in INT[], IN right_in INT)
RETURNS BOOLEAN AS
$$
BEGIN

RETURN (SELECT left_in @> array[right_in]);

END;
$$
LANGUAGE PLPGSQL
IMMUTABLE
;

-- DROP OPERATOR IF EXISTS <@ (INT, INT[]);
CREATE OPERATOR @> (
    PROCEDURE = INT_contained_in_array,
    LEFTARG = INT[],
    RIGHTARG = INT
)
;
COMMENT ON OPERATOR @> (INT[], INT) IS 'Returns TRUE if the left INT is contained by the right array.';

--->>
CREATE OR REPLACE FUNCTION INT_contained_in_array(IN left_in INT, IN right_in INT[])
RETURNS BOOLEAN AS
$$
BEGIN

RETURN (SELECT array[left_in] <@ right_in);

END;
$$
LANGUAGE PLPGSQL
IMMUTABLE
;

-- DROP OPERATOR IF EXISTS <@ (INT, INT[]);
CREATE OPERATOR <@ (
    PROCEDURE = INT_contained_in_array,
    LEFTARG = INT,
    RIGHTARG = INT[]
)
;
COMMENT ON OPERATOR <@ (INT, INT[]) IS 'Returns TRUE if the right INT is contained by the left array.';



--->>
CREATE OR REPLACE FUNCTION array_sum(
	IN 	NUMERIC[]
) RETURNS NUMERIC AS
$$
SELECT SUM(unnest) FROM unnest($1);
$$
LANGUAGE SQL
IMMUTABLE
;


CREATE OR REPLACE FUNCTION min(IN ANYARRAY) RETURNS ANYELEMENT AS
$$
select min(unnest) from unnest($1);
$$
LANGUAGE SQL
IMMUTABLE
;

CREATE OR REPLACE FUNCTION max(IN ANYARRAY) RETURNS ANYELEMENT AS
$$
select max(unnest) from unnest($1);
$$
LANGUAGE SQL
IMMUTABLE
;


--->>
CREATE OR REPLACE FUNCTION email_validate(TEXT) RETURNS BOOLEAN AS
$$
BEGIN
RETURN $1~ '.+\@.+\..+';
END;
$$
LANGUAGE PLPGSQL
IMMUTABLE
;

CREATE DOMAIN EMAIL AS CITEXT CHECK(email_validate(VALUE));


--->>
CREATE OR REPLACE FUNCTION bool_and(IN BOOLEAN, IN BOOLEAN) RETURNS BOOLEAN AS 
$$
select
    CASE
        WHEN $1 IS TRUE AND $2 IS TRUE THEN TRUE
        ELSE FALSE
    END;
$$ 
LANGUAGE SQL
;

DROP OPERATOR IF EXISTS & (BOOLEAN, BOOLEAN);
CREATE OPERATOR & (
    PROCEDURE = bool_and
,   LEFTARG = BOOLEAN
,   RIGHTARG = BOOLEAN
)
;
COMMENT ON OPERATOR & (BOOLEAN, BOOLEAN) IS 'Returns TRUE if both left and right params are TRUE, otherwise FALSE.';


--->>
CREATE OR REPLACE FUNCTION "not"(IN BOOLEAN) RETURNS BOOLEAN AS 
$$
	select NOT $1;
$$
LANGUAGE SQL;

CREATE OPERATOR ! (
    PROCEDURE = not
,   RIGHTARG = BOOLEAN
)
;
COMMENT ON OPERATOR ! (NONE, BOOLEAN) IS 'Returns the opposite ("not") of the left arg.';


--->>
CREATE OR REPLACE FUNCTION safe_add(IN ANYELEMENT, IN ANYELEMENT) RETURNS ANYELEMENT AS 
$$
	select COALESCE($1+$2,0); 
$$
LANGUAGE SQL;

CREATE OPERATOR & (
    PROCEDURE = safe_add
,   LEFTARG = ANYELEMENT
,   RIGHTARG = ANYELEMENT
)
;
COMMENT ON OPERATOR & (ANYELEMENT, ANYELEMENT) IS 'Returns the some of the left and right args and if either is NULL, then returns 0.';


/*
*/

create view custom_operators as

with
    ops as
(
SELECT 
    oid
,   *
FROM pg_operator
WHERE
	oprowner = (select usesysid from pg_user where usename = 'the_architect')
)
,   pgta as
(
select
    oid as oprresult
,   typname
from pg_type
)
,   pgtr as
(
select
    oid as oprright
,   typname as right_type
from pg_type
)
,   pgtl as
(
select
    oid as oprleft
,   typname as left_type
from pg_type
)

select
    oprname as operator
,   left_type
,   right_type
,   typname as return_type
,   COALESCE('('||left_type||')','') & oprname & COALESCE('('||right_type||')','') & '-->' & typname as formula
,   obj_description(ops.oid) as description
,   oprcode as operator_function
-- ,	*
from ops
left join pgtr using (oprright)
left join pgtl using (oprleft)
join pgta using (oprresult)
order by left_type, right_type, return_type
;


CREATE OR REPLACE FUNCTION date_diff (
    units VARCHAR(30)
,   start_t TIMESTAMP
,   end_t TIMESTAMP
) RETURNS INT AS $$
DECLARE
    diff_interval INTERVAL; 
    diff INT = 0;
    years_diff INT = 0;
BEGIN
IF units IN ('yy', 'yyyy', 'year', 'mm', 'm', 'month') THEN
    years_diff = DATE_PART('year', end_t) - DATE_PART('year', start_t);

    IF units IN ('yy', 'yyyy', 'year') THEN
        -- SQL Server does not count full years passed (only difference between year parts)
        RETURN years_diff;
    ELSE
        -- If end month is less than start month it will subtracted
        RETURN years_diff * 12 + (DATE_PART('month', end_t) - DATE_PART('month', start_t)); 
    END IF;
END IF;

-- Minus operator returns interval 'DDD days HH:MI:SS'  
diff_interval = end_t - start_t;

diff = diff + DATE_PART('day', diff_interval);

IF units IN ('wk', 'ww', 'week') THEN
    diff = diff/7;
    RETURN diff;
END IF;

IF units IN ('dd', 'd', 'day') THEN
    RETURN diff;
END IF;

diff = diff * 24 + DATE_PART('hour', diff_interval); 

IF units IN ('hh', 'hour') THEN
    RETURN diff;
END IF;

diff = diff * 60 + DATE_PART('minute', diff_interval);

IF units IN ('mi', 'n', 'minute') THEN
    RETURN diff;
END IF;

diff = diff * 60 + DATE_PART('second', diff_interval);

RETURN diff;
END;
$$
LANGUAGE plpgsql
IMMUTABLE
;

CREATE OR REPLACE VIEW custom_functions as

with
    base as
(
select
    proname
,   proargtypes
,   typname as outputs
,   obj_description(pg_proc.oid) as comment
from pg_proc
join pg_type on pg_type.oid = pg_proc.prorettype
WHERE
    proowner = (select usesysid from pg_user where usename = 'the_architect')
)
,   args_pre as
(
select
    proname
,   unnest(proargtypes) as oid
from base
)
,   args as
(
select
    proname
,   string_agg(typname, ', ') as inputs
from args_pre
join pg_type on pg_type.oid = args_pre.oid
group by proname
)
,   report as
(
select
    proname as name
,   inputs
,   outputs
,   comment
from base
join args using (proname)
)

select
--  proname||'('||args||')'||' --> '||rtns
    *
from report
;


CREATE OR REPLACE VIEW json_extensions as

select
    *
from custom_functions
where
    ('{'||inputs||'}')::text[] && '{jsonb,json}'
OR  outputs in ('json','jsonb')
;-- !!! THIS SCRIPT IS AN INITIALIZATION SCRIPT AND WILL BUILD FROM SCRATCH !!! --

-- ALL IN THIS FILE INTENDED FOR THE GLOBAL SCHEMA --

/*
------------------
	||		||		
	||		||		
	||		||		
*/
DROP TABLE IF EXISTS return_codes;
CREATE TABLE return_codes
(
	return_code		numeric		PRIMARY KEY
,	desc_detail		text
,	desc_support	text
,	desc_customer	text
)
;

COMMENT ON TABLE return_codes is 
$$
-- positive codes are sucesses
-- negative codes are errors

VERSIONS:

	~1~
	- 


$$
;


/*
----------
	||	 
	||	 
	||	 
*/
DROP TABLE IF EXISTS settings;
CREATE TABLE settings 
(
	setting_name	TEXT	PRIMARY KEY
,	value			TEXT
,	value_type		REGTYPE
,	description		TEXT
)
;

COMMENT ON TABLE settings is 
$$
-- settings table is used for single-dimension key-value pairs, organized by category
-- setting_name should be in directory form, e.g.: main_category/sub_category/name
$$
;


/*
----------
	||	 
	||	 
	||	 
*/
DROP TABLE IF EXISTS options;
CREATE TABLE options 
(
	option_name	TEXT
,	properties	JSON
)
;

COMMENT ON TABLE settings is 
$$
-- options table is used for multi-dimension key-value pairs under the title of a single option_id
-- options_name should be in directory form, e.g.: main_category/sub_category/name
$$
;


/*
_|_|_|_|
_|
_|_|_|
_|
_|
*/
CREATE OR REPLACE FUNCTION apiv_settings() RETURNS JSONB AS
$$
BEGIN

RETURN
(
SELECT
	json_agg
	(
	setting_name
	+>
	CASE
		WHEN value_type::text = 'text[]' THEN array_to_json(value::text[])
		WHEN value_type::text = 'int[]' THEN array_to_json(value::int[])
		WHEN value_type::text = 'json' THEN value::json
		WHEN value_type::text = 'jsonb' THEN value::json
		WHEN value_type::text = 'int' THEN to_json(value::int)
	ELSE
		to_json(value)
	END
	)::json
FROM settings
);

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
CREATE OR REPLACE FUNCTION setting_lookup(
	IN	settings_name_in	TEXT
) RETURNS TEXT AS
$$
SELECT value from settings where setting_name = settings_name_in;
$$

LANGUAGE SQL
IMMUTABLE
;

CREATE OPERATOR ?(
    PROCEDURE = setting_lookup
,	RIGHTARG = TEXT
)
;
COMMENT ON OPERATOR ? (NONE, TEXT) IS 'Returns the textual value of the setting_name given in the right arg.';



/*
_|_|_|_|
_|
_|_|_|
_|
_|
*/
CREATE OR REPLACE FUNCTION setting_update(
	IN	settings_name_in	TEXT
,	IN 	value_in			TEXT
) RETURNS TEXT AS
$$
UPDATE settings SET
	value = value_in
where 
	setting_name = settings_name_in
returning value
;
$$

LANGUAGE SQL
VOLATILE
;


/*
_|_|_|_|
_|
_|_|_|
_|
_|
*/
CREATE OR REPLACE FUNCTION options_lookup(
	IN	option_name_in	TEXT
) RETURNS JSON AS
$$
SELECT properties from options where option_name = option_name_in;
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
CREATE OR REPLACE FUNCTION apiv_return_codes(
	OUT json_out 	JSONB
) AS
$$
BEGIN
select 
	json_agg(row_to_json(return_codes)) into json_out 
from return_codes
;
END;
$$
LANGUAGE PLPGSQL
STABLE
;


/*
 ----------------
	||		||		
	||		||		
	||		||		
*/
-- DROP table permissions CASCADE;
create table permissions (
	permission_code	ltree 	primary key
,	description 	TEXT
)
;--- // --- 			--- // --- 
--- \\ ---  GLOBAL	--- \\ ---
--- // --- 			--- // --- 

-- !!! THIS SCRIPT IS AN INITIALIZATION SCRIPT AND WILL BUILD FROM SCRATCH !!! --
-- * because the schema is global, it is destroyed and created in an earlier build file 
-- versioning
/*

Version 0.1
	- 

*/

/*
----------
	||	 
	||	 
	||	 
*/
CREATE TABLE async_queue 
(
	id				SERIAL 		PRIMARY KEY
,	receipt_date	TIMESTAMP	DEFAULT now()
,	scheduled_date	TIMESTAMP
,	exec_env		TEXT		DEFAULT 'any'
,	priority		INT 		DEFAULT NULL

,	exec_date		TIMESTAMP
,	is_error		BOOLEAN
,	exec_sql		TEXT
,	sql_params 		TEXT[]
,	exec_bash		TEXT
,	exec_api		TEXT

,	CONSTRAINT exec_spec CHECK(COALESCE(exec_sql,exec_bash,exec_api) IS NOT NULL)
,	CONSTRAINT check_env CHECK(exec_env <@ (?'dbAPI.environments')::text[])

)
;

COMMENT ON TABLE async_queue is 
$$

$$
;



/*
_|_|_|_|
_|
_|_|_|
_|
_|
*/
CREATE OR REPLACE FUNCTION async_queue_insert_trigger() RETURNS TRIGGER AS
$$
DECLARE
	temp RECORD;
BEGIN

PERFORM pg_notify('async_request'::text,''::text);

RETURN new;

END;
$$

LANGUAGE PLPGSQL
VOLATILE
;


COMMENT ON FUNCTION async_queue_insert_trigger() is 
$$
VERSIONS:

	~1~
	- 

$$
;


CREATE TRIGGER async_queue_exec_notify
	AFTER INSERT
	ON global.async_queue
	FOR EACH ROW
	EXECUTE PROCEDURE async_queue_insert_trigger()
;




/*
_|_|_|_|
_|
_|_|_|
_|
_|
*/
CREATE OR REPLACE FUNCTION async_sql_request(
	IN	exec_sql_in			TEXT
,	IN 	sql_params_in		TEXT[]		DEFAULT NULL
,	IN 	priority_in			INT 		DEFAULT NULL
,	IN 	scheduled_date_in	TIMESTAMP	DEFAULT NULL
,	IN 	domain_in 			TEXT		DEFAULT 'any' -- this should probably be updated to be the issuing domain
) RETURNS VOID AS
$$
BEGIN

-- to use, supply sql code to execute with variables in standard $1, $2... etc. fashion. The array parameter will replace the values in the same order.

INSERT INTO async_queue (exec_sql,sql_params,priority,scheduled_date,exec_env)
VALUES (exec_sql_in,sql_params_in,priority_in,scheduled_date_in,domain_in)
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
CREATE OR REPLACE FUNCTION apix_async_exec_scripts(
	IN	domain_in	TEXT DEFAULT ''
) RETURNS VOID AS
$$
DECLARE
	exec_list	async_queue%ROWTYPE;
	sql_param 	TEXT;
	loop_num	INT := 1;
BEGIN

-- build execution list

SELECT
	*
INTO exec_list
FROM async_queue
WHERE
	exec_date IS NULL
AND	(scheduled_date IS NULL OR scheduled_date >= now())
AND exec_env IN ('any',domain_in)

ORDER BY
	priority NULLS LAST
,	scheduled_date
,	id

LIMIT 1
;


-- replace arguments with values
IF NOT empty(exec_list.sql_params) THEN
	FOREACH sql_param IN ARRAY exec_list.sql_params
	LOOP
		exec_list.exec_sql := replace(exec_list.exec_sql, '$'||loop_num, quote_literal(exec_list.sql_params[loop_num]));
		loop_num := loop_num + 1;
	END LOOP;

UPDATE async_queue SET exec_sql = exec_list.exec_sql where id = exec_list.id;

END IF;



-- execute code
BEGIN

CASE
	WHEN NOT empty(exec_list.exec_sql)
	THEN
		EXECUTE exec_list.exec_sql;

	-- WHEN !empty(exec_list.exec_bash)
	-- THEN

	-- WHEN !empty(exec_list.exec_api)
	-- THEN

	ELSE
END CASE;


EXCEPTION when others THEN
	UPDATE async_queue SET is_error = TRUE, exec_date = NOW() WHERE id = exec_list.id;

END; -- exception watch

UPDATE async_queue SET exec_date = clock_timestamp() WHERE id = exec_list.id;

END;
$$

LANGUAGE PLPGSQL
VOLATILE
;


/*
----------
	||	 
	||	 
	||	 
*/
CREATE TABLE cron_queue 
(
	id				SERIAL 		PRIMARY KEY
,	scheduled_date	TIMESTAMP
,	exec_date		TIMESTAMP
,	is_error		BOOLEAN
,	exec_env		TEXT[]
,	exec_sql		TEXT		NOT NULL
,	exec_bash		TEXT		NOT NULL
,	exec_api		TEXT		NOT NULL
)
;


-- DROP TABLE snapshots;
CREATE TABLE snapshots
(
	snapshot_id		SERIAL	PRIMARY KEY
,	txn_id			BIGINT
,	txn_date		TIMESTAMP
,	txn_table		TEXT
,	txn_snapshot	JSONB
,	txn_delta		JSONB
-- ,	notes			TEXT[]
--		⤷ each note entry is an array of three parts: [date, user, notes]
)
;


CREATE OR REPLACE FUNCTION snapshot_take() RETURNS TRIGGER AS
$$
BEGIN

INSERT INTO snapshots (txn_id, txn_date, txn_table, txn_snapshot, txn_delta)
VALUES
( 
	txid_current()
,	now()
,	TG_TABLE_SCHEMA+'.'+TG_TABLE_NAME
,	row_to_json(NEW)::jsonb-&'{exif}'
,	jsonb_delta(row_to_json(NEW)::JSONB,row_to_json(OLD)::JSONB)
)
;

RETURN NULL;
END;
$$
LANGUAGE PLPGSQL
VOLATILE
;


-------------------



CREATE OR REPLACE FUNCTION snapshot_restore(
	IN	txn_id_in	BIGINT
,	OUT json_out	JSONB
) AS
$$
DECLARE
	snapshot	RECORD;
	sql_exec	TEXT;
BEGIN

-- disable snapshots temporarily? no, because then you cannot undo the undo...
-- SET SESSION dbAPI.snapshot = FALSE;

FOR snapshot IN (SELECT * FROM snapshots WHERE txn_id = txn_id_in order by snapshot_id desc) LOOP

	-- get the changed values list
	select
		string_agg('	'||key||' = '||quote_literal(value), chr(10)+',')
	into sql_exec
	from jsonb_each_casted(snapshot.txn_snapshot)
	;

	-- get the primary key name for the given table
	SELECT
		'table_pk_name'+>attname
	into json_out
	FROM   pg_index i
	JOIN   pg_attribute a ON a.attrelid = i.indrelid AND a.attnum = ANY(i.indkey)
	WHERE  i.indrelid = (snapshot.txn_table)::regclass
	AND    i.indisprimary
	;

	-- set parameters
	json_out := json_out & ('table_name'+>snapshot.txn_table) & ('table_pk_id'+>(snapshot.txn_snapshot->>(json_out->>'table_pk_name')));

	-- build the sql script
	sql_exec :=
	$sql$
	UPDATE %table_name% SET
	$sql$||sql_exec||$sql$
	WHERE
		%table_pk_name% = %table_pk_id%
	$sql$
	;
	sql_exec := replace_variables(sql_exec, json_out);

	-- do the snapshot rollback
	EXECUTE sql_exec;

	-- output diagnostics/reporting
	json_out := json_out & ('sql_exec'+>sql_exec);

	update snapshots SET
		notes =
			prod_note_append(
				notes
			,	array[now()::text, prod_user_num()::text, 'manually restored']
			)
	WHERE
		txn_id = txn_id_in
	;

END LOOP;

json_out := json_out & return_code(1);

END;
$$
LANGUAGE PLPGSQL
VOLATILE
;


CREATE OR REPLACE FUNCTION domain_env(
	OUT domain_env TEXT
) AS
$$
BEGIN

BEGIN
	domain_env := current_setting('dbAPI.domain_env');
EXCEPTION when others 
THEN END;

IF NOT (domain_env <@ (?'dbAPI.environments')::text[]) 
	THEN domain_env := 'dev';
END IF;

END;
$$
LANGUAGE PLPGSQL
IMMUTABLE
;

/*
*/
-- This function standardizes how error emails are recorded and issued
-- It accepts JSON with required parameters and returns JSON, which will only contain a return_code
CREATE OR REPLACE FUNCTION email_error_log(IN json_in JSONB) RETURNS JSONB AS
$$
BEGIN

RETURN emails.add_to_queue(
	'SMTP'
,	'error/db_api'
,	NULL
,	NULL
,		('to'+>?('global/error_email')::text)
	&	json_in
)
;

END;
$$
LANGUAGE PLPGSQL
VOLATILE
;





/*
*/
-- this version of the function just extracts the numerical return_code
CREATE OR REPLACE FUNCTION return_code(IN json_in JSONB) RETURNS INT AS
$$
DECLARE
BEGIN
    return (json_in->>'return_code')::INT;
END;
$$
LANGUAGE PLPGSQL
IMMUTABLE
;

-- this version of the function builds the json object for a return-code
CREATE OR REPLACE FUNCTION return_code(IN rc INT) RETURNS JSONB AS
$$
DECLARE
BEGIN
    return 'return_code' +> rc;
END;
$$
LANGUAGE PLPGSQL
IMMUTABLE
;



-- This version of the function collects a text message payload and issues an error email.
-- It is intended to be used for non-trivial return_codes that need to be actively monitored.
CREATE OR REPLACE FUNCTION return_code(IN rc INT, IN message_payload JSONB) RETURNS JSONB AS
$$
BEGIN
    return 'return_code' +> rc;
END;
$$
LANGUAGE PLPGSQL
VOLATILE
;
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

-- Records
(01200,	'Records'),
(-1200,	'Must supply date_cursor.'),
(-1201,	'Must supply a user_id.'),
(-1202,	'Must supply survey_json.'),
(-1203,	'Must supply data selections.'),
(-1204,	'Invalid period grouping option.'),
(-1205,	'Invalid selection(s).'),

(00000,	'anonymous')
;



--> Bookmark settings
DELETE FROM SETTINGS;
INSERT INTO SETTINGS VALUES
--(setting_name, value, value_type, description)
	('users.classes', '{client,admin}', 'text[]', '')

,	('records.categories', '{transactions, survey}', 'text[]', 'List of record category types.')
,	('records.selectors', '{sum_of_class, sum_of_type, sum_of_period, agg_by_class, agg_by_type, agg_by_period}', 'text[]', 'List of selector options.')
,	('records.agg_by_period.groups', '{YYYY,YYYY-MM,YYYY-Q,YYYY-WW,YYYY-DDD}', 'text[]', 'List of record category types.')

,	('dbAPI.snapshots', 'TRUE', 'boolean', 'Determines whether update snapshots are enabled.')
,	('dbAPI.environments', '{live,dev}', 'text[]', 'List of named development environments.')
,	('dbAPI.environment', current_setting('dbAPI.environment'), 'text', 'The current environment type.')

,	('testing.dummy', '', 'text', '')

;
-- ROLLBACK; BEGIN;

--- // --- 			--- // --- 
--- \\ ---  USERS	--- \\ ---
--- // --- 			--- // --- 

-- !!! THIS SCRIPT IS AN INITIALIZATION SCRIPT AND WILL BUILD FROM SCRATCH !!! --

DROP SCHEMA IF EXISTS users CASCADE;
CREATE SCHEMA users;
SET search_path TO "global";


/*

 ----------------
	||		||		
	||		||		
	||		||		

TABLE: dir

*/


CREATE TABLE users.dir
(
	user_id			SERIAL		
		PRIMARY KEY
,	email 			EMAIL		
		UNIQUE
		NOT NULL
,	user_date		TIMESTAMP	
		DEFAULT now_std()
,	name_first		TEXT
,	name_last		TEXT
,	class			TEXT
		NOT NULL 
		DEFAULT ((?'users.classes')::text[])[1] 
		CHECK (class <@ (?'users.classes')::text[])
,	active			BOOLEAN		
		NOT NULL
		DEFAULT FALSE
,	password		TEXT
,	dashboard		JSONB		
		CHECK (dashboard ?&! ((?'users.classes')::text[]))
)
;


CREATE TRIGGER trigger_snapshot
AFTER UPDATE ON users.dir
FOR EACH ROW
WHEN ((?'dbAPI.snapshots')::BOOLEAN AND old.* IS DISTINCT FROM new.*)
EXECUTE PROCEDURE snapshot_take()
;


/*
_|_|_|_|
_|
_|_|_|
_|
_|
*/
CREATE OR REPLACE FUNCTION users.apiv_dir(
	IN	json_in		JSONB
,	OUT	json_out	JSONB
) 
AS $$
DECLARE
	sql_exec TEXT;
BEGIN

-- validation/cleansing
--// This isn't a clean way of doing things, but until there are more apiv functions that require it, this is where it'll stay.
CASE 
	WHEN json_in?'sort_dir' AND NOT UPPER(json_in->>'sort_dir') <@ '{ASC, DESC}' 
		THEN json_out := return_code(-1002) & ('return_msg'+>'Invalid sort_dir'); RETURN;
	WHEN json_in?'sort_name' AND NOT json_in->>'sort_name' <@ '{email, name_first, name_last, active}' 
		THEN json_out := return_code(-1002) & ('return_msg'+>'Invalid sort_name'); RETURN;
	WHEN json_in?'class' AND NOT json_in->>'class' <@ '{admin, client}' 
		THEN json_out := return_code(-1002) & ('return_msg'+>'Invalid class'); RETURN;
	WHEN json_in?'active' AND NOT UPPER(json_in->>'active') <@ '{TRUE, FALSE}' 
		THEN json_out := return_code(-1002) & ('return_msg'+>'Invalid active state'); RETURN;
	ELSE
END CASE;

-- defaults
json_in := @'{"limit":100, "offset":0, "sort_name":"user_id", "sort_dir":"DESC", "active":"TRUE"}' & json_in;

-- base query
sql_exec :=
$sql$
with
	base as
(
select
	user_id
,	email
,	name_first
,	name_last
,	active
from users.dir
WHERE
	CASE 
		WHEN '%class%' <> '' THEN class = '%class%' 
		WHEN '%active%' <> '' THEN active = '%active%'::BOOLEAN 
		ELSE TRUE
	END

ORDER BY %sort_name% %sort_dir%

LIMIT %limit%
OFFSET %offset%

)
select
	jsonb_agg(row_to_json(base))
FROM base
;
$sql$
;



-- fill in the blanks
sql_exec := replace_variables(sql_exec, json_in);


-- debuging --

IF json_in ? 'debug' THEN
	json_out := json_in & ('sql_exec'+>sql_exec); return;
END IF;


-- run the query
IF 
	sql_exec <> '' THEN
		EXECUTE sql_exec into json_out;
	ELSE
		json_out := return_code(-1002) & ('sql_exec'+>sql_exec);
		RETURN;
END IF;


IF json_out IS NULL
	THEN json_out := '[]';
END IF;

RETURN;

END
$$

LANGUAGE PLPGSQL
VOLATILE
;


CREATE OR REPLACE FUNCTION users.apiv_dir(
	OUT	json_out	JSONB
) 
AS $$
BEGIN

json_out := users.apiv_dir('{}');

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
CREATE OR REPLACE FUNCTION users.apiv_details(
	IN	json_in		JSONB
,	OUT	json_out	JSONB
) 
AS $$
DECLARE
	user_id_in INT := json_in->#'user_id';
BEGIN

-- validation --
IF	NOT json_in?|'{user_id, email}' 
THEN
	json_out := json_out & return_code(-1103);
	RETURN;
END IF;

select
	row_to_json(dir.*)
	into json_out
from users.dir
where
	CASE
		WHEN json_in?'user_id' THEN user_id = json_in->#'user_id'
		WHEN json_in?'email' THEN email = json_in->>'email'
		ELSE FALSE
	END
;

IF json_out is NULL THEN 
	json_out := json_out & return_code(-1000);
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
CREATE OR REPLACE FUNCTION users.apix_create(
	IN	json_in				JSONB
,	OUT json_out			JSONB
) AS
$$
DECLARE
	return_code 	INT;
BEGIN

-- validation
CASE
	WHEN NOT json_in?&'{email, name_last, name_first}' 
		THEN json_out := return_code(-1100); RETURN;
	ELSE
END CASE;

BEGIN
	-- cleanse json_in of all parameters except those accepted
	json_in := json_in#&'{email, name_last, name_first, password, debug}';

	json_out := jsonb_table_insert('users','dir', json_in, json_in#'debug');
EXCEPTION 
	WHEN check_violation 
		THEN json_out := return_code(-1101); RETURN;
	WHEN unique_violation
		THEN json_out := return_code(-1102); RETURN;
	WHEN not_null_violation
		THEN json_out := return_code(-1101); RETURN;
END;

json_out := 
	json_out
&	users.apiv_details(json_out)
&	return_code(0)
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
CREATE OR REPLACE FUNCTION users.apix_update(
	IN	json_in				JSONB
,	OUT json_out			JSONB
) AS
$$
DECLARE
	return_code 	INT;
	error_stuff		TEXT;
BEGIN

-- validation

CASE
	WHEN NOT json_in?&'{user_id}' 
		THEN json_out := return_code(-1103); RETURN;
	ELSE
END CASE;

IF json_in?'password'
	-- IF a password is supplied, then activate user
	THEN json_in:=json_in & ('active'+>TRUE);
END IF;

	BEGIN
		json_out := jsonb_table_update('users','dir', json_in#&'{user_id}', json_in, json_in#'debug');
	EXCEPTION 
		WHEN check_violation THEN
			GET STACKED DIAGNOSTICS error_stuff = CONSTRAINT_NAME;
			json_out := return_code(CASE 
				WHEN error_stuff = 'dir_dashboard_check' THEN -1104
				WHEN error_stuff = 'dir_class_check' THEN -1104
				WHEN error_stuff = 'email_check' THEN -1101
			END); RETURN;
		WHEN unique_violation
			THEN json_out := return_code(-1102); RETURN;
	END;

IF json_in->?'debug' THEN 
	RETURN;
END IF;

json_out := 
	users.apiv_details(json_in)
&	return_code(0)
;

END;
$$

LANGUAGE PLPGSQL
VOLATILE
;-- ROLLBACK; BEGIN;

--- // --- 			--- // --- 
--- \\ ---  RECORDS	--- \\ ---
--- // --- 			--- // --- 

-- !!! THIS SCRIPT IS AN INITIALIZATION SCRIPT AND WILL BUILD FROM SCRATCH !!! --

DROP SCHEMA IF EXISTS records CASCADE;
CREATE SCHEMA records;
SET search_path TO "global";


/*

 ----------------
	||		||		
	||		||		
	||		||		

TABLE: dir

*/


CREATE TABLE records.dir
(
	user_id		INT 
		REFERENCES users.dir(user_id) 
		ON UPDATE CASCADE 
		ON DELETE SET NULL
,	quovo_id	INT UNIQUE
)
;


/*

 ----------------
	||		||		
	||		||		
	||		||		

TABLE: dir

*/


CREATE TABLE records.transactions
(
	sync_id		INT
,	date_cursor	DATE
		NOT NULL	
,	quovo_id	INT 
		REFERENCES records.dir(quovo_id) 
		ON UPDATE CASCADE 
		ON DELETE CASCADE
,	account_id	INT
,	class		TEXT
,	type		TEXT
,	value		DOLLAR

)
;

CREATE SEQUENCE IF NOT EXISTS record_sync_id_seq START 1;

-- Quovo doesn't provide holdings with a sync date, therefore there is no atomic way of matching data with a date (or other batch idenfitier). Because of this, an internal method has been implemented using a sync_id, which increments every time a new batch of transactions are entered in with apix_sync_all. This imposes that every time a sync is performed, it is for all of the user's data (all must be supplied). In otherwords, syncs are absolute, not incremental.


/*

 ----------------
	||		||		
	||		||		
	||		||		

TABLE: dir

*/


CREATE TABLE records.surveys
(
	user_id		INT 
		REFERENCES users.dir(user_id) 
		ON UPDATE CASCADE 
		ON DELETE SET NULL
,	date_cursor	DATE 
		NOT NULL
,	survey_json	JSONB

,	UNIQUE (user_id, date_cursor)

)
;



/*
_|_|_|_|
_|
_|_|_|
_|
_|
*/
CREATE OR REPLACE FUNCTION records.apix_fill_survey(
	IN	json_in				JSONB
,	OUT json_out			JSONB
) AS
$$
DECLARE
BEGIN

-- validation
CASE
	WHEN NOT json_in?'date_cursor' 
		THEN json_out := return_code(-1200); RETURN;
	WHEN NOT json_in?'user_id' 
		THEN json_out := return_code(-1201); RETURN;
	WHEN NOT json_in?'survey_json' 
		THEN json_out := return_code(-1202); RETURN;
	ELSE
END CASE;

INSERT INTO records.surveys
VALUES (
	json_in->#'user_id'
,	json_in->@'date_cursor'
,	json_in->'survey_json'
)
ON CONFLICT (user_id, date_cursor) 
DO UPDATE SET survey_json = json_in->'survey_json'
;

json_out := json_out & return_code(0);

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
CREATE OR REPLACE FUNCTION records.apix_import_transactions(
	IN	json_in				JSONB
,	OUT json_out			JSONB
) AS
$$
DECLARE
	json_row	JSONB;
	json_tmp 	JSONB;
BEGIN

-- validation
CASE
	WHEN NOT json_in?'date_cursor' 
		THEN json_out := return_code(-1200); RETURN;
	ELSE
END CASE;

-- process users
FOR json_row IN (select value from jsonb_array_elements(json_in->'quovo_users')) LOOP

	INSERT INTO records.dir
	VALUES (
			json_row->#'username'
		,	json_row->#'id'
		)
	ON CONFLICT (quovo_id) DO NOTHING -- skip existing account records
	;

END LOOP;

-- process holdings
BEGIN

	json_in := json_in & ('sync_id' +> nextval('record_sync_id_seq'::regclass));

	FOR json_row IN (select value from jsonb_array_elements(json_in->'quovo_holdings')) LOOP

		INSERT INTO records.transactions
		(
			sync_id
		,	date_cursor
		,	quovo_id
		,	account_id
		,	class
		,	type
		,	value
		)
		VALUES
		(
			json_in->#'sync_id'
		,	json_in->@'date_cursor'
		,	json_row->#'user_id'
		,	json_row->#'account_id'
		,	json_row->>'basic_asset_class'
		,	json_row->>'type'
		,	json_row->##'value'
		)
		;

	END LOOP;

EXCEPTION 
	WHEN foreign_key_violation
		THEN json_out := return_code(-1102) & ('error_message' +> ('missing quovo user_id' & (json_row->>'user_id'))); RETURN;
	-- with this implementation, the entire transaction will be lost which will potentially/likely drop the complete/good records along with the incomplete/bad ones. In the future, another method can be used to prevent this.

END;

json_out := json_out & return_code(0);

END
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
CREATE OR REPLACE FUNCTION records.apiv_data_keys(
	IN	json_in				JSONB
,	OUT json_out			JSONB
) AS
$$
DECLARE
BEGIN

json_out :=

	('sum_of_class'+>('class'+>(select jsonb_agg(distinct class) from records.transactions)))
&	('sum_of_type'+>('type'+>(select jsonb_agg(distinct type) from records.transactions)))
&	('sum_of_period'+>'')
&	('agg_by_class'+>'')
&	('agg_by_type'+>'')
&	('agg_by_period'+>('grouping'+>(?'records.agg_by_period.groups')::text[]))
;


END;
$$
LANGUAGE PLPGSQL
VOLATILE
;

CREATE OR REPLACE FUNCTION records.apiv_data_keys(
	OUT json_out			JSONB
) AS
$$
BEGIN

json_out := records.apiv_data_keys('{}');

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
CREATE OR REPLACE FUNCTION records.apiv_data(
	IN	json_in				JSONB
,	OUT json_out			JSONB
) AS
$$
DECLARE
	selections JSONB := json_in->'selections';
	date_selected	DATE;
BEGIN

-- validation
CASE
	WHEN NOT json_in?'date_cursor' 
		THEN json_out := return_code(-1200); RETURN;
	WHEN NOT json_in?'user_id' 
		THEN json_out := return_code(-1201); RETURN;
	WHEN NOT json_in?'selections' 
		THEN json_out := return_code(-1203); RETURN;
	WHEN NOT (json_in->'selections')?|(?'records.selectors')::text[]
		THEN json_out := return_code(-1205); RETURN;
	WHEN 
			selections?'agg_by_period' -- if agg_by_period is specified...
		AND NOT strict(selections->'agg_by_period'->>'grouping' <@ (?'records.agg_by_period.groups')::text[]) -- then it should also be acceptable
		THEN json_out := return_code(-1204); RETURN;
	ELSE
END CASE;


drop table IF EXISTS base;
create temp table base as
select
	date_cursor
,	class
,	type
,	value
from records.transactions
join records.dir using (quovo_id)
where
	user_id = json_in->#'user_id'
order by date_cursor, class, type, value
;

SELECT
	max(date_cursor)
into date_selected
FROM base
where
	date_cursor <= json_in->@'date_cursor'
;

-- Each one of the selection types corresponds to a query group below here.
WITH

	agg_by_class as
(
select
	class as name
,	sum(value) as total_value
from base
where
	date_cursor = date_selected
group by class
order by total_value
)
,	agg_by_type as
(
select
	type as name
,	sum(value) as total_value
from base
where
	date_cursor = date_selected
group by type
order by total_value
)

,	agg_by_period as
(
select
	period as name
,	sum(value) as total_value
from (
	select
		to_char(date_cursor, (selections->'agg_by_period'->>'grouping')) as period
	,	value
	from base
	where
		date_cursor <= json_in->@'date_cursor'
)	sub
group by period
order by total_value
)


,	sum_of_type as
(
select
	sum(value) as total_value
from base
where
	date_cursor = date_selected
and type = selections->'sum_of_type'->>'type'
)
,	sum_of_class as
(
select
	sum(value) as total_value
from base
where
	date_cursor = date_selected
and type = selections->'sum_of_class'->>'class'
)
,	sum_of_period as
(
select
	sum(value) as total_value
from base
where
	date_cursor = date_selected
)

-- Each query group will be called IF the selection is specified in the request. 
-- Query groups are *only* run if the case condition is true, therefore only executing the queries that are actually requested (optimization).
,	report as
(
select
	case when selections ?'agg_by_class' then (select jsonb_agg(row_to_json(agg_by_class)) from agg_by_class) ELSE NULL END agg_by_class
,	case when selections ?'agg_by_type' then (select jsonb_agg(row_to_json(agg_by_type)) from agg_by_type) ELSE NULL END agg_by_type
,	case when selections ?'agg_by_period' then (select jsonb_agg(row_to_json(agg_by_period)) from agg_by_period) ELSE NULL END agg_by_period

,	case when selections ?'sum_of_type' then (select jsonb_agg(row_to_json(sum_of_type)) from sum_of_type) ELSE NULL END sum_of_type
,	case when selections ?'sum_of_class' then (select jsonb_agg(row_to_json(sum_of_class)) from sum_of_class) ELSE NULL END sum_of_class
,	case when selections ?'sum_of_period' then (select jsonb_agg(row_to_json(sum_of_period)) from sum_of_period) ELSE NULL END sum_of_period
)
select
	json_strip_nulls(row_to_json(report))
into json_out
from report
;

IF empty(json_out) THEN
	json_out := return_code(-1206);
	RETURN;
END IF;

DROP TABLE base;

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
CREATE OR REPLACE FUNCTION records.apiv_dashboard(
	IN	json_in				JSONB
,	OUT json_out			JSONB
) AS
$$
DECLARE
	loop_n 		INT := 0;
	dashes		JSONB;
	each_dash 	JSONB;
BEGIN

-- validation
CASE
	WHEN NOT json_in?'date_cursor' 
		THEN json_in := json_in & ('date_cursor'+>current_date);
	WHEN NOT json_in?'user_id' 
		THEN json_out := return_code(-1201); RETURN;
	WHEN NOT (json_in->>'user_mode') <@ ((?'users.classes')::text[]) -- default user_mode is client
		THEN json_in := json_in & ('user_mode'+>'client');
	ELSE
END CASE;


-- grab the dashboard definition for the given user_mode
SELECT
	dashboard->(json_in->>'user_mode') INTO dashes
FROM users.dir 
WHERE
	user_id = json_in->#'user_id'
;

CASE 
	WHEN json_in->>'debug' = 'array_size' 
		THEN json_out := 'array_size'+>(jsonb_array_length(dashes)); return;
	WHEN json_in->>'debug' = 'whats_dashes' 
		THEN json_out := dashes; return;
ELSE
END CASE;

-- prime appending in array mode:
json_out := '[]';


-- for each widget in the dashboard, get its respective data
-- TODO: this needs to be optimized because loops are potentially calling redundant requests
WHILE loop_n < jsonb_array_length(dashes) LOOP

	each_dash := dashes->loop_n;
	
	json_out := 
		json_out
	&	(
			each_dash -- the original dashboard widget info
		&	('data'+>records.apiv_data(
					json_in#&'{user_id,date_cursor}' -- include the date_cursor and user_id
				&	each_dash -- pull the selection specs from the dashboard's saved config
				)
			)
		)
	;
	loop_n := loop_n + 1;

END LOOP;

END;
$$
LANGUAGE PLPGSQL
VOLATILE
;/*
This routine goes through all the existing functions and identifies those that are intended to 
be publically accessible (apiv_* or apix_*) and sets everything up for them automatically. This includes
first allowing access to the appropriate schemas, then granting execution of each function and adding the 
security definer clause so that those functions can be executed without needing to extend any other 
privilages.
*/


CREATE OR REPLACE FUNCTION set_api_permissions() RETURNS VOID AS
$$
DECLARE
	each_function 	TEXT;
	schemas			TEXT;
	domains			TEXT;
BEGIN


-- clear out all privileges to start things from a clean slate
REASSIGN OWNED BY admin_rwx, prod_rx TO the_architect;


select
	string_agg(schema_name,',') into schemas
from information_schema.schemata
where
	schema_owner = 'the_architect'
;
select
	string_agg(domain_name,', ') into domains
from information_schema.domains
where
	domain_schema = 'global'
;

execute 
		'GRANT ALL ON ALL TABLES IN SCHEMA' & schemas & 'to admin_rwx, prod_rx;'
	&	'GRANT ALL ON ALL SEQUENCES IN SCHEMA' & schemas & 'to admin_rwx, prod_rx;'
	&	'GRANT ALL ON DOMAIN' & domains & 'to admin_rwx, prod_rx;'
	&	'GRANT EXECUTE ON ALL FUNCTIONS IN SCHEMA '& schemas &'to admin_rwx, prod_rx;'
	&	'GRANT USAGE ON SCHEMA'& schemas &'to admin_rwx, prod_rx;'
;


-- ALLOW apiv_ and apix_ functions execution:
FOR each_function IN (
-- 
with
	base as
(
SELECT 
	routines.routine_name
,	parameters.data_type
,	parameters.ordinal_position
,	routines.specific_schema
,	parameter_mode

FROM information_schema.routines
left JOIN information_schema.parameters using (specific_name)
WHERE 
	(	routines.routine_name like 'apiv_%'
	OR 	routines.routine_name like 'apix_%'
	)
	OR	routines.routine_name <@ '{setting_update}'

)
,	report as
(
select
	specific_schema
,	routine_name
,	COALESCE(string_agg(data_type, ', ' order by ordinal_position) FILTER (WHERE (parameter_mode = 'IN' OR parameter_mode IS NULL)) ,'') as args
from base
group by specific_schema, routine_name
)
select
	specific_schema||'."'||routine_name ||'"('|| args || ')'
from report

--

) LOOP

	execute 
	'	GRANT EXECUTE ON FUNCTION '|| each_function ||' to admin_rwx, prod_rx, testing_rx;
		ALTER FUNCTION '|| each_function ||' SECURITY DEFINER;
	';

END LOOP;


-- ALLOW operator functions to be used:
FOR each_function IN (
-- 
SELECT
	'"'||prc.proname ||'"('|| array_to_string(array[lt.typname,rt.typname],',') || ')'
FROM pg_operator op
LEFT JOIN pg_namespace ns ON ns.oid = op.oprnamespace
LEFT JOIN pg_type lt ON lt.oid = op.oprleft
LEFT JOIN pg_type rt ON rt.oid = op.oprright
LEFT JOIN pg_proc prc ON prc.oid = op.oprcode
WHERE
	ns.nspname = 'global'
and prc.proname IS NOT NULL


--

) LOOP

	execute 
	'	GRANT EXECUTE ON FUNCTION '|| each_function ||' to admin_rwx, prod_rx, testing_rx;
		-- ALTER FUNCTION '|| each_function ||' SECURITY DEFINER
		;
	';

END LOOP;


END;
$$
LANGUAGE PLPGSQL
VOLATILE
;


CREATE OR REPLACE VIEW user_priviledges AS
with
	base as
(
select
	privilege_type & 'ON' & table_schema ||'.'||column_name & 'TO' & grantee
,	privilege_type
,	grantee
from information_schema.column_privileges

UNION

select
	privilege_type & 'ON' & specific_schema ||'.'||routine_name & 'TO' & grantee
,	privilege_type
,	grantee
from information_schema.routine_privileges

UNION

select
	privilege_type & 'ON' & table_schema ||'.'||table_name & 'TO' & grantee
,	privilege_type
,	grantee
from information_schema.table_privileges

UNION

select
	privilege_type & 'ON' & udt_schema ||'.'||udt_name & 'TO' & grantee
,	privilege_type
,	grantee
from information_schema.udt_privileges

UNION

select
	privilege_type & 'ON' & object_schema ||'.'||object_name & 'TO' & grantee
,	privilege_type
,	grantee
from information_schema.usage_privileges

order by grantee, privilege_type
)

select
	*
from base

;

/*
todo:
- view to see all user privilages
- way to revoke all privs from users
- setup a group for all dbAPI users
- 


*/



DO $$
BEGIN

PERFORM set_api_permissions();

END;
$$
LANGUAGE PLPGSQL
;DO

$$
DECLARE
  loop_n int :=0;
BEGIN

IF current_setting('dbAPI.environment') != 'dev' THEN return;
ELSE END IF;

perform users.apix_create(('email'+>'1@gmail.com')&('name_first'+>'alexi')&('name_last'+>'theodore'));
perform users.apix_create(('email'+>'2@gmail.com')&('name_first'+>'alexi')&('name_last'+>'theodore'));
perform users.apix_create(('email'+>'3@gmail.com')&('name_first'+>'alexi')&('name_last'+>'theodore'));

DELETE FROM records.transactions;

while loop_n < 10000 LOOP

perform 
  records.apix_import_transactions(
@('{
  "debugg":"TRUE"
, "date_cursor":"'|| (current_date + loop_n)::text||'"
, "quovo_users" : [
      {
         "username" : "1",
         "id" : 3962346,
         "name" : null,
         "email" : null,
         "value" : null
      },
      {
         "username" : "2",
         "id" : 4059262,
         "name" : null,
         "value" : 6048.38,
         "email" : null
      },
      {
         "id" : 3962333,
         "username" : "3",
         "name" : null,
         "email" : null,
         "value" : null
      }
   ]
, "quovo_holdings": [
    {
      "account_id": 10652165,
      "basic_asset_class": "Cash & Cash Equivalent",
      "connection_id": 5342267,
      "currency": null,
      "forex_rate": 1,
      "id": 1642730054,
      "price": 1,
      "quantity": 0.77,
      "symbol": "CUR:USD",
      "symbol_name": "U S Dollar",
      "type": "Cash",
      "type_confidence": "Very High",
      "user_id": 3962346,
      "value": 0.77
    },
    {
      "account_id": 10652163,
      "basic_asset_class": "Equity",
      "connection_id": 5342267,
      "currency": null,
      "forex_rate": 1,
      "id": 1642730055,
      "price": 433.01,
      "quantity": 5,
      "symbol": "CMG",
      "symbol_name": "Chipotle Mexican Grill Inc.",
      "type": "Equity",
      "type_confidence": "Very High",
      "user_id": 3962346,
      "value": 2165.05
    },
    {
      "account_id": 10652164,
      "basic_asset_class": "Other",
      "connection_id": 5342267,
      "currency": null,
      "forex_rate": 1,
      "id": 1642730069,
      "price": 116.51,
      "quantity": 16,
      "symbol": "CRBN",
      "symbol_name": "iShares MSCI ACWI Low Carbon Target",
      "type": "ETF",
      "type_confidence": "Very High",
      "user_id": 3962346,
      "value": 1864.16
    },
    {
      "account_id": 10652164,
      "basic_asset_class": "Multi-Asset",
      "connection_id": 5342267,
      "currency": null,
      "forex_rate": 1,
      "id": 1642730077,
      "price": 71.1041,
      "quantity": 27,
      "symbol": "SHE",
      "symbol_name": "SPDR Series Trust SSGA Gender Diversity Index",
      "type": "ETF",
      "type_confidence": "Very High",
      "user_id": 3962346,
      "value": 1919.8107
    },
    {
      "account_id": 10652164,
      "basic_asset_class": "Fixed Income",
      "connection_id": 5342267,
      "currency": null,
      "forex_rate": 1,
      "id": 1642730079,
      "price": 1.23192,
      "quantity": 10000,
      "symbol": "87612EAK2",
      "symbol_name": "Target Corp Debentures 6.35% 11/01/2032",
      "type": "Bond",
      "type_confidence": "Very High",
      "user_id": 3962346,
      "value": 12319.2
    }
  ]
}')

)
;

loop_n := loop_n + 1;

END LOOP;


END;
$$
LANGUAGE PLPGSQL
;

DROP SCHEMA IF EXISTS testing CASCADE;
CREATE SCHEMA testing;

GRANT USAGE ON SCHEMA testing to testing_rx, prod_rx;
GRANT SELECT ON ALL TABLES IN SCHEMA testing to testing_rx, prod_rx;
GRANT REFERENCES ON ALL TABLES IN SCHEMA testing to testing_rx, prod_rx;
GRANT EXECUTE ON ALL FUNCTIONS IN SCHEMA testing to testing_rx, prod_rx;

/*
----------
	||	 
	||	 
	||	 
*/
CREATE TABLE testing.funct_tests 
(
	id				TEXT	PRIMARY KEY
,	exec_order		SERIAL
,	sql_code		TEXT	NOT NULL
,	sql_checker		TEXT
,	rollback_point	TEXT	NOT NULL DEFAULT ''
,	pg_user			NAME 	CHECK(pg_user IN ('testing_rx', 'testing_rwx')) DEFAULT 'testing_rx'
)
;



/*
_|_|_|_|
_|
_|_|_|
_|
_|
*/
CREATE OR REPLACE FUNCTION testing.exec_funct_tests(IN flags JSONB DEFAULT '{}') returns JSONB AS
$$
DECLARE
	funct_test		RECORD;
	report			JSONB;
	errors			JSONB;
	test_result 	JSONB;
	check_pass		BOOLEAN;
	savepoint		TEXT;
	transcript		TEXT;
	exec_code		TEXT;
	pg_user_name 	NAME;

BEGIN

/*
This function goes through the testing.funct_tests table and executes each test SQL code. The SQL is executed and the results are sequentially appended to a global transcript where they can be referenced at another time by any future test as variables in the SQL that will be filled in prior to execution. If the execution test fails, the SQL error is captured in a report. Otherwise, each test response is evaluated based on a supplied test. If the result of that test is FALSE, the error is captured in a report. Lastly, each test is preceeded by a transaction snapshot. If specified, after a test execution is run, the transaction can be rolled back to any prior transaction, as specified.
*/

errors := '[]';

<<clear>>
BEGIN
	perform dblink_disconnect('funct_tests');
EXCEPTION when others THEN
	EXIT clear;

END;

perform dblink_connect('funct_tests','dbname='||current_database()||' user=the_architect');

-- initializing; these are used throughout the function
exec_code := 
	'ROLLBACK; BEGIN;'
||	'GRANT prod_rx to testing_rx' -- the testing user(s) only have permissions during tests. The rest of the time, they have no permissions.
;

transcript := exec_code;

perform dblink_exec('funct_tests', exec_code); -- put everything to follow inside a transaction block

FOR funct_test in (
	SELECT * FROM testing.funct_tests 
	WHERE 
		(CASE 
			WHEN flags?'id_pattern' THEN
				id like flags->>'id_pattern'
			ELSE TRUE
		END
		)
	ORDER By exec_order
	)
LOOP

transcript := transcript || chr(10) || chr(10) || '-- ' || funct_test.id;

RAISE INFO 'Test: %', funct_test.id;

-- each test begins with a savepoint named for it
savepoint := 'SAVEPOINT funct_test_'||funct_test.exec_order||';';

-- replace variables
funct_test.sql_code := replace_variables(funct_test.sql_code,report);

-- execute test code
BEGIN
	-- pg_user_name := funct_test.pg_user;
	-- SET LOCAL ROLE pg_user_name; -- set the designated role to be executing the tests
	exec_code := 
			savepoint || chr(10) -- start with the savepoint
		||	'SET SESSION ROLE ' || funct_test.pg_user || ';' || chr(10)
		||	trim(both chr(10) from funct_test.sql_code); -- this is the code to execute
	transcript := transcript || chr(10) || exec_code; -- append it to the transcript log

	select * INTO test_result from dblink ('funct_tests', exec_code) as f(json_out JSONB); -- execute

	transcript := transcript || chr(10) || 'return code: ' || COALESCE(test_result->>'return_code', 'NO RESULT');

	report := report & (funct_test.id+>test_result); 

	EXCEPTION when others THEN 
		RAISE INFO 
			'⤷ FAILED TEST EXEC.% %'
		,	chr(10)||'SQL Code: ' || funct_test.sql_code
		,	chr(10)||'SQL Error: ' || SQLERRM
		;
		errors := jsonb_array_append(errors,(funct_test.id+>(('SQL ERROR'+>SQLERRM)&('SQL'+>funct_test.sql_code))));
		exec_code := 'ROLLBACK TO  ' || savepoint;
		transcript := transcript || chr(10) || exec_code || ' -- SQL ERROR!';

		perform dblink_exec('funct_tests', exec_code);
-- 		⤷ ROLLBACK if there was an error
		report := report & (funct_test.id+>'NO RESULT');
		return ('report'+>report)&('errors'+>errors)&('transcript'+>transcript); --continue; 
-- 		⤷ skip everything else if the test itself had an exec error
END;

-- execute return check
<<return_check>>
BEGIN

	IF funct_test.sql_checker IS NULL THEN 
		report := jsonb_insert(report, array[funct_test.id::TEXT,'check_pass'], to_jsonb('NO TEST SPECIFIED'::text));
	--	⤷ you don't always have to check the results for the test to be useful/successful

		-- exec_code := '; ROLLBACK TO  ' || savepoint; -- not sure why I did this?
		-- transcript := transcript || chr(10) || exec_code;

		-- perform dblink_exec('funct_tests', exec_code);
		exit return_check;
	END IF;

	-- build the check code by replacing the %return% placeholder with the test results JSON
	funct_test.sql_checker := replace(funct_test.sql_checker, '%return%', quote_literal(COALESCE(test_result,'{}'))||'::JSONB');
	
	-- build exec_code and transcript 
	exec_code := trim(both chr(10) from funct_test.sql_checker);
	transcript := transcript || chr(10) || exec_code;

	-- run the check code and collect the boolean result
	select * INTO check_pass from dblink ('funct_tests', exec_code) as f(check_pass BOOLEAN);

	-- when a check fails:
	IF check_pass IS NOT TRUE THEN
		RAISE INFO 
			'⤷ FAILED CHECK.% % %'
		,	chr(10)||'SQL TEST Code: ' || funct_test.sql_code
		,	chr(10)||'SQL CHECK Code: ' || funct_test.sql_checker
		,	chr(10)||'SQL CHECK Result: ' || test_result::text
		;

		errors := jsonb_array_append(errors, ('id'+>funct_test.id) & ('check_pass'+>'FAILED TEST!') & ('check SQL'+>to_jsonb(funct_test.sql_checker)) & ('test SQL'+>to_jsonb(funct_test.sql_code)) & (test_result#'return_code'));
		return ('report'+>report)&('errors'+>errors)&('transcript'+>transcript); --continue; 
	END IF;

	report := jsonb_insert(report, array[funct_test.id::TEXT,'check_pass'], to_jsonb((check_pass = TRUE)?'{PASSED,FAILED}'));

	-- when the check itself errors out:
	EXCEPTION when others THEN 
		RAISE INFO 
			'⤷ FAILED CHECK EXEC.% %'
		,	chr(10)||'SQL Code: ' || funct_test.sql_code
		,	chr(10)||'SQL Error: ' || SQLERRM
		;

		errors := jsonb_array_append(errors, ('check_pass'+>SQLERRM)&('SQL'+>funct_test.sql_checker));
		report := jsonb_insert(report, array[funct_test.id::text,'check_pass'], to_jsonb('SQL ERROR'::text));

		exec_code := 'ROLLBACK TO ' || savepoint;
		transcript := transcript || chr(10) || exec_code;

		perform dblink_exec('funct_tests', exec_code);
-- 		⤷ ROLLBACK if there was an error

END; -- end of check code


-- if a rollback is specified, then do so:
IF funct_test.rollback_point != '' THEN 

	exec_code := 'ROLLBACK TO SAVEPOINT funct_test_' || (SELECT exec_order from testing.funct_tests where id = funct_test.rollback_point)||';';
	transcript := transcript || chr(10) || exec_code;

	perform dblink_exec('funct_tests',exec_code); 
-- 		⤷ ROLLBACK here if required
END IF;

END LOOP;


IF empty(flags->'pause') THEN 
	exec_code := 'ROLLBACK;'; 
	transcript := transcript || chr(10) || exec_code;
	perform dblink_exec('funct_tests', exec_code);
ELSE
END IF;


perform dblink_disconnect('funct_tests');

return ('report'+>report)&('errors'+>errors)&('transcript'+>transcript);

END;
$$
LANGUAGE PLPGSQL
VOLATILE
;


DELETE FROM testing.funct_tests;


--> Bookmark ft_functional_test_system
INSERT INTO testing.funct_tests (id, sql_code, sql_checker, rollback_point, pg_user) VALUES


-- (
-- 	'functional_test_name'
-- ,	$sql_code$

-- 	$sql_code$
-- ,	$sql_check$

-- 	$sql_check$
-- ,	'rollback_point (functional_test_name)'
	-- optional*
	-- occurs AFTER the check code is run
-- ,	'exec_user: testing_rx | prod_rx | dev_rx'
-- )




/*
funct TESTING (tests the funct testing system before anything else)
*/
(
	'funct_testing/1/basic' -- test basic functionality of sql test
,	$sql_code$
select 'test'+>'success';
	$sql_code$
,	$sql_check$
select (%return%->>'test') = 'success'
	$sql_check$
,	''
,	'testing_rx'
)
-----------------------------
,(
	'funct_testing/1/rollback/1' -- check basic functionality of an execution BEFORE a rollback
,	$sql_code$
select 'new_setting'+>setting_update('testing.dummy','rollback');
	$sql_code$
,	$sql_check$
select ?'testing.dummy' = 'rollback'
	$sql_check$
,	'funct_testing/1/rollback/1'
,	'testing_rx'
)

-----------------------------
,(
	'funct_testing/1/rollback/2' -- check basic functionality of an execution AFTER a rollback
,	$sql_code$
select 'test'+>'success'; -- this is a dummy check really
	$sql_code$
,	$sql_check$
select ?'testing.dummy' != 'rollback'
	$sql_check$
,	''
,	'testing_rx'
)

;



--> Bookmark ft_users
INSERT INTO testing.funct_tests (id, sql_code, sql_checker, rollback_point, pg_user) VALUES
-----------------------------
(
	'users/apix_create/proper/1' -- 
,	$sql_code$
select users.apix_create(('email'+>'funct_testing@gmail.com')&('name_first'+>'alexi')&('name_last'+>'theodore'))
	$sql_code$
,	$sql_check$
select return_code(%return%) = 0;
	$sql_check$
,	''
,	'testing_rx'
)
-----------------------------
,(
	'users/apix_create/inproper/duplicate' -- 
,	$sql_code$
-- this is just the same code as the previous, but repeated to check for unique errors
select users.apix_create(('email'+>'funct_testing@gmail.com')&('name_first'+>'alexi')&('name_last'+>'theodore'))
	$sql_code$
,	$sql_check$
select return_code(%return%) = -1102;
	$sql_check$
,	''
,	'testing_rx'
)
-----------------------------
,(
	'users/apix_update/proper/name_first' -- 
,	$sql_code$
select users.apix_update((@'%users/apix_create/proper/1%'#'user_id')&('name_first'+>'bob'))
-- just updating the name_first
	$sql_code$
,	$sql_check$
select return_code(%return%) = 0;
	$sql_check$
,	''
,	'testing_rx'
)
-----------------------------
,(
	'users/apix_update/proper/password' -- 
,	$sql_code$
select users.apix_update((@'%users/apix_create/proper/1%'#'user_id')&('password'+>'kjabsdljabsdjlbasdjbasd'))
-- just updating the name_first
	$sql_code$
,	$sql_check$
select return_code(%return%) = 0;
	$sql_check$
,	''
,	'testing_rx'
)
-----------------------------
,(
	'users/apix_update/inproper/invalid_email' -- 
,	$sql_code$
select users.apix_update((@'%users/apix_create/proper/1%'#'user_id')&('email'+>'funct_testinggmail.com'))
-- trying to update the email with an invalid one, which should fail
	$sql_code$
,	$sql_check$
select return_code(%return%) = -1101;
	$sql_check$
,	''
,	'testing_rx'
)
-----------------------------
,(
	'users/apiv_details/contents' -- checking structural results 
,	$sql_code$
select users.apiv_details((@'%users/apix_create/proper/1%'#'user_id'))
	$sql_code$
,	$sql_check$
select (%return%)?&array_agg(column_name::text) from information_schema.columns where table_schema = 'users' and table_name = 'dir'
-- this is actually a pretty poor test, but a good test is difficult and requires more maintenance
-- a "good test" would check against a hard-coded return standard
	$sql_check$
,	''
,	'testing_rx'
)
;


/*
*/

--> Bookmark ft_records
INSERT INTO testing.funct_tests (id, sql_code, sql_checker, rollback_point, pg_user) VALUES
-----------------------------
(
	'records/apix_fill_survey/proper/1' -- 
,	$sql_code$
select records.apix_fill_survey(
		@'{
			"debugg":"TRUE"
		,	"date_cursor":"2018-02-02"
		,	"survey_json":{"is_ira_contr":true,"is_401k_contr":false,"ira_contr":123,"question4":12,"1040_L14a":12,"question3":12,"question2":12}
		}'
	&	(@'%users/apix_create/proper/1%'#'user_id')
);
	$sql_code$
,	$sql_check$
select return_code(%return%) = 0;
	$sql_check$
,	''
,	'testing_rx'
)
-----------------------------
,(
	'records/apix_import_transactions/proper/1' -- 
,	$sql_code$
select records.apix_import_transactions(
@('{
	"debugg":"TRUE"
,	"date_cursor":"2018-02-02"
,	"quovo_users" : [
      {
         "username" : "'||(@'%users/apix_create/proper/1%'->>'user_id')||'",
         "id" : 0,
         "name" : null,
         "email" : null,
         "value" : null
      }
   ]
}')

& 
'{
	"quovo_holdings": [
      {
         "basic_asset_class" : "Cash & Cash Equivalent",
         "user_id" : 0,
         "quantity" : 3594.9,
         "forex_rate" : 1,
         "price" : 1,
         "connection_id" : 5342511,
         "symbol_name" : "U S Dollar",
         "account_id" : 10652631,
         "id" : 1725489176,
         "currency" : null,
         "symbol" : "CUR:USD",
         "type" : "Cash",
         "value" : 3594.9,
         "type_confidence" : "Very High"
      },
      {
         "symbol" : "CUR:USD",
         "currency" : null,
         "id" : 1725489177,
         "account_id" : 10652632,
         "symbol_name" : "U S Dollar",
         "type_confidence" : "Very High",
         "type" : "Cash",
         "value" : 1105.43,
         "user_id" : 0,
         "quantity" : 1105.43,
         "basic_asset_class" : "Cash & Cash Equivalent",
         "connection_id" : 5342511,
         "forex_rate" : 1,
         "price" : 1
      },
      {
         "forex_rate" : 1,
         "price" : 1,
         "connection_id" : 5342267,
         "basic_asset_class" : "Cash & Cash Equivalent",
         "quantity" : 0.77,
         "user_id" : 0,
         "type" : "Cash",
         "value" : 0.77,
         "type_confidence" : "Very High",
         "account_id" : 10652165,
         "symbol_name" : "U S Dollar",
         "symbol" : "CUR:USD",
         "id" : 1734807791,
         "currency" : null
      }
   ]
}'
);
	$sql_code$
,	$sql_check$
select return_code(%return%) = 0;
	$sql_check$
,	''
,	'testing_rx'
)
-----------------------------
,(
	'records/apiv_data_keys/1' -- 
,	$sql_code$
select records.apiv_data_keys('{}');
	$sql_code$
,	$sql_check$
select 
	(%return%)?&'{sum_of_class,sum_of_type,sum_of_period,agg_by_class,agg_by_type,agg_by_period}' -- all these keys are present
AND (%return%)?&!'{sum_of_class,sum_of_type,sum_of_period,agg_by_class,agg_by_type,agg_by_period}' -- only these keys are present
;
	$sql_check$
,	''
,	'testing_rx'
)
-----------------------------
,(
	'records/apiv_data/proper/1' -- 
,	$sql_code$
select records.apiv_data(
	'{"selections":{"sum_of_period":{"grouping":"YYYY-DDD"}},"date_cursor":"2018-02-02"}'
&	(@'%users/apix_create/proper/1%'#'user_id')
);
	$sql_code$
,	$sql_check$
select 
	%return%->'sum_of_period'->0->##'total_value' = 4701.10
;
	$sql_check$
,	''
,	'testing_rx'
)


/*
*/

;

select
	((testing.exec_funct_tests('id_pattern_off'+>'/%')->'errors')-'check SQL'**)::text as errors
into temp the_void
;


/*
-- This gives a listing of all api* functions which do not appear to have any explicit functional tests yet. --

with
	api_fs as
(
SELECT
	case_false(specific_schema||'.','global.','') ||routine_name as fnctn
FROM information_schema.routines 
WHERE
	routine_type='FUNCTION'
AND left(routine_name, 4) in ('apiv','apix')
)
,	uses as
(
select
	distinct fnctn
from testing.funct_tests, api_fs
where
	position(fnctn in sql_code) > 0
)
select
	fnctn
from api_fs
where
	fnctn not in (select * from uses)
order by fnctn
;

*/