
COPY (
--Now we look for languages associated with the works
SELECT 
    release_group_name,
    release_count,
    first_release_date_year,
    artist_position,
    mbid,
    primary_type_name,
    secondary_type_name
FROM(
--Start with release groups
(
	SELECT
		id as rg_id,
		name as release_group_name,
		type as pt_id,
		artist_credit as ac_id
	FROM release_group
) as rg

LEFT JOIN
--Artists
(
	SELECT 
		artist_credit as ac_id,
		position as artist_position,
		artist as artist_id
	FROM artist_credit_name
) as acn
ON rg.ac_id = acn.ac_id
LEFT JOIN
(
    SELECT
        id as artist_id,
        gid as mbid
    FROM artist
) as ar
ON acn.artist_id = ar.artist_id
LEFT JOIN
-- First dates
(
	SELECT
		id as rg_id,
		release_count,
		first_release_date_year
	FROM release_group_meta
) as rgm
ON rg.rg_id = rgm.rg_id
LEFT JOIN
--Primary type meaning
(
	SELECT 
		id as pt_id,
		name as primary_type_name
	FROM release_group_primary_type
) as rgpt
ON rg.pt_id = rgpt.pt_id
LEFT JOIN

--Secondary type affiliation
(
	SELECT
		release_group as rg_id,
		secondary_type as st_id
	FROM release_group_secondary_type_join
) as rgstj
ON rg.rg_id = rgstj.rg_id 
LEFT JOIN

--Secondary type meaning
(
	SELECT 
		id as st_id,
		name as secondary_type_name
	FROM release_group_secondary_type
) as rgst
ON rgstj.st_id = rgst.st_id
)
    )
TO '/tmp/mbid_release_group.csv' WITH CSV DELIMITER ',' HEADER;
