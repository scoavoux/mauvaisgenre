COPY (
SELECT 
    mbid,
    area_name,
    area_type
FROM(
    SELECT
        gid as mbid,
        area as area_id
    FROM artist
    WHERE area is not null
    ) as artist
JOIN(
    SELECT
        id as area_id,
        type as type_id,
        name as area_name
    FROM area) as areas
    ON artist.area_id = areas.area_id
JOIN(
    SELECT
        id as type_id,
        name as type
    FROM area_type) as area_type
    ON areas.type_id = area_type.type_id
)
TO '/tmp/area_mauvaisgenre.csv' WITH CSV DELIMITER ',' HEADER;
