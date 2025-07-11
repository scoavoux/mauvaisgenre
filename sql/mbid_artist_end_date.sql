COPY (
  SELECT 
  gid as mbid,
  end_date_year
  FROM artist
  WHERE end_date_year IS NOT NULL
)
TO '/tmp/mbid_artist_end_date.csv' WITH CSV DELIMITER ',' HEADER;
