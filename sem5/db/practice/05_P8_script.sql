-- GROUP BY and HAVING

-- !!!
-- PLEASE RECREATE TABLE BY DROPPING TABLES AND RUNNING SCRIPT P8_02_script AGAIN!!!
-- NEW DATA INSERTED
-- !!!

-- Languages with average rating 
SELECT lang.Name, AVG(place.Rating) as 'Average rating' FROM
	P8_Showplace as place
	JOIN P8_Language as lang ON place.Language_ID=lang.Language_ID
GROUP BY
	lang.Language_ID,
	lang.Name
HAVING
	AVG(place.Rating)>3
ORDER BY
	AVG(place.Rating) DESC
; 

-- Count different types of showplaces
SELECT Type, COUNT(*) as 'Count' FROM 
	P8_Showplace 
GROUP BY Type
ORDER BY COUNT(*) DESC
;

-- Cities where more than one showplace
SELECT P8_City.Name, COUNT(*) FROM
	P8_Showplace
	JOIN P8_City ON P8_Showplace.City_ID=P8_City.City_ID
GROUP BY P8_City.City_ID,
		 P8_City.Name
HAVING COUNT(*) > 1
;

-- Find theatres even if character case is different
SELECT * FROM
	P8_Showplace
WHERE 
	LOWER(Type) LIKE 'theatre'
;

-- Showplaces of specific type with the most lengthy name
SELECT Type, MAX(LEN(Name)) as 'Name length' FROM
	P8_Showplace
GROUP BY
	Type
ORDER BY
	MAX(LEN(Name)) DESC
;

