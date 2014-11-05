-- ADVANCED SELECT

-- Ponds and showplaces of Moscow
(
	SELECT Type, Name AS 'In Moscow' FROM P8_Showplace
	WHERE City_ID = (SELECT City_ID FROM P8_City WHERE Name LIKE 'Moscow')
)
UNION
(
	SELECT Type, Name FROM P8_Pond
	WHERE City_ID = (SELECT City_ID FROM P8_City WHERE Name LIKE 'Moscow')
)
;

-- Showplace full info
SELECT place.Type, place.Name, city.Name, country.Name, lang.Name FROM
	P8_Showplace AS place
	JOIN P8_City AS city ON place.City_ID=city.City_ID
	JOIN P8_Country AS country ON country.Country_ID=city.Country_ID
	JOIN P8_Language AS lang ON lang.Language_ID=place.Language_ID
;

-- Cities which have showplaces without website
SELECT Name FROM
	P8_City
WHERE
	City_ID IN (SELECT City_ID FROM P8_Showplace WHERE Website IS NULL)
;

-- Countries which have showplaces
SELECT Name FROM
	P8_Country
WHERE
	Country_ID = ANY (
		SELECT DISTINCT city.Country_ID FROM
			P8_City AS city
			JOIN P8_Showplace AS place ON city.City_ID=place.City_ID
	)
;

-- All showplaces of these cities have maximum rating
-- And there more than one showplace
SELECT city.Name FROM
	P8_City AS city
WHERE
	(SELECT MAX(Rating) FROM P8_Showplace) = 
		ALL (
			SELECT Rating FROM P8_Showplace AS place WHERE city.City_ID=place.City_ID
		)
	AND
	1 < (SELECT COUNT(*) FROM P8_Showplace AS place WHERE city.City_ID=place.City_ID)
;