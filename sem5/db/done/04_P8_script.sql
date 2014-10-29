-- SELECT JOIN --

-- Show cities of showplaces
SELECT P8_Showplace.Name, P8_City.Name FROM 
	P8_Showplace
	JOIN P8_City ON P8_Showplace.City_ID = P8_City.City_ID
WHERE
	P8_Showplace.Type LIKE 'Museum'
;

-- Cities in US
SELECT P8_City.Name FROM
	P8_City,
	P8_Country
WHERE
	P8_City.Country_ID = P8_Country.Country_ID AND
	P8_Country.Name = 'US'
;

-- Average country population which speaking English
SELECT AVG(CAST(P8_Country.Population AS DECIMAL)) FROM 
	P8_Country 
	JOIN P8_Language ON P8_Country.Language_ID = P8_Language.Language_ID 
WHERE 
	P8_Language.Name LIKE 'English'
;

-- Lakes with country and city ordered by area
SELECT P8_Country.Name, P8_City.Name, P8_Pond.Name, P8_Pond.Area FROM 
	P8_Country 
	JOIN P8_City ON P8_Country.Country_ID = P8_City.Country_ID
	JOIN P8_Pond ON P8_City.City_ID = P8_Pond.City_ID
WHERE 
	P8_Pond.Type LIKE 'Lake'
ORDER BY 
	P8_Pond.Area DESC
;

-- How many websites are registered in cities
SELECT P8_City.Name, COUNT(P8_Showplace.Website) FROM 
	P8_City 
	LEFT JOIN P8_Showplace ON P8_City.City_ID = P8_Showplace.City_ID
GROUP BY 
	P8_City.City_ID,
	P8_City.Name
;

