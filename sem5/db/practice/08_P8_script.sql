-- VIEW

-- View of City and its Language
CREATE VIEW V_City_Lang (CityName, LangName) AS 
	SELECT city.Name, lang.Name FROM 
		P8_City AS city
		JOIN P8_Country AS country ON city.Country_ID = country.Country_ID
		JOIN P8_Language AS lang ON country.Language_ID = lang.Language_ID		
;


-- Show all cities speak Mandarin
SELECT * FROM V_City_Lang
WHERE 
	LangName = 'Mandarin'
;

-- There's a epidemic in India - hindi speak people city in danger
UPDATE V_City_Lang SET 
	CityName = '!!!' + CityName + '!!!'
WHERE 
	LangName = 'Hindi'
;
	
DROP VIEW V_City_Lang;

-------------------------------------------------------------------------------------------------------

-- View of city and it's average rating of show places
CREATE VIEW V_City_Rating (CityID, Rating) AS 
	SELECT city.City_ID, (SELECT AVG(RATING) FROM P8_Showplace AS show WHERE show.City_ID = city.City_ID) FROM 
		P8_City AS city
;

-- Cities with showplaces in it and its rating
SELECT (SELECT Name FROM P8_City WHERE City_ID = CityID) AS 'City', Rating FROM V_City_Rating 
WHERE 
	Rating IS NOT NULL
;

-- Countries where exist city with maximal average rating
SELECT Name FROM P8_Country AS country
WHERE
	EXISTS (SELECT * FROM P8_City WHERE 
		P8_City.Country_ID = country.Country_ID AND
		(SELECT Rating FROM V_City_Rating WHERE CityID = P8_City.City_ID) = (SELECT MAX(Rating) FROM P8_Showplace)
	)
;

DROP VIEW V_City_Rating;

-------------------------------------------------------------------------------------------------------

-- View of Country, City and Showplaces names for reports
CREATE VIEW V_Country_City_Place (Country, City, Place) AS 
	SELECT country.Name, city.Name, show.Name FROM 
		P8_City AS city
		JOIN P8_Country AS country ON city.Country_ID = country.Country_ID
		JOIN P8_Showplace AS show ON city.City_ID = show.City_ID		
;

-- Database human-readble report of all showplaces
SELECT * FROM V_Country_City_Place
ORDER BY
	Country,
	City,
	Place
;

-- Count of showplaces in each country
SELECT Country, COUNT(Place) AS 'Amount of showplaces' FROM V_Country_City_Place
GROUP BY
	Country
ORDER BY
	COUNT(Place) DESC
;

DROP VIEW V_Country_City_Place;