-----------------------------------------------------------
-- Select script --
-----------------------------------------------------------

-- Select all museums
SELECT Type, Name, Rating FROM P8_Showplace WHERE Type LIKE 'Museum';

-- Count of different languages in "our" world
SELECT COUNT(DISTINCT Country_ID) FROM P8_City;

-- List all countries sorted by population (descending) and area
SELECT Name, Population FROM P8_Country WHERE Area > 300 ORDER BY Population DESC, Area;

-- Select all showplaces which has website and the best rating
SELECT Name, Rating, Website FROM P8_Showplace WHERE Website IS NOT NULL AND Rating = (SELECT MAX(Rating) FROM P8_Showplace);

-- Find all lakes with specified area
SELECT Name, Area, Length FROM P8_Pond WHERE Type LIKE 'Lake' AND Area BETWEEN 50 AND 700;