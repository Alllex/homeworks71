-- UPDATE

-- Insert new city
INSERT INTO P8_City SELECT MAX(City_ID) + 1, 'City N', 7 FROM P8_City
;

-- Fresh news: area of river Amazon has changed!
UPDATE P8_Pond SET 
	Area = 600 
WHERE 
	Name LIKE 'Amazon'
;

-- Set rating of cimena Gaumont to the most average rating in the world!
UPDATE P8_Showplace SET 
	Rating = (SELECT CAST(AVG(Rating) AS INT) FROM P8_Showplace)
WHERE
	Name LIKE 'Gaumont'
;

-- Let's free our world from weird showplaces (frankly, the only one)
DELETE FROM P8_Showplace
WHERE
	Rating = (SELECT MIN(Rating) FROM P8_Showplace)
;

-- Ruin pair of cities and their ponds and showplaces
ALTER TABLE P8_Pond DROP CONSTRAINT FK_P8_Pond_City;
ALTER TABLE P8_Pond ADD CONSTRAINT FK_P8_Pond_City
    FOREIGN KEY (City_ID)
    REFERENCES P8_City(City_ID)
    ON DELETE CASCADE
;

ALTER TABLE P8_Showplace DROP CONSTRAINT FK_P8_Showplace_City;
ALTER TABLE P8_Showplace ADD CONSTRAINT FK_P8_Showplace_City
    FOREIGN KEY (City_ID)
    REFERENCES P8_City(City_ID)
    ON DELETE CASCADE
;

DELETE P8_City WHERE Name = 'Oxford'; 
DELETE P8_City WHERE Name = 'Moscow';