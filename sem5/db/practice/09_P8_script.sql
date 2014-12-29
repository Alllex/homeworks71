-- TRIGGERS

-- Checks whether new pond is Lake or River
ALTER TRIGGER P8_Language_Update ON P8_Pond FOR INSERT
AS
	IF ((SELECT Type FROM INSERTED) NOT IN ('River', 'Lake'))
		ROLLBACK;
		 
-- Test for the first trigger
INSERT INTO P8_Pond(Pond_ID, Type, Name, City_ID, Area, Width, Length) VALUES
	((SELECT MAX(Pond_ID) + 1 FROM P8_Pond), 'Laker', 'Ontario', 10, 100, 50, 2)
;	
	
DELETE FROM P8_Pond
WHERE
	Name LIKE 'Ontario'
;

-------------------------------------------

-- Shifts all IDs which 
CREATE TRIGGER P8_Pond_Delete ON P8_Pond FOR DELETE
AS
BEGIN
	DECLARE @RemovedID INT
	SET @RemovedID = (SELECT Pond_ID FROM DELETED)
	UPDATE P8_Pond SET 
		Pond_ID = Pond_ID - 1 
	WHERE 
		Pond_ID > @RemovedID
	; 
END
;
	
-- Test for trigger
DELETE FROM P8_Pond
WHERE
	Pond_ID = 4
;

-- Better DROP ALL TABLES AND THEN RECREATE THEM AFTER TEST!!!

------------------------------------------------------------------------------

-- INDECIES

CREATE INDEX Index_Showplace_Name ON P8_Showplace(Name)
;

CREATE INDEX Index_City_Name_CoutryID ON P8_City(Name, Country_ID)
;

CREATE UNIQUE INDEX Index_Showplace_UniqueWebsite ON P8_Showplace(Showplace_ID, Name, Website)
;
