-- STORED PROCEDURES

-- TESTS
EXECUTE AddShowplace 'A1', 'B1', 'web.com', 4, 'Moscow', 'Russia', 'Russian';
EXECUTE AddShowplace 'A2', 'B2', 'web.com', 4, 'Petergof', 'Russia', 'Russian';
EXECUTE AddShowplace 'A3', 'B3', 'web.com', 4, 'Rainbow City', 'Unicorn Land', 'Unicode';

-- Simple procedure adds new showplace
CREATE PROCEDURE AddShowplace
	@Name			VARCHAR(50),
	@Type			VARCHAR(50),
	@Website		VARCHAR(50),
	@Rating			INT,
	@CityName		VARCHAR(50),
	@CountryName	VARCHAR(50),
	@LanguageName	VARCHAR(50)
AS
BEGIN
	DECLARE @NewID INT
	SET @NewID = (SELECT MAX(Showplace_ID) + 1 FROM P8_Showplace);
	
	EXECUTE AddCity @CityName=@CityName, @CountryName=@CountryName, @LanguageName=@LanguageName;
	
	DECLARE @LangID INT
	SET @LangID = (SELECT Language_ID FROM P8_Language WHERE Name LIKE @LanguageName);	
	
	DECLARE @CityID INT
	SET @CityID = (SELECT City_ID FROM P8_City WHERE Name LIKE @CityName);
		
	INSERT INTO P8_Showplace(Showplace_ID, Type, Name, City_ID, Language_ID, Website, Rating) 
	VALUES (@NewID, @Type, @Name, @CityID, @LangID, @Website, @Rating)
	;
END
;

CREATE PROCEDURE AddLanguage
	@LanguageName VARCHAR(50)
AS
BEGIN
	IF (@LanguageName NOT IN (SELECT Name FROM P8_Language))
	BEGIN
		DECLARE @NewID INT
		SET @NewID = (SELECT MAX(Language_ID) + 1 FROM P8_Language);
		
		INSERT INTO P8_Language(Language_ID, Name) 
		VALUES (@NewID, @LanguageName)
		;
	END
END
;

CREATE PROCEDURE AddCountry
	@CountryName  VARCHAR(50),
	@LanguageName VARCHAR(50)
AS
BEGIN	
	IF (@CountryName NOT IN (SELECT Name FROM P8_Country))
	BEGIN
		EXECUTE AddLanguage @LanguageName=@LanguageName
		
		DECLARE @LangID INT
		SET @LangID = (SELECT Language_ID FROM P8_Language WHERE Name LIKE @LanguageName);
		
		DECLARE @NewID INT
		SET @NewID = (SELECT MAX(Country_ID) + 1 FROM P8_Country);
		
		INSERT INTO P8_Country(Country_ID, Name, Area, Population, Language_ID) 
		VALUES (@NewID, @CountryName, NULL, NULL, @LangID)
		;
	END
END
;

CREATE PROCEDURE AddCity
	@CityName		VARCHAR(50),
	@CountryName	VARCHAR(50),
	@LanguageName	VARCHAR(50)
AS
BEGIN
	IF (@CityName NOT IN (SELECT Name FROM P8_City))
	BEGIN
		EXECUTE AddCountry @CountryName=@CountryName, @LanguageName=@LanguageName;
		
		DECLARE @CountryID INT
		SET @CountryID = (SELECT Country_ID FROM P8_Country WHERE Name LIKE @CountryName);
		
		DECLARE @NewID INT
		SET @NewID = (SELECT MAX(City_ID) + 1 FROM P8_City);
		
		INSERT INTO P8_City(City_ID, Name, Country_ID) 
		VALUES (@NewID, @CityName, @CountryID)
		;
	END
END
;