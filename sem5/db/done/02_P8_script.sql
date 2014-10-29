---------------------------------------------------------------
-- Создание таблиц и PK 
---------------------------------------------------------------
CREATE TABLE P8_Language(
    Language_ID         INTEGER      NOT NULL,
    Name				VARCHAR(20)  NOT NULL,
CONSTRAINT P8_Language_PK PRIMARY KEY (Language_ID)
)
;

CREATE TABLE P8_Country(
    Country_ID          INTEGER      NOT NULL,
    Name				VARCHAR(50)  NOT NULL,
    Area				REAL,
    Population	        REAL,
    Language_ID			INTEGER		 NOT NULL,
CONSTRAINT P8_Country_PK PRIMARY KEY (Country_ID)
)
;

CREATE TABLE P8_City(
    City_ID         INTEGER      NOT NULL,
    Name			VARCHAR(50)  NOT NULL,
    Country_ID		INTEGER		 NOT NULL,
CONSTRAINT P8_City_PK PRIMARY KEY (City_ID)
)
;

CREATE TABLE P8_Showplace(
    Showplace_ID         INTEGER      NOT NULL,
    Type				 VARCHAR(50)  NOT NULL,
    Name				 VARCHAR(50)  NOT NULL,
    City_ID				 INTEGER      NOT NULL,
    Language_ID          INTEGER      NOT NULL,
    Website				 VARCHAR(50),
    Rating				 INTEGER	  DEFAULT 0,
CONSTRAINT P8_Showplace_Rating CHECK (Rating in (0,1,2,3,4,5)),
CONSTRAINT P8_Showplace_PK PRIMARY KEY (Showplace_ID)
)
;

CREATE TABLE P8_Pond(
    Pond_ID         INTEGER      NOT NULL,
    Type				 VARCHAR(50)  NOT NULL,
    Name				 VARCHAR(50)  NOT NULL,
    City_ID				 INTEGER      NOT NULL,
    Area				 REAL      NOT NULL,
    Width				 REAL      NOT NULL,
    Length				 REAL      NOT NULL,
CONSTRAINT P8_Pond_PK PRIMARY KEY (Pond_ID)
)
;

---------------------------------------------------------------
-- Создание FK 
---------------------------------------------------------------

ALTER TABLE P8_Country ADD CONSTRAINT FK_P8_Country_Language 
    FOREIGN KEY (Language_ID)
    REFERENCES P8_Language(Language_ID)
;

ALTER TABLE P8_City ADD CONSTRAINT FK_P8_City_Country 
    FOREIGN KEY (Country_ID)
    REFERENCES P8_Country(Country_ID)
;

ALTER TABLE P8_Showplace ADD CONSTRAINT FK_P8_Showplace_City
    FOREIGN KEY (City_ID)
    REFERENCES P8_City(City_ID)
;

ALTER TABLE P8_Showplace ADD CONSTRAINT FK_P8_Showplace_Language
    FOREIGN KEY (Language_ID)
    REFERENCES P8_Language(Language_ID)
;

ALTER TABLE P8_Pond ADD CONSTRAINT FK_P8_Pond_City
    FOREIGN KEY (City_ID)
    REFERENCES P8_City(City_ID)
;

---------------------------------------------------------------
-- Заполнение таблиц тестовыми данными
---------------------------------------------------------------


INSERT INTO P8_Language(Language_ID, Name) VALUES (1, 'Russian')
INSERT INTO P8_Language(Language_ID, Name) VALUES (2, 'English')
INSERT INTO P8_Language(Language_ID, Name) VALUES (3, 'German')
INSERT INTO P8_Language(Language_ID, Name) VALUES (4, 'French')
INSERT INTO P8_Language(Language_ID, Name) VALUES (5, 'Mandarin')
INSERT INTO P8_Language(Language_ID, Name) VALUES (6, 'Hindi')

INSERT INTO P8_Country(Country_ID, Name, Area, Population, Language_ID) VALUES (1, 'Russia', 1000.0, 1001.0, 1)
INSERT INTO P8_Country(Country_ID, Name, Area, Population, Language_ID) VALUES (2, 'US', 200.0, 201.0, 2)
INSERT INTO P8_Country(Country_ID, Name, Area, Population, Language_ID) VALUES (3, 'Great Britain', 300.0, 301.0, 2)
INSERT INTO P8_Country(Country_ID, Name, Area, Population, Language_ID) VALUES (4, 'German', 200.0, 201.0, 3)
INSERT INTO P8_Country(Country_ID, Name, Area, Population, Language_ID) VALUES (5, 'France', 400.0, 401.0, 4)
INSERT INTO P8_Country(Country_ID, Name, Area, Population, Language_ID) VALUES (6, 'China', 500.0, 501.0, 5)
INSERT INTO P8_Country(Country_ID, Name, Area, Population, Language_ID) VALUES (7, 'Japan', 50.0, 51.0, 5)
INSERT INTO P8_Country(Country_ID, Name, Area, Population, Language_ID) VALUES (8, 'India', 600.0, 601.0, 6)

INSERT INTO P8_City(City_ID, Name, Country_ID) VALUES (1, 'Moscow', 1)
INSERT INTO P8_City(City_ID, Name, Country_ID) VALUES (2, 'Saint-Petersburg', 1)
INSERT INTO P8_City(City_ID, Name, Country_ID) VALUES (3, 'New-York', 2)
INSERT INTO P8_City(City_ID, Name, Country_ID) VALUES (4, 'Chicago', 2)
INSERT INTO P8_City(City_ID, Name, Country_ID) VALUES (5, 'Washington', 2)
INSERT INTO P8_City(City_ID, Name, Country_ID) VALUES (6, 'London', 3)
INSERT INTO P8_City(City_ID, Name, Country_ID) VALUES (7, 'Oxford', 3)
INSERT INTO P8_City(City_ID, Name, Country_ID) VALUES (8, 'Berlin', 4)
INSERT INTO P8_City(City_ID, Name, Country_ID) VALUES (9, 'Paris', 5)
INSERT INTO P8_City(City_ID, Name, Country_ID) VALUES (10, 'Hong-Kong', 6)
INSERT INTO P8_City(City_ID, Name, Country_ID) VALUES (11, 'Tokio', 7)
INSERT INTO P8_City(City_ID, Name, Country_ID) VALUES (12, 'New Deli', 8)

INSERT INTO P8_Showplace(Showplace_ID, Type, Name, City_ID, Language_ID, Website, Rating) VALUES
	(1, 'Museum', 'Art Institute of Chicago', 4, 2, 'one.com', 5)
INSERT INTO P8_Showplace(Showplace_ID, Type, Name, City_ID, Language_ID, Website, Rating) VALUES
	(2, 'Museum', 'National Museum of Anthropology', 9, 4, 'two.com', 5)
INSERT INTO P8_Showplace(Showplace_ID, Type, Name, City_ID, Language_ID, Website, Rating) VALUES
	(3, 'Museum', 'Hermitage', 2, 1, NULL, 5)
INSERT INTO P8_Showplace(Showplace_ID, Type, Name, City_ID, Language_ID, Website, Rating) VALUES
	(4, 'Theater', 'London Theatre', 6, 2, 'three.com', 4)
	
INSERT INTO P8_Pond(Pond_ID, Type, Name, City_ID, Area, Width, Length) VALUES
	(1, 'Lake', 'Baikal', 1, 100, 50, 2)
INSERT INTO P8_Pond(Pond_ID, Type, Name, City_ID, Area, Width, Length) VALUES
	(2, 'Lake', 'Super lake', 7, 500, 1, 500)
INSERT INTO P8_Pond(Pond_ID, Type, Name, City_ID, Area, Width, Length) VALUES
	(3, 'Lake', 'Mega lake', 10, 777, 20, 345)
INSERT INTO P8_Pond(Pond_ID, Type, Name, City_ID, Area, Width, Length) VALUES
	(4, 'River', 'Amazon', 3, 777, 20, 345)
INSERT INTO P8_Pond(Pond_ID, Type, Name, City_ID, Area, Width, Length) VALUES
	(5, 'River', 'Volga', 4, 777, 20, 345)
INSERT INTO P8_Pond(Pond_ID, Type, Name, City_ID, Area, Width, Length) VALUES
	(6, 'River', 'Temza', 5, 777, 20, 345)

---------------------------------------------------------------
-- Удаление таблиц 
---------------------------------------------------------------

DROP TABLE P8_Pond;
DROP TABLE P8_Showplace;
DROP TABLE P8_City;
DROP TABLE P8_Country;
DROP TABLE P8_Language;


