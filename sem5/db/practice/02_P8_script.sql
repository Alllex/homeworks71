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
INSERT INTO P8_City(City_ID, Name, Country_ID) VALUES (11, 'Tokyo', 7)
INSERT INTO P8_City(City_ID, Name, Country_ID) VALUES (12, 'New Deli', 8)

INSERT INTO P8_City(City_ID, Name, Country_ID) VALUES (13, 'Saratov', 1)
INSERT INTO P8_City(City_ID, Name, Country_ID) VALUES (14, 'Omsk', 1)
INSERT INTO P8_City(City_ID, Name, Country_ID) VALUES (15, 'Perm', 1)
INSERT INTO P8_City(City_ID, Name, Country_ID) VALUES (16, 'Ufa', 1)

INSERT INTO P8_City(City_ID, Name, Country_ID) VALUES (17, 'Bristol', 3)
INSERT INTO P8_City(City_ID, Name, Country_ID) VALUES (18, 'Liverpool', 3)
INSERT INTO P8_City(City_ID, Name, Country_ID) VALUES (19, 'Manchester', 3)
INSERT INTO P8_City(City_ID, Name, Country_ID) VALUES (20, 'Newcastle', 3)
INSERT INTO P8_City(City_ID, Name, Country_ID) VALUES (21, 'Nottingham', 3)
INSERT INTO P8_City(City_ID, Name, Country_ID) VALUES (22, 'Sheffield', 3)

INSERT INTO P8_City(City_ID, Name, Country_ID) VALUES (24, 'Los Angeles', 2)
INSERT INTO P8_City(City_ID, Name, Country_ID) VALUES (25, 'Houston', 2)
INSERT INTO P8_City(City_ID, Name, Country_ID) VALUES (26, 'Philadelphia', 2)
INSERT INTO P8_City(City_ID, Name, Country_ID) VALUES (27, 'Phoenix', 2)
INSERT INTO P8_City(City_ID, Name, Country_ID) VALUES (28, 'Dallas', 2)
INSERT INTO P8_City(City_ID, Name, Country_ID) VALUES (29, 'Detroit', 2)
INSERT INTO P8_City(City_ID, Name, Country_ID) VALUES (30, 'Las Vegas', 2)
INSERT INTO P8_City(City_ID, Name, Country_ID) VALUES (31, 'Orlando', 2)
INSERT INTO P8_City(City_ID, Name, Country_ID) VALUES (32, 'Buffalo', 2)
INSERT INTO P8_City(City_ID, Name, Country_ID) VALUES (33, 'Salt Lake City', 2)


INSERT INTO P8_Showplace(Showplace_ID, Type, Name, City_ID, Language_ID, Website, Rating) VALUES
	(1, 'Museum', 'Art Institute of Chicago', 4, 2, 'one.com', 5)
INSERT INTO P8_Showplace(Showplace_ID, Type, Name, City_ID, Language_ID, Website, Rating) VALUES
	(2, 'Museum', 'National Museum of Anthropology', 9, 4, 'two.com', 4)
INSERT INTO P8_Showplace(Showplace_ID, Type, Name, City_ID, Language_ID, Website, Rating) VALUES
	(3, 'Museum', 'Hermitage', 2, 1, NULL, 5)
INSERT INTO P8_Showplace(Showplace_ID, Type, Name, City_ID, Language_ID, Website, Rating) VALUES
	(4, 'Theatre', 'London Theatre', 6, 2, 'three.com', 4)
	
INSERT INTO P8_Showplace(Showplace_ID, Type, Name, City_ID, Language_ID, Website, Rating) VALUES 
	(5, 'Road', 'Route 66', 24, 2, 'route66.com', 2)
INSERT INTO P8_Showplace(Showplace_ID, Type, Name, City_ID, Language_ID, Website, Rating) VALUES 
	(6, 'Academy', 'Pennsylvania Academy of the Fine Arts', 26, 2, 'pafa.com', 4)
INSERT INTO P8_Showplace(Showplace_ID, Type, Name, City_ID, Language_ID, Website, Rating) VALUES 
	(7, 'Building', 'Freedom Tower', 3, 2, NULL, 3)
INSERT INTO P8_Showplace(Showplace_ID, Type, Name, City_ID, Language_ID, Website, Rating) VALUES 
	(8, 'Building', 'White house', 5, 2, 'wh.gov', 5)
INSERT INTO P8_Showplace(Showplace_ID, Type, Name, City_ID, Language_ID, Website, Rating) VALUES 
	(9, 'Cimena', 'Kreuzberg', 8, 3, NULL, 4)
INSERT INTO P8_Showplace(Showplace_ID, Type, Name, City_ID, Language_ID, Website, Rating) VALUES 
	(10, 'Cinema', 'Gaumont', 9, 4, 'gaumont.fr', 3)
INSERT INTO P8_Showplace(Showplace_ID, Type, Name, City_ID, Language_ID, Website, Rating) VALUES 
	(11, 'Theatre', 'Moscow Art Theatre', 1, 1, 'mat.ru', 5)
INSERT INTO P8_Showplace(Showplace_ID, Type, Name, City_ID, Language_ID, Website, Rating) VALUES 
	(12, 'Theatre', 'Ostankino Palace', 1, 1, 'op.ru', 4)
INSERT INTO P8_Showplace(Showplace_ID, Type, Name, City_ID, Language_ID, Website, Rating) VALUES 
	(13, 'Theatre', 'HKRep Blackbox', 10, 5, 'hkrep.hk', 3)
INSERT INTO P8_Showplace(Showplace_ID, Type, Name, City_ID, Language_ID, Website, Rating) VALUES 
	(14, 'Theatre', 'Adventure Stage Chicago', 4, 2, 'adventure.ch', 4)
	
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


