
CREATE TABLE Passenger(
    Passenger_ID        INTEGER      NOT NULL IDENTITY,
    Name				VARCHAR(50)  NOT NULL,
    Email				VARCHAR(50),
CONSTRAINT Passenger_PK PRIMARY KEY (Passenger_ID)
)
;

CREATE TABLE Luggage(
    Luggage_ID        INTEGER      NOT NULL IDENTITY,
    Passenger_ID      INTEGER    NOT NULL,
    Weight		      REAL		   NOT NULL,
CONSTRAINT Luggage_PK PRIMARY KEY (Luggage_ID)
)
;

CREATE TABLE Airplane(
	Airplane_ID        INTEGER      NOT NULL IDENTITY,
    Name 		       VARCHAR(20)  NOT NULL,
CONSTRAINT Airplane_PK PRIMARY KEY (Airplane_ID)
)
;

CREATE TABLE CheckoutWay(
	CheckoutWay_ID        INTEGER      NOT NULL IDENTITY,
    Name 		          VARCHAR(20)  NOT NULL,
CONSTRAINT CheckoutWay_PK PRIMARY KEY (CheckoutWay_ID)
)
;

CREATE TABLE Seat(
	Seat_ID        INTEGER      NOT NULL IDENTITY,
    Name 		       VARCHAR(20)  NOT NULL,
CONSTRAINT Seat_PK PRIMARY KEY (Seat_ID)
)
;

CREATE TABLE SeatClass(
	SeatClass_ID        INTEGER      NOT NULL IDENTITY,
    Name 		       VARCHAR(20)  NOT NULL,
CONSTRAINT SeatClass_PK PRIMARY KEY (SeatClass_ID)
)
;

CREATE TABLE Airport(
	Airport_ID        INTEGER      NOT NULL IDENTITY,
    Name 		       VARCHAR(20)  NOT NULL,
CONSTRAINT Airport_PK PRIMARY KEY (Airport_ID)
)
;

CREATE TABLE Flight(
	Flight_ID        INTEGER      NOT NULL IDENTITY,
	Airplane_ID      INTEGER      NOT NULL,
	Departure        INTEGER      NOT NULL,
	Arrival          INTEGER      NOT NULL,
    Name 		     VARCHAR(20)  NOT NULL,
CONSTRAINT Flight_PK PRIMARY KEY (Flight_ID)
)
;

CREATE TABLE Ticket(
	Ticket_ID        INTEGER      NOT NULL IDENTITY,
	Passenger_ID     INTEGER      NOT NULL,
	Luggage_ID       INTEGER      ,
	Seat_ID			 INTEGER      NOT NULL,
	SeatClass_ID     INTEGER      NOT NULL,
	Flight_ID        INTEGER      NOT NULL,
	CheckoutWay_ID   INTEGER      NOT NULL,
CONSTRAINT Ticket_PK PRIMARY KEY (Ticket_ID)
)
;

CREATE TABLE DBLog(
	DBLog_ID	INTEGER NOT NULL IDENTITY,
	OperationName		VARCHAR(100) NOT NULL,
	OperationDesc		VARCHAR(500),
CONSTRAINT DBLog_PK PRIMARY KEY (DBLog_ID)
)
;

-------------------------------------------------------------------------

ALTER TABLE Luggage ADD CONSTRAINT FK_Luggage_Passenger 
    FOREIGN KEY (Passenger_ID)
    REFERENCES Passenger(Passenger_ID)
;

ALTER TABLE Flight ADD CONSTRAINT FK_Flight_Airplane
    FOREIGN KEY (Airplane_ID)
    REFERENCES Airplane(Airplane_ID)
;

ALTER TABLE Flight ADD CONSTRAINT FK_Flight_Airport_Departure
    FOREIGN KEY (Departure)
    REFERENCES Airport(Airport_ID)
;

ALTER TABLE Flight ADD CONSTRAINT FK_Flight_Airport_Arrival
    FOREIGN KEY (Arrival)
    REFERENCES Airport(Airport_ID)
;

ALTER TABLE Ticket ADD CONSTRAINT FK_Ticket_Passenger 
    FOREIGN KEY (Passenger_ID)
    REFERENCES Passenger(Passenger_ID)
;

ALTER TABLE Ticket ADD CONSTRAINT FK_Ticket_Luggage 
    FOREIGN KEY (Luggage_ID)
    REFERENCES Luggage(Luggage_ID)
;

ALTER TABLE Ticket ADD CONSTRAINT FK_Ticket_Seat 
    FOREIGN KEY (Seat_ID)
    REFERENCES Seat(Seat_ID)
;

ALTER TABLE Ticket ADD CONSTRAINT FK_Ticket_SeatClass 
    FOREIGN KEY (SeatClass_ID)
    REFERENCES SeatClass(SeatClass_ID)
;

ALTER TABLE Ticket ADD CONSTRAINT FK_Ticket_Flight 
    FOREIGN KEY (Flight_ID)
    REFERENCES Flight(Flight_ID)
;

ALTER TABLE Ticket ADD CONSTRAINT FK_Ticket_CheckoutWay 
    FOREIGN KEY (CheckoutWay_ID)
    REFERENCES CheckoutWay(CheckoutWay_ID)
;

----------------------------------------------------------------------------


INSERT INTO Passenger(Name, Email) VALUES ('Vasya', 'vasya.com')
INSERT INTO Passenger(Name, Email) VALUES ('Petr', 'pert.uk')

INSERT INTO Luggage(Passenger_ID, Weight) VALUES (1, 5)

INSERT INTO Airplane(Name) VALUES ('Boing 747')

INSERT INTO CheckoutWay(Name) VALUES ('Cash')
INSERT INTO CheckoutWay(Name) VALUES ('Visa')
INSERT INTO CheckoutWay(Name) VALUES ('MasterCard')

INSERT INTO Seat(Name) VALUES ('1')
INSERT INTO Seat(Name) VALUES ('2')

INSERT INTO SeatClass(Name) VALUES ('A')
INSERT INTO SeatClass(Name) VALUES ('S')

INSERT INTO Airport(Name) VALUES ('Airport A')
INSERT INTO Airport(Name) VALUES ('Airport B')

INSERT INTO Flight(Airplane_ID, Departure, Arrival, Name) VALUES (1, 1, 2, 'Flight #1')

INSERT INTO Ticket(Passenger_ID, Luggage_ID, Seat_ID, SeatClass_ID, Flight_ID, CheckoutWay_ID) VALUES 
			      (1, 1, 1, 1, 1, 2)
INSERT INTO Ticket(Passenger_ID, Luggage_ID, Seat_ID, SeatClass_ID, Flight_ID, CheckoutWay_ID) VALUES 
			      (2, NULL, 2, 2, 1, 1)
			      
			      
---------------------------------------------------------------------------------------

/*
DROP TABLE DBLog;
DROP TABLE Ticket;
DROP TABLE Flight;
DROP TABLE Luggage;
DROP TABLE Passenger;
DROP TABLE Airplane;
DROP TABLE Airport;
DROP TABLE CheckoutWay;
DROP TABLE Seat;
DROP TABLE SeatClass;
*/