ALTER PROCEDURE buyTicket
	@PassengerID	INT,
	@Flight			NVARCHAR(50),
	@Seat			NVARCHAR(50),
	@SeatClass		NVARCHAR(50),
	@PaymentBy		NVARCHAR(50)
AS	
	INSERT INTO Ticket(Passenger_ID, Luggage_ID, Seat_ID, SeatClass_ID, Flight_ID, PaymentBy_ID) VALUES 
			      (
			        @PassengerID, 
					NULL, 
					(SELECT Seat_ID FROM Seat WHERE Name=@Seat), 
					(SELECT SeatClass_ID FROM SeatClass WHERE Name=@SeatClass), 
					(SELECT Flight_ID FROM Flight WHERE Name=@Flight), 
					(SELECT PaymentBy_ID FROM PaymentBy WHERE Name=@PaymentBy)
				  )
			      
	UPDATE Passenger SET FlightCount = FlightCount + 1 WHERE Passenger_ID=@PassengerID
	