ALTER PROCEDURE getPassengerByPage
	@Page INT,
	@Size INT
AS
	CREATE TABLE #Num
	(
		ID INT NOT NULL IDENTITY,
		Pass_ID INT NOT NULL
	)
	;
	INSERT INTO #Num SELECT Passenger_ID FROM Passenger;
	SELECT TOP(@Size) Passenger_ID, Name
	FROM #Num JOIN Passenger ON #Num.Pass_ID=Passenger.Passenger_ID
	WHERE #Num.ID > (@Page - 1) * @Size
	;