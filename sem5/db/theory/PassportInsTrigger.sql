ALTER TRIGGER passport ON Passenger
FOR INSERT
AS 
	IF EXISTS(
		SELECT * FROM inserted AS data WHERE data.Passport IS NULL OR LEN(data.Passport)<>10
	)
	BEGIN
		RAISERROR('Passport of passenger has incorrect length.', 18, 1);
		ROLLBACK TRANSACTION;
	END;
	
