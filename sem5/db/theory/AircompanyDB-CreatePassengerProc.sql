ALTER PROCEDURE dbo.createPassenger
	@Name NVARCHAR(50),
	@Email NVARCHAR(50)
AS
	SET NOCOUNT ON;
	DECLARE @CountPass INT
	SET @CountPass = (SELECT COUNT(*) FROM Passenger WHERE Email=@Email)
	IF @CountPass = 0 BEGIN
		INSERT INTO Passenger(Name, Email) VALUES (@Name, @Email)
		INSERT INTO Journal(OperationName, OperationDesc) VALUES ('createPassenger', 'Name = ' + @Name + ' Email = ' + @Email)
	END ELSE 
		INSERT INTO Journal(OperationName, OperationDesc) VALUES ('createPassenger', 'Name = ' + @Name + ' Email = ' + @Email + '; Cannot create passenger with duplicate email')
