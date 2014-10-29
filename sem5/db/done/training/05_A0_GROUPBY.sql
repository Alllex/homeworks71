-- Training GROUP BY and HAVING

SELECT A0_Person.Person_ID, A0_Person.Last_Name FROM
	A0_Employee,
	A0_Order,
	A0_Person
WHERE 
	A0_Employee.Employee_ID=A0_Order.Employee_ID AND
	A0_Person.Person_ID=A0_Employee.Person_ID AND
	A0_Order.Is_Done=1
GROUP BY
	A0_Person.Person_ID,
	A0_Person.Last_Name,
	A0_Person.First_Name
HAVING
	COUNT(*) > 5
;