
SELECT * FROM
	A0_Order
WHERE 
	Employee_ID IN (SELECT Employee_ID FROM A0_Employee WHERE Spec='student')
; 

SELECT p.Last_Name FROM
	A0_Employee as e
	JOIN A0_Person as p ON e.Person_ID=p.Person_ID 
WHERE
	Employee_ID NOT IN (SELECT Employee_ID FROM A0_Order)
;

SELECT serv.Name, ord.Time_Order, p_emp.Last_Name, pet.Nick, p_own.Last_Name FROM
	A0_Order as ord
	JOIN A0_Employee as emp ON ord.Employee_ID=emp.Employee_ID
	JOIN A0_Pet as pet ON ord.Pet_ID=pet.Pet_ID
	JOIN A0_Person p_emp ON emp.Person_ID=p_emp.Person_ID
	JOIN A0_Person p_own ON ord.Owner_ID=p_own.Person_ID
	JOIN A0_Service serv ON ord.Service_ID=serv.Service_ID
;

SELECT Comments FROM A0_Order
UNION
SELECT Description FROM A0_Owner
UNION
SELECT Description FROM A0_Pet
;