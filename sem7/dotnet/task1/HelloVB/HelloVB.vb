Public Class HelloVB : Inherits HelloCSharp
    Protected Overrides Sub MeetMedved()
        Console.WriteLine("Preved from VB.NET")
        MyBase.MeetMedved()
    End Sub
End Class
