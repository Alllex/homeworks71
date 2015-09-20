namespace HelloProgrammingLang

type public HelloFSharp = 
    class
        inherit HelloVB
        new() = {}
        override this.MeetMedved() = do 
            printfn "Preved from F#"
            base.MeetMedved()
    end

