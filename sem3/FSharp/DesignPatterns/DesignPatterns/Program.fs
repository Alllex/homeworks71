open FlyweightExample

open System
open System.Windows.Forms
open System.Drawing

let rand = new Random()
let w = 500
let h = 500
let form = new Form(Width = w, Height = h)
let pen() = new Pen(Color.FromArgb(rand.Next(255),rand.Next(255),rand.Next(255)), float32 (rand.Next(5) + 2))
let printingFunction = 
    fun (points : Point[]) -> 
        form.Paint.Add(fun e -> e.Graphics.SmoothingMode <- Drawing2D.SmoothingMode.HighQuality
                                e.Graphics.DrawPolygon(pen(), points))

let generatePolygon(n : int) =
    [| for i in 1 .. n -> new Point(rand.Next(w), rand.Next(h)) |]

let sf = new ShapeFactory(printingFunction)

let maxTotalCreated = 4

form.Click.Add(
    fun _ -> 
        let n = rand.Next(maxTotalCreated) + 3
        let shape = sf.GetShape(n)
        shape.Print(generatePolygon(n))
        form.Text <- sprintf "Created: %d; Printed: %d" (sf.TotalCreated) (sf.TotalRequested)
        form.Invalidate()
    )

[<STAThread>]
do Application.Run(form)