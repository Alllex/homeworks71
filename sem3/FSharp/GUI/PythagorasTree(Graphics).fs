open System
open System.Drawing
open System.Windows.Forms

let width = 1000
let height = 700

let pointConvert (p : Point) = new Point(p.X, height - p.Y)

let form =
    let image = new Bitmap(width, height)            
    let rnd = System.Random()  
    let canvas = Graphics.FromImage(image)      
    let rec tree (n : int) (p : Point) (len : int) (angle : float) (color : Color) (b : float) = 
        if n > 0 then
            let offsetX : int = int (float len * (Math.Cos(angle)))
            let offsetY : int = int (float len * (Math.Sin(angle)))
            let pen = new Pen(color, float32 b)
            let p1 = Point(p.X + offsetX, p.Y + offsetY)
            canvas.DrawLine(pen, pointConvert p, pointConvert p1)
            let newLen = (len * 7 / 9)
            let newColor = Color.FromArgb(rnd.Next() % 255, rnd.Next() % 255, rnd.Next() % 255) 
            let newBrushWidth = b * 7.0 / 9.0
            tree (n - 1) p1 newLen (angle + Math.PI / 4.0) newColor newBrushWidth
            tree (n - 1) p1 newLen (angle - Math.PI / 4.0) newColor newBrushWidth
             
                         
    tree 17 (new Point(width / 2, 50)) 150 (Math.PI / 2.0) (Color.Brown) 20.0
                   
    let temp = new Form() in temp.Paint.Add(fun e -> e.Graphics.DrawImage(image, 0, 0))
    temp.Size <- Size(width, height)
    temp
    
    
[<STAThread>]
do Application.Run(form)