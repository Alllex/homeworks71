
open System
open System.Windows.Forms
open System.Drawing
open WebCrawler

type WebCrawlerForm(w:int,h:int) as this = 
    class
        inherit Form(Text="WebCrawler: ",Width=w,Height=h)
        let logHeight = 400
        let tbStartUrl = new TextBox(Text="",Left=0,Top=5,Width=300)
        let tbLog = new TextBox(
                                    Multiline=true,
                                    ScrollBars=ScrollBars.Vertical,
                                    Text="",
                                    Left=0,
                                    Top=h-logHeight-50,
                                    Width=w-50,
                                    Height=logHeight,
                                    Visible=true,
                                    Enabled=true,
                                    ReadOnly=true
                               )
        let bStart = new Button(Text="Start",Top=5,Left=500)
        do
            this.Controls.Add(tbLog)
            this.Controls.Add(bStart)
            this.Controls.Add(tbStartUrl)
            bStart.Click.Add(fun _ -> 
                let wc = new WebCrawler(tbStartUrl.Text)
                wc.LogMessage.Add(tbLog.AppendText)
                wc.FileDownloadCompleted.Add(tbLog.AppendText)
                this.Text <- wc.Start())
            
    end

let form = new WebCrawlerForm(700,500)

Application.Run(form)