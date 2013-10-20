
module WebCrawler

open System.Net
open Microsoft.FSharp.Control.WebExtensions
open System.IO
open System.Text.RegularExpressions
open System.Collections

type List<'T> with
    static member removeDuplicates xs =
        List.foldBack (fun x acc -> if List.tryFind ((=) x) acc <> None then acc else x::acc) xs []
    end

type System.String with
    member s.tillLastChar(c : char) = s.Substring(0, s.LastIndexOf('/') + 1)
    member s.toWinObjName = 
        let rightChar c = (List.tryFind ((=) c) ['/';'\\';':';'?';'\"';'*';'<';'>']) <> None
        (String.map (fun c -> if rightChar c then '_' else c) s)

type WebCrawlerStates = Ready | Working

type WebCrawler(url : string, minSizePic, maxSizePic: int64) =
    class

        static let urlRegex = new Regex("\"(?<addr>[^\s\"]+?)\"") 
        static let picRegex = new Regex("(\"|\')(?<pic>[^\"\?=]+\.(png|jpe?g|gif))(\'|\"|\?|=)+?")

        static let maxSizeDefault = 3L * 1024L * 1024L // 3 Mb
        static let minSizeDefault = 200L * 1024L // 10 Kb
        static let depthLimit = 10
        
        static let deniedInSiteUrl = [".css"; ".js"; ".ico"; ".jpg"; ".png"; ".gif"]
        
        static let filterDenied (siteUrl : string) = 
            List.fold (&&) true <| List.map (fun d -> not (siteUrl.Contains(d))) deniedInSiteUrl

        static let isSubUrl (dom : string) (url : string) =
            (*try
                let a = dom.IndexOf('/') + 2
                let b = dom.Substring(a).IndexOf('/') + a
                url.StartsWith(dom.Substring(0, b))
            with | ex -> false*)
            url.StartsWith(dom)

        static let buildUrl (dom : string) (url : string) = 
            if url.StartsWith("http") then url
            else if url.StartsWith("//") then "http:" + url
                 else if dom.EndsWith("/") then if url.StartsWith("/") then dom + url.Substring(1) 
                                                else dom + url
                      else dom.tillLastChar('/') + url

// -----------------------------------------------------------------------------------------

        let startUrl = url
        let folderName = url.toWinObjName
        
        let EUrlSearchCompleted = new Event<_>()
        let EFilesDownloadCompleted = new Event<_>()
        let EFileDownloadCompleted = new Event<_>()
        let ELogMessage = new Event<_>()

        let minSize = minSizePic
        let maxSize = maxSizePic

        let mutable state = Ready
        
        let checkedPages = ArrayList.Synchronized (new ArrayList())

        let sizePredicate (url : string) = 
            try
                let wreq = WebRequest.Create(url)
                let wres = wreq.GetResponse()
                let size = wres.ContentLength
                wres.Close()
                size > minSize && size < maxSize
            with | ex -> false
            
        let downloadPic (fileUrl : string) = 
          async {
            try
                ELogMessage.Trigger("\r\nStart download: " + fileUrl)
                let wc = new WebClient()
                wc.DownloadFileCompleted.Add(fun e -> (*
                    if not (e.Cancelled || e.Error = null) then **)EFileDownloadCompleted.Trigger("\r\nDownloaded: " + fileUrl))
                wc.DownloadFile(new System.Uri(fileUrl), folderName + "\\" + fileUrl.toWinObjName) 
            with | ex -> ()
          }

        let findUrls (url : string) = 
            try 
                let webClient = new WebClient()
                let html = webClient.DownloadString(new System.Uri(url))
                seq { for m in urlRegex.Matches(html) -> m.Result("${addr}") }
                    |> Seq.map (buildUrl url)
                    |> Seq.filter ((<>) url)
                    |> Seq.filter (isSubUrl url)
                    |> Seq.filter filterDenied
            with | ex -> Seq.empty

        let downloadPics (url : string) = 
            async {
                try
                    let webClient = new WebClient()
                    let html = webClient.DownloadString(new System.Uri(url))
                    ELogMessage.Trigger("\r\nSearch for pics on: " + url)
                    seq { for m in picRegex.Matches(html) -> m.Result("${pic}") }
                        |> Seq.map (buildUrl url)
                        |> Seq.map downloadPic
                        |> Async.Parallel
                        |> Async.RunSynchronously
                        |> ignore
                with | ex -> ()
            }

        let rec crawl (url : string) = 
            async {
                try
                    if not(checkedPages.Contains(url)) then 
                        ELogMessage.Trigger("\r\nCrawl site: " + url)
                        Async.Start (downloadPics url)
                        checkedPages.Add(url) |> ignore
                        findUrls url
                            |> Seq.map crawl
                            |> Async.Parallel
                            |> Async.RunSynchronously
                            |> ignore
                with | ex -> ()
            }
            


// -----------------------------------------------------------------------------------------

        do
            if not <| Directory.Exists(folderName) then Directory.CreateDirectory(folderName) |> ignore 
             
             
// -----------------------------------------------------------------------------------------

        new(url : string) = WebCrawler(url, minSizeDefault, maxSizeDefault)    
                
        member public this.Start() =
            match state with
            | Ready -> Async.Start (crawl startUrl); state <- Working; "Started"
            | _ -> "Already working"
            
            
        [<CLIEvent>]
        member public this.UrlSearchCompleted = EUrlSearchCompleted.Publish
        [<CLIEvent>]
        member public this.FileDownloadCompleted = EFileDownloadCompleted.Publish
        [<CLIEvent>]
        member public this.FilesDownloadCompleted = EFilesDownloadCompleted.Publish
        [<CLIEvent>]
        member public this.LogMessage = ELogMessage.Publish
    end