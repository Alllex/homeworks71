open System
open System.Net
open Microsoft.FSharp.Control.WebExtensions
open System.IO
open System.Text.RegularExpressions
open System.Collections

type System.String with
    member s.tillLastChar(c : char) = s.Substring(0, s.LastIndexOf('/') + 1)
    member s.toWinObjName = 
        let rightChar c = (List.tryFind ((=) c) ['/';'\\';':';'?';'\"';'*';'<';'>']) <> None
        (String.map (fun c -> if rightChar c then '_' else c) s)

let urlRegex = new Regex("\"(?<addr>[^\s\"]+)\"$") 
let picRegex = new Regex("(\"|\')(?<pic>[^\"\?=]+\.(png|jpe?g|gif))(\'|\"|\?|=)+?")
let deniedInSiteUrl = [".css"; ".js"; ".ico"; ".jpg"; ".png"; ".gif"]
let isNotDeniedUrl (siteUrl : string) = List.fold (&&) true <| List.map (fun d -> not (siteUrl.Contains(d))) deniedInSiteUrl

let isSubUrl (dom : string) (url : string) =
    try
        let a = dom.IndexOf('/') + 2
        let b = dom.Substring(a).IndexOf('/') + a
        url.StartsWith(dom.Substring(0, b))
    with | ex -> false

let buildUrl (dom : string) (url : string) = 
    if url.StartsWith("http") then url
    else if url.StartsWith("//") then "http:" + url
            else if dom.EndsWith("/") then if url.StartsWith("/") then dom + url.Substring(1) 
                                           else dom + url
                 else dom.tillLastChar('/') + url
                 
let downloadedPics = new Concurrent.ConcurrentDictionary<string, bool>()

let startUrl = "http://octodex.github.com/"
let folderName = startUrl.toWinObjName
let minSize = 500 * 1024 // 500Kb

if not <| Directory.Exists(folderName) then Directory.CreateDirectory(folderName) |> ignore

let findPicsAsync url html = async { 
    let picUrls = [for m in picRegex.Matches(html) -> m.Result("${pic}") ]
                    |> List.map    (buildUrl url)
                    |> List.filter (fun url -> if downloadedPics.ContainsKey url then false else downloadedPics.GetOrAdd(url, true))
    return picUrls
    }

let downloadPicAsync (picUrl : string) = async {
    try
        let wreq = WebRequest.Create(picUrl)
        let wres = wreq.GetResponse()
        let size = (int) wres.ContentLength
        wres.Close()
        if size > minSize then 
            let wc = new WebClient()
            wc.DownloadFile(new System.Uri(picUrl), folderName + "\\" + picUrl.toWinObjName)
            printfn "Downloaded (%iKb): %s" (size / 1024) picUrl
    with | ex -> ()
    }

let picsAsync url html = async {
    printfn "Start downloading from %s" url
    let! picUrls = findPicsAsync url html
    picUrls
    |> Seq.map downloadPicAsync
    |> Async.Parallel
    |> Async.RunSynchronously 
    |> ignore
    printfn "All pics (%i) downloaded form: %s" (List.length picUrls) url
    }
    
let checkedPages = new Concurrent.ConcurrentDictionary<string, bool>()

let findUrlsAsync url html = async { 
    let urls = [ for m in urlRegex.Matches(html) -> m.Result("${addr}") ]
                |> List.map    (buildUrl url)
                |> List.filter (isSubUrl url)
                |> List.filter isNotDeniedUrl
                |> List.filter (fun url -> if checkedPages.ContainsKey url then false else checkedPages.GetOrAdd(url, true))
    return urls
}

let rec urlsAsync prefix url html = async {
    let! urls = findUrlsAsync url html
    urls
    |> Seq.mapi (fun i url -> processUrlAsync (prefix + "." + (i + 1).ToString()) url)
    |> Async.Parallel
    |> Async.RunSynchronously 
    |> ignore
    ()
    }

and processUrlAsync prefix (url : string) = async {
    try
        let webClient = new WebClient()
        let html = webClient.DownloadString(new System.Uri (url))
        ("Task number: " + prefix + "\nStart processing: " + url) |> printfn "%A"
        [picsAsync url html; urlsAsync prefix url html]
         |> Async.Parallel
         |> Async.RunSynchronously
         |> ignore
        ("Task number: " + prefix + "\nEnd processing:   " + url) |> printfn "%A"
    with ex -> () //"Error: " + url + "\nMessage:\n" + ex.Message |> printfn "%A"
}

checkedPages.GetOrAdd(startUrl, true) |> ignore
processUrlAsync "1" startUrl |> Async.RunSynchronously

printfn "My collection:"
checkedPages.Keys |> Seq.toList |> List.map (printfn "%A") |> ignore
printfn "\nAmount of downloaded pictures: %i" (downloadedPics.Count)