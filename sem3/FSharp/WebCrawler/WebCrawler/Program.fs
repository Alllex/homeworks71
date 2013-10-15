open System.Net
open Microsoft.FSharp.Control.WebExtensions
open System.IO
open System.Text.RegularExpressions


let rmDup list = List.foldBack (fun x acc -> if List.tryFind ((=) x) acc <> None then acc else x::acc) list []

let linkRegex = new Regex("\"(?<addr>[^\"\?=]+)(\"|\?|=)+?")
let picRegex = Regex("(\"|\')(?<pic>[^\"\?=]+\.(png|jpe?g))(\'|\"|\?|=)+?")

let tillLastSlash (link : string) =
    link.Substring(0,link.LastIndexOf('/') + 1)

let processLink (dom : string) (link : string) = 
    if link.StartsWith("http") then link
    else if link.StartsWith("//") then "http:" + link
         else if dom.EndsWith("/") then
                if link.StartsWith("/") then dom + link.Substring(1) else dom + link
              else tillLastSlash dom + link

let folder = "pics"                         
if not <| Directory.Exists(folder) then Directory.CreateDirectory(folder) |> ignore

let localName name = 
    let rightChar c = (List.tryFind ((=) c) ['/';'\\';':';'?';'\"';'*';'<';'>']) <> None
    folder + "\\" +  (String.map (fun c -> if rightChar c then '_' else c) name)

let download link =
  async { 
    try
        let wc = new WebClient()
        wc.DownloadFileAsync(new System.Uri(link), localName link) 
    with | ex -> () //printfn "Download stoped: pic[%s] msg:%s" link (ex.Message)
  }


let maxSize = 1024000L
let minSize = 200000L

let sizePredicate (url : string) = 
    let wr = WebRequest.Create(url)
    let wres = wr.GetResponse()
    let size = wres.ContentLength
    wres.Close()
    size > minSize && size < maxSize

let downloadPicsTask (link : string) =
  async {
    try  
        let webClient = new WebClient()
        let! html = webClient.AsyncDownloadString(new System.Uri(link))
        let pics = [for m in picRegex.Matches(html) -> m.Result("${pic}")]
                    |> List.map (processLink link)
                    |> List.filter ((<>) "")
                    |> List.filter (fun (s:string) -> s.IndexOf("1366") > 0)
                    |> rmDup
                    |> List.filter sizePredicate
                    |> Seq.map download
                    |> Async.Parallel 
                    |> Async.RunSynchronously
                    |> ignore
        ()
        printfn "END: Site [%s] processed" <| link.Substring(0,70)
    with
        | ex -> ()// printfn "Downloading pics problem: site[%s] msg:%s" link (ex.Message)
  }

  
let linkRegex' = new Regex("\"(?<addr>[^\s\"]+?)\"") 

let findLinks' link =  
    try 
        let webClient = new WebClient()
        let html = webClient.DownloadString(new System.Uri(link))
        let links = [for m in linkRegex'.Matches(html) -> m.Result("${addr}")]
                        |> List.map (processLink link)
                        |> List.filter ((<>) "")
                        |> rmDup
                        |> List.filter (fun (s:string) -> s.IndexOf("hdwallpapers") > 0)
        links
    with 
        | ex -> []//printfn "Finding links problem: site[%s] msg:%s" link (ex.Message); []


let startUrl = "http://www.hdwallpapers.in/"

let mutable checked' = []

let rec crawlTheNet (deepth : int) (url : string) =
    let links' = url :: findLinks' url |> List.filter (fun s -> List.tryFind ((=) s) checked' = None)
    checked' <- links' @ checked'
    links'
        |> Seq.map downloadPicsTask
        |> Async.Parallel 
        |> Async.RunSynchronously
        |> ignore//*)
    if deepth > 0 then 
        List.map (crawlTheNet (deepth - 1)) links' |> ignore

crawlTheNet 5 startUrl