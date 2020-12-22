open System.Text.RegularExpressions

module StringUtils =
    let toCharArray (str: string) = str.ToCharArray()
    
    let splitString sep (str: string) = str.Split(sep)
    
    let splitStringAt i (str: string) = (str.Substring(0, i), str.Substring(i))

module RegexUtils =
    let regexMatch regex =
        let regex = (Regex regex)
        regex.Match

    let captureGroups regex str =
        let rMatch = regexMatch regex str
        rMatch.Groups
    
    let firstCaptureGroup regex str =
        let groups = captureGroups regex str
        groups.[1].Value
        
    let doesMatchRegex regex str =
        let rMatch = regexMatch regex str
        rMatch.Success
        
module ArrayUtils =
    let countIf f = Array.filter f >> Array.length
