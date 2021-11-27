module LogLevels

let message (logLine: string) : string =
    logLine
        .Substring(logLine.IndexOf(": ") + 1)
        .Replace("\r", "")
        .Replace("\n", "")
        .Replace("\t", "")
        .Trim()

let logLevel (logLine: string) : string =
    let s = logLine.IndexOf("[") + 1
    let e = logLine.IndexOf("]") - 1
    logLine.Substring(s, e).ToLower()

let reformat (logLine: string) : string =
    $"{message logLine} ({logLevel logLine})"
