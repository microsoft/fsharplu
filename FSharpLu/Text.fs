(*

Copyright (c) Microsoft Corporation.

Description:

    String manipulation helper functions.

Author:

    William Blum (WiBlum) created 9/27/2012

Revision history:
    Repackaged into FSharpLu on 2/18/2015

*)

module Microsoft.FSharpLu.Text

/// File path comparer
type CaseInsensitiveComparer() =
    interface System.Collections.Generic.IComparer<string> with
        member __.Compare(f1, f2) =
            System.String.Compare(f1, f2, System.StringComparison.OrdinalIgnoreCase)

/// Returns true if text starts with the specified prefix
let startsWith prefix (text:System.String) =
    text.StartsWith prefix

/// Returns true if text ends with the specified suffix
let endWith prefix (text:System.String) =
    text.EndsWith prefix

/// Remove count characters from the end of the specified string
let chop count (text:System.String) =
    text.Remove(text.Length-count)

/// Remove leading and trailing occurrences of a set of characters
let trim chars (text:System.String) =
    text.Trim chars

/// Remove trailing occurrences of a set of characters
let trimEnd chars (text:System.String) =
    text.TrimEnd chars

/// Remove leading occurrences of a set of characters
let trimStart chars (text:System.String) =
    text.TrimStart chars

/// Skip count number of characters from the specified string
let skip (text:string) count =
    text.Substring(count, text.Length-count)

/// Remove a prefix from the specified string
let skipPrefix prefix (text:string) =
    if text.StartsWith prefix then
        skip text prefix.Length
    else
        text

/// Remove a suffix from the specified string
let removeSuffix suffix (text:string) =
    if text.EndsWith suffix then
        chop suffix.Length text
    else
        text

/// Remove a prefix from the specified string case insensitively
let skipPrefixCaseInsensitive prefix (text:string) =
    if text.StartsWith(prefix, System.StringComparison.OrdinalIgnoreCase) then
        skip text prefix.Length
    else
        text

/// Remove a suffix case insensitively (used on file paths)
let removeSuffixCaseInsensitive suffix (text:string) =
    if text.EndsWith(suffix, System.StringComparison.OrdinalIgnoreCase) then
        chop suffix.Length text
    else
        text

/// Remove part following the the first occurrence of a given string
let removeAfter marker (text:string) =
    let markPosition = text.IndexOf(marker, System.StringComparison.OrdinalIgnoreCase)
    if markPosition >= 0 then
        text.Remove markPosition
    else
        text

/// Return the right n-most characters from the string
/// where n is smaller than the length of the string.
let right n (text:string) =
    text.Substring(text.Length-n,n)

/// Split a string based on the specified array of character separators
let split charSeparators (text:string) =
    text.Split charSeparators

/// Split a string on a string separator
let splitOnString (stringSeparators:string[]) (text:string) =
    text.Split(stringSeparators, System.StringSplitOptions.None)
    
let splitOnStringNoEmptyEntries (stringSeparators:char[]) (text:string) =
    text.Split(stringSeparators, System.StringSplitOptions.RemoveEmptyEntries)

/// Split a string based on the specified array of character separators
let splitNoEmptyEntries (charSeparators:char[]) (text:string) =
    text.Split(charSeparators, System.StringSplitOptions.RemoveEmptyEntries)

/// Strip trailing and prefix character
let stripQuotes =
    skipPrefix "\""
    >> removeSuffix "\""

/// Split a string at the first occurrence of a character
let splitOnce (charSep:char) (text:string) =
    let pos = text.IndexOf(charSep)
    if pos < 0 then
        invalidArg "text" "Separator not present in the string"
    else
        text.Substring(0, pos), text.Substring(pos+1)

/// Join a sequence of strings
let join separator (values:seq<string>) =
    System.String.Join(separator,values)

/// Truncate a string to a maximum number of characters
let truncate max (text:string) =
    let length = text.Length
    if length <= max then
        text
    else
        text.Substring(0,max)

/// longest common prefix of two strings
let longestCommonPrefixLength (s1:string) (s2:string) =
    let chop = Seq.map2 (<>) s1 s2
    match Seq.tryFindIndex id chop with
    | None -> min (s1.Length) (s2.Length)
    | Some i -> i

/// Indent lines in a text
let indent count =
    let prefix = System.String(' ', count)
    splitOnString [|System.Environment.NewLine|]
    >> Seq.map (fun line -> prefix + line)
    >> join System.Environment.NewLine

/// Encode a string to Base64
let encodeToBase64 (toEncode:string) =
    toEncode |> System.Text.ASCIIEncoding.UTF8.GetBytes |> System.Convert.ToBase64String

/// Decode a Base64 encoded string
let decodeFromBase64 (base64Encoded:byte[]) =
    let decodedString = System.Text.Encoding.UTF8.GetString(base64Encoded)
    System.Convert.FromBase64String(decodedString)