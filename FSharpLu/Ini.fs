// Copyright (c) Microsoft Corporation.
/// INI configuration file helpers
module Microsoft.FSharpLu.Ini

open System.IO
open System.Text.RegularExpressions
open Microsoft.FSharpLu.Collections

/// The name of an unnamed section in an INI configuration
[<Literal>]
let UnnamedSectionName = "_unnamed_"

/// The parsed section of an INI configuration
type Section = Map<string,string>

/// The Parsed INI configuration
type Configuration = Map<string,Section>

/// Gets a section from an Configuration (returns an empty section if the section name was not found)
let getSection (sectionName:string) (configuration:Configuration) =
    configuration.TryFind(sectionName) |> Option.defaultValue Map.empty

/// True if the section name was found Ini configuration
let hasSection (sectionName:string) (configuration:Configuration) : bool =
    configuration.TryFind(sectionName) |> Option.isSome

/// Tries to get a section from an Configuration
let tryGetSection (sectionName:string) (configuration:Configuration) =
    configuration.TryFind(sectionName)

/// True if the named parameter exists in the section
let hasParameter (parameterName:string) (section:Section) : bool =
    section.TryFind(parameterName) |> Option.isSome

/// Tries to get the value of a parameter in the section
let tryGetValue (parameterName:string) (section:Section) =
    section.TryFind(parameterName)

/// Gets the value of a parameter in the section, fail if the parameter name is not found
let getValueOrFail (parameterName:string) (section:Section) =
    match section.TryFind(parameterName) with
    | Some value ->
        value
    | None ->
        failwith (sprintf "ini parameter %s not found" parameterName)

/// Gets the value of a parameter in the section (returns the specified default value if not found)
let getValueOrDefault (parameterName:string) (defaultValue:string) (section:Section) =
    section.TryFind(parameterName) |> Option.defaultValue defaultValue

// True if the parameter exists in the section
let hasParameterInSection (sectionName:string) (parameterName:string) (configuration:Configuration) =
    configuration
    |> getSection sectionName
    |> hasParameter parameterName

/// Tries to get the value of a parameter in the named section
let tryGetValueFromSection (sectionName:string) (parameterName:string) (configuration:Configuration) =
    configuration
    |> getSection sectionName
    |> tryGetValue parameterName

/// Gets the value of a parameter in the named section (returns the default value if not found)
let getValueFromSectionOrDefault (sectionName:string) (parameterName:string)  (defaultValue:string) (configuration:Configuration) =
    configuration
    |> tryGetValueFromSection sectionName parameterName
    |> Option.defaultValue defaultValue

/// Regex parser for INI configuration file syntax
let private matcher = Regex("\s*\[(?<section>[^\]]+?)\s*]|^;(?<comment>.*)$|\s*(?<name>[^;=]+?)\s*=\s*(?<value>.*?)\s*$|(?<whitespace>\s*)", RegexOptions.Compiled|||RegexOptions.Singleline)

let private (|Section|NameValue|Whitespace|Comment|Error|) (line:string) =
    match matcher.Match(line) with
    | matchResult when matchResult.Success && matchResult.Groups.["section"].Success ->
        Section matchResult.Groups.["section"].Value
    | matchResult when matchResult.Success && matchResult.Groups.["name"].Success && matchResult.Groups.["value"].Success ->
        NameValue( matchResult.Groups.["name"].Value, matchResult.Groups.["value"].Value)
    | matchResult when matchResult.Success && matchResult.Groups.["comment"].Success ->
        Comment matchResult.Groups.["comment"].Value
    | matchResult when matchResult.Success && matchResult.Groups.["whitespace"].Success ->
        Whitespace
    | _ ->
        Error

/// Parse an ini configuration from a sequence of lines
let parseConfigurationFromLines (lines:string seq) (sourceContext:string) : Configuration =
    let leadingNameValueLinesWithNoSection =
        lines
        |> Seq.takeWhile(fun line -> match line with Section _ -> false | _ -> true)

    let configuration:Configuration =
        lines |> Seq.foldi(fun lineNumber sectionMap line ->
            match line with
            | Section sectionName ->
                let section:Section =
                    lines
                    |> Seq.skip (lineNumber + 1)
                    |> Seq.takeWhile(fun line -> match line with Section name -> false | _ -> true )
                    |> Seq.foldi(fun sectionLineNumber section line ->
                        match line with
                        | NameValue(name,value) ->
                            section.Add(name,value)
                        | Error ->
                            failwith (sprintf "Invalid line detected in ini file at line #%i in section %s of %s" (lineNumber + sectionLineNumber) sectionName sourceContext)
                        | _ ->
                            section
                    ) Map.empty

                sectionMap.Add(sectionName, section)
            | Error ->
                failwith (sprintf "Invalid line detected in ini file at line #%i of %s" lineNumber sourceContext)
            | _ ->
                sectionMap
        ) Map.empty

    if Seq.isEmpty leadingNameValueLinesWithNoSection then
        configuration
    else
        let unnamedSection:Section =
            leadingNameValueLinesWithNoSection
            |> Seq.foldi(fun lineNumber section line ->
                match line with
                | NameValue(name,value) ->
                    section.Add(name,value)
                | Error ->
                    failwith (sprintf "Invalid line detected in ini file at line #%i in unnamed section of %s" lineNumber sourceContext)
                | _ ->
                    section
            ) Map.empty
        configuration.Add(UnnamedSectionName,unnamedSection)

/// Read and parse an ini file and return the IniFile type containing of all sections which, in turn, contains a dictionary of all name/value pairs for that section
let readConfigurationFile (path:string) =
    parseConfigurationFromLines (System.IO.File.ReadAllLines(path)) path

/// Convert to a list of ini text lines
let configurationToLines (configuration:Configuration) =
    let unnamedSection =
        if configuration.ContainsKey(UnnamedSectionName) then
            configuration.Item(UnnamedSectionName)
            |> Seq.map(fun nameValuePair -> sprintf "%s=%s" nameValuePair.Key nameValuePair.Value)
            |> Seq.toList
        else
            []

    let namedSections =
        configuration
        |> Seq.sortBy(fun nvp -> nvp.Key)
        |> Seq.filter(fun nvp -> nvp.Key <> UnnamedSectionName)
        |> Seq.map(fun nameValuePair ->
            sprintf "[%s]" nameValuePair.Key
            ::
            (nameValuePair.Value
            |> Seq.sortBy(fun nvp -> nvp.Key)
            |> Seq.map(fun nameValuePair -> sprintf "%s=%s" nameValuePair.Key nameValuePair.Value) |> Seq.toList)
            )
    List.append unnamedSection (List.concat namedSections)

/// Writes the ini file to disk
let writeConfigurationFile (configuration:Configuration) (path:string) =
    File.WriteAllLines(path, configurationToLines configuration)

/// Converts this IniFile type to a string
let configurationToString configuration =
    configurationToLines configuration
    |> List.toSeq
    |> String.concat System.Environment.NewLine
