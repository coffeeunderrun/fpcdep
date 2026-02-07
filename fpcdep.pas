program fpcdep;

{$mode objfpc}{$H+}

uses
  Classes, GetOpts, SysUtils, StrUtils;

const
  UnitExtensions: array[0..1] of String = ('.pas', '.pp');

var
  UnitPaths: TStringList;
  IncludePaths: TStringList;
  ProcessedFiles: TStringList;
  CollectedDeps: TStringList;

function TrimQuotes(const Str: String): String;
var
  TrimmedStr: String;
begin
  TrimmedStr := Trim(Str);

  if Length(TrimmedStr) < 2 then exit(TrimmedStr);

  if (TrimmedStr[1] = '''') and (TrimmedStr[Length(TrimmedStr)] = '''') or
     (TrimmedStr[1] = '"') and (TrimmedStr[Length(TrimmedStr)] = '"') then
    Result := Copy(TrimmedStr, 2, Length(TrimmedStr) - 2)
  else
    Result := TrimmedStr;
end;

function FileExistsCaseInsensitive(const FileName: String): String;
var
  Dir, Name, Ext: String;
  SearchRec: TSearchRec;
  Mask: String;
begin
  Result := '';
  Dir := ExtractFilePath(FileName);
  Ext := ExtractFileExt(FileName);
  Name := ChangeFileExt(ExtractFileName(FileName), '');

  if Ext.IsEmpty then Ext := '*';
  Mask := Name + Ext;

  if Dir.IsEmpty then Dir := GetCurrentDir + PathDelim;

  // Search for files matching the mask in a case-insensitive way
  if FindFirst(Dir + Mask, faAnyFile, SearchRec) = 0 then begin
    Result := IncludeTrailingPathDelimiter(Dir) + SearchRec.Name;
    FindClose(SearchRec);
  end;
end;

function ResolveIncludePath(const IncName, BaseDir: String): String;
var
  Candidate: String;
  I: Integer;
begin
  Result := '';
  if IncName.IsEmpty then exit;

  // If absolute or relative and exists
  Candidate := IncName;
  if FileExists(Candidate) then begin
    Result := ExpandFileName(Candidate);
    exit;
  end;

  // Try relative to baseDir
  if not BaseDir.IsEmpty then begin
    Candidate := IncludeTrailingPathDelimiter(BaseDir) + IncName;
    if FileExists(Candidate) then begin
      Result := ExpandFileName(Candidate);
      exit;
    end;
  end;

  // Search in IncludePaths
  for I := 0 to IncludePaths.Count - 1 do begin
    Candidate := IncludeTrailingPathDelimiter(IncludePaths[I]) + IncName;
    if FileExists(Candidate) then begin
      Result := ExpandFileName(Candidate);
      exit;
    end;
  end;
end;

procedure AddDependency(const FilePath: String);
begin
  if FilePath.IsEmpty then exit;
  if CollectedDeps.IndexOf(FilePath) = -1 then CollectedDeps.Add(FilePath);
end;

procedure ProcessUnitName(const UnitName, BaseDir: String);
var
  I, K: Integer;
  Candidate: String;
  LowerStr: String;
  Ext: String;
begin
  if UnitName.IsEmpty then exit;

  // Search in UnitPaths with possible extensions
  for I := 0 to UnitPaths.Count - 1 do begin
    for K := 0 to High(UnitExtensions) do begin
      Ext := UnitExtensions[K];

      if (not UnitPaths[I].IsEmpty) then
        Candidate := IncludeTrailingPathDelimiter(UnitPaths[I]) + UnitName + Ext
      else
        Candidate := UnitName + Ext;

      // Try direct match first
      if FileExists(Candidate) then begin
        AddDependency(ExpandFileName(Candidate));
        exit;
      end;

      // Try case-insensitive search
      LowerStr := ExtractFilePath(Candidate) + LowerCase(ExtractFileName(Candidate));
      if not FileExistsCaseInsensitive(LowerStr).IsEmpty then begin
        AddDependency(ExpandFileName(FileExistsCaseInsensitive(LowerStr)));
        exit;
      end;
    end;
  end;

  // Fallback to current dir and baseDir
  for K := 0 to High(UnitExtensions) do begin
    Ext := UnitExtensions[K];
    Candidate := UnitName + Ext;

    // Try direct match first
    if FileExists(Candidate) then begin
      AddDependency(ExpandFileName(Candidate));
      exit;
    end;

    // Try case-insensitive search in current dir
    if (not BaseDir.IsEmpty) and (K = 0) then begin
      Candidate := IncludeTrailingPathDelimiter(BaseDir) + UnitName + Ext;

      if FileExists(Candidate) then begin
        AddDependency(ExpandFileName(Candidate));
        exit;
      end;
    end;
  end;
end;

function ExtractIncludes(const ContentStr: String): TStringList;
var
  I, J: Integer;
  LowerStr: String;
  TrimmedStr: String;
begin
  Result := TStringList.Create;

  // Look for {$...} directives
  I := 1;
  while I <= Length(ContentStr) do begin
    if (I <= Length(ContentStr) - 2) and (ContentStr[I] = '{') and (ContentStr[I + 1] = '$') then begin
      J := I + 2;
      while (J <= Length(ContentStr)) and (ContentStr[J] <> '}') do Inc(J);

      // Extract directive content and check if it's an include
      if J <= Length(ContentStr) then begin
        TrimmedStr := Copy(ContentStr, I + 2, J - (I + 2));
        LowerStr := LowerCase(Trim(TrimmedStr));

        // Check if it starts with "i", or "include"
        if (Pos('i ', LowerStr) = 1) or (Pos('include', LowerStr) = 1) then begin
          // Extract the filename part after the directive
          if Pos(' ', TrimmedStr) > 0 then TrimmedStr := Trim(Copy(TrimmedStr, Pos(' ', TrimmedStr) + 1, MaxInt)) else TrimmedStr := '';
          // If we got a filename, add it to the result
          if not TrimmedStr.IsEmpty then Result.Add(TrimQuotes(TrimmedStr));
        end;
      end;

      // Move past this directive
      I := J + 1;
      continue;
    end;

    Inc(I);
  end;
end;

function SanitizeContent(const ContentStr: String): String;
var
  I: Integer;
  OutStr: String;
  InStr: Boolean;
begin
  OutStr := ContentStr;
  InStr := False;
  I := 1;

  // This loop will replace all characters inside string literals and comments with spaces, preserving line breaks
  while I <= Length(ContentStr) do begin
    if not InStr and (ContentStr[I] = '''') then begin
      InStr := True;
      OutStr[I] := ' ';
      Inc(I);

      // Handle string literals, including escaped single quotes
      while (I <= Length(ContentStr)) do begin
        OutStr[I] := ' ';

        // Handle escaped single quotes ('' becomes ')
        if (ContentStr[I] = '''') then begin
          Inc(I);

          // If the next character is also a single quote, it's an escaped quote, so we skip it
          if (I <= Length(ContentStr)) and (ContentStr[I] = '''') then begin
            OutStr[I] := ' ';
            Inc(I);
            continue;
          end;

          break;
        end;

        Inc(I);
      end;

      InStr := False;
      continue;
    end;

    // Handle // comments
    if (I <= Length(ContentStr) - 1) and (ContentStr[I] = '/') and (ContentStr[I + 1] = '/') then begin
      while (I <= Length(ContentStr)) and (ContentStr[I] <> #10) do begin OutStr[I] := ' '; Inc(I); end;
      continue;
    end;

    // Handle (* ... *) comments
    if (I <= Length(ContentStr) - 1) and (ContentStr[I] = '(') and (ContentStr[I + 1] = '*') then begin
      // Move past the opening (* and replace with spaces
      while (I <= Length(ContentStr) - 1) and not ((ContentStr[I] = '*') and (ContentStr[I + 1] = ')')) do begin
        OutStr[I] := ' ';
        Inc(I);
      end;

      // Move past the closing *)
      if I <= Length(ContentStr) - 1 then begin
        OutStr[I] := ' ';
        OutStr[I + 1] := ' ';
        I := I + 2;
      end;

      continue;
    end;

    // Handle { ... } comments
    if ContentStr[I] = '{' then begin
      while (I <= Length(ContentStr)) and (ContentStr[I] <> '}') do begin OutStr[I] := ' '; Inc(I); end;
      if I <= Length(ContentStr) then begin OutStr[I] := ' '; Inc(I); end;
      continue;
    end;

    Inc(I);
  end;

  Result := OutStr;
end;

function ExtractUsesBlocks(const ContentStr: String): TStringList;
var
  I, J: Integer;
  LowerStr: String;
begin
  Result := TStringList.Create;
  LowerStr := LowerCase(ContentStr);

  // Look for "uses" blocks
  I := 1;
  while I <= Length(LowerStr) do begin
    I := PosEx('uses', LowerStr, I);
    if I = 0 then break;

    // Ensure "uses" is not part of an identifier (e.g. "abuses")
    if (I > 1) and (CharInSet(LowerStr[I - 1], ['a'..'z', '0'..'9', '_'])) then begin
      Inc(I, 4);
      continue;
    end;

    // Move past "uses" and any following whitespace
    J := I + 4;
    while (J <= Length(LowerStr)) and (LowerStr[J] in [#9, #10, #13, ' ']) do Inc(J);
    while (J <= Length(LowerStr)) and (LowerStr[J] <> ';') do Inc(J);

    // Extract the uses block content
    if J <= Length(LowerStr) then begin
      Result.Add(Trim(Copy(ContentStr, I + 4, J - (I + 4))));
      I := J + 1;
    end else
      break;
  end;
end;

procedure ProcessSourceFile(const FileName: String);
var
  I, J, InPos: Integer;
  Block, UnitTok, IncResolved: String;
  FullPath, ContentStr, SanitizedStr: String;
  Includes, Units, UsesBlocks, ContentStrList: TStringList;
begin
  if FileName.IsEmpty then exit;

  // Expand to full path for consistent tracking
  FullPath := ExpandFileName(FileName);

  // Avoid processing the same file multiple times (handle circular includes)
  if ProcessedFiles.IndexOf(FullPath) <> -1 then exit;
  ProcessedFiles.Add(FullPath);
  AddDependency(FullPath);

  if not FileExists(FullPath) then exit;

  // Read the file content
  try
    ContentStrList := TStringList.Create;
    ContentStrList.LoadFromFile(FullPath);
    ContentStr := ContentStrList.Text;
  finally
    ContentStrList.Free;
  end;

  // Extract include directives and process included files
  Includes := ExtractIncludes(ContentStr);
  try
    for I := 0 to Includes.Count - 1 do begin
      IncResolved := ResolveIncludePath(Includes[I], ExtractFilePath(FullPath));
      if not IncResolved.IsEmpty then ProcessSourceFile(IncResolved);
    end;
  finally
    Includes.Free;
  end;

  // Remove strings and comments to avoid false positives for "uses" and include directives
  SanitizedStr := SanitizeContent(ContentStr);
  UsesBlocks := ExtractUsesBlocks(SanitizedStr);

  // Process each uses block to extract unit dependencies
  Units := TStringList.Create;
  try
    // Each uses block may contain multiple units separated by commas, and may have "in" clauses
    for I := 0 to UsesBlocks.Count - 1 do begin
      Block := UsesBlocks[I];

      // Split the block into unit tokens using comma as a delimiter
      Units.Clear;
      Units.Delimiter := ',';
      Units.StrictDelimiter := True;
      Units.DelimitedText := StringReplace(Block, #13#10, ' ', [rfReplaceAll]);

      // Process each unit token
      for J := 0 to Units.Count - 1 do begin
        UnitTok := Trim(Units[J]);
        if UnitTok.IsEmpty then continue;

        // Check for "in" clause to handle units specified with a file path
        InPos := Pos(' in ', LowerCase(UnitTok));
        if InPos > 0 then begin
          UnitTok := Trim(Copy(UnitTok, InPos + 4, MaxInt));
          UnitTok := TrimQuotes(UnitTok);

          // Try to resolve the path specified in the "in" clause
          IncResolved := ResolveIncludePath(UnitTok, ExtractFilePath(FullPath));

          // If we couldn't resolve the path, it might be a unit name that was mistakenly put in an "in" clause
          if IncResolved.IsEmpty then IncResolved := UnitTok;

          // If we resolved it to a file, add it as a dependency
          AddDependency(IncResolved);
          continue;
        end;

        // If there's an "=", it means it's an alias (e.g. "MyUnit = SomeUnit")
        if Pos('=', UnitTok) > 0 then UnitTok := Trim(Copy(UnitTok, Pos('=', UnitTok) + 1, MaxInt));

        // Remove any trailing semicolons
        UnitTok := Trim(StringReplace(UnitTok, ';', '', [rfReplaceAll]));

        // Process the unit name to find the corresponding file
        ProcessUnitName(UnitTok, ExtractFilePath(FullPath));
      end;
    end;
  finally
    UsesBlocks.Free;
    Units.Free;
  end;
end;

procedure PrintUsage;
begin
  Writeln('Free Pascal Dependency Generator version 1.0');
  Writeln('fpcdep [options] <input>');
  Writeln('Options:');
  Writeln('  -Fu <path>    Add <path> to unit search path (can repeat)');
  Writeln('  -Fi <path>    Add <path> to include search path (can repeat)');
  Writeln('  -I  <path>    Add <path> to include search path (can repeat)');
  Writeln('  -o  <file>    Output dependency file (default: <input>.d)');
  Writeln('  -t  <target>  Target name to put before colon in dependency file (default: <input>.o)');
  Writeln('  -h            Show this help message');
end;

var
  SrcFile: String = '';
  OutFile: String = '';
  TargetName: String = '';
  OptCh: Char;

begin
  try
    UnitPaths := TStringList.Create;
    IncludePaths := TStringList.Create;

    ProcessedFiles := TStringList.Create;
    ProcessedFiles.Sorted := True;
    ProcessedFiles.Duplicates := dupIgnore;

    CollectedDeps := TStringList.Create;
    CollectedDeps.Sorted := True;
    CollectedDeps.Duplicates := dupIgnore;

    // Parse command-line arguments
    OptErr := false;
    repeat
      OptCh := GetOpt('ho:t:F:I:');
      case OptCh of
        'h': begin PrintUsage; exit; end;
        'o': OutFile := Trim(OptArg);
        't': TargetName := Trim(OptArg);
        'F': begin
          case OptArg[1] of
            'i': IncludePaths.AddStrings(Trim(Copy(OptArg, 2, MaxInt)));
            'u': UnitPaths.AddStrings(Trim(Copy(OptArg, 2, MaxInt)));
          end;
        end;
        'I': IncludePaths.AddStrings(Trim(OptArg));
      end;
    until OptCh = EndOfOptions;

    // The remaining argument is the source file
    if OptInd <= ParamCount then
      SrcFile := ParamStr(OptInd)
    else begin
      PrintUsage;
      exit;
    end;

    // Set default output file and target name if not provided
    if OutFile.IsEmpty then OutFile := ChangeFileExt(SrcFile, '.d');
    if TargetName.IsEmpty then TargetName := ChangeFileExt(SrcFile, '.o');

    // Ensure current directory is in search paths
    if UnitPaths.IndexOf('') = -1 then UnitPaths.Add('');
    if IncludePaths.IndexOf('') = -1 then IncludePaths.Add('');

    // Process the source file and collect dependencies
    ProcessSourceFile(SrcFile);

    // Write the collected dependencies to the output file in Makefile format
    with TStringList.Create do try
      Add(TargetName + ': ' + StringReplace(CollectedDeps.Text, sLineBreak, ' ', [rfReplaceAll]));
      SaveToFile(OutFile);
    finally
      Free;
    end;

  finally
    UnitPaths.Free;
    IncludePaths.Free;
    ProcessedFiles.Free;
    CollectedDeps.Free;
  end;
end.
