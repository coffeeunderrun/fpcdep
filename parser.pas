unit Parser;

{$mode objfpc}{$H+}

interface

uses Classes;

type
  TExtractCallback = function (const Content : String) : TStringList;

  TParserOptions = record
    UnitPrereqPath : String;
    UnitPrereqExtension : String;
    UnitSourceExtensions : array of String;
    ExtractIncludeCallback : TExtractCallback;
    ExtractUnitCallback : TExtractCallback;
  end;

var
  ParserOptions : TParserOptions;

procedure AddIncludeSearchPath(const Path : String); inline;
procedure AddUnitSearchPath(const Path : String); inline;

function ParseSourceFile(const FilePath : String) : Boolean;
function WriteOutputFile(const FilePath, TargetName : String) : Boolean;

implementation

uses SysUtils;

var
  IncludeSearchPaths, UnitSearchPaths : TStringList;
  ParsedFiles, Prerequisites : TStringList;

procedure AddIncludeSearchPath(const Path : String);
begin
  IncludeSearchPaths.Add(Path);
end;

procedure AddUnitSearchPath(const Path : String);
begin
  UnitSearchPaths.Add(Path);
end;

function ParseSourceFile(const FilePath : String) : Boolean;
var
  InputFile : TStringList;
  InputFileContent : String;

  IncludeFileNames, UnitNames : TStringList;
  IncludeFileName, UnitName : String;

  SearchRec : TSearchRec;
  SearchPath, SearchUnitExtension, CandidateFilePath : String;
begin
  // Avoid parsing the same file multiple times
  ParsedFiles.Add(FilePath);
  Prerequisites.Add(FilePath);

  if FilePath.IsEmpty then begin
    Writeln('Error: No input file specified.');
    exit(false);
  end;

  if not FileExists(FilePath) then begin
    Writeln(Format('Warning: File "%s" does not exist.', [FilePath]));
    exit(false);
  end;

  // Read content from the source file
  InputFile := TStringList.Create;
  try
    try
      InputFile.LoadFromFile(FilePath);
      InputFileContent := InputFile.Text;
    except
      Writeln(Format('Error: Could not read input file "%s".', [FilePath]));
      exit(false);
    end;
  finally
    InputFile.Free;
  end;

  // Extract include file names from the source file
  if Assigned(ParserOptions.ExtractIncludeCallback) then begin
    IncludeFileNames := ParserOptions.ExtractIncludeCallback(InputFileContent);
    try
      for IncludeFileName in IncludeFileNames do begin
        for SearchPath in IncludeSearchPaths do begin
          if FindFirst(IncludeTrailingPathDelimiter(SearchPath) + '*', faAnyFile, SearchRec) <> 0 then
            continue;

            repeat
              if Lowercase(SearchRec.Name) <> Lowercase(IncludeFileName) then continue;

              CandidateFilePath := IncludeTrailingPathDelimiter(SearchPath) + SearchRec.Name;

              // Do not parse the same file multiple times to avoid infinite recursion
              if ParsedFiles.IndexOf(CandidateFilePath) >= 0 then continue;

              // Recursively parse the included file to extract its prerequisites
              if not ParseSourceFile(CandidateFilePath) then exit(false);

              // Add the found include file to prerequisites
              if Prerequisites.IndexOf(CandidateFilePath) < 0 then Prerequisites.Add(CandidateFilePath);
              break;
            until FindNext(SearchRec) <> 0;

            FindClose(SearchRec);
        end;
      end;
    finally
      IncludeFileNames.Free;
    end;
  end;

  { TODO: Handle specific unit extensions for "uses" statements, e.g., uses MyUnit in 'MyUnit.pas';
    Currently, only the unit name is extracted, and the source file is searched for using '.pas' or '.pp' extensions. }
  // Extract unit names from the source file
  if Assigned(ParserOptions.ExtractUnitCallback) then begin
    UnitNames := ParserOptions.ExtractUnitCallback(InputFileContent);
    try
      for UnitName in UnitNames do begin
        for SearchPath in UnitSearchPaths do begin
          for SearchUnitExtension in ParserOptions.UnitSourceExtensions do begin
            if FindFirst(IncludeTrailingPathDelimiter(SearchPath) + '*' + SearchUnitExtension, faAnyFile, SearchRec) <> 0 then
              continue;

            CandidateFilePath := ChangeFileExt(UnitName, SearchUnitExtension);

            repeat
              if Lowercase(SearchRec.Name) <> Lowercase(CandidateFilePath) then continue;

              // The source is only to ensure the unit isn't part of the RTL; the prerequisite is the compiled unit file
              CandidateFilePath := ChangeFileExt(SearchRec.Name, ParserOptions.UnitPrereqExtension);
              CandidateFilePath := IncludeTrailingPathDelimiter(ParserOptions.UnitPrereqPath) + CandidateFilePath;

              // Add the found unit file to prerequisites
              if Prerequisites.IndexOf(CandidateFilePath) < 0 then Prerequisites.Add(CandidateFilePath);
              break;
            until FindNext(SearchRec) <> 0;

            FindClose(SearchRec);
          end;
        end;
      end;
    finally
      UnitNames.Free;
    end;
  end;

  Result := true;
end;

function WriteOutputFile(const FilePath, TargetName : String) : Boolean;
var
  OutputFle : TextFile;
begin
  try
    try
      AssignFile(OutputFle, FilePath);
      Rewrite(OutputFle);
      WriteLn(OutputFle, Format('%s : %s', [TargetName, Prerequisites.DelimitedText]));
    except
      Writeln(Format('Error: Could not write to output file "%s".', [FilePath]));
      exit(false);
    end;
  finally
    CloseFile(OutputFle);
  end;

  Result := true;
end;

initialization
  IncludeSearchPaths := TStringList.Create;
  IncludeSearchPaths.Duplicates := dupIgnore;
  IncludeSearchPaths.Delimiter := PathSeparator;

  UnitSearchPaths := TStringList.Create;
  UnitSearchPaths.Duplicates := dupIgnore;
  UnitSearchPaths.Delimiter := PathSeparator;

  ParsedFiles := TStringList.Create;
  ParsedFiles.Duplicates := dupIgnore;

  Prerequisites := TStringList.Create;
  Prerequisites.Duplicates := dupIgnore;
  Prerequisites.Delimiter := ' ';

  with ParserOptions do begin
    UnitPrereqPath := '';
    UnitPrereqExtension := '.ppu';
    UnitSourceExtensions := ['.pas', '.pp'];
  end;

finalization
  IncludeSearchPaths.Free;
  UnitSearchPaths.Free;
  ParsedFiles.Free;
  Prerequisites.Free;
end.
