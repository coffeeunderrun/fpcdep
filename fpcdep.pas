program fpcdep;

{$mode objfpc}{$H+}

uses Classes, GetOpts, Parser, RegExpr, StrUtils, SysUtils;

const
  VERSION = '1.1.0';

function TrimQuotes(const Content: String): String;
var
  ContentPtr: PChar;
begin
  if Content.IsEmpty then exit(Content);
  ContentPtr := PChar(Content);
  if not (ContentPtr[0] in ['"', '''']) then exit(Content);
  Result := AnsiExtractQuotedStr(ContentPtr, ContentPtr[0]);
end;

function ExtractIncludes(const Content : String) : TStringList;
const
  INCLUDE_REGEX = '\{\$(?:I|INCLUDE)\s+(?:''([^'']+)''|([^\s}]+))';
var
  RegEx : TRegExpr;
begin
  Result := TStringList.Create;
  Result.Duplicates := dupIgnore;

  RegEx := TRegExpr.Create;
  try
    // Case insensitive
    RegEx.ModifierI := true;

    RegEx.Expression := INCLUDE_REGEX;

    if RegEx.Exec(Content) then repeat
      if not RegEx.Match[1].IsEmpty then Result.Add(Trim(RegEx.Match[1]))
      else if not RegEx.Match[2].IsEmpty then Result.Add(Trim(RegEx.Match[2]));
    until not RegEx.ExecNext;
  finally
    RegEx.Free;
  end;
end;

{ TODO: Consider a proper tokenizer to parse "uses" statements }
function ExtractUnits(const Content : String) : TStringList;
var
  ContentLength: Integer;
  ContentPtr, BufPtr, FoundPtr, AfterUsesPtr, ScanPtr: PChar;
  K, P, InPos: Integer;
  Block, Token, Clause: String;
  BlockLen: Integer;
begin
  Result := TStringList.Create;
  Result.Duplicates := dupIgnore;

  ContentLength := Length(Content);
  ContentPtr := PChar(Content);
  BufPtr := ContentPtr;

  while Assigned(BufPtr) and (BufPtr^ <> #0) do begin
    // Search for "uses" as a whole word
    FoundPtr := SearchBuf(
      BufPtr,
      ContentLength - (BufPtr - ContentPtr),
      0,
      ContentLength - (BufPtr - ContentPtr),
      'uses',
      [soWholeWord]
    );

    if FoundPtr = nil then Break;

    // Move past "uses"
    AfterUsesPtr := FoundPtr + 4;

    // Skip whitespace after "uses"
    while (AfterUsesPtr^ <> #0) and (AfterUsesPtr^ in [#9, #10, #13, ' ']) do Inc(AfterUsesPtr);

    // Find end of the uses block (semicolon or end of buffer)
    ScanPtr := AfterUsesPtr;
    while (ScanPtr^ <> #0) and (ScanPtr^ <> ';') do Inc(ScanPtr);

    // Extract the block of text containing unit names
    BlockLen := ScanPtr - AfterUsesPtr;
    if BlockLen > 0 then begin
      SetString(Block, AfterUsesPtr, BlockLen);
      Block := Trim(Block);

      K := 1;
      while K <= Length(Block) do begin
        P := PosEx(',', Block, K);
        if P = 0 then P := Length(Block) + 1;

        Token := Trim(Copy(Block, K, P - K));

        // Remove trailing semicolon if present
        if (Length(Token) > 0) and (Token[Length(Token)] = ';') then SetLength(Token, Length(Token) - 1);

        // Remove any " in 'file'" clause (keep only the unit name)
        InPos := PosEx(' in ', LowerCase(Token), 1);
        if InPos > 0 then begin
          Clause := Trim(Copy(Token, InPos + 4, MaxInt));
          Clause := TrimQuotes(Clause);
          Token := Clause;
        end;

        if not Token.IsEmpty then Result.Add(Token);

        K := P + 1;
      end;
    end;

    // Continue scanning after the semicolon (if present)
    BufPtr := ScanPtr;
    if (BufPtr^ <> #0) then Inc(BufPtr);
  end;
end;

var
  SourceFile, OutputFile, TargetName : String;

procedure Help;
begin
  Writeln(Format('Free Pascal Dependency Generator version %s', [VERSION]));
  Writeln('fpcdep [options] <input>');
  Writeln('Options:');
  Writeln(' -h      Show this help message');
  Writeln(' -o <x>  Output to file <x> (default is input filename with .d extension)');
  Writeln(' -t <x>  Rule target, e.g., <x> : prerequisites (default is input filename with .o extension)');
  Writeln(' -I <x>  Add <x> to include search path');
  Writeln(' -Fi<x>  Add <x> to include search path');
  Writeln(' -Fu<x>  Add <x> to unit search path');
  Writeln(' -FU<x>  Set the prerequisite unit path to <x> (default is the directory of the target)');
  Writeln(' -Pe<x>  Set the prerequisite unit extension to <x> (default is .ppu)');
  Halt(0);
end;

procedure ParseCommandLineOptions;
var
  OptCh: Char;
begin
  OptErr := false;
  repeat
    OptCh := GetOpt('ho:t:I:F:P:');
    case OptCh of
      'h': Help;
      'o': OutputFile := Trim(OptArg);
      't': TargetName := Trim(OptArg);
      'I': AddIncludeSearchPath(OptArg);
      'F': case OptArg[1] of
        'i': AddIncludeSearchPath(OptArg.SubString(1));
        'u': AddUnitSearchPath(OptArg.SubString(1));
        'U': ParserOptions.UnitPrereqPath := OptArg.SubString(1);
      end;
      'P': case OptArg[1] of
        'e': ParserOptions.UnitPrereqExtension := Trim(OptArg.SubString(1));
      end;
    end;
  until OptCh = EndOfOptions;

  // The remaining argument is the input file
  if OptInd <= ParamCount then SourceFile := ParamStr(OptInd) else Help;
end;

begin
    ParseCommandLineOptions;

    // Set output file and target name if not specified
    if OutputFile.IsEmpty then OutputFile := ChangeFileExt(SourceFile, '.d');
    if TargetName.IsEmpty then TargetName := ChangeFileExt(SourceFile, '.o');

    with ParserOptions do begin
      // Set prerequisite unit path to the directory of the target if not specified
      if UnitPrereqPath.IsEmpty then UnitPrereqPath := ExtractFilePath(TargetName);

      ExtractIncludeCallback := @ExtractIncludes;
      ExtractUnitCallback := @ExtractUnits;
    end;

    if ParseSourceFile(SourceFile) then WriteOutputFile(OutputFile, TargetName);
end.
