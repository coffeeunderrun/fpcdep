# fpcdep

Small Free Pascal utility to generate Makefile-style dependency files from Pascal source.

## Required

`fpc`
`make`

## Build

```bash
fpcmake && make
```

## Install

```bash
make install
```

## Clean

```bash
make clean
```

## Usage

```bash
fpcdep [options] <input>
```

### Options

`-o <x>`
Output to file \<x\> (default is input filename with .d extension)

`-t <x>`
Rule target, e.g., \<x\> : prerequisites (default is input filename with .o extension)

`-I <x>`
Add \<x\> to include search path

`-Fi <x>`
Add \<x\> to include search path

`-Fu <x>`
 Add \<x\> to unit search path

`-FU <x>`
Set the prerequisite unit path to \<x\> (default is the directory of the target)

`-Pe <x>`
Set the prerequisite unit extension to \<x\> (default is .ppu)

> [!NOTE]
> Any options not listed above will be ignored rather than returning an error. This will allow any flag variables passed to `fpc` to also be passed to `fpcdep`.

> [!IMPORTANT]
> Prerequisites will not be added if `fpcdep` cannot find them in any of the provided search paths.

> [!IMPORTANT]
> For units, `fpcdep` will look for the source files, `.pas` or `.pp`, and not compiled, `.ppu`.

## Example

### Input

myprg.pas
```pascal
program myprg;
uses myunit;
begin
end.
```

myunit.pas
```pascal
unit myunit;
interface
{$I myinc1.inc}
implementation
end.
```

myinc1.inc
```pascal
{$I myinc2.inc}
```

### Output

```bash
fpcdep -Pe.o -FUunits -tmyprg myprg.pas
fpcdep -tunits/myunit.o myunit.pas
```

myprg.d
```makefile
myprg : myprg.pas units/myunit.o
```

myunit.d
```makefile
units/myunit.o : myunit.pas myinc1.inc myinc2.inc
```

## License

MIT License.
See [LICENSE](LICENSE) file.
