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

`-Fu <path>`
Add <path> to unit search path

`-Fi <path>`
Add <path> to include search path

`-I <path>`
Add <path> to include search path

`-o <file>`
Output dependency file (default: \<input\>.d)

`-t <target>`
Target name to put before colon in dependency file (default: \<input\>.o)

> [!NOTE]
> Any options not listed above will be ignored rather than returning an error. This will allow any flag variables passed to `fpc` to also be passed to `fpcdep`.

## License

MIT License.
See [LICENSE](LICENSE) file.
