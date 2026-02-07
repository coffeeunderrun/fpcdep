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
Add <path> to unit search path (can repeat)

`-Fi <path>`
Add <path> to include search path (can repeat)

`-I <path>`
Add <path> to include search path (can repeat)

`-o <file>`
Output dependency file (default: \<input\>.d)

`-t <target>`
Target name to put before colon in dependency file (default: \<input\>.o)

## License

MIT License.
See [LICENSE](LICENSE) file.
