# Computational Logic

## Installation

For example, on macOS, you can install [SWI-Prolog](https://formulae.brew.sh/formula/swi-prolog) with Homebrew:

```sh
brew install swi-prolog
```

In this case, the executable path for the `arthurwang.vsc-prolog` extension should be set to `/opt/homebrew/bin/swipl`:

```json
{
  "prolog.executablePath": "/opt/homebrew/bin/swipl"
}
```

## Testing

```sh
cd prolog
swipl --goal=cli -o cli -c cli.pl
pytest
```
