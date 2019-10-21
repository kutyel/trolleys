# trolleys

🛒 Haskell program to schedule trolleys and shifts randomly!

## Usage

Get [ghcup](https://www.haskell.org/ghcup/)! (or [stack](https://docs.haskellstack.org/en/stable/README/))

```sh
$ curl https://get-ghcup.haskell.org -sSf | sh
```

Create a YAML file in the root directory with the **shifts** and **volunteers** you want to cover:

```yaml
# config.yml
shifts: [M, T, W, H, F, S]
volunteers:
  - name: Flavio
    availability: [S]
  - name: Lydia
    availability: [M, H, F, S]
  # ...
```

Run the program!

```sh
$ stack build
$ stack exec trolleys
```
