# doodles

- Using `text-icu` library in Haskell on Mac OS [ref1](https://stackoverflow.com/a/7865018/8163324) & [ref2](https://github.com/haskell/cabal/issues/2997)

    ```sh
    brew install icu4c
    brew info icu4c # lookup the real lib-dirs/include-dirs
    ```

    create a `cabal.project.local` file at the project's root:

    ```cabal
    # cabal.project.local

    -- for MacOS
    package text-icu
      extra-include-dirs: /opt/homebrew/opt/icu4c/include
      extra-lib-dirs: /opt/homebrew/opt/icu4c/lib
    ```

- Using `hasql` library in Haskell

    ```sh
    # MacOS
    brew install postgres
    # Ubuntu
    apt install libpq-dev
    ```
