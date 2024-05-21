# Deprecated

```cabal
executable cron-csv-reader
  import:           warnings
  default-language: GHC2021
  hs-source-dirs:   test
  main-is:          CronCsvReader.hs
  build-depends:
    , base
    , misclib
    , text

executable dir-csv-reader
  import:           warnings
  default-language: GHC2021
  hs-source-dirs:   test
  main-is:          DirCsvReader.hs
  build-depends:
    , base
    , bytestring
    , cassava
    , directory
    , filepath
    , text
    , vector

executable yaml-csv-reader
  import:           warnings
  default-language: GHC2021
  hs-source-dirs:   test
  main-is:          YamlCsvReader.hs
  build-depends:
    , aeson
    , base
    , bytestring
    , cassava
    , directory
    , filepath
    , misclib
    , text
    , yaml

executable show-csv
  import:           warnings
  default-language: GHC2021
  hs-source-dirs:   test
  main-is:          ShowCsv.hs
  ghc-options:      -threaded -Wall -Wcompat -O2
  build-depends:
    , base
    , brick
    , bytestring
    , cassava
    , microlens
    , microlens-mtl
    , microlens-th
    , text
    , vector
    , vty
    , vty-crossplatform

executable show-csv-pro
  import:           warnings
  default-language: GHC2021
  hs-source-dirs:   test
  main-is:          ShowCsvPro.hs
  ghc-options:      -threaded -Wall -Wcompat -O2
  build-depends:
    , base
    , brick
    , bytestring
    , cassava
    , microlens
    , microlens-mtl
    , microlens-th
    , text
    , vector
    , vty
    , vty-crossplatform

executable csv-read
  import:           warnings
  default-language: GHC2021
  hs-source-dirs:   test
  main-is:          CsvRead.hs
  build-depends:
    , base
    , bytestring
    , text

executable cron-csv-reader3
  import:           warnings
  default-language: GHC2021
  hs-source-dirs:   test
  main-is:          CronCsvReader3.hs
  build-depends:
    , base
    , bytestring

executable cron-csv-reader2
  import:           warnings
  default-language: GHC2021
  hs-source-dirs:   test
  main-is:          CronCsvReader2.hs
  build-depends:
    , base
    , bytestring
    , text
    , text-icu

executable show-csv2
  import:           warnings
  default-language: GHC2021
  hs-source-dirs:   test
  main-is:          ShowCsv2.hs
  ghc-options:      -threaded -Wall -Wcompat -O2
  build-depends:
    , base
    , misclib

executable csv-read2
  import:           warnings
  default-language: GHC2021
  hs-source-dirs:   test
  main-is:          CsvRead2.hs
  build-depends:
    , base
    , bytestring
    , cassava
    , text
    , vector

executable csv-read3
  import:           warnings
  default-language: GHC2021
  hs-source-dirs:   test
  main-is:          CsvRead3.hs
  build-depends:
    , base
    , bytestring
    , cassava
    , text
    , text-icu
    , vector

executable csv-read4
  import:           warnings
  default-language: GHC2021
  hs-source-dirs:   test
  main-is:          CsvRead4.hs
  build-depends:
    , base
    , bytestring
    , hw-dsv
    , text
    , unordered-containers
    , vector

executable csv-read5
  import:           warnings
  default-language: GHC2021
  hs-source-dirs:   test
  main-is:          CsvRead5.hs
  build-depends:
    , base
    , bytestring
    , hw-dsv
    , text
    , unordered-containers
    , vector

executable spawn-proc
  import:           warnings
  default-language: GHC2021
  hs-source-dirs:   test
  main-is:          SpawnProc.hs
  build-depends:
    , base
    , process

executable bchan-demo
  import:           warnings
  default-language: GHC2021
  hs-source-dirs:   test
  ghc-options:      -threaded -Wall -Wcompat -O2
  main-is:          BChanDemo.hs
  build-depends:
    , base
    , brick
    , microlens
    , microlens-mtl
    , microlens-th
    , process
    , vty
    , vty-crossplatform

```
