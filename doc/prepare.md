Prepare to use EasyMode
=======================

EasyMode depends on several language extensions:

- OverloadedStrings
- OverloadedRecordDot
- DuplicateRecordFields
- NoFieldSelectors
- NoImplicitPrelude

For a new cabal project, it is suggested to add these to your cabal file to enable these language extensions globally:

```
common common-options
  build-depends:
      base >=4.7 && <5
    , easy-mode
  default-language: Haskell2010
  default-extensions:
      OverloadedStrings
      OverloadedRecordDot
      DuplicateRecordFields
      NoFieldSelectors
      NoImplicitPrelude

library
  import: common-options
```

And for single file, you can add Pragmas like:

```haskell
{-# language OverloadedStrings #-}
{-# language OverloadedRecordDot #-}
{-# language DuplicateRecordFields #-}
{-# language NoFieldSelectors #-}
{-# language NoImplicitPrelude #-}

import EasyMode
```

Or in a single line:

```haskell
{-# language OverloadedStrings,OverloadedRecordDot,DuplicateRecordFields,NoFieldSelectors,NoImplicitPrelude #-}

import EasyMode
```
