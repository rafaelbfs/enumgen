# enumgen
DSL to Generate Haskell Enum instances

You create a enum using
```
[enumq|
  enum SampleEnum <Manual/Auto>
    1:EnumItemOne
    5:EnumItemFive
    ..............
|]
```

OBS: It hasn't been tested for Auto mode

```
An example from ghcisandbox.hs 
    Language.Haskell.TH.Quote.quoteDec
      enumq
      "\n\
      \    enum EnTest Manual\n\
      \        1:EnTstOne\n\
      \        2:EnTstTwo\n\
      \        10:EnTstTen\n"
 ```
 
 is expanded to
======>
```
    data EnTest
      = EnTstOne | EnTstTwo | EnTstTen | Invalid_EnTest
      deriving (Show)
    instance Enum EnTest where
      fromEnum EnTstOne = 1
      fromEnum EnTstTwo = 2
      fromEnum EnTstTen = 10
      fromEnum _ = -1
      toEnum 1 = EnTstOne
      toEnum 2 = EnTstTwo
      toEnum 10 = EnTstTen
      toEnum _ = Invalid_EnTest
```


