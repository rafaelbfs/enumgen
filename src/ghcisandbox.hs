{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-missing-fields #-}

module GhciSandbox

where

import           EnumGen.EnumParser         (enumq)
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax (Quasi, addTopDecls, returnQ, runQ)


testDecl =
    [d|

        data EnumTest = Tst1 | Tst2 | Tst3 | Invalid deriving (Show)

        instance Enum EnumTest where
            fromEnum Tst1 = 1
            fromEnum Tst2 = 2
            fromEnum Tst3 = 3
            fromEnum _    = (- 1)

            toEnum 1 = Tst1
            toEnum 2 = Tst2
            toEnum 3 = Tst3
            toEnum _ = Invalid
    |]


[enumq|
    enum EnTest Manual
        1:EnTstOne
        2:EnTstTwo
        10:EnTstTen
|]

{-
[DataD [] EnumTest_0 [] Nothing [NormalC Tst1_1 [],NormalC Tst2_2 [],NormalC Tst3_3 [],NormalC Invalid_4 []] [],
InstanceD Nothing [] (AppT (ConT GHC.Enum.Enum) (ConT EnumTest_0))
    [   FunD GHC.Enum.fromEnum
        [                       Clause [ConP Tst1_1 []] (NormalB (LitE (IntegerL 1))) [],
                                Clause [ConP Tst2_2 []] (NormalB (LitE (IntegerL 2))) [],
                                Clause [ConP Tst3_3 []] (NormalB (LitE (IntegerL 3))) [],
                                Clause [WildP] (NormalB (AppE (VarE GHC.Num.negate) (LitE (IntegerL 1)))) []
        ],
        FunD GHC.Enum.toEnum
        [                     Clause [LitP (IntegerL 1)] (NormalB (ConE Tst1_1)) [],
                              Clause [LitP (IntegerL 2)] (NormalB (ConE Tst2_2)) [],
                              Clause [LitP (IntegerL 3)] (NormalB (ConE Tst3_3)) [],
                              Clause [WildP] (NormalB (ConE Invalid_4)) []
        ]
    ]
] -- end [Dec]


-}
