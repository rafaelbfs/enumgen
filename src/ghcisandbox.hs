{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-missing-fields #-}

{-
   Copyright 2017 Rafael Felix

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

-}

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

        toEnumTest:: Int -> EnumTest
        toEnumTest i = toEnum i ::EnumTest
    |]


[enumq|
    enum EnTest Manual
        1:EnTstOne
        2:EnTstTwo
        10:EnTstTen
|]

{-
[DataD [] EnumTest_1 [] Nothing [NormalC Tst1_2 [],NormalC Tst2_3 [],NormalC Tst3_4 [],NormalC Invalid_5 []] [ConT GHC.Show.Show],
InstanceD Nothing [] (AppT (ConT GHC.Enum.Enum) (ConT EnumTest_1))
[FunD GHC.Enum.fromEnum [Clause [ConP Tst1_2 []] (NormalB (LitE (IntegerL 1))) [],
                         Clause [ConP Tst2_3 []] (NormalB (LitE (IntegerL 2))) [],
                         Clause [ConP Tst3_4 []] (NormalB (LitE (IntegerL 3))) [],
                         Clause [WildP] (NormalB (AppE (VarE GHC.Num.negate) (LitE (IntegerL 1)))) []],
FunD GHC.Enum.toEnum [Clause [LitP (IntegerL 1)] (NormalB (ConE Tst1_2)) [],
                      Clause [LitP (IntegerL 2)] (NormalB (ConE Tst2_3)) [],
                      Clause [LitP (IntegerL 3)] (NormalB (ConE Tst3_4)) [],
                      Clause [WildP] (NormalB (ConE Invalid_5)) []]
                    ],
SigD toEnumTest_0 (AppT (AppT ArrowT (ConT GHC.Types.Int)) (ConT EnumTest_1)),
FunD toEnumTest_0 [Clause [VarP i_6] (NormalB (SigE (AppE (VarE GHC.Enum.toEnum) (VarE i_6)) (ConT EnumTest_1))) []]]

-}
