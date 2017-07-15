{-
   Copyright 2017 Rafael Felix

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# OPTIONS_GHC -fno-warn-orphans -fno-warn-missing-fields #-}

module EnumGen.EnumParser (
    enumParser, EnumAR(..), EnumDecl(..), EnumIdType(..), EnumItem(..), enumq
)

where
import           Data.Maybe
import           Language.Haskell.TH        (Body (..), Clause (..), Con (..),
                                             Dec (..), Exp (..), ExpQ, Name,
                                             Pat (..), Q, Type (..), integerL,
                                             litE, lookupTypeName,
                                             lookupValueName, mkName, stringL)
import           Language.Haskell.TH.Lib    (integerL)
import           Language.Haskell.TH.Quote
import           Language.Haskell.TH.Syntax (liftString, returnQ, runQ)
import           Text.Parsec                (ParseError, ParsecT, Stream, many,
                                             many1, optionMaybe, parse, (<|>))
import           Text.Parsec.Char           (char, digit, endOfLine, letter,
                                             spaces, string, upper)
import           Text.Parsec.String         (Parser)

data EnumIdType = Auto | Manual deriving (Show, Eq)


data EnumDecl = EnumDecl{declName ::String, idType::EnumIdType} deriving (Show, Eq)
data EnumAR = EnumAR {decl:: EnumDecl, items:: [EnumItem]} deriving (Show, Eq)
data EnumItem = EItem {itemId:: Int, itemName:: String} deriving (Show, Eq)

enumq =  QuasiQuoter { quoteDec = enumQuoter, quoteExp = liftString   }

ename:: Parser String
ename = (:) <$> upper <*> many (letter <|> digit <|> char '_')

uint:: Parser Int
uint = read <$> many1 digit

maybeid:: Parser EnumIdType
maybeid = fromMaybe Auto <$> optionMaybe ((string "Manual" *> return Manual) <|> (string "Auto" *> return Auto))

edecl:: Parser EnumDecl
edecl = EnumDecl <$> (spaces *> string "enum" *> spaces *> ename <* spaces)  <*> maybeid

eitem:: EnumDecl -> Parser EnumItem
eitem EnumDecl{idType = idt} = case idt of
    Auto   -> EItem <$> (spaces *> return 0) <*> ename <* spaces
    Manual -> EItem <$> (spaces *> (uint <* char ':')) <*> ename <* spaces

enumParser:: Parser EnumAR
enumParser = do
    decl <- edecl
    items <- many (eitem decl)
    return (EnumAR decl items)

itemsConNames:: String -> [EnumItem] -> [Name]
itemsConNames dName items = map (mkName . itemName) items ++ [mkName $ "Invalid_" ++ dName ]

itemsConsT:: [Name] -> [Con]
itemsConsT = map (\nam -> NormalC nam [])

buildExpr:: EnumAR -> Q [Dec]
buildExpr ar = do
    enumClassName <- fmap fromJust $ lookupTypeName "Enum"
    fromEnumName <- fmap fromJust $ lookupValueName "fromEnum"
    toEnumName <- fmap fromJust $ lookupValueName "toEnum"
    showName <- fmap fromJust $ lookupTypeName "Show"
    intName <- fmap fromJust $ lookupTypeName "Int"
    returnQ [DataD [] enumName [] Nothing consList [ConT showName],
        InstanceD Nothing [] (AppT (ConT enumClassName) (ConT enumName))
        [FunD fromEnumName $ map mkFromEnumPat (idNames ar) ,
        FunD toEnumName $ map mkToEnumPat (idNames ar) ],
        SigD funName (AppT (AppT ArrowT (ConT intName)) (ConT enumName)),
        FunD funName [Clause [VarP paramName] (NormalB (SigE (AppE (VarE toEnumName) (VarE paramName )) (ConT enumName))) []]]
    where
        dName = declName $ decl ar
        iType = idType $ decl ar
        enumName = mkName dName
        namesList = itemsConNames dName $ items ar
        consList = itemsConsT namesList
        funName = mkName $ "to" ++ dName
        paramName = mkName "pEid"

        buildIdList:: [EnumItem] -> [Int]
        buildIdList its = case iType of
            Auto   ->   [1 .. (length its)] ++ [- 1]
            Manual ->   map itemId its ++ [- 1]
        idNames::EnumAR -> [(Int, Name)]
        idNames EnumAR{items = is} = zip (buildIdList is) (itemsConNames dName is)
        mkFromEnumPat::(Int, Name) -> Clause
        mkFromEnumPat (itid, itname)  =  if itid /= -1
            then Clause [ConP itname []] (NormalB $ LitE $ integerL $ toInteger itid) []
            else Clause [WildP] (NormalB $ LitE $ integerL $ toInteger itid) []
        mkToEnumPat::(Int, Name)  -> Clause
        mkToEnumPat (itid, itname)  =  if itid /= -1
            then Clause [LitP $ integerL $ toInteger itid ] (NormalB $ ConE itname) []
            else Clause [WildP] (NormalB $ ConE itname) []


enumQuoter:: String -> Q [Dec]
enumQuoter = quote . parse enumParser ""
    where
        quote:: Either ParseError EnumAR -> Q [Dec]
        quote (Right result) = buildExpr result
        quote (Left _      ) = return []
