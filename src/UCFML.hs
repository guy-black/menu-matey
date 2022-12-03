module UCFML ( ParsedFile (...)
             , UCFMLmodel
             , UCFMLmeta
             , UCFMLbody
             , OptType (...)
             , OptSect
             , Option
             , Input (...)
             , FixedSetIn
             , FixedSetOpt
             , TextInput
             , NumbInput
             , ListIn
             , CompIn
             , Validator
             , UCFMLBool (...)
             , UCFMLNum (...)
             , UCFMLOperator (...)
             , UCFMLComparison (...)
             , UCFMLGate (...)
             , UCFMLExpr (...) ) where

import qualified Data.Text as T

{-
  **** types for representing UCFML ****
-}

data UCFMLFile = NoFile
               | NotAFile
               | RawFile T.Text
               | ParseError T.Text
               | ParsedFile UCFMLmodel

data UCFMLModel = UCFMLMdel
    { meta     :: UCFMLMeta
    , body     :: UCFMLBody
    , template :: UCFMLTemplate
    }

data UCFMLMeta =  UCFMLmeta
    { title    :: UCFMLText
    , about    :: UCFMLText
    , upstream :: UCFMLText
    }

type UCFMLBody = [ OptType ]

data OptType = Single Option
             | Several OptSect

data OptSect = OptSect
    { title   :: UCFMLText
    , about   :: UCFMLText
    , options :: [ Option ]
    }

data Option = Option
    { title  :: UCFMLText
    , about  :: UCFMLText
    , refvar :: UCFMLText -- ?????? maybe not the best way????? maybe a special different type??? idk????
    , input  :: Input
    }

data Input = Dropdown FixedSetIn
           | Checkbox FixedSetIn
           | RadiButt FixedSetIn
           | Textual  TextInput
           | Numeric  NumbInput
           | Listed   ListIn Input
           | Compound CompIn Input

data FixedSetIn = FixedSetIn
    { optlist  :: [ FixedSetOpt ]
    , minSel   :: Maybe Int -- these are the only real "validator" associated with checkbox, dropdown, or radio button inputs
    , maxSel   :: Maybe Int -- if no min or max selected options required then set both to Nothing, otherwise set to Just whateve the limit is
    }

data FixedSetOpt = FixedSetOpt
    { def :: UCFMLBool -- whether this option is selected by default
    , ext :: UCFMLText -- user facing text associated with the value
    , int :: UCFMLText -- internal text asscociated with the value
    }

data TextInput = TextInput
    { minLn     :: Maybe Int -- Just the minimum number of character, or Nothing for no minimumm
    , maxLn     :: Maybe Int -- Just the maximum number of character, or Nothing for no maximumm
    , required  :: UCFMLBool -- whether or not there needs to be a valid value to generate a file
    , def       :: UCFMLText -- the text to be used by default
    , validator :: [ Validator]

data NumbInput = NumbInput
    { floating  :: UCFMLBool
    , signed    :: UCFMLBool
    , LRange    :: Maybe UCFMLNum
    , URange    :: Maybe UCFMLNum
    , base      :: NumBase
    , required  :: Bool
    , def       :: Maybe UCFMLNum
    , validator :: [ Validator ]
    }

data ListIn = ListIn
    { minLn   :: Maybe Int -- Nothing for no minimum or Just minimum for the minimum list length
    , maxLn   :: Maybe Int -- Nothing for no maximum or Just maximum for maximum length
    , def     :: [ UCFMLVal ]
    , input   :: Input
    }

data UCFMLVal = UBool UCFMLBool
              | UNum  UCFMLNum
              | UText UCFMLText

data CompIn = [ (UCFMLText, Input ) ] -- [ (label, input) ]

data Validator = MustBe UCFMLBool
               | MustNot UCFMLBool

type UCFMLBool = Unset
               | Unresolved UCFMLBoolExpr
               | Resolved Bool

data UCFMLNum = Unset
              | UnsignedInt Int
              | UnsignedFloat Float
              | SignedInt Int
              | SignedFloat Float
              | Unresolved UCFMLNumExpr

data UCFMLText = Unset
               | Resolved Text
               | Unresolved UCFMLTextExpr

data UCFMLExpr = BoolGen UCFMLBoolExpr
               | NumGen UCFMLNumExpr
               | TextGen UCFMLTextExpr

data UCFMLBoolExpr = RegExp UCFMLRegExp
                   | BoolLog UCFMLBoolLog
                   | BoolComp UCFMLBoolComp

data UCFMLRegExp = UCFMLRegExp
    { source :: UCFMLText
    , regex  :: Text -- ?? idk maybe a better type for this ??
    }

data UCFMLBoolLog = UCFMLBoolLog
    { left  :: UCFMLBool
    , gate  :: UCFMLGate
    , right :: UCFMLBool
    }

data UCFMLBoolComp = UCFMLBoolComp
    { left  :: UCFMLNum
    , comp  :: UCFMLComparison
    , right :: UCFMLNum
    }

data UCFMLNumExpr = UCFMLNumExpr
    { left  :: UCFMLNum
    , Op    :: UCFMLOperator
    , right :: UCFMLNum
    }

data UCFMLTextExpr = SedExp UCFMLSedExp
                   | Conc UCFMLConcat

data UCFMLSedExp = UCFMLSedExp
    { source :: UCFMLText
    , sedexp :: Text -- again, probably a better type for this
    }

data UCFMLConcat = UCFMLConcat
    { between :: UCFMLText
    , texts   :: [ UCFMLText ]
    }

data UCFMLOperator = Add
                   | Sub
                   | Mul
                   | Div
                   | IntDiv
                   | RemDiv
                   | Exp

data UCFMLComparison = EQ
                     | NEQ
                     | GT
                     | LT
                     | GTE
                     | LTE

data UCFMLGate = AND
               | NAND
               | OR
               | NOR
               | XOR
               | XNOR
               | NEVER
               | ALWYS
               | IDL
               | NIDL
               | IDR
               | NIDR
               | LNOTR
               | NLNTR
               | RNOTL
               | NRNTL

type UCFMLTemplate = [ UCFMLConFile ]

data UCFMLConFile = UCFMLConFile
    { fileName :: UCFMLText
    , filePath :: UCFMLFilePath
    , lines    :: [ UCFMLText ]
    }

data UCFMLFilePath = UnixStyle UCFMLText
                   | WinStyle UCFMLText

{-
  **** Functions for these data types ****
-}
