module UCFML ( UCFMLFile (..)
             , UCFMLModel
             , UCFMLMeta
             , UCFMLBody
             , OptType (..)
             , OptSect
             , Option
             , Input (..)
             , FixedSetIn
             , FixedSetOpt
             , TextInput
             , NumbInput
             , ListIn
             , CompIn
             , Validator
             , UCFMLBool (..)
             , UCFMLNum (..)
             , UCFMLOperator (..)
             , UCFMLComparison (..)
             , UCFMLGate (..)
             , UCFMLExpr (..) ) where

import qualified Data.Text as T

{-
  **** types for representing UCFML ****
-}

data UCFMLFile = NoFile
               | NotAFile
               | RawFile T.Text
               | ParseError T.Text
               | ParsedFile UCFMLModel
               deriving (Eq, Show)

data UCFMLModel = UCFMLMdel
    { meta     :: UCFMLMeta
    , body     :: UCFMLBody
    , template :: UCFMLTemplate
    } deriving (Eq, Show)

data UCFMLMeta =  UCFMLmeta
    { mentitle :: UCFMLText
    , aboutmen :: UCFMLText
    , upstream :: UCFMLText
    , author   :: UCFMLText
    } deriving (Eq, Show)

data UCFMLBody = UCFMLBody [ OptType ]
               deriving (Eq, Show)

data OptType = Single Option
             | Several OptSect
             deriving (Eq, Show)

data OptSect = OptSect
    { sectitle :: UCFMLText
    , aboutsec :: UCFMLText
    , options  :: [ Option ]
    } deriving (Eq, Show)

data Option = Option
    { optitle :: UCFMLText
    , aboutop :: UCFMLText
    , refvar  :: UCFMLText -- ?????? maybe not the best way????? maybe a special different type??? idk????
    , input   :: Input
    } deriving (Eq, Show)

data Input = Dropdown FixedSetIn
           | Checkbox FixedSetIn
           | RadiButt FixedSetIn
           | Textual  TextInput
           | Numeric  NumbInput
           | Listed   ListIn Input
           | Compound CompIn Input
           deriving (Eq, Show)

data FixedSetIn = FixedSetIn
    { optlist  :: [ FixedSetOpt ]
    , minSel   :: Maybe Int -- these are the only real "validator" associated with checkbox, dropdown, or radio button inputs
    , maxSel   :: Maybe Int -- if no min or max selected options required then set both to Nothing, otherwise set to Just whateve the limit is
    } deriving (Eq, Show)

data FixedSetOpt = FixedSetOpt
    { isdef :: UCFMLBool -- whether this option is selected by default
    , ext   :: UCFMLText -- user facing text associated with the value
    , int   :: UCFMLText -- internal text asscociated with the value
    } deriving (Eq, Show)

data TextInput = TextInput
    { timinLn     :: Maybe Int -- Just the minimum number of character, or Nothing for no minimumm
    , timaxLn     :: Maybe Int -- Just the maximum number of character, or Nothing for no maximumm
    , tireqd      :: UCFMLBool -- whether or not there needs to be a valid value to generate a file
    , tidef       :: UCFMLText -- the text to be used by default
    , tivali      :: [ Validator]
    } deriving (Eq, Show)

data NumbInput = NumbInput
    { floating  :: UCFMLBool
    , signed    :: UCFMLBool
    , lrange    :: Maybe UCFMLNum
    , urange    :: Maybe UCFMLNum
    , base      :: NumBase
    , nireqd    :: Bool
    , nidef     :: Maybe UCFMLNum
    , nivali    :: [ Validator ]
    } deriving (Eq, Show)

data NumBase = Binary
             | Octal
             | Decimal
             | Hexadecimal
             deriving (Eq, Show)

data ListIn = ListIn
    { liminLn :: Maybe Int -- Nothing for no minimum or Just minimum for the minimum list length
    , limaxLn :: Maybe Int -- Nothing for no maximum or Just maximum for maximum length
    , lidef   :: [ UCFMLVal ]
    , liinput   :: Input
    } deriving (Eq, Show)

data UCFMLVal = UBool UCFMLBool
              | UNum  UCFMLNum
              | UText UCFMLText
              deriving (Eq, Show)

data CompIn = CompIn [(UCFMLText, Input )] -- [ (label, input) ]
            deriving (Eq, Show)

data Validator = MustBe UCFMLBool
               | MustNot UCFMLBool
               deriving (Eq, Show)

data UCFMLBool = UnsetB
               | UnresolvedB UCFMLBoolExpr
               | ResolvedB Bool
               deriving (Eq, Show)

data UCFMLNum = UnsetN
              | UnsignedInt Int
              | UnsignedFloat Float
              | SignedInt Int
              | SignedFloat Float
              | UnresolvedN UCFMLNumExpr
              deriving (Eq, Show)

data UCFMLText = UnsetT
               | Resolved T.Text
               | UnresolvedT UCFMLTextExpr
               deriving (Eq, Show)

data UCFMLExpr = BoolGen UCFMLBoolExpr
               | NumGen UCFMLNumExpr
               | TextGen UCFMLTextExpr
               deriving (Eq, Show)

data UCFMLBoolExpr = RegExp UCFMLRegExp
                   | BoolLog UCFMLBoolLog
                   | BoolComp UCFMLBoolComp
                   deriving (Eq, Show)

data UCFMLRegExp = UCFMLRegExp
    { source :: UCFMLText
    , regex  :: UCFMLText -- ?? idk maybe a better type for this ??
    } deriving (Eq, Show)

data UCFMLBoolLog = UCFMLBoolLog
    { blleft  :: UCFMLBool
    , gate  :: UCFMLGate
    , blright :: UCFMLBool
    } deriving (Eq, Show)

data UCFMLBoolComp = UCFMLBoolComp
    { bcleft  :: UCFMLNum
    , comp    :: UCFMLComparison
    , bcright :: UCFMLNum
    } deriving (Eq, Show)

data UCFMLNumExpr = UCFMLNumExpr
    { neleft  :: UCFMLNum
    , op      :: UCFMLOperator
    , neright :: UCFMLNum
    } deriving (Eq, Show)

data UCFMLTextExpr = SedExp UCFMLSedExp
                   | Conc UCFMLConcat
                   deriving (Eq, Show)

data UCFMLSedExp = UCFMLSedExp
    { sedinp :: UCFMLText
    , sedexp :: UCFMLText -- again, probably a better type for this
    } deriving (Eq, Show)

data UCFMLConcat = UCFMLConcat
    { between :: UCFMLText
    , texts   :: [ UCFMLText ]
    } deriving (Eq, Show)

data UCFMLOperator = Add
                   | Sub
                   | Mul
                   | Div
                   | IntDiv
                   | RemDiv
                   | Exp
                   deriving (Eq, Show)

data UCFMLComparison = EQ
                     | NEQ
                     | GT
                     | LT
                     | GTE
                     | LTE
                     deriving (Eq, Show)

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
               deriving (Eq, Show)

data UCFMLTemplate = UCFMLTemplate [ UCFMLConFile ] deriving (Eq, Show)

data UCFMLConFile = UCFMLConFile
    { fileName :: UCFMLText
    , filePath :: UCFMLFilePath
    , lines    :: [ UCFMLText ]
    } deriving (Eq, Show)


data UCFMLFilePath = UnixStyle UCFMLText
                   | WinStyle UCFMLText
                   deriving (Eq, Show)

{-
  **** Functions for these data types ****
-}
