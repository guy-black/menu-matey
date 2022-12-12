{-# LANGUAGE OverloadedStrings #-}

             -- TODO, make sure to only export what's needed
module UCFML {-( UCFMLFile (..)
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
             , UCFMLExpr (..) )-} where

import qualified Data.Text as T

-- ----------------------------
-- types for representing UCFML
-- ----------------------------

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

-- ------------------------------
-- Functions for parsing raw Text
-- ------------------------------

-- new plan
-- -- no quoted text in a ucfml model should be parsed as a symbol of any sort
-- -- dancing around quoted bits parsing the comments is hard
-- -- dequote whole file first before processing

-- Dequoting funcitions and datatypes
-- -----------------------------------

data Dequoted = Dequoted
    { qtMap :: Map QtToken Text
    , notQt :: [PossQuot]
    }

data QtToken = QtToken Int

-- # of \n in quotate   #of chars after last newline
data QtLns = QtLns Int Int

data PossQhot = Quo QtToken QtLns
              | NoQ Text

noqMap :: (T.Text -> a) -> [PossQuot] -> [a]
noqMap _ [] = []
noqMap f (p:ps) =
  case p of
    NoQ t -> (f p):(noqMap f ps)
    Quo _ -> (noqMap f ps)

dequote :: Dequoted -> T.Text -> Maybe Dequoted
dequote (Dequote qto nqt) rt =
  -- look for first "
  let (bef,rst) = T.break ('"'==) rt in

  -- if rst is empty, then no more quote symbols. there is nothing left to check
  if (rst == "") then
    -- reverse nqt before returning the value so everything looks right
    Just Dequoted qto reverse$((NoQ bef):nqt)

  -- check if the " comes on the same line as and after a line comment "--"
  else if ("--" `T.isInfixOf`(head . reverse . T.lines) $ bef) then
    -- if yes then this is a commented quote, recurse with all of bef AND the rest of the line in rest
    -- added to notQt in acc, and the rest of rst after the linebreak
    let (ln,txt) = T.break (=='\n') rst in
      dequote (Dequote qto ((NoQ (bef<>ln)):nqt)) txt

  -- check if its inside a block comment or just hanging loose or a full quot
  else
    case ((length . fst . breakOn "\"")T.tail $ rst)`ordering`((length . fst . breakOn "-}")T.tail $ rst) of
      EQ -> Nothing -- unclosed quotation mark, return nothing for parse error
      GT -> -- in a block comment, add everything up to the block closing bloc

      LT -> -- is quoted text, add to map and recurse with (snd . breakOn "\"")T.tail $ rst


-- remove all user comments --
-- ---------------------------
-- remove any comments from the file before messing further with it
-- remove any lines where the first nonwhitespace charactes are "--"
-- check all lines for an unquotted "--"
-- remove any text wrapped in unquoted "{-" and  "-}" and the brackets too
{- -- *********** START OF BIG COMMENT COMMENT BLOCK **********
uncomment :: T.Text -> T.Text
uncomment = (rmBlkCmt . rmPrLnCmt . rmWhLnCmt)

-- remove whole line comments
rmWhLnCmt :: T.Text -> T.Text
rmWhLnCmt rt = T.unlines $ filter (\x->(not . T.isPrefixOf "--")(T.stripStart x)) $ T.lines rt

-- remove partial line comments
rmPrLnCmt :: T.Text -> T.Text
rmPrLnCmt rt = T.unlines (rmPrLnCmt' [] (T.lines rt))

rmPrLnCmt' :: [T.Text] -> [T.Text] -> [T.Text]
rmPrLnCmt' acc [] = reverse acc
rmPrLnCmt' acc (t:ts) =
  case ((T.isInfixOf "--" t),(T.elem '"' t)) of
    -- TODO both quotation mark(s) and line comment are in the line
    (True, True) ->
      case (ordering (T.length ((fst . T.breakOn "--")t))(T.length ((fst . T.breakOn "\"")t))) of
        LT -> rmPrLnCmt' ((T.breakOn "--" t):acc) ts -- comment was first, treat as if there was only a comment
        GT -> -- quote was first
          -- TODO: change this to only cound non escaped quote
          -- --  in "this is a string with a \" randomly in it"
          -- --     ^count                    ^dont count     ^ count
          if (T.count "\"" t) > 1 then -- there is more than one quote in the line
            -- ignore line up through the closing quote, check the rest as regular
          else -- there is only one unclosed quote on the line
            -- see else case on False True
        _ -> undefined -- should never happen

    -- no quotation mark(s) yes line comment
    (True, False) -> rmPrLnCmt' ((T.breakOn "--" t):acc) ts

    -- TODO yes quotation mark(s) no line comment.
    (False, True) ->

          -- TODO: change this to only cound non escaped quote
          -- --  in "this is a string with a \" randomly in it"
          -- --     ^count                    ^dont count     ^ count
      if even (T.count "\"" t) then -- all open quotes closed themselves, no comments, treat as regular line
        rmPrLnCmt' (t:acc) ts
      else -- if there is an unclosed quotation
        -- add this line to the acc like any other with no comment
        -- add all subsequent lines to acc until I find one with a closing quote
        -- go back to regular loop ignoring up through the closing quote

    -- neither quotation mark(s) nor line comment
    (False, False) -> rmPrLnCmt' (t:acc) ts

qRmPrLnCmt' :: [T.Text] -> [T.Text] -> [T.Text]
qRmPrLnCmt' acc [] = reverse acc
qRmPrLnCmt' acc (t:ts) =
  -- if t contains no nonescaped " then
    -- qRmPrLnCmt' (t:acc) ts
  -- else if there is a nonescaped quote
    -- if there are no quotes or -- after the closing quote then
      -- rmPrLnCmt' (t:acc) ts
    -- else
      -- if the comment is first then
        -- rmPrLnCmt' ((T.breakOn "--" t):acc) ts
      -- else if the quote is first


-- remove block commenta
rmBlkCmt :: T.Text -> T.Text
rmBlkCmt rt = undefined
-- look for quotes and {-
-- -- if find a quote skip foward to its close and start look again
-- -- if find a "{-", then find the next "-}" and delete the brackets and any text inbetween
   -- recurse on the new text without the block comments until end

-} -- ******* END BIG COMMENT COMMENT BLOCK *********
-- split into Meta, Body, and template --
-- --------------------------------------
-- check that ther are only 3 lines of text touching first two columns, one for each section
-- if so return they're correlated sections, otherwise nothing
initSplit :: T.Text -> Either T.Text ([T.Text], [T.Text], [T.Text])
initSplit tx = undefined

-- try to parse first chunk into UCFMLMeta --
-- ------------------------------------------
metagen :: T.Text -> Either T.Text UCFMLMeta
metagen rt = undefined

-- try to parse second chunk into UCFMLBody --
-- -------------------------------------------
bodygen :: T.Text -> Either T.Text UCFMLBody
bodygen rt = undefined

-- try to parse last chunk into UCFMLTemplate --
-- -------------------------------------------
templategen :: T.Text -> Either T.Text UCFMLTemplate
templategen rt = undefined

-- one function to run them all
-- -----------------------------
bigParseFunc :: T.Text -> UCFMLFile
bigParseFunc = undefined
