{-# LANGUAGE OverloadedStrings #-}

-- TODO, make sure to only export what's needed
module UCFML where

import qualified Data.Text as T
import qualified Data.Map as M
import Data.Char

-- ----------------------------
-- types for representing UCFML
-- ----------------------------

-- data type to represent whether or not a file was passed as an argument, if that
-- file can be read, and if that file can be parsed into a UCFMLModel
data UCFMLFile = NoFile -- no file was passed as an argument
               | NotAFile --  cannot find/read the file passed as an argument
               | RawFile T.Text -- file was read and is in raw text
               | ParseError T.Text -- file couldn't be parsed because <reason>
               | ParsedFile UCFMLModel -- file parsed succesfully
               deriving (Eq, Show)

data UCFMLModel = UCFMLModel
    { dict     :: M.Map T.Text UCFMLVal
    , meta     :: UCFMLMeta
    , body     :: UCFMLBody
    , template :: UCFMLTemplate
    } deriving (Eq, Show)

data UCFMLMeta =  UCFMLMeta
    { mentitle :: UCFMLText
    , aboutmen :: UCFMLText
    , upstream :: UCFMLText
    , author   :: UCFMLText
    } deriving (Eq, Show)

data UCFMLBody = UCFMLList OptType
               deriving (Eq, Show)

data OptType = Single  Option
             | Several OptSect
             | CondOpt OptType
             deriving (Eq, Show)

data OptSect = OptSect
    { sectitle :: UCFMLText
    , aboutsec :: UCFMLText
    , options  :: UCFMLList Option
    } deriving (Eq, Show)

data Option = Option
    { optitle :: UCFMLText
    , aboutop :: UCFMLText
    , refvar  :: UCFMLText -- needs to be UCFMLText because the refVar of an option needs
                                -- to be able to be set programmatically by forEach
    , input   :: Input
    } deriving (Eq, Show)

data Input = Dropdown FixedSetIn
           | Checkbox FixedSetIn
           | RadiButt FixedSetIn
           | Textual  TextInput
           | Numeric  NumbInput
           | Listed   ListIn Input
           | Compound CompIn Input
           | CondIn   Input
           deriving (Eq, Show)

data FixedSetIn = FixedSetIn
    { optlist  :: UCFMLList FixedSetOpt
    , minSel   :: UCFMLNum -- these are the only real
    , maxSel   :: UCFMLNum -- "validator" for fixed set inputs
    } deriving (Eq, Show)

data FixedSetOpt = FixedSetOpt
    { isdef :: Bool             -- whether this option is selected by default
    , ext   :: UCFMLText -- user facing text associated with the value
    , int   :: UCFMLText -- internal text asscociated with the value
    } deriving (Eq, Show)

data TextInput = TextInput
    { timinLn   :: UCFMLNum -- Just the minimum number of character, or Nothing for no minimumm
    , timaxLn   :: UCFMLNum -- Just the maximum number of character, or Nothing for no maximumm
    , tireqd    :: UCFMLBool -- whether or not there needs to be a valid value to generate a file
    , tidef     :: UCFMLText -- the text to be used by default
    , tivali    :: UCFMLList Validator
    } deriving (Eq, Show)

data NumbInput = NumbInput
    { floating  :: UCFMLBool
    , signed    :: UCFMLBool
    , lrange    :: UCFMLNum
    , urange    :: UCFMLNum
    , base      :: NumBase
    , nireqd    :: UCFMLBool
    , nidef     :: UCFMLNum
    , nivali    :: UCFMLList Validator
    } deriving (Eq, Show)

data NumBase = Binary
             | Octal
             | Decimal
             | Hexadecimal
             | CondBase Numbase
             deriving (Eq, Show)

data ListIn = ListIn
    { liminLn :: UCFMLNum -- Nothing for no minimum or Just minimum for the minimum list length
    , limaxLn :: UCFMLNum -- Nothing for no maximum or Just maximum for maximum length
    , lidef   :: UCFMLVal -- can either be a Cond [UCFMLVal] or a CondL UCFMLVal
    , liinput :: Input
    } deriving (Eq, Show)

data UCFMLVal = UBool UCFMLBool
              | UNum  UCFMLNum
              | UText UCFMLText
              | UGate UCFMLGate
              | UOp   UCFMLOperator
              | UComp UCFMLComparison
              | UList UCFMLList UCFMLVal
              deriving (Eq, Show)

data CompIn = CompIn UCFMLList ( UCFMLText, Input ) -- [ (label, input) ]
            deriving (Eq, Show)

data Validator = MustBe (UCFMLBool)
               | MustNot (UCFMLBool)
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
               | ResolvedT T.Text
               | UnresolvedT UCFMLTextExpr
               deriving (Eq, Show)

data UCFMLList a = ResolvedL [a]
                 | Unresolved UCFMLListExpr
                 | CondL UCFMLList (UCFMLBool, a)

data UCFMLExpr = BoolGen UCFMLBoolExpr
               | NumGen UCFMLNumExpr
               | TextGen UCFMLTextExpr
               | ListGen UCFMLListExpr
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
    , gate    :: UCFMLGate
    , blright :: UCFMLBool
    } deriving (Eq, Show)

data UCFMLBoolComp = UCFMLBoolComp
    { bcleft  :: UCFMLNum
    , comp    :: UCFMLComparison
    , bcright :: UCFMLNum
    } deriving (Eq, Show)

data UCFMLNumExpr = Arith NumExpr
                  | Length GenLength

data NumExpr = NumExpr
    { neleft  :: UCFMLNum
    , op      :: UCFMLOperator
    , neright :: UCFMLNum
    } deriving (Eq, Show)

data GenLength = TextLn UCFMLText
               | ListLn [UCFMLVal]

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

-- repeat inset reverse foreach
data UCFMLListExpr = RepeatLiExp  -- ListRepeat
                   | CombineLiExp -- ListInset
                   | ForEachLiExp -- ListForeach
                   | ReverseLiExp -- [ UCFMLVal ]

data UCFMLOperator = Add
                   | Sub
                   | Mul
                   | Div
                   | IntDiv
                   | RemDiv
                   | Exp
                   | UnsetO
                   deriving (Eq, Show)

data UCFMLComparison = EQ
                     | NEQ
                     | GT
                     | LT
                     | GTE
                     | LTE
                     | UnsetC
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
               | UnsetG
               deriving (Eq, Show)

data UCFMLTemplate = UCFMLTemplate [ UCFMLConFile ] deriving (Eq, Show)

data UCFMLConFile = UCFMLConFile
    { fileName :: UCFMLText
    , filePath :: UCFMLFilePath
    , lines    :: [ UCFMLText ]
    } deriving (Eq, Show)

data UCFMLFilePath = UnixStyle UCFMLText
                   | WinStyle UCFMLText
                   | UnsetFP
                   deriving (Eq, Show)

-- -----------------
-- parsing funcitons
-- -----------------


-- empty model to fill in as we go
emptyModel :: UCFMLModel
emptyModel = UCFMLModel emptyMeta emptyBody emptyTemp

emptyMeta :: UCFMLMeta
emptyMeta = UCFMLMeta UnsetT UnsetT UnsetT UnsetT

emptyBody :: UCFMLBody
emptyBody = UCFMLBody []

emptyTemp :: UCFMLTemplate
emptyTemp = UCFMLTemplate []


-- stations the parser can be at
data FsmStates = RdDict  -- reading dictionary
               | RdMeta  -- reading meta
               | RdBody  -- reading body
               | RdTemp  -- reading template
               | RdText  -- reading a text value
               | RdNumb  -- reading a number value
               | RdBool  -- reading a boolean value
               | RdGate  -- reading a gate value
               | RdOper  -- reading a operator value
               | RdComp  -- reading a comp value
               | RdBase  -- reading a base value
               | EOF     -- file ended in a place that's not obviously wrong (inside brackets or quotes etc)


fsm :: [FsmStates] -> UCFMLModel -> (Int, Int) -> T.Text -> UCFMLFile

-- empty fms means looking for top layer tags
fsm [] pmod lc' rt' =
  let (lc@(ln,col), rawtxt) = skipWsCom lc' rt' in
    if T.null rawtxt then                      -- if end of file
      fsm (EOF:sl) pmod lc rawtxt              -- then goto eof station

    else if ("Dictionary:" `T.isPrefixOf` rawtxt) then
      fsm (RdDict:sl) pmod (ln,(col+11)) (T.drop 11 rawtxt)

    else if ("Meta:" `T.isPrefixOf` rawtxt) then
      fsm (RdMeta:sl) pmod (ln,(col+5)) (T.drop 5 rawtxt)

    else if ("Body:" `T.isPrefixOf` rawtxt) then
      fsm (RdBody:sl) pmod (ln,(col+5)) (T.drop 5 rawtxt)

    else if ("Template:" `T.isPrefixOf` rawtxt) then
      fsm (RdTemp:sl) pmod (ln,(col+9)) (T.drop 9 rawtxt)

    else ParseError $ "expected either Dictionary: Meta: Body: or Template: at " <> showt lc

-- fsm at the EOF station
fsm sl@(EOF:_) pmod lc _ = verifyPartMod pmod

-- fsm at the RdMeta station
fsm sl@(RdMeta:xs) pmod lc' rt' =
  let (lc@(ln,col), rawtxt) = skipWsCom lc' rt' in
    if T.null rawtxt then                      -- if end of file
      fsm (EOF:sl) pmod lc rawtxt              -- then goto eof station

    else if ("Title:" `T.isPrefixOf` rawtxt) then -- handle a Title: tag
      undefined

    else
      undefined


skipWsCom :: (Int, Int) -> T.Text -> ((Int, Int), T.Text)
skipWsCom ls@(ln, col) rawtxt =
  if (isSpace $ T.head rawtxt) then          -- if it start with white space
    let (ws,rst) = T.span isSpace rawtxt in  -- then update lc and recurse
      ((addWS lc ws), rst)
  else if ("{-" `T.isPrefixOf` rawtxt) then -- open block comment
    let
      cmt = cmt' <> (T.take 2 rst')
      rst = T.drop 2 rst'
      (cmt', rst') = T.breakOn "-}" rawtxt
    in
      ((addWS lc cmt), rst)

  else if ("--" `T.isPrefixOf` rawtxt) then -- handle single line comment
    (((ln+1),0), (T.unlines (tail (T.lines rawtxt)))) -- inc ln, 0 col, pick up again at next line

  else (lc,rawtxt) -- nothing to skip, just pass the input back out


----- A PROBLEM FOR FUTURE ME -----
-- I'm counting all characters as a single column here because wsedit counts them
-- later this might cause problems with reporting the exact column where an error is found
-- but the line should be right still, I will need to test this on different editors to see
-- how they handle tabs and column counts.  For now I will make a note to mention exact col
-- may be a bit off on error reports
---- END PROBLEM FOR FUTURE ME ----
-- update a (ln, col) after a chunk of text
addWS :: (Int, Int) -> T.Text -> (Int, Int)
addWS (ln, col) ws =
  if (T.null . T.filter ('\n'==)) ws then -- if there are no \n in ws
    (ln,col+(T.length ws)) -- preserve ln and increase col by length ws
  else                    -- if there is at least  \n
    (ln+nl,ncol) where   -- increse ln by number of newlines and set col to length of everything after last newline
      nl = T.length $ T.filter ('\n'==) ws
      ncol = T.length $ T.takeWhileEnd ('\n'/=) ws


-- make sure that it's a valid UCFMLModel and give an error if it's not
-- check to make sure none of the Unset_ values are used
verifyModel :: UCFMLModel -> UCFMLFile
verifyModel = undefined

showt :: Show a => a -> T.Text
showt = (T.pack . show)


-- ignore all this until I remember wtf I was doing
-- okay nvm this is about parsing out a text value
-- I will rework this after fixing up the data types to add a conditional constructor
{--


-- in goes doc coords and the rawtxt
-- out comes new doc coords and either the UCFMLText and the rest of the text, or an error
textgenfsm :: (Int, Int) -> T.Text -> ((Int, Int), Either (UCFMLText, T.Text) T.Text)
textgenfsm lc@(ln,col) rawtxt =
  if T.null rawtxt then  -- if the text is empty
    (lc, Left (UnsetT, rawtxt))   -- then return an unset UCFMLText

  else if isSpace $ T.head rawtxt then -- if it starts with whitespace char
    let (ws,rst) = T.span isSpace rawtxt in -- then trim update lc and go on
      textgenfsm (addWS lc ws) rst

  else if T.head rawtxt == '"' then -- opening a quote, read up until next " as Resolved
    let                             -- look for next " that is NOT preceded by \
      rawquo = T.drop 1 rawtxt
      mcquo = findClosedQuote "" rawquo
    in
      case mcquo of
        Just (quot, rst) ->
          ((nln, ncol + 1), Left ((Resolved quot) rst)) where
            (nln, ncol) = addWS lc quot
        Nothing ->
          (lc, Right "could not find closing \" for \" at")

  else if "Concat:" `T.isPrefixOf` rawtxt then -- if it's a concat

  else if "SedExp:" `T.isPrefixOf` rawtxt then -- if it's a sedExp

  else if "Cond:" `T.isPrefixOf` rawtxt then -- handle the conditional

  else -- this isn't a quoted text, white space, EOF, SedExp, Concat, or Cond.
    (lc, Left (UnsetT, rawtxt))   -- then return an unset UCFMLText

  -- I just remembered that I have to handle conditionals basically everywhere except inside quoted text or comments
  -- ughhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhhh
  -- okay so really I only have to add it to RdTLT and RdMeta and textgenfsm
  -- and then remembere to account for it in every other thing I add
  -- smaller ughhhhhhhh

findCloseQuote :: T.Text -> T.Text -> Maybe (T.Text, T.Text)
findCloseQuote acc txt =
  if T.null txt then -- if no closing quote is found
    Nothing
  else if T.head txt = '\' then -- if the next letter is an escape character
    findCloseQuote (acc <> T.take 2 txt) (T.drop 2 txt)
  else if T.head txt = '"' then
    Just (acc, (T.Drop 1 txt))
  else
    findCloseQuote (acc <> T.take 1 txt) (T.drop 1 txt)
--}

