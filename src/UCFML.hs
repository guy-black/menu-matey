{-# LANGUAGE OverloadedStrings #-}

-- TODO, make sure to only export what's needed
module UCFML where

import qualified Data.Text as T
import Data.Char

-- ----------------------------
-- types for representing UCFML
-- ----------------------------

-- data type for representing a conditionally selected bit of UCFML
data Cond a = CondS [(UCFMLBool, a)] -- cond that resolves to single value
            | CondL [(UCFMLBool, a)] -- condL that resolves to list of values

-- data type to represent whether or not a file was passed as an argument, if that
-- file can be read, and if that file can be parsed into a UCFMLModel
data UCFMLFile = NoFile -- no file was passed as an argument
               | NotAFile --  cannot find/read the file passed as an argument
               | RawFile T.Text -- file was read and is in raw text
               | ParseError T.Text -- file couldn't be parsed because <reason>
               | ParsedFile UCFMLModel -- file parsed succesfully
               deriving (Eq, Show)

data UCFMLModel = UCFMLModel
    { meta     :: Cond UCFMLMeta
    , body     :: Cond UCFMLBody
    , template :: Cond UCFMLTemplate
    , optMap   :: Map Text UCFMLVal
    } deriving (Eq, Show)

data UCFMLMeta =  UCFMLMeta
    { mentitle :: Cond UCFMLText
    , aboutmen :: Cond UCFMLText
    , upstream :: Cond UCFMLText
    , author   :: Cond UCFMLText
    } deriving (Eq, Show)

data UCFMLBody = Cond [ Cond OptType ]
               deriving (Eq, Show)

data OptType = Single  Cond Option
             | Several Cond OptSect
             deriving (Eq, Show)

data OptSect = OptSect
    { sectitle :: Cond UCFMLText
    , aboutsec :: Cond UCFMLText
    , options  :: Cond [ Cond Option ]
    } deriving (Eq, Show)

data Option = Option
    { optitle :: Cond UCFMLText
    , aboutop :: Cond UCFMLText
    , refvar  :: Cond UCFMLText -- needs to be UCFMLText because the refVar of an option needs
                                -- to be able to be set programmatically by forEach
    , input   :: Cond Input
    } deriving (Eq, Show)

data Input = Dropdown Cond FixedSetIn
           | Checkbox Cond FixedSetIn
           | RadiButt Cond FixedSetIn
           | Textual  Cond TextInput
           | Numeric  Cond NumbInput
           | Listed   Cond ListIn Input
           | Compound Cond CompIn Input
           deriving (Eq, Show)

data FixedSetIn = FixedSetIn
    { optlist  :: Cond [ Cond FixedSetOpt ]
    , minSel   :: Cond UCFMLNum -- these are the only real "validator" associated with checkbox, dropdown, or radio button inputs
    , maxSel   :: Cond UCFMLNum -- if no min or max selected options required then set both to Nothing, otherwise set to Just whateve the limit is
    } deriving (Eq, Show)

data FixedSetOpt = FixedSetOpt
    { isdef :: Bool      -- whether this option is selected by default
    , ext   :: Cond (UCFMLText) -- user facing text associated with the value
    , int   :: Cond (UCFMLText) -- internal text asscociated with the value
    } deriving (Eq, Show)

data TextInput = TextInput
    { timinLn     :: Cond UCFMLNum -- Just the minimum number of character, or Nothing for no minimumm
    , timaxLn     :: Cond UCFMLNum -- Just the maximum number of character, or Nothing for no maximumm
    , tireqd      :: Cond UCFMLBool -- whether or not there needs to be a valid value to generate a file
    , tidef       :: Cond UCFMLText -- the text to be used by default
    , tivali      :: Cond [ Cond Validator]
    } deriving (Eq, Show)

data NumbInput = NumbInput
    { floating  :: Cond UCFMLBool
    , signed    :: Cond UCFMLBool
    , lrange    :: Cond UCFMLNum
    , urange    :: Cond UCFMLNum
    , base      :: Cond NumBase
    , nireqd    :: Cond UCFMLBool
    , nidef     :: Cond UCFMLNum
    , nivali    :: Cond [ Cond Validator ]
    } deriving (Eq, Show)

data NumBase = Binary
             | Octal
             | Decimal
             | Hexadecimal
             deriving (Eq, Show)

data ListIn = ListIn
    { liminLn :: Cond UCFMLNum -- Nothing for no minimum or Just minimum for the minimum list length
    , limaxLn :: Cond UCFMLNum -- Nothing for no maximum or Just maximum for maximum length
    , lidef   :: Cond UCFMLVal -- can either be a Cond [UCFMLVal] or a CondL UCFMLVal
    , liinput :: Cond Input
    } deriving (Eq, Show)

data UCFMLVal = UBool UCFMLBool
              | UNum  UCFMLNum
              | UText UCFMLText
              | UGate UCFMLGate
              | UOp   UCFMLOperator
              | UComp UCFMLComparison
              | UList [UCFMLVal]
              deriving (Eq, Show)

data CompIn = CompIn Cond [Cond (Cond UCFMLText, Cond Input)] -- [ (label, input) ]
            deriving (Eq, Show)

data Validator = MustBe (Cond UCFMLBool)
               | MustNot (Cond UCFMLBool)
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
data FsmStates = RdTLT   -- reading top layer tags
               | RdBlCmt -- reading block comment
               | RdMeta  -- reading meta
               | RdBody  -- reading body
               | RdTemp  -- reading template
               | EOF     -- file ended in a place that's not obviously wrong (inside brackets or quotes etc)


fsm :: [FsmStates] -> UCFMLModel -> (Int, Int) -> T.Text -> UCFMLFile
-- fsm with an empty list passed to it, shouldn't happen
fsm [] _ _ _ = ParseError "somehow fsm got an empty [FsmStates]"

-- fsm at the reading top layer tags station
fsm sl@(RdTLT:xs) pmod lc@(ln,col) rawtxt =
  if T.null rawtxt then                      -- if end of file
    fsm (EOF:sl) pmod lc rawtxt              -- then goto eof station

  else if (isSpace $ T.head rawtxt) then     -- if it start with white space
    let (ws,rst) = T.span isSpace rawtxt in  -- then update lc and recurse
      fsm sl pmod (addWS lc ws) rst

  else if ("{-" `T.isPrefixOf` rawtxt) then -- open block comment
    fsm (RdBlCmt:sl) pmod (ln,(col+2)) (T.drop 2 rawtxt)

  else if ("--" `T.isPrefixOf` rawtxt) then -- handle single line comment this time I already know I'm not in a quote so no worries
    fsm sl pmod ((ln+1),0) (T.unlines (tail (T.lines rawtxt))) -- inc ln, 0 col, pick up again at next line

  else if ("Cond:" `T.isPrefixOf` rawtxt) then
    fsm (RdCond:sl) pmod (ln,(col + 5)) (T.drop 5 rawtxt)

  else if ("Meta:" `T.isPrefixOf` rawtxt) then
    fsm (RdMeta :sl) pmod (ln,(col+5)) (T.drop 5 rawtxt)

  else if ("Body:" `T.isPrefixOf` rawtxt) then
    fsm (RdBody:sl) pmod (ln,(col+5)) (T.drop 5 rawtxt)

  else if ("Template:" `T.isPrefixOf` rawtxt) then
    fsm (RdTemp:sl) pmod (ln,(col+9)) (T.drop 9 rawtxt)

  else ParseError $ "Was expecting either a top level tag, but thats not what I found at " <> (T.pack . show) lc

-- fsm at the EOF station
fsm sl@(EOF:_) pmod lc _ = verifyPartMod pmod

-- fsm at the RdBlCmt station
fsm sl@(RdBlCmt:xs) pmod lc@(ln,col) rawtxt = fsm xs pmod (addWS lc cmt) rst where
  cmt = cmt' <> (T.take 2 rst')
  rst = T.drop 2 rst'
  (cmt', rst') = T.breakOn "-}" rawtxt

-- fsm at the RdMeta station
fsm sl@(RdMeta:xs) pmod lc@(ln,col) rawtxt =
  if T.null rawtxt then                      -- if end of file
    fsm (EOF:sl) pmod lc rawtxt              -- then goto eof station

  else if (isSpace $ T.head rawtxt) then     -- if it start with white space
    let (ws,rst) = T.span isSpace rawtxt in  -- then update lc and recurse
      fsm sl pmod (addWS lc ws) rst

  else if ("{-" `T.isPrefixOf` rawtxt) then -- open block comment
    fsm (RdBlCmt:sl) pmod (ln,(col+2)) (T.drop 2 rawtxt)

  else if ("--" `T.isPrefixOf` rawtxt) then -- handle single line comment this time I already know I'm not in a quote so no worries
    fsm sl pmod ((ln+1),0) (T.unlines (tail (T.lines rawtxt))) -- inc ln, 0 col, pick up again at next line

  else if ("Cond" `T.isPrefixOf` rawtxt) then -- handle a conditional
    undefined -- TODO : THIS

  else if ("Title:" `T.isPrefixOf` rawtxt) then -- handle a Title: tag
    let
      rawtxt' = T.drop 6 rawtxt
      lc' = (ln, (col))
      (newlc, textorerror) = textgenfsm
    in
      case textorerror of
        Left (uctxt, rst) ->
          fsm sl newpmod newlc rst where
            newpmod = pmod {meta = newmeta}
            newmeta = (meta pmod) { title = (uctxt) }
        Right error -> ParseError $ error <> (T.pack . show) newlc
  else undefined

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

----- A PROBLEM FOR FUTURE ME -----
-- I'm counting /t as as single column here because wsedit counts them
-- later this might cause problems with reporting the exact column where an error is found
-- but the line should be right still, I will need to test this on different editors to see
-- how they handle tabs and column counts.  For now I will make a note to mention exact col
-- may be a bit off on error reports
---- END PROBLEM FOR FUTURE ME ----
-- update a (ln, col) after a chunk of white space
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
verifyPartMod :: UCFMLModel -> UCFMLFile
verifyPartMod = undefined
