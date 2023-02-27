{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# HLINT ignore "Use camelCase" #-}
{-# HLINT ignore "Redundant bracket" #-}
module Hangman where
import System.IO
import Text.Read   (readMaybe)
import Control.Monad


-- adapted from MagicSum.hs
data State = State InternalState [Char]  -- internal_state, available_actions
         deriving (Ord, Eq, Show)

        
data Result = EndOfGame Double State    -- end of game: value, starting state
            | ContinueGame State        -- continue with new state
         deriving (Eq, Show)

type Game = Char -> State -> Result

-- type Player = State -> Action -- I don't think we need Player, it's used as an "opponent" in MagicSum

-- Actions for player
data Action = Letter Char  -- a move for a player is just Char
         deriving (Ord,Eq)


-- "global" variables
type InternalState = ([Char], [Char], Int, Int)   -- may need to be modified
-- (letters guessed, word to guess, # of guesses left, # of hints left)

----- Word Bank Functions ---

-- Citation: https://www.kaggle.com/datasets/ramiromelo/10000-words?resource=download; https://random-ize.com/randomize-list/
-- Took 1000 words from referenced list and used random generator to shuffle words
-- Word bank
wordBank :: [[Char]]
wordBank = ["instrument","lands","remark","rugby","please","announces","prof","carlos","tokyo","household","chest","projectors","involved","priorities","terrain","critical","south","kevin","regions","subscription","julia","certificates","linking","centered","diversity","court","aa",
            "countries","equations","dump","cds","meanwhile","artwork","reseller","legal","filing","orchestra","bobby","berkeley","map","venture","fathers","finals","tripadvisor","unemployment","seeker","constructed","eating","coffee","spell","pulling","aside","cathedral","unlikely",
            "giants","elite","yard","visible","tear","gothic","coal","his","confidentiality","podcast","wikipedia","lens","apparent","nations","automated","archives","belle","atlantic","assumption","carlo","expected","presenting","teachers","flyer","pieces","restriction","endorsement",
            "aerospace","essential","aqua","sydney","interact","jvc","workstation","converter","attribute","specialists","unknown","quantities","postcards","coding","involves","blues","ancient","turkish","reading","anaheim","connection","pay","virtual","mice","priced","panasonic","nhs",
            "introduce","constitutes","casa","earth","folding","adds","ride","baths","trader","calvin","adapters","deserve","password","qty","lite","oldest","verified","primary","bizarre","insert","definitions","readily","ia","practitioner","alumni","weddings","watt","outcome","alone",
            "celebrity","plenty","headers","customs","anyway","pole","discharge","employees","roy","granted","ist","advertisements","accountability","lauderdale","land","went","net","member","simulations","berlin","desert","lane","decide","jd","magazines","formerly","oaks","focused",
            "cord","startup","attorney","assessed","kid","token","rev","operator","plant","compromise","tar","attacks","sold","jackets","tender","box","possible","punk","hb","delivery","church","readings","ent","avon","competition","planets","graphics","usual","breeding","kills",
            "consumption","except","somehow","wagon","says","vast","release","pm","ears","roommate","main","producers","willing","serving","changes","crucial","lone","informed","marked","reunion","juan","tribal","tulsa","poster","authentication","yards","activity","survivors","photography",
            "diverse","characters","hardwood","glenn","imperial","conviction","cock","genuine","guard","legend","documentary","extract","partial","jam","powerpoint","mainstream","assumptions","ins","belkin","nav","continental","butterfly","outlook","force","cet","ec","they","cloth","car",
            "prerequisite","cz","exports","hotels","harder","salt","electricity","polymer","utilize","weights","sex","biotechnology","limitations","couples","temporary","surname","wind","games","poor","cruises","your","fast","nodes","sampling","roberts","tell","marks","young","jimmy",
            "photographer","interaction","dl","letter","supporting","british","survivor","knights","things","levels","clarity","ds","scenic","theft","civil","moreover","museum","deutsch","labs","genealogy","packed","clinton","architects","menu","zambia","learned","engaged","compound",
            "modified","portugal","contract","component","joshua","xx","alleged","typically","slight","thats","gym","breaking","resources","concentrate","self","hold","negative","phoenix","talent","promise","euro","wy","blessed","computed","chi","typical","texture","wondering","contrary",
            "whether","questions","viking","grain","blond","feeling","amp","catherine","worship","latvia","rh","carnival","toshiba","fitted","learners","drawing","medium","arnold","frequencies","anything","computer","tables","spain","ser","eco","investigated","call","rebel","removed","subject",
            "securities","climate","musician","nude","cdna","cancer","guarantee","guardian","ie","accurately","steven","notion","biodiversity","retain","pix","modifications","analyze","then","incidents","european","em","detroit","instrumental","marriott","syndication","zip","presently","dick","styles",
            "qc","stanley","reverse","breasts","academics","spirits","cheese","transparency","rely","catch","engaging","madness","neighbor","copper","perl","development","assists","fake","pairs","focal","ceiling","jpeg","bridges","definition","economies","overnight","journalist","generations","merger",
            "without","jelsoft","payable","equilibrium","morocco","newton","fioricet","mechanism","headset","thirty","siemens","commander","greek","contracts","zope","gossip","andorra","hazards","vacations","can","seq","bidding","cookbook","earning","throw","game","family","racing","hansen","lives",
            "sensitive","midwest","columns","op","rabbit","transform","disorder","ext","back","tm","arranged","ethical","sustainability","yn","connecticut","knowing","estimation","jobs","ordering","liability","functions","totals","forwarding","among","lexmark","minimize","shadows","networking","leg",
            "designation","milton","transaction","remarkable","still","programme","cradle","lighter","relaxation","availability","lafayette","christ","upgrade","climb","odd","seas","twelve","se","influences","dir","company","sudan","bible","exec","geological","vg","bedroom","breach","cult","prototype",
            "third","trusts","logged","browsing","conference","inner","excellence","entering","smoking","against","attack","tip","comparable","measured","chrome","acids","divine","oil","donated","anatomy","metal","ppm","launches","visit","grant","themes","driver","mobile","po","closes","leeds","syracuse",
            "stopped","detect","apparently","deaths","military","applications","tab","amd","class","tea","rn","vintage","hacker","she","comedy","tanzania","duncan","recipients","exclusive","citation","compensation","every","francis","governing","worldwide","picks","glen","pressing","wish","we","figure",
            "keyboards","von","abandoned","news","chief","dealer","part","infant","individual","nuts","commercial","school","tracking","accommodations","princess","protest","leu","stored","two","legacy","divisions","insulin","confidential","sequence","launched","illustration","video","la","pregnancy","fr",
            "obviously","passwords","stephanie","eu","habitat","logging","candidate","innovation","am","novels","signature","variable","christian","experience","boutique","paying","exists","confirm","porn","anti","opportunity","quebec","mediterranean","examines","thumbzilla","involvement","establishment",
            "production","oakland","painful","western","pipeline","dubai","sega","polished","script","reggae","philip","scene","shows","fall","odds","accessible","plaza","programmes","gb","parcel","reads","real","pools","syndrome","approaches","conditional","aircraft","tabs","square","moving","spray",
            "positions","excerpt","corpus","ping","eds","strengths","solutions","by","emphasis","albert","worker","explore","increased","iceland","caution","cafe","subscribe","eligible","diameter","serbia","nec","cosmetic","geography","abilities","ot","fin","trio","calendars","eval","cm","decided","crops",
            "perform","coordinates","travels","organizational","maintenance","rapids","exhibits","shopping","diagram","nails","camera","processors","column","memories","specifics","four","sku","neutral","radiation","prefer","adequate","equivalent","choose","hitting","el","paraguay","rx","charming","worm",
            "counts","ware","unsubscribe","ftp","host","expenditures","boobs","patrick","hostel","existing","mirror","ought","productivity","los","therapy","authentic","agreement","easier","coordinated","variation","service","defined","minneapolis","getting","entity","river","therefore","vi","posters",
            "amount","towns","windsor","measuring","post","praise","gm","automobiles","rate","attractive","lot","albuquerque","parts","jacob","constraint","formation","robertson","temple","staff","cl","twice","luther","shades","lee","porsche","but","dont","univ","tx","cal","worcester","air","legally",
            "holding","marble","travesti","shorter","enhanced","asks","telecommunications","rover","transcripts","terminology","freight","occasion","statement","annoying","aluminium","date","scenes","hey","tile","settings","mtv","assisted","concord","shooting","nvidia","tonight","uruguay","joy","won",
            "quad","organisation","flush","trips","compile","source","google","taught","cat","expense","earn","cave","entrepreneur","examined","lover","bryan","capitol","hash","scary","cabinet","evaluate","theoretical","tract","gate","magnificent","respondents","shadow","corrected","became","goat",
            "facilitate","klein","assessment","weeks","danny","possession","withdrawal","triangle","package","maintained","elementary","off","urge","undergraduate","thickness","frequency","algorithms","todd","slovakia","tournaments","folders","bangladesh","corner","mothers","axis","periodic","fed",
            "murphy","leave","dpi","chambers","resulted","devoted","memo","navy","bacon","receive","wanting","mutual","medline","art","amended","fla","annually","technician","jeans","mainly","printers","temperature","arcade","tee","official","angeles","fig","peter","persian","maximize","kenya","producing",
            "sap","definitely","qualified","pastor","psychological","planners","signup","injury","pocket","repair","deals","sugar","oxide","movie","articles","phentermine","sticks","vancouver","distributors","trucks","subsequently","cash","smile","gardening","optical","ink","parents","steel","winners",
            "test","afterwards","discussing","blood","supplied","renaissance","currency","behind","topic","live","blocks","intended","offset","mount","administrators","photos","mitsubishi","words","continuing","dozen","semi","round","beneficial","uncertainty","eur","beliefs","copy","become","agriculture",
            "iii","remarks","champagne","oxford","relationships","when","agents","accepted","iron","televisions","transition","formats","thick","charter","accredited","tiger","belly","warning","indonesian","papua","wage","merchant","viewer","flip","council","killing","reality","households","dimensions","adjacent","md","harley","night","bali"] -- list of words

-- filters out words less than 4 letters since we have 6 guesses
wBFilter :: [[Char]]
wBFilter = filter (\x -> length x >= 4) wordBank -- filter

-- function for finding length of word bank
wBLength :: [t] -> Int
wBLength [] = 0
wBLength (h:t) = 1 + wBLength t


--prints out file
{-
main :: IO()
main = do
    contents <- readFile "wordbank.txt"
    putStr contents
-}

--prints out wordbank.txt as a list of strings
{-
wBList :: IO [String]
wBList = do
    contents <- readFile "wordbank.txt"
    return (lines contents)
-}


----- Hangman Start State Function -----

-- empty list for letters guessed so far
-- empty word 
-- start wth 6 guesses since there is only six body parts
-- start with 3 hints to use
-- set the initial state

hangmanStart :: State
hangmanStart = State ([], "", 6, 3) ['a'..'z']  --  change for letters

generateWord :: State -> Int -> State
generateWord (State (ltrs_guessed, word, guesses, hints) avail) num = 
    State (ltrs_guessed, wordBank !! num, guesses, hints) avail

----- Hangman Game ------
hangman :: Game
hangman move (State (ltrs_guessed, word, guesses, hints) available) 
    | win move word ltrs_guessed          = EndOfGame 1  hangmanStart     -- player wins
    | guesses==1                          = EndOfGame 0  hangmanStart     -- no more guesses, player loses
    | move `elem` word               = 
        ContinueGame (State ((move:ltrs_guessed), word, guesses, hints)   -- correct guess, number of guess is not reduced
                        [act | act <- available, act /= move])
    | otherwise                           =
          ContinueGame (State ((move:ltrs_guessed), word, guesses - 1, hints)   -- reduce a guess
                        [act | act <- available, act /= move])

                        
-- takes in move, the answer, the letters guessed and returns true if word_str matches ans
win :: Char -> [Char] -> [Char] -> Bool
win move word ltrs_guessed =  and [ x `elem` move:ltrs_guessed | x <- word]

    

----- Helper Functions -----


-- returns a string, displaying the letter if guessed correctly and dashes if not, 
-- and dashes for rest of letters not guessed, and displaying the letters guessed correctly before
word_str :: [Char] -> [Char] -> Char -> [Char]
word_str ltrs_g ans l  = [if (x == l || x `elem` ltrs_g ) then x else '_' | x <- ans]

-- checks if input is an alphabet letter
isAlphabet :: Char -> Bool
isAlphabet i = i `elem` "abcdefghijklmnopqrstuvwxyz"

-- Citation: https://www.cs.ubc.ca/~poole/cs312/2023/as2/As2sol.hs
-- if a character is an upper-case letter, returns the lower-case letter, otherwise remain unchanged
toLower :: Char -> Char
toLower x 
    | x `elem` "ABCDEFGHIJKLMNOPQRSTUVWXYZ" = toEnum(fromEnum x + fromEnum 'a' - fromEnum 'A')
    | otherwise = x



-- to update # of hints in internal state
updateHint :: State -> State
updateHint (State (ltrs_guessed, word, guesses, hints) avail)
    | hints == 0 = State (ltrs_guessed, word, guesses, hints) avail
    | otherwise  = State (ltrs_guessed, word, guesses, hints - 1) avail



-- Takes the answer and letters already guessed and returns the first letter that is in word and has not been guessed yet
reveal_letter :: [Char] -> [Char] -> Char
reveal_letter ans ltrs_g = head [c | c <- ans, c `notElem` ltrs_g] 


-- checks if char is a vowel
isVowel :: Char -> Bool 
isVowel x = x `elem` "aeiou"

-- returns num of vowels in string
num_vowels :: [Char] -> Int 
num_vowels = length . filter isVowel

----- Drawing the hangman -----
drawHangman:: State -> IO()
drawHangman (State (ltrs_guessed, word, guesses, hints) avail) 
    | guesses == 6   = sixGuesses
    | guesses == 5   = fiveGuesses
    | guesses == 4   = fourGuesses
    | guesses == 3   = threeGuesses
    | guesses == 2   = twoGuesses
    | guesses == 1   = oneGuess
    | guesses == 0   = zeroGuesses


----- functions to print the hangman based on guesses -----

-- 0 wrong guesses, 6 guesses left        
sixGuesses:: IO()
sixGuesses = 
    do
        putStrLn("+-----------+")
        putStrLn("|           |")
        putStrLn("|           ")
        putStrLn("|          ")
        putStrLn("|          ")
        putStrLn("=")

-- 1 wrong guesses, 5 guesses left        
fiveGuesses:: IO()
fiveGuesses = 
    do
        putStrLn("+-----------+")
        putStrLn("|           |")
        putStrLn("|           O")
        putStrLn("|          ")
        putStrLn("|          ")
        putStrLn("=")

-- 2 wrong guesses, 4 guesses left        
fourGuesses:: IO()
fourGuesses = 
    do
        putStrLn("+-----------+")
        putStrLn("|           |")
        putStrLn("|           O")
        putStrLn("|           |")
        putStrLn("|          ")
        putStrLn("=")

-- 3 wrong guesses, 3 guesses left        
threeGuesses:: IO()
threeGuesses = 
    do
        putStrLn("+-----------+")
        putStrLn("|           |")
        putStrLn("|           O")
        putStrLn("|          \\|")
        putStrLn("|          ")
        putStrLn("=")

-- 4 guesses, 2 guesses left        
twoGuesses:: IO()
twoGuesses = 
    do
        putStrLn("+-----------+")
        putStrLn("|           |")
        putStrLn("|           O")
        putStrLn("|          \\|/")
        putStrLn("|          ")
        putStrLn("=")

-- 5 guesses, 1 guess left        
oneGuess:: IO()
oneGuess = 
    do
        putStrLn("+-----------+")
        putStrLn("|           |")
        putStrLn("|           O")
        putStrLn("|          \\|/")
        putStrLn("|          / ")
        putStrLn("=")


-- 6 guesses, 0 guesses left        
zeroGuesses:: IO()
zeroGuesses = 
    do
        putStrLn("+-----------+")
        putStrLn("|           |")
        putStrLn("|           O")
        putStrLn("|          \\|/")
        putStrLn("|          / \\")
        putStrLn("=")

