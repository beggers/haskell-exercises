module Main where

import Data.Text (pack, toLower, unpack)

type CompanyId = Int
type Name = String -- First name, last name, legal name, trade name, legal name
type FirstName = Name
type LastName = Name
type PersonLegalName = Name
type CompanyLegalName = Name
type TradeName = Name
type Email = String
type PhoneNumber = String
type Bank = String
type LinkId = Int

data User = User {
    firstName :: FirstName
    , lastName :: LastName
    , userEmailAddress :: Email
}

instance Show User where
    show (User f l _) = f ++ l

data MercuryCustomer = MercuryCustomer {
    cxCompanyId :: CompanyId
    , users :: [User]
    , tradeName :: TradeName
    , legalName :: CompanyLegalName
    , cxEmailAddress :: Email
    , phoneNumber :: PhoneNumber
    }
data ThirdPartyBank = ThirdPartyBank {
    linkCompanyId :: CompanyId
    , linkId :: LinkId
    , bank :: Bank
    , names :: [Name] -- Sometimes these are people names, sometimes company names.
    , emails :: [Email]
    , phoneNumbers :: [PhoneNumber]
    }

data LinkCheckResult = LinkCheckResult LinkId Bool

instance Show LinkCheckResult where
    show (LinkCheckResult lId match) = "Link " ++ show lId ++ ": " ++ (matchStr match)
        where matchStr m = if m then "Match" else "Mismatch"

-- Substrings which don't have a real impact on the meaning of a string.
-- TODO we should probably do something better than just remove these, but
-- this works for now.
--
-- Case-insensitive.
filteredSubstrings :: [String]
filteredSubstrings = ["Mr", "Ms", "Mrs", "Mx", "Inc", "LLC", "Jr", "Sr"]

-- Characters that don't have a real impact on the meaning of a string.
filteredCharacters :: String
filteredCharacters = " -,.()'"

-- Hard-coded data so I don't have to deal with JSON parsing
mercuryCustomers :: [MercuryCustomer]
mercuryCustomers = [
    MercuryCustomer 1 [(User "John" "Smith" "john@example.com")]
                    "Mercury" "Mercury Technologies" "contact@mercury.com"
                    "5557609870"
    , MercuryCustomer 2 [(User "Emmanuel" "Windal" "biker76@example.fr")]
                    "Bikes by Emmanuel" "Sports Inc." "biker76@example.fr"
                    "5550989870"
    , MercuryCustomer 3 [(User "Ram" "Nguyen" "ram@example.com")] "Ram's Kitchen"
                    "Ram's Kitchen" "ram@example.fr" "5550989532"
    , MercuryCustomer 4 [(User "Cyril" "Windhorst" "cy@example.com")]
                    "Cy's Megacorp" "Cy's Megacorp LLC." "cy@example.com"
                    "5550109988"
    , MercuryCustomer 5 [(User "Lassad" "Riviero" "lassad@hey.com")] "Lassad's Music"
                    "LASSAD MEDIA INC" "lassad@example.com" "9915554035"
    ]

thirdPartyBanks :: [ThirdPartyBank]
thirdPartyBanks = [
    ThirdPartyBank 1 1 "Chase" ["John B. Smith", "Mercury Technologies"]
        ["john@example.com", "alice@example.com"] ["(555)-760-9870"]
    , ThirdPartyBank 1 2 "Wells Fargo" [] ["james@example.com"] ["(555)-760-9870"]
    , ThirdPartyBank 2 3 "Capital One" ["Emmanuel Francisco Windal"] []
        ["555 098 9870"]
    , ThirdPartyBank 2 4 "Chase" ["Ullyses S. Strahd"] ["strahd@example.com"] []
    , ThirdPartyBank 3 5 "Wells Fargo" ["Rams Kitchen"] ["ram@example.fr"] []
    , ThirdPartyBank 4 6 "Chase" ["Cy J. Windhorst"] ["cy1776@example.com"] []
    , ThirdPartyBank 4 7 "Huntington" ["Mr. Cyril Windhorst"] [] ["5550109988"]
    , ThirdPartyBank 4 8 "Cheyenne Bank and Trust" ["Cyril", "James Smith"]
        [] ["555-010-9988"]
    , ThirdPartyBank 5 9 "" ["IN MEDIA RES PUBLISHING", "Jerry Galdwell"]
        ["jerry@example.com"] ["555-132-0911"]
    ]

nickNames :: [[String]]
nickNames = [
    ["cyrus","cy","cyril"],
    ["johannes","jonathan","john","johnny"]
    ]

main :: IO ()
main = do
    printResults $ map (checkLinkForMatch mercuryCustomers) thirdPartyBanks

-- Check the passed link for a match with any of the passed MercuryCustomers. Returns
-- the LinkId of the link along with whether it had a match.
checkLinkForMatch :: [MercuryCustomer] -> ThirdPartyBank -> LinkCheckResult
checkLinkForMatch cxs link =
    LinkCheckResult (linkId link) $ matchSingle link (customerWithId (linkCompanyId link) cxs)

-- Gets the MercuryCustomer from the list with the given CompanyId. Blows up
-- if no such customer exists :^)
customerWithId :: CompanyId -> [MercuryCustomer] -> MercuryCustomer
customerWithId _ [] = error "No customer found! Ahhhh!"
customerWithId i (c:cs) = if cxCompanyId c == i then c else customerWithId i cs

matchSingle :: ThirdPartyBank -> MercuryCustomer -> Bool
matchSingle l cx = score >= 2
    where score = foldr (+) 0 [ phonesMatch (phoneNumber cx) (phoneNumbers l)
                                , emailMatch (cxEmailAddress cx) (emails l) -- TODO we should use emails from the Users part of the record too.
                                , companyNameMatch (legalName cx) (tradeName cx) (names l)
                                , userNameMatch (map show $ users cx) (names l)]

-- Takes two lists and gives a match score between them. Right now the implementation
-- is dumb: the first parameter is the score. If any element in the first list
-- (fuzzy) matches any element in the second list, returns the score. Else,
-- returns 0. TODO we should make this smarter.
matchWithScore :: (Show a) => Int -> [a] -> [a] -> Int
matchWithScore s as as' = if match then s else 0
    where match = anyMatchInLists as as'

-- Checks if the phone number is in the list of phone numbers with some fuzzy matching.
phonesMatch :: PhoneNumber -> [PhoneNumber] -> Int
phonesMatch p ps = matchWithScore 1 [preprocessPhone p] (map preprocessPhone ps)

preprocessPhone :: PhoneNumber -> PhoneNumber
preprocessPhone = downcaseAndRemovePunctuation

emailMatch :: Email -> [Email] -> Int
emailMatch e es = matchWithScore 1 [preprocessEmail e] (map preprocessEmail es)

preprocessEmail :: Email -> Email
preprocessEmail = downcaseAndRemovePunctuation

-- Takes in a company's legal name, trade name, and the list of names associated
-- with a link. Returns 1 if either the legal or trade name appears in the names
-- list, 0 if not.
companyNameMatch :: CompanyLegalName -> TradeName -> [Name] -> Int
companyNameMatch cname tname ns = matchWithScore 1 (map preprocessBusinessName [cname, tname]) (map preprocessBusinessName ns)

-- Pre-processes a business name for matching. In particular, removes strings which
-- don't seriously impact the meaning of the name, such as "LLC".
preprocessBusinessName :: Name -> Name
preprocessBusinessName = removeExtras . downcaseAndRemovePunctuation

userNameMatch :: [PersonLegalName] -> [PersonLegalName] -> Int
userNameMatch ps ps' = matchWithScore 1 (concat $ map preprocessPersonName ps) (concat $ map preprocessPersonName ps')

preprocessPersonName :: Name -> [Name]
preprocessPersonName = nickomorphs . removeMiddleName . removeExtras . downcaseAndRemovePunctuation

-- Get "nickomorphic" strings, which is like homomorphic but for nicknames.
-- Basically, turns a String into possible synonymous Strings with instances
-- of names replaced by possible nicknames.
nickomorphs :: String -> [String]
nickomorphs = (:[]) -- TODO

removeMiddleName :: Name -> Name
removeMiddleName = id

-- Checks for a fuzzy match between two lists of possible match candidates. If
-- any element in the first list matches any element in the second list, returns
-- True. Else, returns False.
anyMatchInLists :: (Show a) => [a] -> [a] -> Bool
anyMatchInLists as as' = any id $ [(==) x y | x <- (map show as), y <- (map show as')]

downcaseAndRemovePunctuation :: String -> String
downcaseAndRemovePunctuation = filter (\c -> not $ elem c filteredCharacters) . unpack . toLower . pack 

-- Removes extraneous/uninformative substrings from the String, such as "LLC"
-- "Mr", etc. Combined with punctuation removal, makes strings more easy to
-- fuzzy match.
--
-- TODO this removes some actual info, e.g. if a business's name is
-- "Plinc" we would turn it into just "Pl". That's probably fine for now but not
-- great for a real product.
removeExtras :: String -> String
removeExtras = id -- TODO

printResults :: [LinkCheckResult] -> IO ()
printResults results = do
    putStrLn $ "Total matches: " ++ show (length $ filter (\(LinkCheckResult _ m) -> m == True) results) 
    putStrLn $ "Total mismatches: " ++ show (length $ filter (\(LinkCheckResult _ m) -> m == False) results)
    putStrLn ""
    putStrLn $ unlines (map show results)
