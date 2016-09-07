import Text.ParserCombinators.Parsec hiding (spaces)
import Numeric
import System.Environment
import Data.List


data Vertex = Vert Double Double Double
    deriving (Show)
data Face   = Face [Vertex]

    deriving (Show)
data Model  = Model [Face]

    deriving (Show)



spaceparser :: Parser ()
spaceparser = skipMany $ char ' '

skipTillEOL :: Parser Char
skipTillEOL = (skipMany1 $ noneOf ['\n']) >> newline

spaceString :: String -> Parser String
spaceString str = do
    spaceparser
    x <- string str
    spaceparser
    return str

intparser :: Parser Double
intparser = do
    x <- many1 digit
    return (read x :: Double)

floatparser :: Parser Double
floatparser = do
    x <- many1 digit
    char '.'
    y <- many1 digit
    return (fst . head $ readFloat (x ++ "." ++ y) :: Double)

sientificparser :: Parser Double
sientificparser = do
    x <- (try floatparser) <|> (try intparser)
    char 'e'
    y <- basicnumparser
    return (fst. head $ readFloat ((show x) ++ "e" ++ (show y)) :: Double)

negsientificparser :: Parser Double
negsientificparser = do
    char '-'
    x <- sientificparser
    return $ negate x

negParser :: Parser Double
negParser = do
    char '-'
    x <- (try floatparser) <|> (try intparser)
    return $ negate x

basicnumparser :: Parser Double
basicnumparser = (try negParser) <|> (try floatparser) <|> (try intparser)

numparser :: Parser Double
numparser = (try negsientificparser) <|> (try sientificparser) <|> (try basicnumparser)

vertexparser :: Parser Vertex
vertexparser = do
    spaceString "vertex"
    x <- numparser
    spaceparser
    y <- numparser
    spaceparser
    z <- numparser
    return $ Vert x y z

faceparser :: Parser Face
faceparser = do
    spaceString "facet normal"
    skipTillEOL
    spaceString "outer loop"
    newline
    v1 <- vertexparser
    newline
    v2 <- vertexparser
    newline
    v3 <- vertexparser
    newline
    spaceString "endloop"
    newline
    spaceString "endfacet"
    newline
    return $ Face [v1, v2, v3]

modelparser :: Parser Model
modelparser = do
    string "solid "
    skipTillEOL
    faces <- many1 faceparser
    string "endsolid "
    return $ Model faces

parseSTL :: String -> Model
parseSTL str = case parse modelparser "STL" str of
    Left err  -> error $ show err
    Right val -> val


printFace (Face (verts))  = "3 16 " ++ (showverts verts)

showverts ((Vert x y z):xs) = (show y) ++ " " ++ (show z) ++ " " ++ (show x) ++ " " ++ (showverts xs)
showverts _ = ""

createDAT :: Model -> String
createDAT (Model (xs)) = intercalate "\n" $ map (printFace) xs


main :: IO ()
main = do
    args <- getArgs
    if (not . null $ args) then do
        let name  = head args
        str      <- readFile name
        let model   = parseSTL str
        let outname = (take (length name - 3) name ) ++ "dat"
        writeFile outname (createDAT model)
        putStrLn $ "created file: " ++ outname
    else do
        putStrLn "Usage: STL2LDRAW filename.stl"
