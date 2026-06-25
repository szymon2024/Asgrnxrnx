-- 2026-06-26
{-
   Asgrnxrnx to narzędzie wiersza poleceń dla plików obserwacyjnych
   RINEX 3.04 z ASG‑EUPOS, które tworzy nowy plik RINEX bez
   nadmiarowych typów obserwacji.

   Główne kroki algorytmu:
   
   1. Odczytaj typy obserwacji z nagłówka pliku RINEX.
   
   2. Oznacz używane typy obserwacji czyli te, które zawierają dane w
   ciele pliku RINEX. W tym celu przeszukuj całe ciało pliku RINEX:
   
   a) początkowo przyjmij, że żadne typy nie zawierają danych,

   b) jeśli napotkasz dane dla typu, oznacz ten typ jako użyty i nie
   sprawdzaj więcej danych dla tego typu.

   Wynikiem jest mapa użycia.

   3. Usuń nieużywane typy obserwacji i zbuduje nowy nagłówek pliku
   rinex.

   4. Przetransformuj mapę użycia do mapy pozycji tylko użytych typów.

   5. Zbuduj nowe ciało tylko dla użytych typów.

   6. Zapisz nagłówek i ciało do nowego pliku RINEX.
   
-}

----------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Builder as B
    
import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)
import           Data.Char              (isSpace)
import           Data.Int               (Int64)
import           System.FilePath        (splitFileName, (</>))
import           System.Environment     (getArgs)
import           System.Exit            (exitFailure, exitSuccess)
import           System.IO              (hFlush, stdout, stderr)
import           System.Directory       (doesFileExist)
import           Control.Monad          (when, unless, forM_)
import           Text.Printf            (printf, hPrintf)
import           Data.List              ((\\))
import           Data.Array.Unboxed
import           Control.Monad.ST
import           Data.Array.ST
    
import           Data.Time.Clock     (getCurrentTime, diffUTCTime)

----------------------------------------------------------------------

type ObsType       = L8.ByteString

type Sys           = Char

----------------------------------------------------------------------
    
programVersion :: String
programVersion = "1.0.3"

fieldLen :: Int64
fieldLen = 16

----------------------------------------------------------------------
-- Entry to program
main :: IO ()
main = do
    args <- getArgs

    let verbose = "--gadatliwie" `elem` args || "-g" `elem` args
        dryRun  = "--na-sucho"   `elem` args || "-n" `elem` args
        args'   = filter (`notElem` ["--gadatliwie", "-g"
                                    ,"--na-sucho"  , "-n"]) args
            
    case args' of
      ["--pomoc"]  -> showHelp
      ["-p"]       -> showHelp
      ["--wersja"] -> showVersion
      ["-w"]       -> showVersion

      [input] -> do
          runWithOptions input Nothing verbose dryRun

      [input, "-o", output] ->
          runWithOptions input (Just output) verbose dryRun

      _ -> do
          hPrintf stderr "Użycie   asgrnxrnx [--gadatliwie] WEJŚCIE [-o WYJŚCIE]\n\
                         \Spróbuj  asgrnxrnx --pomoc\n\n"
          exitFailure

----------------------------------------------------------------------

-- | Version printing function
showVersion :: IO ()
showVersion = do
  printf "asgrnxrnx wersja %s\n" programVersion
  exitSuccess

----------------------------------------------------------------------

-- | Help printing function
showHelp :: IO ()
showHelp = do
  printf "asgrnxrnx - program tworzy z pliku obserwacyjnego RINEX 3.04 ASG-EUPOS\n\
         \            plik RINEX bez nadmiarowych typów obserwacji.\n\
         \            Nadmiarowe typy obserwacji zawierają tylko dane obserwacyjne\n\
         \            wypełnione spacjami.\n\
         \            Program nie modyfikuje pliku wejściowego.\n\
         \\n\
         \Użycie: asgrnxrnx [--gadatliwie] [--na-sucho] WEJŚCIE [-o WYJŚCIE]\n\
         \Opcje:\n\
         \  -o WYJŚCIE       zapisuje wynik do WYJŚCIE,\n\
         \  -g, --gadatliwie wypisuje usunięte typy obserwacji,\n\
         \  -n, --na sucho   tryb testowy - nie zapisuje pliku,\n\
         \  -p, --pomoc      pokazuje tę pomoc,\n\
         \  -w, --wersja     pokazuje wersję programu.\
         \\n\
         \Argumenty:\n\
         \ WEJŚCIE  plik wejściowy RINEX\n\
         \ WYJŚCIE  plik wyjściowy RINEX (opcjonalny)\n\
         \\n\
         \Jeśli WYJŚCIE nie zostanie podane, program utworzy plik:\n\
         \ cleaned_WEJŚCIE\n\
         \\n\
         \Program blokuje nadpisanie pliku wejściowego i ostrzega,\n\
         \jeśli plik wyjściowy już istnieje.\n\
         \\n"
  exitSuccess

----------------------------------------------------------------------  

validate :: FilePath -> Maybe FilePath -> Bool -> IO FilePath
validate input mOutput dryRun = do
    inExists <- doesFileExist input
    unless inExists $ do
        hPrintf stderr "Błąd: plik wejściowy \"%s\" nie istnieje.\n" input
        exitFailure

    -- Set the output file name
    let output = case mOutput of
            Just o  -> o
            Nothing -> addPrefix "cleaned_" input    

    when (input == output && not dryRun) $ do
        hPrintf stderr "Błąd: plik wejściowy i wyjściowy nie mogą mieć tej samej nazwy.\n"
        exitFailure

    outExists <- doesFileExist output
    when (outExists && not dryRun) $ do
        printf "Plik wyjściowy \"%s\" już istnieje.\n" output
        putStr "Nadpisać? [t/n]: "
        hFlush stdout
        ans <- getLine
        when (ans /= "t" && ans /= "T") $ do
            hPrintf stderr "Przerwano - plik nie został nadpisany.\n"
            exitFailure

    return output

----------------------------------------------------------------------

-- | Add prefix to file name
addPrefix :: String -> FilePath -> FilePath
addPrefix prefix path =
    let (dir, file) = splitFileName path
    in dir </> (prefix ++ file)           

----------------------------------------------------------------------

runWithOptions :: FilePath -> Maybe FilePath -> Bool -> Bool -> IO ()
runWithOptions input mOutput verbose dryRun = do
  
    output <- validate input mOutput dryRun
                   
    -- If everything is OK, start the proper processing
    t0 <- getCurrentTime
          
    printf "Czytam   %s\n" input      
    bs <- L8.readFile input


    let (diffTMap, bs') = asgrnxrnx bs
        !_ = L8.take 1 bs'                        -- forcing error evaluation
                          
    -- Verbose                 
    when verbose $ do
      printf "Usunięte typy obserwacji:\n"
      if Map.null diffTMap
      then
          do
            printf "  (nic nie usunięto)\n"
            exitSuccess  
      else
          forM_ (Map.toList diffTMap) $ \(sys, (n, obs)) -> do
            printf "  System %c (%d): " sys n
            L8.putStrLn (L8.intercalate " " obs)

    -- Dry run
    when dryRun $ do
      printf "Typy obserwacji do usunięcia:\n"
      if Map.null diffTMap
      then printf "  (nie ma nic do usunięcia)\n"
      else
          forM_ (Map.toList diffTMap) $ \(sys, (n, obs)) -> do
            printf "  System %c (%d): " sys n
            L8.putStrLn (L8.intercalate " " obs)
      exitSuccess

    -- Saving a file
    printf "Zapisuję %s\n" output                       -- 
    L8.writeFile output bs'
    printf "Gotowe\n"

    t <- getCurrentTime
    let diffT = diffUTCTime t t0
    when (diffT >= 0.001) $
         printf "Czas przetwarzania: %.3f s.\n" (realToFrac diffT::Double)

----------------------------------------------------------------------

-- | Processing function
asgrnxrnx
  :: L8.ByteString
  -> (Map Sys (Int, [ObsType]), L8.ByteString)
asgrnxrnx bs = 
  let (ts, body) = readObsTypes bs
      uMap       = runST (markUsedObsTypes ts body)
      ts'        = deleteUnused ts uMap
      hdrB       = buildNewHeader ts' bs
      iMap       = transform uMap
      bodyB      = buildNewBody iMap body
      diffTMap   =
            Map.differenceWith
                   (\(n, sts) (n' , sts') ->
                        let gone = sts \\ sts'
                        in if null gone then Nothing else Just (n-n', gone))
                   (Map.fromList ts)
                   (Map.fromList ts')
              
   in if not (checkFile bs)
      then
          errorWithoutStackTrace
          "Błąd sprawdzenia pliku RINEX."
      else
          (diffTMap, B.toLazyByteString (hdrB <> bodyB))

----------------------------------------------------------------------

-- | Transform usage tables to position tables of only the observation
--   types used.
transform
  :: Map Sys (Int, UArray Int Bool)
  -> Map Sys (Int, UArray Int Int)
transform =
  Map.map (\(n, arr) -> (n, indicesToArray arr))

indicesToArray :: UArray Int Bool -> UArray Int Int
indicesToArray arr =
  let (lo, hi) = bounds arr
      trueIdxs = [i * 16 | i <- [lo..hi], arr ! i]
      newBounds = (0, length trueIdxs - 1)
  in listArray newBounds trueIdxs

----------------------------------------------------------------------

checkFile
    :: L8.ByteString
    -> Bool
checkFile bs0
    | L8.null bs0            =  errorWithoutStackTrace
                                "Pusty plik wejściowy."
                               
    | rnxVer bs0 /=  "3.04"  =  errorWithoutStackTrace
                                "Błąd\n\
                                \Plik wejściowy nie jest w wersji 3.04."

    | rnxFileType bs0 /= "O" = errorWithoutStackTrace
                               "Błąd\n\
                               \Plik wejściowy nie jest \
                               \plikiem obserwacyjnym."
    | otherwise = True
                  
    where
      rnxVer      = trim . takeField  0 9
      rnxFileType = trim . takeField 20 1

----------------------------------------------------------------------
-- Pass 2 - Build new rinex body for modified observation types
----------------------------------------------------------------------

buildNewBody
    :: Map Sys (Int, UArray Int Int)
    -> L8.ByteString
    -> B.Builder
buildNewBody uMap = passLines
    where
      passLines :: L8.ByteString -> B.Builder
      passLines bs
          | L8.null bs = mempty
          | sys == '>' =
              let (b, bs') = buildEpochLine bs
              in b <> passLines bs'
          | otherwise  =
              case Map.lookup sys uMap of
                Nothing       ->
                    errorWithoutStackTrace$ "Nieznany system satelitarny \'"
                         ++ [sys] ++ "' w \""
                         ++ L8.unpack (L8.take 30 bs) ++ "\""
                            
                Just (n, arr) -> 
                  let
                      (l3,  bs1) = L8.splitAt 3 bs
                      b          = collectUsedObservations arr bs1
                      bs2        = L8.drop (fromIntegral n * fieldLen) bs1
                      (eol, bs3) = readEOL bs2
                  in B.lazyByteString l3
                     <> b
                     <> B.lazyByteString eol
                     <> passLines bs3
          where
            sys = L8.head bs

      buildEpochLine :: L8.ByteString -> (B.Builder, L8.ByteString)            
      buildEpochLine bs =
          let (l56, bs1) = L8.splitAt 56 bs
              (xs,  bs2) = readToEOL bs1
              (eol, bs3) = readEOL bs2
          in (B.lazyByteString l56
             <> B.lazyByteString xs
             <> B.lazyByteString eol, bs3)

      collectUsedObservations
          :: UArray Int Int
          -> L8.ByteString
          -> B.Builder
      collectUsedObservations arr bs =
          foldMap step (elems arr)
          where
            step i = B.lazyByteString (takeField (fromIntegral i) fieldLen bs)

----------------------------------------------------------------------
-- Pass 2 - Build new rinex header with modified observation types
----------------------------------------------------------------------
buildNewHeader
    :: [(Sys, (Int, [ObsType]))]                  -- ^ observation types list
    -> L8.ByteString                              -- ^ observation rinex file content
    -> B.Builder                                  -- ^ new header
buildNewHeader sts = passLines False
    where
      passLines :: Bool -> L8.ByteString -> B.Builder
      passLines replaced bs
          | L8.null bs   = mempty
          | lookEOH bs   = fst $ buildLastLine bs
          | lookLabel bs == "SYS / # / OBS TYPES"
          , not replaced =
              let
                  (_, bs1) = L8.splitAt 80 bs
                  (eol, bs2) = readEOL bs1
                  b          = foldMap (buildRnxSysObsTypes eol) sts
              in b <> passLines True bs2
          | lookLabel bs == "SYS / # / OBS TYPES"
          , replaced     = passLines replaced (dropLine80 bs)
          | otherwise    = 
              -- don't change line
              let (b, bs') = buildLine bs
              in b <> passLines replaced bs'

      buildLine bs =
          let (l80, bs1) = L8.splitAt 80 bs
              (eol, bs2) = readEOL bs1
          in (B.lazyByteString l80 <> B.lazyByteString eol, bs2)
                 
      buildLastLine bs =
          let
              (l73, bs1) = L8.splitAt 73 bs
              (xs,  bs2) = readToEOL bs1
              (eol, bs3) = readEOL bs2
          in (B.lazyByteString l73
              <> B.lazyByteString xs
              <> B.lazyByteString eol, bs3)

----------------------------------------------------------------------

readToEOL :: L8.ByteString -> (L8.ByteString, L8.ByteString)
readToEOL = L8.break (`L8.elem` "\n\r")

----------------------------------------------------------------------

takeToEOL :: L8.ByteString -> L8.ByteString
takeToEOL = L8.takeWhile (not . (`L8.elem` "\r\n"))

----------------------------------------------------------------------

-- | Build header observation types lines for satellite system
buildRnxSysObsTypes :: L8.ByteString -> (Sys, (Int, [ObsType])) -> B.Builder
buildRnxSysObsTypes eol (sys, (n, ts)) =
    let pieces = chunk 13 ts
    in case pieces of
         []       -> mempty
         (p:ps) -> buildFirstLine eol sys n p
                     <> buildLines eol ps

      where
        buildFirstLine eol sys n ts =
            B.char8 sys
            <> B.lazyByteString "  "
            <> B.lazyByteString (L8.pack (printf "%3d" n))
            <> foldMap (\t -> B.lazyByteString " " <> B.lazyByteString t) ts
            <> B.lazyByteString (L8.replicate (60 - (6 + fromIntegral (length ts) * 4)) ' ')
            <> B.lazyByteString "SYS / # / OBS TYPES "
            <> B.lazyByteString eol

        buildLines _ [] = mempty
        buildLines eol (ts:tss) =
            B.lazyByteString "      "           
            <> foldMap (\t -> B.lazyByteString " " <> B.lazyByteString t) ts
            <> B.lazyByteString (L8.replicate (60 - (6 + fromIntegral (length ts) * 4)) ' ')
            <> B.lazyByteString "SYS / # / OBS TYPES "               
            <> B.lazyByteString eol
            <> buildLines eol tss
        
        -- | Splitting the list into n pieces
        chunk :: Int -> [a] -> [[a]]
        chunk _ [] = []
        chunk k xs =
            let (a,b) = splitAt k xs
            in a : chunk k b

----------------------------------------------------------------------
  
-- | Delete unused observation types from list of observation types
deleteUnused
    :: [(Sys, (Int, [ObsType]))]                  -- ^ observation types grouped by system
    -> Map Sys (Int, UArray Int Bool)             -- ^ map of usage observation types
    -> [(Sys, (Int, [ObsType]))]                  -- ^ uesed observation types grouped by system
deleteUnused sts uMap =
    [ (sys, (n',ts'))
    | (sys, (n, ts)) <- sts
    , let (n', ts') = filterUsed (n, ts) (Map.lookup sys uMap)
    , n' > 0
    ]
    where
      filterUsed ::  (Int, [ObsType]) -> Maybe (Int, (UArray Int Bool)) -> (Int, [ObsType])
      filterUsed (n, ts) Nothing   = (n, ts)
      filterUsed (_, ts) (Just (_, us)) =
          foldr (f us) (0,[]) (zip [0..] ts)
      f us (i, t) (n, acc) = if us ! i
                             then (n+1, t: acc)
                             else (n, acc)

----------------------------------------------------------------------
-- Pass 1 - Mark used observation types
----------------------------------------------------------------------

-- | Creates a map of observation type usage. The observation type
--   used is entered as True.
markUsedObsTypes
    :: [(Sys, (Int, [ObsType]))]
    -> L8.ByteString
    -> ST s (Map Sys (Int, UArray Int Bool))
markUsedObsTypes ts bs = do
    ts' <- mapM convertOne ts
    let tMap = Map.fromList ts'
    uMap <- passLines tMap bs

    Map.traverseWithKey
      (\_ (n, arr) -> do
         frozen <- freeze arr
         return (n, frozen)
      ) uMap
    
    where
      
      convertOne :: (Sys, (Int, [ObsType])) -> ST s (Sys, (Int, STUArray s Int Bool))
      convertOne (sys, (n, _)) = do
        arr <- newArray (0, n-1) False :: ST s (STUArray s Int Bool)
        return (sys, (n, arr))

      passLines
          :: (Map Sys (Int, STUArray s Int Bool))
          -> L8.ByteString -> ST s (Map Sys (Int, STUArray s Int Bool))
      passLines m bs
          | L8.null bs = return m
          | sys == '>' = passLines m (dropEpochLine bs)
          | otherwise  =
              case Map.lookup sys m of
                Nothing       ->
                    errorWithoutStackTrace$ "Nieznany system satelitarny \'"
                         ++ [sys] ++ "' w \""
                         ++ L8.unpack (L8.take 30 bs) ++ "\""
                            
                Just (n, arr) -> do
                  markFields n arr bs
                  passLines m (dropLine n bs)
                            
       where
         sys = L8.head bs


         markFields :: Int -> STUArray s Int Bool -> L8.ByteString -> ST s () 
         markFields n arr bs =
             forM_ [0 .. n-1] $ \i -> do
                 used <- readArray arr i
                 unless used $ do
                     let field = takeField (3 + fieldLen * fromIntegral i) fieldLen bs
                     unless (L8.all isSpace field) $
                         writeArray arr i True
            
         dropEpochLine = snd . readEOL . dropToEOL . L8.drop 56
         dropLine n    = snd . readEOL . L8.drop (3 + fieldLen * fromIntegral n)


----------------------------------------------------------------------
-- Pass 1 - Read observation types from observation RINEX 3.04 file
----------------------------------------------------------------------
        
obsTypeFieldLen :: Int64
obsTypeFieldLen = 3

maxObsTypesPerLine :: Int
maxObsTypesPerLine = 13

-- Observation type positions in the line:
-- 6?,13(1X,A3).
-- It is constructed only once, because it is top-level constant.
posA :: UArray Int Int64
posA = listArray (0,12) [7,11..55]
                     

----------------------------------------------------------------------
                    
readObsTypes :: 
     L8.ByteString
  -> ([(Sys, (Int, [ObsType]))], L8.ByteString)
readObsTypes bs
    | L8.null bs                            = ([], bs)
    | lookEOH bs                            = ([], dropLastLine bs)
    | lookLabel bs == "SYS / # / OBS TYPES" =
        let (sys, n, n', ts) = readObsTypesFirstLine bs
            bs1 = dropLine80 bs
            (ts', bs2) = readObsTypesContLines n' (reverse ts) bs1
            (sts, bs3) = readObsTypes bs2
        in ((sys, (n, ts')) : sts, bs3)
    | otherwise =
        readObsTypes (dropLine80 bs)
    where
      
      dropLastLine =  snd . readEOL . dropToEOL . L8.drop 73

----------------------------------------------------------------------                    

readObsTypesFirstLine
    :: L8.ByteString
    -> (Sys, Int, Int, [ObsType])
readObsTypesFirstLine bs
    | sys `L8.elem` "GREJCIS"  =
      let 
            k   = min n maxObsTypesPerLine
            fs  = takeObsTypes k bs
        in if n>=0
           then (sys, n, n-k, fs)
           else errorWithoutStackTrace
                    "Zadeklarowano ujemną liczbę typów obserwacji."
    | sys == ' '               =
        errorWithoutStackTrace
        "Brak oznaczenia systemu satelitarnego w linii \n\
              \z etykietą SYS / # / OBS TYPES."
    | otherwise                =
        errorWithoutStackTrace $
          "Błąd\n\
          \Nieoczekiwany system satelitarny '" ++ [sys]
          ++ "' w nagłówku pliku wejściowego.\n\
          \Dozwolone: G, R, E, J, C, I, S."
    where
      sys = L8.index bs 0
      n   = getFieldInt 3 3 bs


----------------------------------------------------------------------

readObsTypesContLines
     :: Int
     -> [ObsType]
     -> L8.ByteString
     -> ([ObsType], L8.ByteString)
readObsTypesContLines n acc bs
    | n == 0     = (reverse acc, bs)              -- exit
    | n <  0     = errorWithoutStackTrace
                   "Odczytano więcej typów obserwacji niż\
                   \zostało zadeklarowanych."
    | L8.null bs = errorWithoutStackTrace
                   "Niespodziewanie napotkano koniec nagłowka \
                   \podczas odczytu typów obserwacji."
    | lookEOH bs = errorWithoutStackTrace
                   "Napotkano koniec nagłówka zanim odczytano \
                   \wszystkie typy obserwacji."

    | lookLabel bs == "SYS / # / OBS TYPES" =
        let k    = min n maxObsTypesPerLine
            fs   = takeObsTypes k bs
            bs1  = dropLine80 bs
            acc' = reverse fs ++ acc
        in readObsTypesContLines (n-k) acc' bs1

    | otherwise =
        readObsTypesContLines n acc (dropLine80 bs)

----------------------------------------------------------------------

{-# INLINE takeObsTypes #-}                                                    
takeObsTypes
  :: Int                                -- ^ Number of observatin types to take
  -> L8.ByteString
  -> [ObsType]
takeObsTypes n bs = go 0
    where
      len = obsTypeFieldLen
            
      go i
          | i >= n    = []
          | otherwise =
              let pos = (posA ! i)
                  fld = takeField pos len bs
              in if L8.any isSpace fld
                 then errorWithoutStackTrace$
                          "Nie można odczytać typu obserwacji na \
                          \pozycji " ++ show pos ++ " z \""
                          ++ L8.unpack (L8.take (pos+len) bs) ++ "\"."
                 else fld : go (i+1)

----------------------------------------------------------------------
            
lookLabel :: L8.ByteString -> L8.ByteString
lookLabel = trim . L8.drop 60 . L8.take 80

----------------------------------------------------------------------

dropLine80 :: L8.ByteString -> L8.ByteString
dropLine80   =  snd . readEOL . L8.drop 80

----------------------------------------------------------------------

dropToEOL :: L8.ByteString -> L8.ByteString
dropToEOL = L8.dropWhile (not . (`L8.elem` "\r\n"))

----------------------------------------------------------------------

readEOL :: L8.ByteString -> (L8.ByteString, L8.ByteString)
readEOL bs =
    case L8.uncons bs of
      Just ('\n', rest)  -> ("\n", rest)
      Just ('\r', rest1) -> case L8.uncons rest1 of
                              Just ('\n', rest2) -> ("\r\n", rest2)
                              _                  -> ("\r"  , rest1)
      _                  -> errorWithoutStackTrace
                            $ "Nie można znaleźć końca linii w \""
                                  ++ L8.unpack (L8.take 30 bs) ++ "\""

----------------------------------------------------------------------

lookEOH :: L8.ByteString -> Bool
lookEOH   = (== "END OF HEADER") . L8.take 13 . dropSpace . L8.drop 60

----------------------------------------------------------------------

-- | Trim leading and trailing whitespace from a ByteString.              
trim :: L8.ByteString -> L8.ByteString
trim = L8.dropWhile isSpace . L8.dropWhileEnd isSpace

----------------------------------------------------------------------

-- | Drop leading whitespaces from a lazy ByteString.              
dropSpace :: L8.ByteString -> L8.ByteString
dropSpace = L8.dropWhile isSpace
             
----------------------------------------------------------------------

takeField :: Int64 -> Int64 -> L8.ByteString -> L8.ByteString
takeField start len = L8.take len . L8.drop start

----------------------------------------------------------------------

-- | Get Int value from ByteString field.
getFieldInt
    :: Int64                            -- ^ start position of field
    -> Int64                            -- ^ length of field
    -> L8.ByteString                    
    -> Int
getFieldInt start len bs = do
  case L8.readInt (trim f) of
    Just (val, rest)
      | L8.null rest -> val
    _                ->
        errorWithoutStackTrace $ unwords
                  [ "\nNie można odczytać liczby całkowitej z pola\n na pozycji =", show start
                  , "\n o długości =", show len
                  , "\n pole =", show f
                  , "\nLinia:", show $ L8.takeWhile (not . (`L8.elem` "\n\r")) bs
                  ]
  where
      f = takeField start len bs                      
