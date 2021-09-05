-- https://edwilliams.org/sunrise_sunset_algorithm.htm
import Data.Fixed
import Data.Maybe (catMaybes)
import Data.Char (toLower)
import Data.List (isPrefixOf)
import Data.Map (Map)
import qualified Data.Map as Map
import Debug.Trace (trace, traceShow)
import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.LocalTime
import System.Environment 
import System.IO
import System.Directory (getHomeDirectory, doesFileExist)

lower :: String -> String 
lower = map toLower

safeHead :: [a] -> Maybe a
safeHead [] = Nothing 
safeHead (x:_) = Just x

loadLocationsFromFile ::  IO (Maybe (Map String (Double, Double)))
loadLocationsFromFile = do 
    home <- getHomeDirectory
    path <- return $ home ++ "/locations.txt"
    exists <- doesFileExist path
    if exists 
        then do 
            contents <- readFile path 
            return $ Just $ parseLocations contents
        else return Nothing

parseLocations :: String -> Map String (Double, Double)
parseLocations input = Map.fromList $ catMaybes $ map parseLocationLine $ lines input

parseLocationLine :: String -> Maybe (String, (Double, Double))
parseLocationLine line = 
    let parts = splitOn ' ' line 
     in case parts of 
         [p1, p2, p3] -> (readDouble p2 >>= (\d1 -> (readDouble p3) >>= (\d2 -> Just (d1, d2)))) >>= \loc -> Just (p1, loc)
         _            -> Nothing

readDouble :: String -> Maybe Double 
readDouble str = let readRes = (reads str :: [(Double, String)])
                  in case readRes of 
                      [(d, remaining)] -> if (length remaining == 0) then Just d else Nothing
                      _                -> Nothing

splitOn :: Char -> String -> [String]
splitOn del str = splitOn' del $ consumeUntil del str 

    where
        splitOn' :: Char -> (String, String) -> [String]
        splitOn' del (match, "") = [match]
        splitOn' del (match, remaining) = match : (splitOn' del $ consumeUntil del remaining)

consumeUntil :: Char -> String -> (String, String)
consumeUntil del "" = ("", "")
consumeUntil del str = 
    consumeUntil' del str ""

    where 
        consumeUntil' :: Char -> String -> String -> (String, String)
        consumeUntil' del (x:xs) acc = if (x == del) then (acc, xs) else consumeUntil' del xs (acc ++ [x])
        consumeUntil' _ "" acc = (acc, "")

-- Allow for shorter keys to be used if there is no ambiguity & ignore case when searching
searchMap :: String -> Map String b -> Maybe (String, b)
searchMap term map = 
    case Map.lookup term map of 
        Nothing -> 
            let filteredMap = Map.filterWithKey (\k _ -> isPrefixOf (lower term) (lower k)) map
            in if (Map.size filteredMap) == 1 
                then (safeHead.(Map.toList)) filteredMap 
                else Nothing
        Just v  -> Just (term, v)

zenOfficial = 90 + (50.0/60.0) :: Double
zenCivil = 96.0 :: Double 
zenNautical = 102 :: Double
zenAstronomical = 108 :: Double

-- Redefine trig functions to degree mode - this keeps code closer to original alamac algorithm 
deg :: Double -> Double 
deg = (*) $ 180 / pi
rad :: Double -> Double
rad = (*) $ pi / 180
sin' :: Double -> Double
sin' = sin.rad
cos' :: Double -> Double
cos' = cos.rad
tan' :: Double -> Double
tan' = tan.rad
asin' :: Double -> Double
asin' = deg.asin
acos' :: Double -> Double
acos' = deg.acos
atan' :: Double -> Double 
atan' = deg.atan

dayOfYear :: Int -> Int -> Int -> Int
dayOfYear year month day = 
    let 
        year' = fromIntegral year :: Double
        n1 = floor $ 275 * (fromIntegral month) / 9
        n2 = floor $ (fromIntegral (month + 9)) / 12
        x = fromIntegral $ year - 4 * floor(year' / 4) + 2 :: Double
        n3 = 1 + floor(x / 3)
    in n1 - (n2 * n3) + day - 30


lngHour :: Double -> Double
lngHour lng = lng / 15

approxTime :: Int -> Double -> Bool -> Double
approxTime dayOfYear lngHour isRising = 
    let lngAdjusted =  (if isRising then 6 else 18) - lngHour 
     in (fromIntegral dayOfYear) + (lngAdjusted / 24)

meanAnomaly :: Double -> Double 
meanAnomaly approxTime = (0.9856 * approxTime) - 3.289

trueLng :: Double -> Double 
trueLng meanAnomaly =  (meanAnomaly + (1.916 * sin'(meanAnomaly)) + (0.020 * sin'(2 *meanAnomaly)) + 282.634) `Data.Fixed.mod'` 360

rightAscHours :: Double -> Double 
rightAscHours trueLng = 
    let ra = atan'(0.91764 * tan'(trueLng)) `Data.Fixed.mod'` 360 :: Double
        lquad = (floor (trueLng / 90)) * 90 :: Int
        rquad = (floor (ra / 90)) * 90 :: Int
    in (ra + fromIntegral (lquad - rquad)) / 15

sinDec trueLng = 0.39782 * sin'(trueLng)
cosDec sinD = cos'(asin'(sinD))

localHourAngle :: Double -> Double -> Double -> Double -> Bool -> Maybe Double
localHourAngle zenith sinDec cosDec latitude isRising = 
    let cosLha = (cos'(zenith) - (sinDec * sin'(latitude))) / (cosDec * cos'(latitude))
     in if cosLha <= 1 && cosLha >= -1 
        then fmap (/15) $ Just $ if isRising then  (360 - acos'(cosLha)) else  acos' cosLha
        else Nothing

localMeanTime :: Double -> Double -> Double ->  Double 
localMeanTime rightAsc approxTime hourAngle = hourAngle + rightAsc - (0.06571 * approxTime) - 6.622

utc :: Double -> Double ->  Double
utc lngHour localMeanTime = (localMeanTime - lngHour) `Data.Fixed.mod'` 24

sunTime longitude latitude (year, month, day) zenith isRising = 
    let n = dayOfYear year month day 
        longHour = lngHour longitude 
        t = approxTime n longHour isRising 
        l = trueLng $ meanAnomaly t
        sDec = sinDec l 
        h = localHourAngle zenith sDec (cosDec sDec) latitude isRising
        tLocal = fmap (localMeanTime (rightAscHours l) t) h
      in fmap (Main.utc longHour) tLocal

sunset  (year, month, day) longitude latitude = sunTime longitude latitude (year, month, day) zenOfficial False
sunrise (year, month, day) longitude latitude = sunTime longitude latitude (year, month, day) zenOfficial True

today :: IO (Int, Int, Int)
today = fmap (\(a, b, c) -> (fromInteger a, b, c)) $ fmap (toGregorian.utctDay) getCurrentTime 

currentUtcOfset :: IO Int 
currentUtcOfset =  fmap timeZoneMinutes $ getCurrentTime >>= getTimeZone

toHoursMinutes :: Double -> Int -> (Int, Int)
toHoursMinutes time' offset = 
    let time = time' + (fromIntegral offset / 60.0)
        hours = floor time 
        minutes = round $ 60 * (time - fromIntegral hours)
     in (hours, minutes)

showHoursMins :: (Int, Int) -> String
showHoursMins (hour, minutes) = (show hour) ++ ":" ++ (show minutes)
    
sunTimeToString :: Int -> Double -> String
sunTimeToString offsetMinutes timeHours = showHoursMins $ ((flip toHoursMinutes)offsetMinutes) $ timeHours 

main :: IO ()
main = do
    args <- getArgs
    fileLocations <- loadLocationsFromFile
    locationLookup <- return $ fileLocations >>= (\locations -> safeHead args >>= (flip searchMap) locations)
    case locationLookup of 
        Just (locationName, (long, lat)) -> do 
            putStr locationName
            putStr " "
            date <- today 
            utcOfset <- currentUtcOfset
            sunriseTime <- return $ sunTime long lat date zenOfficial True
            sunriseString <- return $ fmap (sunTimeToString utcOfset) sunriseTime
            sunsetime <- return $ sunTime long lat date zenOfficial False
            sunsetString <- return $ fmap (sunTimeToString utcOfset) sunsetime

            putStr $ case sunriseString of 
                Just x -> x
                Nothing -> "No sunrise"
            putStr " -> "
            putStrLn $ case sunsetString of 
                Just x -> x
                Nothing -> "No sunset"
        Nothing -> do 
            putStrLn "Location not found"

traceN :: Show a => [a] -> b -> b 
traceN [] def = def
traceN (x:xs) def = traceN (traceShow x xs) def

-- Debug use only
force :: Maybe a -> a 
force (Just x) = x
force (Nothing) = error "force Nothing"