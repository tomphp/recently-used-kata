module RecentlyUsedList where

newtype RecentlyUsedList = RecentlyUsedList [String]

emptyList :: RecentlyUsedList
emptyList = RecentlyUsedList []

add :: String -> RecentlyUsedList -> RecentlyUsedList
add item (RecentlyUsedList list) = RecentlyUsedList newList
  where newList = item : filter (/= item) list

getMostRecent :: RecentlyUsedList -> Maybe String
getMostRecent = getByIndex 0

getByIndex :: Int -> RecentlyUsedList -> Maybe String
getByIndex index (RecentlyUsedList list)
  | index < 0 = Nothing
  | length list > index = Just (list !! index)
  | otherwise = Nothing