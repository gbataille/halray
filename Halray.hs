import System.Environment

-- | 'main' runs the main program
main :: IO ()
main = getArgs >>= print . haqify . head

haqify :: String -> String
haqify = ("Haq! " ++)
