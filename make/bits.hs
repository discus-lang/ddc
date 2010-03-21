
import Foreign.Storable		(sizeOf)
import Foreign.Ptr			(Ptr)
import Foreign.C.Types		(CChar)


pointerSize :: Int
pointerSize = 8 * sizeOf (undefined :: Ptr CChar)


main :: IO ()
main
 =	putStrLn $ show pointerSize

