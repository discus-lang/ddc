{-# LANGUAGE UndecidableInstances #-}

module DDC.Data.Write where
import Data.Text                (Text)
import qualified Data.Text      as T

class Write o a where
 write :: o -> a -> IO ()


text  :: Write o Text  => o -> Text -> IO ()
text o t = write o t
{-# INLINE text #-}


string :: Write o Text => o -> String -> IO ()
string o s = write o (T.pack s)
{-# INLINE string #-}


space :: Write o Text => o -> IO ()
space o = text o " "
{-# INLINE space #-}


line  :: Write o Text => o -> IO ()
line o  = text o "\n"
{-# INLINE line #-}


punc    :: (Write o Text, Write o a)
        => o -> Text -> [a] -> IO ()
punc o tx xx
 = go xx
 where  go []   = return ()
        go (x : xs)
         = do   write o x
                write o tx
                go xs
        {-# INLINE go #-}
{-# INLINE punc #-}


punc'   :: Write o Text
        => o -> Text -> [IO ()] -> IO ()
punc' o tx xx
 = go xx
 where  go []   = return ()
        go (x : xs)
         = do   x
                write o tx
                go xs
        {-# INLINE go #-}
{-# INLINE punc' #-}


vwrite    :: (Write o a, Write o Text)
        => o -> [a] -> IO ()
vwrite o xs = punc o "\n" xs
{-# INLINE vwrite #-}


vwrite'    :: Write o Text
        => o -> [IO ()] -> IO ()
vwrite' o xs = punc' o "\n" xs
{-# INLINE vwrite' #-}


brackets :: Write o Text => o -> IO () -> IO ()
brackets o comp
 = text o "[" >> comp >> text o "]"


braces   :: Write o Text => o -> IO () -> IO ()
braces o comp
 = text o "{" >> comp >> text o "}"


parens   :: Write o Text => o -> IO () -> IO ()
parens o comp
 = text o "(" >> comp >> text o ")"


dquotes  :: Write o Text => o -> IO () -> IO ()
dquotes o comp
 = text o "\"" >> comp >> text o "\""


instance Write o Text => Write o Int where
 write o i      = write o (T.pack $ show i)
 {-# INLINE write #-}

instance Write o Text => Write o Integer where
 write o i      = write o (T.pack $ show i)
 {-# INLINE write #-}

instance Write o Text => Write o Double where
 write o i      = write o (T.pack $ show i)
 {-# INLINE write #-}


