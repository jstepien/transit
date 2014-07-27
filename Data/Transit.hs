module Data.Transit ( Data.Transit.encode
                    , Data.Transit.decode
                    ) where

import Data.Transit.Internal as I

encode :: (Repr a s, ToTransit v) => a -> v -> s
encode repr = I.encode repr . toTransit

decode :: (Repr a s, FromTransit v) => a -> s -> Maybe v
decode repr str = I.decode repr str >>= fromTransit
