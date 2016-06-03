# hyphelm
Elm hyphenation library. Currently only polish words are supported.

## Example usage

```elm

import Hyphelm exposing (Language(..), syllabize)

syllabizedWord : List String
syllabizedWord =
  syllabize Polish "absurd"
```
