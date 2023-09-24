# wotd
A simple word of the day program written in Haskell: displays a new word and its 
definition every day.

Uses the
[_Webster's Unabridged English Dictionary_](https://www.gutenberg.org/ebooks/29765), 
which I got from
[here](https://github.com/matthewreagan/WebstersEnglishDictionary).

dictionary.txt syntax per line (regular expression):\
`"\body*":"\body*"` where `\body` = `([^"\\]|\\"|\\\\|\\/|\\n)`.

## Installation
Run `install.sh`. Requires `ghc`.

## Usage
`wotd` - Print today's word.

`wotd -h` - Display help menu.

`wotd -r` - Define a random word.

