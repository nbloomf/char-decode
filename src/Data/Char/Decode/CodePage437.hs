module Data.Char.Decode.CodePage437
  ( CodePage437(..)
  ) where

import Data.Typeable (Typeable())
import Data.Word (Word8)

import Data.Char.Decode.Class

-- | Data from ftp://www.unicode.org/Public/MAPPINGS/VENDORS/MICSFT/PC/CP437.TXT
data CodePage437
  = CodePage437
  deriving (Eq, Show, Typeable)

instance ByteEncoding CodePage437 where
  decodeByte CodePage437 = decodeCodePage437
  encodeByte CodePage437 = encodeCodePage437



decodeCodePage437 :: Word8 -> Char
decodeCodePage437 c = case c of
  0x00 -> '\x0000' -- NULL
  0x01 -> '\x0001' -- START OF HEADING
  0x02 -> '\x0002' -- START OF TEXT
  0x03 -> '\x0003' -- END OF TEXT
  0x04 -> '\x0004' -- END OF TRANSMISSION
  0x05 -> '\x0005' -- ENQUIRY
  0x06 -> '\x0006' -- ACKNOWLEDGE
  0x07 -> '\x0007' -- BELL
  0x08 -> '\x0008' -- BACKSPACE
  0x09 -> '\x0009' -- HORIZONTAL TABULATION
  0x0a -> '\x000a' -- LINE FEED
  0x0b -> '\x000b' -- VERTICAL TABULATION
  0x0c -> '\x000c' -- FORM FEED
  0x0d -> '\x000d' -- CARRIAGE RETURN
  0x0e -> '\x000e' -- SHIFT OUT
  0x0f -> '\x000f' -- SHIFT IN
  0x10 -> '\x0010' -- DATA LINK ESCAPE
  0x11 -> '\x0011' -- DEVICE CONTROL ONE
  0x12 -> '\x0012' -- DEVICE CONTROL TWO
  0x13 -> '\x0013' -- DEVICE CONTROL THREE
  0x14 -> '\x0014' -- DEVICE CONTROL FOUR
  0x15 -> '\x0015' -- NEGATIVE ACKNOWLEDGE
  0x16 -> '\x0016' -- SYNCHRONOUS IDLE
  0x17 -> '\x0017' -- END OF TRANSMISSION BLOCK
  0x18 -> '\x0018' -- CANCEL
  0x19 -> '\x0019' -- END OF MEDIUM
  0x1a -> '\x001a' -- SUBSTITUTE
  0x1b -> '\x001b' -- ESCAPE
  0x1c -> '\x001c' -- FILE SEPARATOR
  0x1d -> '\x001d' -- GROUP SEPARATOR
  0x1e -> '\x001e' -- RECORD SEPARATOR
  0x1f -> '\x001f' -- UNIT SEPARATOR
  0x20 -> '\x0020' -- SPACE
  0x21 -> '\x0021' -- EXCLAMATION MARK
  0x22 -> '\x0022' -- QUOTATION MARK
  0x23 -> '\x0023' -- NUMBER SIGN
  0x24 -> '\x0024' -- DOLLAR SIGN
  0x25 -> '\x0025' -- PERCENT SIGN
  0x26 -> '\x0026' -- AMPERSAND
  0x27 -> '\x0027' -- APOSTROPHE
  0x28 -> '\x0028' -- LEFT PARENTHESIS
  0x29 -> '\x0029' -- RIGHT PARENTHESIS
  0x2a -> '\x002a' -- ASTERISK
  0x2b -> '\x002b' -- PLUS SIGN
  0x2c -> '\x002c' -- COMMA
  0x2d -> '\x002d' -- HYPHEN-MINUS
  0x2e -> '\x002e' -- FULL STOP
  0x2f -> '\x002f' -- SOLIDUS
  0x30 -> '\x0030' -- DIGIT ZERO
  0x31 -> '\x0031' -- DIGIT ONE
  0x32 -> '\x0032' -- DIGIT TWO
  0x33 -> '\x0033' -- DIGIT THREE
  0x34 -> '\x0034' -- DIGIT FOUR
  0x35 -> '\x0035' -- DIGIT FIVE
  0x36 -> '\x0036' -- DIGIT SIX
  0x37 -> '\x0037' -- DIGIT SEVEN
  0x38 -> '\x0038' -- DIGIT EIGHT
  0x39 -> '\x0039' -- DIGIT NINE
  0x3a -> '\x003a' -- COLON
  0x3b -> '\x003b' -- SEMICOLON
  0x3c -> '\x003c' -- LESS-THAN SIGN
  0x3d -> '\x003d' -- EQUALS SIGN
  0x3e -> '\x003e' -- GREATER-THAN SIGN
  0x3f -> '\x003f' -- QUESTION MARK
  0x40 -> '\x0040' -- COMMERCIAL AT
  0x41 -> '\x0041' -- LATIN CAPITAL LETTER A
  0x42 -> '\x0042' -- LATIN CAPITAL LETTER B
  0x43 -> '\x0043' -- LATIN CAPITAL LETTER C
  0x44 -> '\x0044' -- LATIN CAPITAL LETTER D
  0x45 -> '\x0045' -- LATIN CAPITAL LETTER E
  0x46 -> '\x0046' -- LATIN CAPITAL LETTER F
  0x47 -> '\x0047' -- LATIN CAPITAL LETTER G
  0x48 -> '\x0048' -- LATIN CAPITAL LETTER H
  0x49 -> '\x0049' -- LATIN CAPITAL LETTER I
  0x4a -> '\x004a' -- LATIN CAPITAL LETTER J
  0x4b -> '\x004b' -- LATIN CAPITAL LETTER K
  0x4c -> '\x004c' -- LATIN CAPITAL LETTER L
  0x4d -> '\x004d' -- LATIN CAPITAL LETTER M
  0x4e -> '\x004e' -- LATIN CAPITAL LETTER N
  0x4f -> '\x004f' -- LATIN CAPITAL LETTER O
  0x50 -> '\x0050' -- LATIN CAPITAL LETTER P
  0x51 -> '\x0051' -- LATIN CAPITAL LETTER Q
  0x52 -> '\x0052' -- LATIN CAPITAL LETTER R
  0x53 -> '\x0053' -- LATIN CAPITAL LETTER S
  0x54 -> '\x0054' -- LATIN CAPITAL LETTER T
  0x55 -> '\x0055' -- LATIN CAPITAL LETTER U
  0x56 -> '\x0056' -- LATIN CAPITAL LETTER V
  0x57 -> '\x0057' -- LATIN CAPITAL LETTER W
  0x58 -> '\x0058' -- LATIN CAPITAL LETTER X
  0x59 -> '\x0059' -- LATIN CAPITAL LETTER Y
  0x5a -> '\x005a' -- LATIN CAPITAL LETTER Z
  0x5b -> '\x005b' -- LEFT SQUARE BRACKET
  0x5c -> '\x005c' -- REVERSE SOLIDUS
  0x5d -> '\x005d' -- RIGHT SQUARE BRACKET
  0x5e -> '\x005e' -- CIRCUMFLEX ACCENT
  0x5f -> '\x005f' -- LOW LINE
  0x60 -> '\x0060' -- GRAVE ACCENT
  0x61 -> '\x0061' -- LATIN SMALL LETTER A
  0x62 -> '\x0062' -- LATIN SMALL LETTER B
  0x63 -> '\x0063' -- LATIN SMALL LETTER C
  0x64 -> '\x0064' -- LATIN SMALL LETTER D
  0x65 -> '\x0065' -- LATIN SMALL LETTER E
  0x66 -> '\x0066' -- LATIN SMALL LETTER F
  0x67 -> '\x0067' -- LATIN SMALL LETTER G
  0x68 -> '\x0068' -- LATIN SMALL LETTER H
  0x69 -> '\x0069' -- LATIN SMALL LETTER I
  0x6a -> '\x006a' -- LATIN SMALL LETTER J
  0x6b -> '\x006b' -- LATIN SMALL LETTER K
  0x6c -> '\x006c' -- LATIN SMALL LETTER L
  0x6d -> '\x006d' -- LATIN SMALL LETTER M
  0x6e -> '\x006e' -- LATIN SMALL LETTER N
  0x6f -> '\x006f' -- LATIN SMALL LETTER O
  0x70 -> '\x0070' -- LATIN SMALL LETTER P
  0x71 -> '\x0071' -- LATIN SMALL LETTER Q
  0x72 -> '\x0072' -- LATIN SMALL LETTER R
  0x73 -> '\x0073' -- LATIN SMALL LETTER S
  0x74 -> '\x0074' -- LATIN SMALL LETTER T
  0x75 -> '\x0075' -- LATIN SMALL LETTER U
  0x76 -> '\x0076' -- LATIN SMALL LETTER V
  0x77 -> '\x0077' -- LATIN SMALL LETTER W
  0x78 -> '\x0078' -- LATIN SMALL LETTER X
  0x79 -> '\x0079' -- LATIN SMALL LETTER Y
  0x7a -> '\x007a' -- LATIN SMALL LETTER Z
  0x7b -> '\x007b' -- LEFT CURLY BRACKET
  0x7c -> '\x007c' -- VERTICAL LINE
  0x7d -> '\x007d' -- RIGHT CURLY BRACKET
  0x7e -> '\x007e' -- TILDE
  0x7f -> '\x007f' -- DELETE
  0x80 -> '\x00c7' -- LATIN CAPITAL LETTER C WITH CEDILLA
  0x81 -> '\x00fc' -- LATIN SMALL LETTER U WITH DIAERESIS
  0x82 -> '\x00e9' -- LATIN SMALL LETTER E WITH ACUTE
  0x83 -> '\x00e2' -- LATIN SMALL LETTER A WITH CIRCUMFLEX
  0x84 -> '\x00e4' -- LATIN SMALL LETTER A WITH DIAERESIS
  0x85 -> '\x00e0' -- LATIN SMALL LETTER A WITH GRAVE
  0x86 -> '\x00e5' -- LATIN SMALL LETTER A WITH RING ABOVE
  0x87 -> '\x00e7' -- LATIN SMALL LETTER C WITH CEDILLA
  0x88 -> '\x00ea' -- LATIN SMALL LETTER E WITH CIRCUMFLEX
  0x89 -> '\x00eb' -- LATIN SMALL LETTER E WITH DIAERESIS
  0x8a -> '\x00e8' -- LATIN SMALL LETTER E WITH GRAVE
  0x8b -> '\x00ef' -- LATIN SMALL LETTER I WITH DIAERESIS
  0x8c -> '\x00ee' -- LATIN SMALL LETTER I WITH CIRCUMFLEX
  0x8d -> '\x00ec' -- LATIN SMALL LETTER I WITH GRAVE
  0x8e -> '\x00c4' -- LATIN CAPITAL LETTER A WITH DIAERESIS
  0x8f -> '\x00c5' -- LATIN CAPITAL LETTER A WITH RING ABOVE
  0x90 -> '\x00c9' -- LATIN CAPITAL LETTER E WITH ACUTE
  0x91 -> '\x00e6' -- LATIN SMALL LIGATURE AE
  0x92 -> '\x00c6' -- LATIN CAPITAL LIGATURE AE
  0x93 -> '\x00f4' -- LATIN SMALL LETTER O WITH CIRCUMFLEX
  0x94 -> '\x00f6' -- LATIN SMALL LETTER O WITH DIAERESIS
  0x95 -> '\x00f2' -- LATIN SMALL LETTER O WITH GRAVE
  0x96 -> '\x00fb' -- LATIN SMALL LETTER U WITH CIRCUMFLEX
  0x97 -> '\x00f9' -- LATIN SMALL LETTER U WITH GRAVE
  0x98 -> '\x00ff' -- LATIN SMALL LETTER Y WITH DIAERESIS
  0x99 -> '\x00d6' -- LATIN CAPITAL LETTER O WITH DIAERESIS
  0x9a -> '\x00dc' -- LATIN CAPITAL LETTER U WITH DIAERESIS
  0x9b -> '\x00a2' -- CENT SIGN
  0x9c -> '\x00a3' -- POUND SIGN
  0x9d -> '\x00a5' -- YEN SIGN
  0x9e -> '\x20a7' -- PESETA SIGN
  0x9f -> '\x0192' -- LATIN SMALL LETTER F WITH HOOK
  0xa0 -> '\x00e1' -- LATIN SMALL LETTER A WITH ACUTE
  0xa1 -> '\x00ed' -- LATIN SMALL LETTER I WITH ACUTE
  0xa2 -> '\x00f3' -- LATIN SMALL LETTER O WITH ACUTE
  0xa3 -> '\x00fa' -- LATIN SMALL LETTER U WITH ACUTE
  0xa4 -> '\x00f1' -- LATIN SMALL LETTER N WITH TILDE
  0xa5 -> '\x00d1' -- LATIN CAPITAL LETTER N WITH TILDE
  0xa6 -> '\x00aa' -- FEMININE ORDINAL INDICATOR
  0xa7 -> '\x00ba' -- MASCULINE ORDINAL INDICATOR
  0xa8 -> '\x00bf' -- INVERTED QUESTION MARK
  0xa9 -> '\x2310' -- REVERSED NOT SIGN
  0xaa -> '\x00ac' -- NOT SIGN
  0xab -> '\x00bd' -- VULGAR FRACTION ONE HALF
  0xac -> '\x00bc' -- VULGAR FRACTION ONE QUARTER
  0xad -> '\x00a1' -- INVERTED EXCLAMATION MARK
  0xae -> '\x00ab' -- LEFT-POINTING DOUBLE ANGLE QUOTATION MARK
  0xaf -> '\x00bb' -- RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK
  0xb0 -> '\x2591' -- LIGHT SHADE
  0xb1 -> '\x2592' -- MEDIUM SHADE
  0xb2 -> '\x2593' -- DARK SHADE
  0xb3 -> '\x2502' -- BOX DRAWINGS LIGHT VERTICAL
  0xb4 -> '\x2524' -- BOX DRAWINGS LIGHT VERTICAL AND LEFT
  0xb5 -> '\x2561' -- BOX DRAWINGS VERTICAL SINGLE AND LEFT DOUBLE
  0xb6 -> '\x2562' -- BOX DRAWINGS VERTICAL DOUBLE AND LEFT SINGLE
  0xb7 -> '\x2556' -- BOX DRAWINGS DOWN DOUBLE AND LEFT SINGLE
  0xb8 -> '\x2555' -- BOX DRAWINGS DOWN SINGLE AND LEFT DOUBLE
  0xb9 -> '\x2563' -- BOX DRAWINGS DOUBLE VERTICAL AND LEFT
  0xba -> '\x2551' -- BOX DRAWINGS DOUBLE VERTICAL
  0xbb -> '\x2557' -- BOX DRAWINGS DOUBLE DOWN AND LEFT
  0xbc -> '\x255d' -- BOX DRAWINGS DOUBLE UP AND LEFT
  0xbd -> '\x255c' -- BOX DRAWINGS UP DOUBLE AND LEFT SINGLE
  0xbe -> '\x255b' -- BOX DRAWINGS UP SINGLE AND LEFT DOUBLE
  0xbf -> '\x2510' -- BOX DRAWINGS LIGHT DOWN AND LEFT
  0xc0 -> '\x2514' -- BOX DRAWINGS LIGHT UP AND RIGHT
  0xc1 -> '\x2534' -- BOX DRAWINGS LIGHT UP AND HORIZONTAL
  0xc2 -> '\x252c' -- BOX DRAWINGS LIGHT DOWN AND HORIZONTAL
  0xc3 -> '\x251c' -- BOX DRAWINGS LIGHT VERTICAL AND RIGHT
  0xc4 -> '\x2500' -- BOX DRAWINGS LIGHT HORIZONTAL
  0xc5 -> '\x253c' -- BOX DRAWINGS LIGHT VERTICAL AND HORIZONTAL
  0xc6 -> '\x255e' -- BOX DRAWINGS VERTICAL SINGLE AND RIGHT DOUBLE
  0xc7 -> '\x255f' -- BOX DRAWINGS VERTICAL DOUBLE AND RIGHT SINGLE
  0xc8 -> '\x255a' -- BOX DRAWINGS DOUBLE UP AND RIGHT
  0xc9 -> '\x2554' -- BOX DRAWINGS DOUBLE DOWN AND RIGHT
  0xca -> '\x2569' -- BOX DRAWINGS DOUBLE UP AND HORIZONTAL
  0xcb -> '\x2566' -- BOX DRAWINGS DOUBLE DOWN AND HORIZONTAL
  0xcc -> '\x2560' -- BOX DRAWINGS DOUBLE VERTICAL AND RIGHT
  0xcd -> '\x2550' -- BOX DRAWINGS DOUBLE HORIZONTAL
  0xce -> '\x256c' -- BOX DRAWINGS DOUBLE VERTICAL AND HORIZONTAL
  0xcf -> '\x2567' -- BOX DRAWINGS UP SINGLE AND HORIZONTAL DOUBLE
  0xd0 -> '\x2568' -- BOX DRAWINGS UP DOUBLE AND HORIZONTAL SINGLE
  0xd1 -> '\x2564' -- BOX DRAWINGS DOWN SINGLE AND HORIZONTAL DOUBLE
  0xd2 -> '\x2565' -- BOX DRAWINGS DOWN DOUBLE AND HORIZONTAL SINGLE
  0xd3 -> '\x2559' -- BOX DRAWINGS UP DOUBLE AND RIGHT SINGLE
  0xd4 -> '\x2558' -- BOX DRAWINGS UP SINGLE AND RIGHT DOUBLE
  0xd5 -> '\x2552' -- BOX DRAWINGS DOWN SINGLE AND RIGHT DOUBLE
  0xd6 -> '\x2553' -- BOX DRAWINGS DOWN DOUBLE AND RIGHT SINGLE
  0xd7 -> '\x256b' -- BOX DRAWINGS VERTICAL DOUBLE AND HORIZONTAL SINGLE
  0xd8 -> '\x256a' -- BOX DRAWINGS VERTICAL SINGLE AND HORIZONTAL DOUBLE
  0xd9 -> '\x2518' -- BOX DRAWINGS LIGHT UP AND LEFT
  0xda -> '\x250c' -- BOX DRAWINGS LIGHT DOWN AND RIGHT
  0xdb -> '\x2588' -- FULL BLOCK
  0xdc -> '\x2584' -- LOWER HALF BLOCK
  0xdd -> '\x258c' -- LEFT HALF BLOCK
  0xde -> '\x2590' -- RIGHT HALF BLOCK
  0xdf -> '\x2580' -- UPPER HALF BLOCK
  0xe0 -> '\x03b1' -- GREEK SMALL LETTER ALPHA
  0xe1 -> '\x00df' -- LATIN SMALL LETTER SHARP S
  0xe2 -> '\x0393' -- GREEK CAPITAL LETTER GAMMA
  0xe3 -> '\x03c0' -- GREEK SMALL LETTER PI
  0xe4 -> '\x03a3' -- GREEK CAPITAL LETTER SIGMA
  0xe5 -> '\x03c3' -- GREEK SMALL LETTER SIGMA
  0xe6 -> '\x00b5' -- MICRO SIGN
  0xe7 -> '\x03c4' -- GREEK SMALL LETTER TAU
  0xe8 -> '\x03a6' -- GREEK CAPITAL LETTER PHI
  0xe9 -> '\x0398' -- GREEK CAPITAL LETTER THETA
  0xea -> '\x03a9' -- GREEK CAPITAL LETTER OMEGA
  0xeb -> '\x03b4' -- GREEK SMALL LETTER DELTA
  0xec -> '\x221e' -- INFINITY
  0xed -> '\x03c6' -- GREEK SMALL LETTER PHI
  0xee -> '\x03b5' -- GREEK SMALL LETTER EPSILON
  0xef -> '\x2229' -- INTERSECTION
  0xf0 -> '\x2261' -- IDENTICAL TO
  0xf1 -> '\x00b1' -- PLUS-MINUS SIGN
  0xf2 -> '\x2265' -- GREATER-THAN OR EQUAL TO
  0xf3 -> '\x2264' -- LESS-THAN OR EQUAL TO
  0xf4 -> '\x2320' -- TOP HALF INTEGRAL
  0xf5 -> '\x2321' -- BOTTOM HALF INTEGRAL
  0xf6 -> '\x00f7' -- DIVISION SIGN
  0xf7 -> '\x2248' -- ALMOST EQUAL TO
  0xf8 -> '\x00b0' -- DEGREE SIGN
  0xf9 -> '\x2219' -- BULLET OPERATOR
  0xfa -> '\x00b7' -- MIDDLE DOT
  0xfb -> '\x221a' -- SQUARE ROOT
  0xfc -> '\x207f' -- SUPERSCRIPT LATIN SMALL LETTER N
  0xfd -> '\x00b2' -- SUPERSCRIPT TWO
  0xfe -> '\x25a0' -- BLACK SQUARE
  0xff -> '\x00a0' -- NO-BREAK SPACE

  _ -> error $ "Word8 value '" ++ show c ++ " out of range in CodePage437"



encodeCodePage437 :: Char -> Either Char Word8
encodeCodePage437 c = case c of
  '\x0000' -> Right 0x00 -- NULL
  '\x0001' -> Right 0x01 -- START OF HEADING
  '\x0002' -> Right 0x02 -- START OF TEXT
  '\x0003' -> Right 0x03 -- END OF TEXT
  '\x0004' -> Right 0x04 -- END OF TRANSMISSION
  '\x0005' -> Right 0x05 -- ENQUIRY
  '\x0006' -> Right 0x06 -- ACKNOWLEDGE
  '\x0007' -> Right 0x07 -- BELL
  '\x0008' -> Right 0x08 -- BACKSPACE
  '\x0009' -> Right 0x09 -- HORIZONTAL TABULATION
  '\x000a' -> Right 0x0a -- LINE FEED
  '\x000b' -> Right 0x0b -- VERTICAL TABULATION
  '\x000c' -> Right 0x0c -- FORM FEED
  '\x000d' -> Right 0x0d -- CARRIAGE RETURN
  '\x000e' -> Right 0x0e -- SHIFT OUT
  '\x000f' -> Right 0x0f -- SHIFT IN
  '\x0010' -> Right 0x10 -- DATA LINK ESCAPE
  '\x0011' -> Right 0x11 -- DEVICE CONTROL ONE
  '\x0012' -> Right 0x12 -- DEVICE CONTROL TWO
  '\x0013' -> Right 0x13 -- DEVICE CONTROL THREE
  '\x0014' -> Right 0x14 -- DEVICE CONTROL FOUR
  '\x0015' -> Right 0x15 -- NEGATIVE ACKNOWLEDGE
  '\x0016' -> Right 0x16 -- SYNCHRONOUS IDLE
  '\x0017' -> Right 0x17 -- END OF TRANSMISSION BLOCK
  '\x0018' -> Right 0x18 -- CANCEL
  '\x0019' -> Right 0x19 -- END OF MEDIUM
  '\x001a' -> Right 0x1a -- SUBSTITUTE
  '\x001b' -> Right 0x1b -- ESCAPE
  '\x001c' -> Right 0x1c -- FILE SEPARATOR
  '\x001d' -> Right 0x1d -- GROUP SEPARATOR
  '\x001e' -> Right 0x1e -- RECORD SEPARATOR
  '\x001f' -> Right 0x1f -- UNIT SEPARATOR
  '\x0020' -> Right 0x20 -- SPACE
  '\x0021' -> Right 0x21 -- EXCLAMATION MARK
  '\x0022' -> Right 0x22 -- QUOTATION MARK
  '\x0023' -> Right 0x23 -- NUMBER SIGN
  '\x0024' -> Right 0x24 -- DOLLAR SIGN
  '\x0025' -> Right 0x25 -- PERCENT SIGN
  '\x0026' -> Right 0x26 -- AMPERSAND
  '\x0027' -> Right 0x27 -- APOSTROPHE
  '\x0028' -> Right 0x28 -- LEFT PARENTHESIS
  '\x0029' -> Right 0x29 -- RIGHT PARENTHESIS
  '\x002a' -> Right 0x2a -- ASTERISK
  '\x002b' -> Right 0x2b -- PLUS SIGN
  '\x002c' -> Right 0x2c -- COMMA
  '\x002d' -> Right 0x2d -- HYPHEN-MINUS
  '\x002e' -> Right 0x2e -- FULL STOP
  '\x002f' -> Right 0x2f -- SOLIDUS
  '\x0030' -> Right 0x30 -- DIGIT ZERO
  '\x0031' -> Right 0x31 -- DIGIT ONE
  '\x0032' -> Right 0x32 -- DIGIT TWO
  '\x0033' -> Right 0x33 -- DIGIT THREE
  '\x0034' -> Right 0x34 -- DIGIT FOUR
  '\x0035' -> Right 0x35 -- DIGIT FIVE
  '\x0036' -> Right 0x36 -- DIGIT SIX
  '\x0037' -> Right 0x37 -- DIGIT SEVEN
  '\x0038' -> Right 0x38 -- DIGIT EIGHT
  '\x0039' -> Right 0x39 -- DIGIT NINE
  '\x003a' -> Right 0x3a -- COLON
  '\x003b' -> Right 0x3b -- SEMICOLON
  '\x003c' -> Right 0x3c -- LESS-THAN SIGN
  '\x003d' -> Right 0x3d -- EQUALS SIGN
  '\x003e' -> Right 0x3e -- GREATER-THAN SIGN
  '\x003f' -> Right 0x3f -- QUESTION MARK
  '\x0040' -> Right 0x40 -- COMMERCIAL AT
  '\x0041' -> Right 0x41 -- LATIN CAPITAL LETTER A
  '\x0042' -> Right 0x42 -- LATIN CAPITAL LETTER B
  '\x0043' -> Right 0x43 -- LATIN CAPITAL LETTER C
  '\x0044' -> Right 0x44 -- LATIN CAPITAL LETTER D
  '\x0045' -> Right 0x45 -- LATIN CAPITAL LETTER E
  '\x0046' -> Right 0x46 -- LATIN CAPITAL LETTER F
  '\x0047' -> Right 0x47 -- LATIN CAPITAL LETTER G
  '\x0048' -> Right 0x48 -- LATIN CAPITAL LETTER H
  '\x0049' -> Right 0x49 -- LATIN CAPITAL LETTER I
  '\x004a' -> Right 0x4a -- LATIN CAPITAL LETTER J
  '\x004b' -> Right 0x4b -- LATIN CAPITAL LETTER K
  '\x004c' -> Right 0x4c -- LATIN CAPITAL LETTER L
  '\x004d' -> Right 0x4d -- LATIN CAPITAL LETTER M
  '\x004e' -> Right 0x4e -- LATIN CAPITAL LETTER N
  '\x004f' -> Right 0x4f -- LATIN CAPITAL LETTER O
  '\x0050' -> Right 0x50 -- LATIN CAPITAL LETTER P
  '\x0051' -> Right 0x51 -- LATIN CAPITAL LETTER Q
  '\x0052' -> Right 0x52 -- LATIN CAPITAL LETTER R
  '\x0053' -> Right 0x53 -- LATIN CAPITAL LETTER S
  '\x0054' -> Right 0x54 -- LATIN CAPITAL LETTER T
  '\x0055' -> Right 0x55 -- LATIN CAPITAL LETTER U
  '\x0056' -> Right 0x56 -- LATIN CAPITAL LETTER V
  '\x0057' -> Right 0x57 -- LATIN CAPITAL LETTER W
  '\x0058' -> Right 0x58 -- LATIN CAPITAL LETTER X
  '\x0059' -> Right 0x59 -- LATIN CAPITAL LETTER Y
  '\x005a' -> Right 0x5a -- LATIN CAPITAL LETTER Z
  '\x005b' -> Right 0x5b -- LEFT SQUARE BRACKET
  '\x005c' -> Right 0x5c -- REVERSE SOLIDUS
  '\x005d' -> Right 0x5d -- RIGHT SQUARE BRACKET
  '\x005e' -> Right 0x5e -- CIRCUMFLEX ACCENT
  '\x005f' -> Right 0x5f -- LOW LINE
  '\x0060' -> Right 0x60 -- GRAVE ACCENT
  '\x0061' -> Right 0x61 -- LATIN SMALL LETTER A
  '\x0062' -> Right 0x62 -- LATIN SMALL LETTER B
  '\x0063' -> Right 0x63 -- LATIN SMALL LETTER C
  '\x0064' -> Right 0x64 -- LATIN SMALL LETTER D
  '\x0065' -> Right 0x65 -- LATIN SMALL LETTER E
  '\x0066' -> Right 0x66 -- LATIN SMALL LETTER F
  '\x0067' -> Right 0x67 -- LATIN SMALL LETTER G
  '\x0068' -> Right 0x68 -- LATIN SMALL LETTER H
  '\x0069' -> Right 0x69 -- LATIN SMALL LETTER I
  '\x006a' -> Right 0x6a -- LATIN SMALL LETTER J
  '\x006b' -> Right 0x6b -- LATIN SMALL LETTER K
  '\x006c' -> Right 0x6c -- LATIN SMALL LETTER L
  '\x006d' -> Right 0x6d -- LATIN SMALL LETTER M
  '\x006e' -> Right 0x6e -- LATIN SMALL LETTER N
  '\x006f' -> Right 0x6f -- LATIN SMALL LETTER O
  '\x0070' -> Right 0x70 -- LATIN SMALL LETTER P
  '\x0071' -> Right 0x71 -- LATIN SMALL LETTER Q
  '\x0072' -> Right 0x72 -- LATIN SMALL LETTER R
  '\x0073' -> Right 0x73 -- LATIN SMALL LETTER S
  '\x0074' -> Right 0x74 -- LATIN SMALL LETTER T
  '\x0075' -> Right 0x75 -- LATIN SMALL LETTER U
  '\x0076' -> Right 0x76 -- LATIN SMALL LETTER V
  '\x0077' -> Right 0x77 -- LATIN SMALL LETTER W
  '\x0078' -> Right 0x78 -- LATIN SMALL LETTER X
  '\x0079' -> Right 0x79 -- LATIN SMALL LETTER Y
  '\x007a' -> Right 0x7a -- LATIN SMALL LETTER Z
  '\x007b' -> Right 0x7b -- LEFT CURLY BRACKET
  '\x007c' -> Right 0x7c -- VERTICAL LINE
  '\x007d' -> Right 0x7d -- RIGHT CURLY BRACKET
  '\x007e' -> Right 0x7e -- TILDE
  '\x007f' -> Right 0x7f -- DELETE
  '\x00c7' -> Right 0x80 -- LATIN CAPITAL LETTER C WITH CEDILLA
  '\x00fc' -> Right 0x81 -- LATIN SMALL LETTER U WITH DIAERESIS
  '\x00e9' -> Right 0x82 -- LATIN SMALL LETTER E WITH ACUTE
  '\x00e2' -> Right 0x83 -- LATIN SMALL LETTER A WITH CIRCUMFLEX
  '\x00e4' -> Right 0x84 -- LATIN SMALL LETTER A WITH DIAERESIS
  '\x00e0' -> Right 0x85 -- LATIN SMALL LETTER A WITH GRAVE
  '\x00e5' -> Right 0x86 -- LATIN SMALL LETTER A WITH RING
  '\x00e7' -> Right 0x87 -- LATIN SMALL LETTER C WITH CEDILLA
  '\x00ea' -> Right 0x88 -- LATIN SMALL LETTER E WITH CIRCUMFLEX
  '\x00eb' -> Right 0x89 -- LATIN SMALL LETTER E WITH DIAERESIS
  '\x00e8' -> Right 0x8a -- LATIN SMALL LETTER E WITH GRAVE
  '\x00ef' -> Right 0x8b -- LATIN SMALL LETTER I WITH DIAERESIS
  '\x00ee' -> Right 0x8c -- LATIN SMALL LETTER I WITH CIRCUMFLEX
  '\x00ec' -> Right 0x8d -- LATIN SMALL LETTER I WITH GRAVE
  '\x00c4' -> Right 0x8e -- LATIN CAPITAL LETTER A WITH DIAERESIS
  '\x00c5' -> Right 0x8f -- LATIN CAPITAL LETTER A WITH RING
  '\x00c9' -> Right 0x90 -- LATIN CAPITAL LETTER E WITH ACUTE
  '\x00e6' -> Right 0x91 -- LATIN SMALL LIGATURE AE
  '\x00c6' -> Right 0x92 -- LATIN CAPITAL LIGATURE AE
  '\x00f4' -> Right 0x93 -- LATIN SMALL LETTER O WITH CIRCUMFLEX
  '\x00f6' -> Right 0x94 -- LATIN SMALL LETTER O WITH DIAERESIS
  '\x00f2' -> Right 0x95 -- LATIN SMALL LETTER O WITH GRAVE
  '\x00fb' -> Right 0x96 -- LATIN SMALL LETTER U WITH CIRCUMFLEX
  '\x00f9' -> Right 0x97 -- LATIN SMALL LETTER U WITH GRAVE
  '\x00ff' -> Right 0x98 -- LATIN SMALL LETTER Y WITH DIAERESIS
  '\x00d6' -> Right 0x99 -- LATIN CAPITAL LETTER O WITH DIAERESIS
  '\x00dc' -> Right 0x9a -- LATIN CAPITAL LETTER U WITH DIAERESIS
  '\x00a2' -> Right 0x9b -- CENT SIGN
  '\x00a3' -> Right 0x9c -- POUND SIGN
  '\x00a5' -> Right 0x9d -- YEN SIGN
  '\x20a7' -> Right 0x9e -- PESETA SIGN
  '\x0192' -> Right 0x9f -- LATIN SMALL LETTER F WITH HOOK
  '\x00e1' -> Right 0xa0 -- LATIN SMALL LETTER A WITH ACUTE
  '\x00ed' -> Right 0xa1 -- LATIN SMALL LETTER I WITH ACUTE
  '\x00f3' -> Right 0xa2 -- LATIN SMALL LETTER O WITH ACUTE
  '\x00fa' -> Right 0xa3 -- LATIN SMALL LETTER U WITH ACUTE
  '\x00f1' -> Right 0xa4 -- LATIN SMALL LETTER N WITH TILDE
  '\x00d1' -> Right 0xa5 -- LATIN CAPITAL LETTER N WITH TILDE
  '\x00aa' -> Right 0xa6 -- FEMININE ORDINAL INDICATOR
  '\x00ba' -> Right 0xa7 -- MASCULINE ORDINAL INDICATOR
  '\x00bf' -> Right 0xa8 -- INVERTED QUESTION MARK
  '\x2310' -> Right 0xa9 -- REVERSED NOT SIGN
  '\x00ac' -> Right 0xaa -- NOT SIGN
  '\x00bd' -> Right 0xab -- VULGAR FRACTION ONE HALF
  '\x00bc' -> Right 0xac -- VULGAR FRACTION ONE QUARTER
  '\x00a1' -> Right 0xad -- INVERTED EXCLAMATION MARK
  '\x00ab' -> Right 0xae -- LEFT-POINTING DOUBLE ANGLE QUOTATION MARK
  '\x00bb' -> Right 0xaf -- RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK
  '\x2591' -> Right 0xb0 -- LIGHT SHADE
  '\x2592' -> Right 0xb1 -- MEDIUM SHADE
  '\x2593' -> Right 0xb2 -- DARK SHADE
  '\x2502' -> Right 0xb3 -- BOX DRAWINGS LIGHT VERTICAL
  '\x2524' -> Right 0xb4 -- BOX DRAWINGS LIGHT VERTICAL AND LEFT
  '\x2561' -> Right 0xb5 -- BOX DRAWINGS VERTICAL SINGLE AND LEFT
  '\x2562' -> Right 0xb6 -- BOX DRAWINGS VERTICAL DOUBLE AND LEFT
  '\x2556' -> Right 0xb7 -- BOX DRAWINGS DOWN DOUBLE AND LEFT
  '\x2555' -> Right 0xb8 -- BOX DRAWINGS DOWN SINGLE AND LEFT
  '\x2563' -> Right 0xb9 -- BOX DRAWINGS DOUBLE VERTICAL AND LEFT
  '\x2551' -> Right 0xba -- BOX DRAWINGS DOUBLE VERTICAL
  '\x2557' -> Right 0xbb -- BOX DRAWINGS DOUBLE DOWN AND LEFT
  '\x255d' -> Right 0xbc -- BOX DRAWINGS DOUBLE UP AND LEFT
  '\x255c' -> Right 0xbd -- BOX DRAWINGS UP DOUBLE AND LEFT
  '\x255b' -> Right 0xbe -- BOX DRAWINGS UP SINGLE AND LEFT
  '\x2510' -> Right 0xbf -- BOX DRAWINGS LIGHT DOWN AND LEFT
  '\x2514' -> Right 0xc0 -- BOX DRAWINGS LIGHT UP AND RIGHT
  '\x2534' -> Right 0xc1 -- BOX DRAWINGS LIGHT UP AND HORIZONTAL
  '\x252c' -> Right 0xc2 -- BOX DRAWINGS LIGHT DOWN AND HORIZONTAL
  '\x251c' -> Right 0xc3 -- BOX DRAWINGS LIGHT VERTICAL AND RIGHT
  '\x2500' -> Right 0xc4 -- BOX DRAWINGS LIGHT HORIZONTAL
  '\x253c' -> Right 0xc5 -- BOX DRAWINGS LIGHT VERTICAL AND HORIZONTAL
  '\x255e' -> Right 0xc6 -- BOX DRAWINGS VERTICAL SINGLE AND RIGHT
  '\x255f' -> Right 0xc7 -- BOX DRAWINGS VERTICAL DOUBLE AND RIGHT
  '\x255a' -> Right 0xc8 -- BOX DRAWINGS DOUBLE UP AND RIGHT
  '\x2554' -> Right 0xc9 -- BOX DRAWINGS DOUBLE DOWN AND RIGHT
  '\x2569' -> Right 0xca -- BOX DRAWINGS DOUBLE UP AND HORIZONTAL
  '\x2566' -> Right 0xcb -- BOX DRAWINGS DOUBLE DOWN AND HORIZONTAL
  '\x2560' -> Right 0xcc -- BOX DRAWINGS DOUBLE VERTICAL AND RIGHT
  '\x2550' -> Right 0xcd -- BOX DRAWINGS DOUBLE HORIZONTAL
  '\x256c' -> Right 0xce -- BOX DRAWINGS DOUBLE VERTICAL AND HORIZONTAL
  '\x2567' -> Right 0xcf -- BOX DRAWINGS UP SINGLE AND HORIZONTAL
  '\x2568' -> Right 0xd0 -- BOX DRAWINGS UP DOUBLE AND HORIZONTAL
  '\x2564' -> Right 0xd1 -- BOX DRAWINGS DOWN SINGLE AND HORIZONTAL
  '\x2565' -> Right 0xd2 -- BOX DRAWINGS DOWN DOUBLE AND HORIZONTAL
  '\x2559' -> Right 0xd3 -- BOX DRAWINGS UP DOUBLE AND RIGHT
  '\x2558' -> Right 0xd4 -- BOX DRAWINGS UP SINGLE AND RIGHT
  '\x2552' -> Right 0xd5 -- BOX DRAWINGS DOWN SINGLE AND RIGHT
  '\x2553' -> Right 0xd6 -- BOX DRAWINGS DOWN DOUBLE AND RIGHT
  '\x256b' -> Right 0xd7 -- BOX DRAWINGS VERTICAL DOUBLE AND HORIZONTAL
  '\x256a' -> Right 0xd8 -- BOX DRAWINGS VERTICAL SINGLE AND HORIZONTAL
  '\x2518' -> Right 0xd9 -- BOX DRAWINGS LIGHT UP AND LEFT
  '\x250c' -> Right 0xda -- BOX DRAWINGS LIGHT DOWN AND RIGHT
  '\x2588' -> Right 0xdb -- FULL BLOCK
  '\x2584' -> Right 0xdc -- LOWER HALF BLOCK
  '\x258c' -> Right 0xdd -- LEFT HALF BLOCK
  '\x2590' -> Right 0xde -- RIGHT HALF BLOCK
  '\x2580' -> Right 0xdf -- UPPER HALF BLOCK
  '\x03b1' -> Right 0xe0 -- GREEK SMALL LETTER ALPHA
  '\x00df' -> Right 0xe1 -- LATIN SMALL LETTER SHARP S
  '\x0393' -> Right 0xe2 -- GREEK CAPITAL LETTER GAMMA
  '\x03c0' -> Right 0xe3 -- GREEK SMALL LETTER PI
  '\x03a3' -> Right 0xe4 -- GREEK CAPITAL LETTER SIGMA
  '\x03c3' -> Right 0xe5 -- GREEK SMALL LETTER SIGMA
  '\x00b5' -> Right 0xe6 -- MICRO SIGN
  '\x03c4' -> Right 0xe7 -- GREEK SMALL LETTER TAU
  '\x03a6' -> Right 0xe8 -- GREEK CAPITAL LETTER PHI
  '\x0398' -> Right 0xe9 -- GREEK CAPITAL LETTER THETA
  '\x03a9' -> Right 0xea -- GREEK CAPITAL LETTER OMEGA
  '\x03b4' -> Right 0xeb -- GREEK SMALL LETTER DELTA
  '\x221e' -> Right 0xec -- INFINITY
  '\x03c6' -> Right 0xed -- GREEK SMALL LETTER PHI
  '\x03b5' -> Right 0xee -- GREEK SMALL LETTER EPSILON
  '\x2229' -> Right 0xef -- INTERSECTION
  '\x2261' -> Right 0xf0 -- IDENTICAL TO
  '\x00b1' -> Right 0xf1 -- PLUS-MINUS SIGN
  '\x2265' -> Right 0xf2 -- GREATER-THAN OR EQUAL TO
  '\x2264' -> Right 0xf3 -- LESS-THAN OR EQUAL TO
  '\x2320' -> Right 0xf4 -- TOP HALF INTEGRAL
  '\x2321' -> Right 0xf5 -- BOTTOM HALF INTEGRAL
  '\x00f7' -> Right 0xf6 -- DIVISION SIGN
  '\x2248' -> Right 0xf7 -- ALMOST EQUAL TO
  '\x00b0' -> Right 0xf8 -- DEGREE SIGN
  '\x2219' -> Right 0xf9 -- BULLET OPERATOR
  '\x00b7' -> Right 0xfa -- MIDDLE DOT
  '\x221a' -> Right 0xfb -- SQUARE ROOT
  '\x207f' -> Right 0xfc -- SUPERSCRIPT LATIN SMALL LETTER N
  '\x00b2' -> Right 0xfd -- SUPERSCRIPT TWO
  '\x25a0' -> Right 0xfe -- BLACK SQUARE
  '\x00a0' -> Right 0xff -- NO-BREAK SPACE

  _ -> Left c
