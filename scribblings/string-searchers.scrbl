#lang scribble/manual
@require[@for-label[string-searchers
                    typed/racket/base]]

@title{String Searching Algorithms}
@author[@author+email["Shawn Wagner" "shawnw.mobile@gmail.com"]]

@local-table-of-contents[#:style 'immediate-only]

@defmodule[string-searchers]

Provides a variety of string search algorithms written in Typed
Racket. They look for sequences of exact code points or bytes, not
equivalencies. When using with non-ASCII text, consider normalizing
strings first.

@section{Searching for single strings}

Functions for searching for a single substring in strings.

@subsection{Knuth-Morris-Pratt}

Search for strings using the
@hyperlink["https://en.wikipedia.org/wiki/Knuth%E2%80%93Morris%E2%80%93Pratt_algorithm"]{Knuth-Morris-Pratt}
algorithm. These functions are also available in the
@racket[string-searchers/kmp] module, without the @racketidfont{kmp-} prefix.

@defproc[(kmp-string-contains [haystack String] [needle String] [start Index 0] [end Index (string-length haystack)])
         (Option Index)]{

Returns the first index of haystack where needle occurs, or
@code{#f} if not found. If searching for the same substring many
times, prefer compiling a matcher object and using the following
functions instead.

}

@defproc[(kmp-string-contains-ci [haystack String] [needle String] [start Index 0] [end Index (string-length haystack)])
         (Option Index)]{

Returns the first index of haystack where needle occurs, ignoring
case, or @code{#f} if not found. If searching for the same substring
many times, prefer compiling a matcher object and using the following
functions instead.

}

@defproc[(kmp-make-matcher [pattern String] [#:case-insensitive case-insensitive? Boolean #f]) kmp-matcher]{

Compiles a search string into a matcher object, optionally using
case-insensitive searching.

}

@defproc[(kmp-matcher? [obj Any]) Boolean]{

Returns true if its argument is a @racketidfont{kmp-matcher} object.

}

@defproc[(kmp-matcher-ci? [m kmp-matcher]) Boolean]{

Tests if a matcher is case-insensitive or not

}

@defproc[(kmp-matcher-pattern [m kmp-matcher]) String]{

Returns the search string for this matcher.

}

@defproc[(kmp-find-string [m kmp-matcher] [text String] [start Index 0] [end Index (string-length text)])
         (Option Index)]{

Return the index of the first occurance of the matcher's search string
in text, or @code{#f} if not found.

}

@defproc[(kmp-find-all-strings [m kmp-matcher] [text String] [start Index 0] [end Index (string-length text)] [#:overlap overlap? Boolean #t])
         (Listof Index)]{

Return the indexs of all occurances of the matcher's search string in
text, or an empty list if not found. If the overlap option is true,
found matches can overlap - searching for @code{"bb"} in @code{"bbb"}
will return @code{'(0 1)} when true, @code{(0)} when false.

}

@subsection{Boyer-Moore-Horspool}

Search for strings using the
@hyperlink["https://en.wikipedia.org/wiki/Boyer%E2%80%93Moore%E2%80%93Horspool_algorithm"]{Boyer-Moore-Horspool}
algorithm. These functions are also available in the
@racket[string-searchers/bmh] module, without the @racketidfont{bmh-}
prefix.

@subsubsection{Strings}

@defproc[(bmh-string-contains [haystack String] [needle String] [start Index 0] [end Index (string-length haystack)])
         (Option Index)]{

Returns the first index of haystack where needle occurs, or
@racket[#f] if not found. If searching for the same substring many
times, prefer compiling a matcher object and using the following
functions instead.

}

@defproc[(bmh-string-contains-ci [haystack String] [needle String] [start Index 0] [end Index (string-length haystack)])
         (Option Index)]{

Returns the first index of haystack where needle occurs, ignoring
case, or @racket[#f] if not found. If searching for the same substring
many times, prefer compiling a matcher object and using the following
functions instead.

}

@defproc[(bmh-make-matcher [pattern String] [#:case-insensitive case-insensitive? Boolean #f]) bmh-matcher]{

Compiles a search string into a matcher object, optionally using
case-insensitive searching.

}

@defproc[(bmh-matcher? [obj Any]) Boolean]{

Returns true if its argument is a @racketidfont{bmh-matcher} object.

}

@defproc[(bmh-matcher-ci? [m bmh-matcher]) Boolean]{

Tests if a matcher is case-insensitive or not

}

@defproc[(bmh-matcher-pattern [m bmh-matcher]) String]{

Returns the search string for this matcher.

}

@defproc[(bmh-find-string [m bmh-matcher] [text String] [start Index 0] [end Index (string-length text)])
         (Option Index)]{

Return the index of the first occurance of the matcher's search string
in text, or @racket[#f] if not found.

}

@defproc[(bmh-find-all-strings [m bmh-matcher] [text String] [start Index 0] [end Index (string-length text)] [#:overlap overlap? Boolean #t])
         (Listof Index)]{

Return the indexes of all occurances of the matcher's search string in
text, or an empty list if not found. If the overlap option is true,
found matches can overlap - searching for @code{"bb"} in @code{"bbb"}
will return @code{'(0 1)} when true, @code{'(0)} when false.

}

@subsubsection{Byte Strings}

Note: There are no case-insensitive routines for byte strings.

@defproc[(bmh-byte-string-contains [haystack Bytes] [needle Bytes] [start Index 0] [end Index (bytes-length haystack)])
         (Option Index)]{

Returns the first index of haystack where needle occurs, or
@racket[#f] if not found. If searching for the same substring many
times, prefer compiling a matcher object and using the following
functions instead.

}

@defproc[(bmh-make-byte-matcher [pattern Bytes]) bmh-byte-matcher]{

Return a new matcher that searches for the given byte string.

}

@defproc[(bmh-byte-matcher? [obj Any]) Boolean]{

Returns true if its argument is a @racketidfont{bmh-byte-matcher} object.

}

@defproc[(bmh-byte-matcher-pattern [m bmh-byte-matcher]) Bytes]{

Returns the search byte string for this matcher.

}

@defproc[(bmh-find-byte-string [m bmh-byte-matcher] [text Bytes] [start Index 0] [end Index (bytes-length text)])
         (Option Index)]{

Return the index of the first occurance of the matcher's search byte
string in text, or @racket[#f] if not found.

}

@defproc[(bmh-find-all-byte-strings [m bmh-byte-matcher] [text Bytes] [start Index 0] [end Index (bytes-length text)] [#:overlap overlap? Boolean #t])
         (Listof Index)]{

Return the indexes of all occurances of the matcher's search byte
string in text, or an empty list if not found. If the overlap option
is true, found matches can overlap - searching for @code{#"bb"} in
@code{#"bbb"} will return @code{'(0 1)} when true, @code{'(0)} when
false.

}

@section{Searching for multiple strings}

Sections for searching for any of multiple different substrings in a string.

@subsection{Aho-Corasick}

Search for strings using the
@hyperlink["https://en.wikipedia.org/wiki/Aho%E2%80%93Corasick_algorithm"]{Aho-Corasick}
algorithm. These functions are also available in the
@racket[string-searchers/ahoc] module, without the
@racketidfont{ahoc-} prefix.

@defproc[(ahoc-make-matcher [s0 String] [s1 String] ...) ahoc-matcher]{

Create a new Aho-Corasick matcher object that looks for the given string(s).

}

@defproc[(ahoc-matcher? [obj Any]) Boolean]{

Tests if its argument is an Aho-Corasick matcher or not.

}

@defproc[(ahoc-matcher-patterns [m ahoc-matcher]) (Listof String)]{

Return the search strings this matcher looks for.

}


@defproc[(ahoc-find-string [m ahoc-matcher] [text String]) (Option (Pair Index String))]{

Returns the first index of a matched string and which string it is, or
@code{#f} if there are no matches.

}

@defproc[(ahoc-find-allstrings [m ahoc-matcher] [text String]) (List (Pair Index String))]{

Returns the locations of all matched strings, or an empty list if none
match. If one of the search strings is a prefix of another, multiple
results can have the same index.

}